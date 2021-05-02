// TODO(jack) Set velocity and groove.

mod midi;
mod seq;

use std::collections::BTreeMap;
use std::default::Default;
use std::io::{stdin, stdout, Write};
use std::mem::replace;
use std::sync::atomic::{AtomicBool, Ordering};
use std::thread::spawn;

use crossbeam::channel::{Receiver, Sender};
use crossbeam::select;
use termion::cursor::HideCursor;
use termion::event::{Event, Key};
use termion::input::{MouseTerminal, TermRead};
use termion::raw::IntoRawMode;
use termion::screen::AlternateScreen;
use tinyvec::ArrayVec;

fn euclid(pulses: usize, steps: usize, rotate: usize) -> impl Iterator<Item = bool> {
    let mut acc = (steps - 1 + rotate * pulses) % steps;
    (0..steps).map(move |_| {
        acc += pulses;
        if acc >= steps {
            acc %= steps;
            true
        } else {
            false
        }
    })
}

struct NotificationHandler {
    shutdown_tx: crossbeam::channel::Sender<()>,
}

impl jack::NotificationHandler for NotificationHandler {
    fn shutdown(&mut self, _status: jack::ClientStatus, _reason: &str) {
        self.shutdown_tx.send(()).unwrap();
    }
}

struct ProcessHandler<'a> {
    midi_in_port: jack::Port<jack::MidiIn>,
    midi_out_port: jack::Port<jack::MidiOut>,

    held_incoming_midi_notes: [[Option<(f64, f64)>; 128]; 16],
    held_outgoing_midi_notes: [[i32; 128]; 16],

    midi_event_queue: Vec<(f64, midi::Event)>,

    clock: f64,

    seq: Vec<(f64, seq::Event)>,
    seq_rx: Receiver<Vec<(f64, seq::Event)>>,

    seq_event_tx: Sender<(f64, seq::Event)>,
    state_tx: Sender<f64>,

    flush: &'a AtomicBool,
    record: &'a AtomicBool,

    shutdown_tx: Sender<()>,
}

impl<'a> jack::ProcessHandler for ProcessHandler<'a> {
    fn process(
        &mut self,
        client: &jack::Client,
        process_scope: &jack::ProcessScope,
    ) -> jack::Control {
        let Self {
            midi_in_port,
            midi_out_port,
            held_incoming_midi_notes,
            held_outgoing_midi_notes,
            midi_event_queue,
            clock,
            seq,
            seq_rx,
            seq_event_tx,
            state_tx,
            flush,
            record,
            shutdown_tx,
        } = self;
        if let Some(new_seq) = seq_rx.try_iter().last() {
            *seq = new_seq;
        }

        let mut writer = midi_out_port.writer(process_scope);
        if flush.load(Ordering::Relaxed) {
            for (channel, notes) in held_outgoing_midi_notes.iter_mut().enumerate() {
                for (note, count) in notes.iter_mut().enumerate() {
                    for _ in 0..*count {
                        writer
                            .write(&jack::RawMidi {
                                time: 0,
                                bytes: midi::Event::NoteOff {
                                    channel: channel as u8,
                                    note: note as u8,
                                }
                                .to_bytes()
                                .as_slice(),
                            })
                            .unwrap();
                    }
                    *count = 0;
                }
            }
            shutdown_tx.send(()).unwrap();
            return jack::Control::Continue; // Quit causes problems.
        }

        let bpm = 75.0;
        let beats = 4.0;
        let dt = bpm / 60.0 / beats * client.buffer_size() as f64 / client.sample_rate() as f64;

        for jack::RawMidi {
            time: sample,
            bytes,
        } in midi_in_port.iter(process_scope)
        {
            if let Some(event) = midi::Event::from_bytes(bytes) {
                let time = sample as f64 / client.buffer_size() as f64 * dt;
                midi_event_queue.push((time, event));
                if record.load(Ordering::Relaxed) {
                    let time = *clock + time;
                    match event {
                        midi::Event::NoteOff { channel, note } => {
                            if let Some((start, velocity)) =
                                held_incoming_midi_notes[channel as usize][note as usize].take()
                            {
                                seq_event_tx
                                    .send((
                                        start,
                                        seq::Event::Note {
                                            channel,
                                            note,
                                            velocity,
                                            length: (time - start).rem_euclid(1.0),
                                        },
                                    ))
                                    .unwrap();
                            }
                        }
                        midi::Event::NoteOn {
                            channel,
                            note,
                            velocity,
                        } => {
                            held_incoming_midi_notes[channel as usize][note as usize] =
                                Some((time, (velocity as f64 - 1.0) / 126.0));
                        }
                        midi::Event::ControlChange {
                            channel,
                            controller,
                            value,
                        } => {
                            seq_event_tx
                                .send((
                                    time,
                                    seq::Event::ControlChange {
                                        channel,
                                        controller,
                                        value,
                                    },
                                ))
                                .unwrap();
                        }
                    }
                }
            }
        }

        // Schedule MIDI events from the sequence.
        fn schedule(midi_event_queue: &mut Vec<(f64, midi::Event)>, t: f64, event: seq::Event) {
            match event {
                seq::Event::Note {
                    channel,
                    note,
                    velocity,
                    length,
                } => {
                    midi_event_queue.push((
                        t,
                        midi::Event::NoteOn {
                            channel,
                            note,
                            velocity: 1 + (126.0 * velocity) as u8,
                        },
                    ));
                    midi_event_queue.push((t + length, midi::Event::NoteOff { channel, note }));
                }
                seq::Event::ControlChange {
                    channel,
                    controller,
                    value,
                } => {
                    midi_event_queue.push((
                        t,
                        midi::Event::ControlChange {
                            channel,
                            controller,
                            value,
                        },
                    ));
                }
            }
        }
        let t1 = *clock;
        let t2 = *clock + dt;
        *clock = if t2 < 1.0 {
            for &mut (time, event) in seq {
                if t1 <= time && time < t2 {
                    schedule(midi_event_queue, time - t1, event);
                }
            }
            t2
        } else {
            let t2 = t2 % 1.0;
            for &mut (time, event) in seq {
                let t = match time {
                    t if t1 <= t => t - t1,
                    t if t < t2 => t + 1.0 - t1,
                    _ => continue,
                };
                schedule(midi_event_queue, t, event);
            }
            t2
        };
        state_tx.send(*clock).unwrap();

        midi_event_queue.sort_unstable_by(|a, b| b.partial_cmp(a).unwrap());
        while let Some((time, event)) = midi_event_queue.pop() {
            let sample = (time / dt * client.buffer_size() as f64) as u32;
            if sample >= client.buffer_size() {
                midi_event_queue.push((time, event));
                break;
            }
            writer
                .write(&jack::RawMidi {
                    time: sample,
                    bytes: event.to_bytes().as_slice(),
                })
                .unwrap();
            match event {
                midi::Event::NoteOff { channel, note } => {
                    held_outgoing_midi_notes[channel as usize][note as usize] -= 1
                }
                midi::Event::NoteOn { channel, note, .. } => {
                    held_outgoing_midi_notes[channel as usize][note as usize] += 1
                }
                _ => (),
            }
        }

        for (time, _) in midi_event_queue {
            *time -= dt;
        }

        jack::Control::Continue
    }
}

static FLUSH: AtomicBool = AtomicBool::new(false);
static RECORD: AtomicBool = AtomicBool::new(true);

#[derive(Copy, Clone, Debug, Default)]
struct Step {
    velocity: f64,
    length: f64,
    delay: f64,
}

const MAX_STEPS: usize = 32;
type Track = ArrayVec<[Option<Step>; MAX_STEPS]>;

type Channel = u8;
type Note = u8;

#[derive(Clone, Default)]
struct Tracks(BTreeMap<(Channel, Note), Track>);

impl Tracks {
    fn transpose(&mut self, degrees: i32) {
        let Tracks(tracks) = self;
        *tracks = tracks
            .iter()
            .map(|(&(channel, note), &steps)| {
                let intervals = [2, 2, 1, 2, 2, 2, 1];
                let octave = note / 12;
                let degree = intervals
                    .iter()
                    .enumerate()
                    .take_while({
                        let mut acc = 0;
                        move |&(_, &x)| {
                            acc += x;
                            acc <= note % 12
                        }
                    })
                    .last()
                    .map_or(0, |(i, _)| i + 1);
                let constrained_note: i32 =
                    ((octave * 7) as i32 + degree as i32 + degrees).clamp(0, 62); // I did some back-of-the-napkin math.
                let octave = constrained_note / 7;
                let degree = constrained_note % 7;
                let new_note =
                    12 * octave + intervals.iter().take(degree as usize).sum::<u8>() as i32;
                ((channel, new_note as u8), steps)
            })
            .collect();
    }
    fn build_seq(&self) -> Vec<(f64, seq::Event)> {
        let mut seq: Vec<_> = self
            .0
            .iter()
            .flat_map(|(&(channel, note), steps)| {
                steps.iter().enumerate().filter_map(move |(i, step)| {
                    step.map(|step| (channel, note, i as f64 / steps.len() as f64, step))
                })
            })
            .map(
                |(
                    channel,
                    note,
                    t,
                    Step {
                        velocity,
                        length,
                        delay,
                    },
                )| {
                    (
                        t + delay,
                        seq::Event::Note {
                            channel,
                            note,
                            length,
                            velocity,
                        },
                    )
                },
            )
            .collect();
        seq.sort_unstable_by(|a, b| a.partial_cmp(b).unwrap());
        seq
    }
}

struct Ui {
    record: bool,
    clock: u16,
    focus_row: usize,
    focus_step: usize,

    tracks: Tracks,
    undo: Vec<Tracks>,
    redo: Vec<Tracks>,
}

impl Ui {
    fn print_header(&self, writer: &mut impl Write) -> std::io::Result<()> {
        write!(
            writer,
            "{}{}",
            termion::cursor::Goto(1, 1),
            termion::clear::CurrentLine
        )?;
        if self.record {
            write!(
                writer,
                "{}{}[record]{}{}\r\n",
                termion::color::Bg(termion::color::Red),
                termion::color::Fg(termion::color::Black),
                termion::color::Bg(termion::color::Reset),
                termion::color::Fg(termion::color::Reset),
            )?;
        }
        Ok(())
    }
    fn print_tracks(&self, writer: &mut impl Write) -> std::io::Result<()> {
        write!(writer, "{}", termion::cursor::Goto(1, 2))?;
        let mut last_channel = None;
        for (row, (&(channel, note), steps)) in self.tracks.0.iter().enumerate() {
            if last_channel
                .map(|last_channel| last_channel != channel)
                .unwrap_or(true)
            {
                write!(writer, "c{:02} ", channel)?;
                last_channel = Some(channel);
            } else {
                write!(writer, "    ")?;
            }
            write!(writer, "n{:03} |", note)?;
            for (i, (spaces, step)) in euclid(steps.len(), MAX_STEPS, 0)
                .filter_map({
                    let mut acc = 1;
                    move |pulse| {
                        if pulse {
                            let spaces = acc;
                            acc = 0;
                            Some(spaces)
                        } else {
                            acc += 1;
                            None
                        }
                    }
                })
                .zip(steps)
                .enumerate()
            {
                if row == self.focus_row {
                    if i == self.focus_step {
                        write!(
                            writer,
                            "{}{}",
                            termion::color::Bg(termion::color::Red),
                            termion::color::Fg(termion::color::Black),
                        )?;
                    } else {
                        write!(
                            writer,
                            "{}{}",
                            termion::color::Bg(termion::color::Blue),
                            termion::color::Fg(termion::color::Black),
                        )?;
                    }
                }
                for _ in 0..spaces {
                    write!(writer, " ")?;
                }
                let ch = if step.is_some() { "." } else { " " };
                write!(
                    writer,
                    "{}{}{}",
                    ch,
                    termion::color::Bg(termion::color::Reset),
                    termion::color::Fg(termion::color::Reset),
                )?;
            }
            write!(writer, "|\r\n")?;
        }

        // Print step information to the right.
        let x = 12 + MAX_STEPS as u16;
        let y = 2;
        if let Some(step) = self
            .tracks
            .0
            .values()
            .nth(self.focus_row)
            .and_then(|steps| steps.get(self.focus_step).copied())
            .flatten()
        {
            write!(
                writer,
                "{}v={}{}l={}{}d={}",
                termion::cursor::Goto(x, y),
                step.velocity,
                termion::cursor::Goto(x, y + 1),
                step.length,
                termion::cursor::Goto(x, y + 2),
                step.delay,
            )?;
        } else {
            write!(
                writer,
                "{}{}{}{}{}{}",
                termion::cursor::Goto(x, y),
                termion::clear::UntilNewline,
                termion::cursor::Goto(x, y + 1),
                termion::clear::UntilNewline,
                termion::cursor::Goto(x, y + 2),
                termion::clear::UntilNewline,
            )?;
        }

        Ok(())
    }
    fn print_clock(&self, writer: &mut impl Write) -> std::io::Result<()> {
        let row = 2 + self.tracks.0.iter().count() as u16;
        write!(
            writer,
            "{}{}^\r\n",
            termion::cursor::Goto(10 + self.clock, row),
            termion::clear::CurrentLine,
        )?;
        Ok(())
    }
}

fn main() {
    let (client, _) = jack::Client::new(
        "cord",
        jack::ClientOptions::NO_START_SERVER | jack::ClientOptions::USE_EXACT_NAME,
    )
    .unwrap();
    let midi_in_port = client.register_port("in", jack::MidiIn::default()).unwrap();
    let midi_out_port = client
        .register_port("out", jack::MidiOut::default())
        .unwrap();

    let (shutdown_tx, shutdown_rx) = crossbeam::channel::bounded(2);
    let (seq_tx, seq_rx) = crossbeam::channel::bounded(0);
    let (seq_event_tx, seq_event_rx) = crossbeam::channel::bounded(256);
    let (state_tx, state_rx) = crossbeam::channel::bounded(1024);
    let async_client = client
        .activate_async(
            NotificationHandler {
                shutdown_tx: shutdown_tx.clone(),
            },
            ProcessHandler {
                midi_in_port,
                midi_out_port,
                held_incoming_midi_notes: [[None; 128]; 16],
                held_outgoing_midi_notes: [[0; 128]; 16],
                midi_event_queue: Vec::with_capacity(64),
                clock: 0.0,
                seq: Vec::new(),
                seq_rx,
                seq_event_tx,
                state_tx,
                flush: &FLUSH,
                record: &RECORD,
                shutdown_tx,
            },
        )
        .unwrap();

    for (source_port, destination_port) in &[
        ("jack-keyboard:midi_out", "cord:in"),
        ("cord:out", "csound6-midi_in:port"),
    ] {
        async_client
            .as_client()
            .connect_ports_by_name(source_port, destination_port)
            .unwrap();
    }

    let default_hook = std::panic::take_hook();
    std::panic::set_hook(Box::new(move |info| {
        println!("{}", termion::screen::ToMainScreen);
        default_hook(info);
    }));
    let stdin = stdin();
    let mut stdout = {
        let stdout = stdout().into_raw_mode().unwrap();
        let stdout = AlternateScreen::from(stdout);
        let stdout = MouseTerminal::from(stdout);
        HideCursor::from(stdout)
    };
    let (event_tx, event_rx) = crossbeam::channel::bounded(0);
    spawn(move || {
        for event in stdin.events() {
            let event = event.unwrap();
            if event_tx.send(event).is_err() {
                return;
            }
        }
    });

    let mut ui = Ui {
        record: RECORD.load(Ordering::Relaxed),
        clock: 0,
        focus_row: 0,
        focus_step: 0,

        tracks: Tracks::default(),
        undo: Vec::new(),
        redo: Vec::new(),
    };
    seq_tx.send(ui.tracks.build_seq()).unwrap();

    ui.print_header(&mut stdout).unwrap();

    'running: loop {
        select! {
            recv(shutdown_rx) -> _ => return,
            recv(seq_event_rx) -> result => {
                let (time, event) = result.unwrap();
                match event {
                    seq::Event::Note {channel, note, velocity, length} => {
                        let mut steps = ui.tracks.0.get(&(channel, note)).copied().unwrap_or((0..16).map(|_| None).collect());
                        let step = (time * steps.len() as f64) as usize;
                        let delay = time % (1.0 / steps.len() as f64);
                        steps[step] = Some(Step{velocity, length, delay});
                        ui.tracks.0.insert((channel, note), steps);
                        seq_tx.send(ui.tracks.build_seq()).unwrap();
                    },
                    _ => continue 'running,
                }
                ui.print_tracks(&mut stdout).unwrap();
            },
            recv(event_rx) -> result => {
                let event = result.unwrap();
                match event {
                    Event::Key(Key::Ctrl('c')) => break 'running,
                    Event::Key(Key::Delete) => {
                        ui.undo.push(ui.tracks.clone());
                        ui.tracks = Tracks::default();
                    },
                    Event::Key(Key::Char('Q')) => {
                        ui.undo.push(ui.tracks.clone());
                        for step in ui.tracks.0.values_mut().flat_map(|steps| steps.iter_mut()).filter_map(|step| step.as_mut()) {
                            step.delay = 0.0;
                        }
                        seq_tx.send(ui.tracks.build_seq()).unwrap();
                    },
                    Event::Key(Key::Ctrl('l')) => {
                        write!(stdout, "{}", termion::clear::All).unwrap();
                        ui.print_header(&mut stdout).unwrap();
                        ui.print_tracks(&mut stdout).unwrap();
                        ui.print_clock(&mut stdout).unwrap();
                    }
                    Event::Key(Key::Char('<')) => {
                        ui.undo.push(ui.tracks.clone());
                        ui.tracks.transpose(-1);
                        seq_tx.send(ui.tracks.build_seq()).unwrap();
                        ui.print_tracks(&mut stdout).unwrap();
                    }
                    Event::Key(Key::Char('>')) => {
                        ui.undo.push(ui.tracks.clone());
                        ui.tracks.transpose(1);
                        seq_tx.send(ui.tracks.build_seq()).unwrap();
                        ui.print_tracks(&mut stdout).unwrap();
                    }
                    Event::Key(Key::Char('r')) => {
                        ui.record = !ui.record;
                        RECORD.store(ui.record, Ordering::Relaxed);
                        ui.print_header(&mut stdout).unwrap();
                    }
                    Event::Key(Key::Char('h')) => {
                        if let Some(steps) = ui.tracks.0.values().nth(ui.focus_row) {
                            ui.focus_step = ui.focus_step.checked_sub(1).unwrap_or(steps.len() - 1);
                        }
                        ui.print_tracks(&mut stdout).unwrap();
                    }
                    Event::Key(Key::Char('l')) => {
                        if let Some(steps) = ui.tracks.0.values().nth(ui.focus_row) {
                            ui.focus_step = (ui.focus_step + 1) % steps.len();
                        }
                        ui.print_tracks(&mut stdout).unwrap();
                    }
                    Event::Key(Key::Char('k')) => {
                        ui.focus_row = ui.focus_row.checked_sub(1).unwrap_or_else(|| ui.tracks.0.iter().count() - 1);
                        ui.print_tracks(&mut stdout).unwrap();
                    }
                    Event::Key(Key::Char('j')) => {
                        let rows = ui.tracks.0.iter().count();
                        if rows > 0 {
                            ui.focus_row = (ui.focus_row + 1) % rows;
                            ui.print_tracks(&mut stdout).unwrap();
                        }
                    }
                    Event::Key(Key::Char('u')) => {
                        if let Some(tracks) = ui.undo.pop() {
                            let tracks = replace(&mut ui.tracks, tracks);
                            ui.redo.push(tracks);
                            ui.print_tracks(&mut stdout).unwrap();
                            seq_tx.send(ui.tracks.build_seq()).unwrap();
                        }
                    }
                    Event::Key(Key::Ctrl('r')) => {
                        if let Some(tracks) = ui.redo.pop() {
                            let tracks = replace(&mut ui.tracks, tracks);
                            ui.undo.push(tracks);
                            ui.print_tracks(&mut stdout).unwrap();
                            seq_tx.send(ui.tracks.build_seq()).unwrap();
                        }
                    }
                    _ => (),
                }
            },
            recv(state_rx) -> result => {
                let clock = result.unwrap();
                let step = (clock * 32 as f64) as u16;
                if ui.clock != step {
                    ui.clock = step;
                    ui.print_clock(&mut stdout).unwrap();
                }
            }
        }
    }

    FLUSH.store(true, Ordering::Relaxed);
    shutdown_rx.recv().unwrap();
}
