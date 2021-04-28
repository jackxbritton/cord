mod midi;
mod seq;

use std::default::Default;
use std::io::{stdin, stdout, Write};
use std::iter::{once, repeat};
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

#[derive(Copy, Clone, Debug, Default)]
struct Step {
    velocity: f64,
    length: f64,
    delay: f64,
}

const MAX_STEPS: usize = 16;
type Track = ArrayVec<[Option<Step>; MAX_STEPS]>;
type Tracks = [[Option<Track>; 128]; 16];

struct Ui {
    step: u16,
    tracks: Tracks,
}

impl Ui {
    fn iter_tracks(&self) -> impl Iterator<Item = (u8, u8, &Track)> + '_ {
        self.tracks.iter().enumerate().flat_map(|(channel, notes)| {
            notes.iter().enumerate().filter_map(move |(note, steps)| {
                steps
                    .as_ref()
                    .map(|steps| (channel as u8, note as u8, steps))
            })
        })
    }
    fn transpose(&mut self, degrees: i32) {
        let mut new_tracks: Tracks = [[None; 128]; 16];
        for (channel, note, steps) in self.iter_tracks() {
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
            let new_note = 12 * octave + intervals.iter().take(degree as usize).sum::<u8>() as i32;
            new_tracks[channel as usize][new_note as usize] = Some(*steps);
        }
        self.tracks = new_tracks;
    }
    fn print_tracks(&self, writer: &mut impl Write) -> std::io::Result<()> {
        write!(writer, "{}", termion::cursor::Goto(1, 1))?;
        for (channel, notes) in self.tracks.iter().enumerate() {
            for ((note, steps), first) in notes
                .iter()
                .enumerate()
                .filter_map(|(note, steps)| steps.map(|steps| (note, steps)))
                .zip(once(true).chain(repeat(false)))
            {
                if first {
                    write!(writer, "c{:02} ", channel)?;
                } else {
                    write!(writer, "    ")?;
                }
                write!(writer, "n{:03} |", note)?;
                for step in steps.iter() {
                    write!(writer, "{}", if step.is_some() { "." } else { " " })?;
                }
                write!(writer, "|\r\n")?;
            }
        }
        Ok(())
    }
    fn print_cursor(&self, writer: &mut impl Write) -> std::io::Result<()> {
        let row = 1 + self
            .tracks
            .iter()
            .flat_map(|notes| notes.iter())
            .filter_map(|steps| steps.as_ref())
            .count() as u16;
        write!(
            writer,
            "{}{}^\r\n",
            termion::cursor::Goto(10 + self.step, row),
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
        step: 0,
        tracks: [[None; 128]; 16],
    };
    seq_tx.send(tracks_to_seq(&ui)).unwrap();

    fn tracks_to_seq(ui: &Ui) -> Vec<(f64, seq::Event)> {
        let mut seq: Vec<_> = ui
            .iter_tracks()
            .flat_map(|(channel, note, steps)| {
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

    'running: loop {
        select! {
            recv(shutdown_rx) -> _ => return,
            recv(seq_event_rx) -> result => {
                let (time, event) = result.unwrap();
                match event {
                    seq::Event::Note {channel, note, velocity, length} => {
                        let mut steps = ui.tracks[channel as usize][note as usize].unwrap_or(ArrayVec::from([None; 16]));
                        let step = (time * steps.len() as f64) as usize;
                        let delay = time % (1.0 / steps.len() as f64);
                        steps[step] = Some(Step{velocity, length, delay});
                        ui.tracks[channel as usize][note as usize] = Some(steps);
                        seq_tx.send(tracks_to_seq(&ui)).unwrap();
                    },
                    _ => continue 'running,
                }
                ui.print_tracks(&mut stdout).unwrap();
            },
            recv(event_rx) -> result => {
                let event = result.unwrap();
                match event {
                    Event::Key(Key::Ctrl('c')) => break 'running,
                    Event::Key(Key::Delete) => ui.tracks = [[None; 128]; 16],
                    Event::Key(Key::Char('Q')) => {
                        let steps = ui.tracks
                            .iter_mut()
                            .flat_map(|notes| notes.iter_mut())
                            .filter_map(|steps| steps.as_mut())
                            .flat_map(|steps| steps.iter_mut())
                            .filter_map(|step| step.as_mut());
                        for step in steps {
                            step.delay = 0.0;
                        }
                        seq_tx.send(tracks_to_seq(&ui)).unwrap();
                    },
                    Event::Key(Key::Ctrl('l')) => {
                        write!(stdout, "{}", termion::clear::All).unwrap();
                        ui.print_tracks(&mut stdout).unwrap();
                        ui.print_cursor(&mut stdout).unwrap();
                    }
                    Event::Key(Key::Char('<')) => {
                        ui.transpose(-1);
                        seq_tx.send(tracks_to_seq(&ui)).unwrap();
                        ui.print_tracks(&mut stdout).unwrap();
                    }
                    Event::Key(Key::Char('>')) => {
                        ui.transpose(1);
                        seq_tx.send(tracks_to_seq(&ui)).unwrap();
                        ui.print_tracks(&mut stdout).unwrap();
                    }
                    _ => (),
                }
            },
            recv(state_rx) -> result => {
                let clock = result.unwrap();
                let step = (clock * 16 as f64) as u16;
                if ui.step != step {
                    ui.step = step;
                    ui.print_cursor(&mut stdout).unwrap();
                }
            }
        }
    }

    FLUSH.store(true, Ordering::Relaxed);
    shutdown_rx.recv().unwrap();
}
