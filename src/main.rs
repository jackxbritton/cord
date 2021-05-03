// TODO(jack) Set velocity and groove.

mod midi;
mod seq;

use std::collections::{BTreeMap, HashMap};
use std::default::Default;
use std::io::{stdin, stdout};
use std::iter::{once, repeat};
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

    held_incoming_midi_notes: HashMap<(Channel, Note), (f64, f64)>,
    held_outgoing_midi_notes: HashMap<(Channel, Note), i32>,

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
            for (&(channel, note), count) in held_outgoing_midi_notes {
                for _ in 0..*count {
                    writer
                        .write(&jack::RawMidi {
                            time: 0,
                            bytes: midi::Event::NoteOff { channel, note }.to_bytes().as_slice(),
                        })
                        .unwrap();
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
                                held_incoming_midi_notes.remove(&(channel, note))
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
                            held_incoming_midi_notes
                                .insert((channel, note), (time, (velocity as f64 - 1.0) / 126.0));
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
                    *held_outgoing_midi_notes.entry((channel, note)).or_insert(0) -= 1;
                }
                midi::Event::NoteOn { channel, note, .. } => {
                    *held_outgoing_midi_notes.entry((channel, note)).or_insert(0) += 1;
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

#[derive(Clone, Default)]
struct Tracks(BTreeMap<Channel, BTreeMap<Note, Track>>);

impl Tracks {
    fn transpose(&mut self, degrees: i32) {
        let Tracks(tracks) = self;
        *tracks = tracks
            .iter()
            .map(|(&channel, notes)| {
                (
                    channel,
                    notes
                        .iter()
                        .map(|(&note, &steps)| {
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
                            let new_note = 12 * octave
                                + intervals.iter().take(degree as usize).sum::<u8>() as i32;
                            (new_note as u8, steps)
                        })
                        .collect(),
                )
            })
            .collect();
    }
    fn build_seq(&self) -> Vec<(f64, seq::Event)> {
        let mut seq: Vec<_> = self
            .0
            .iter()
            .flat_map(|(&channel, notes)| {
                notes
                    .iter()
                    .map(move |(&note, steps)| (channel, note, steps))
            })
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
}

// TODO(jack) Focus mode is either step, note, or channel.
// Operations should then apply to every step in the focus.

type Channel = u8;
type Note = u8;

#[derive(Debug, Copy, Clone)]
enum Focus {
    Channel(Channel),
    Note {
        channel: Channel,
        note: Note,
    },
    Step {
        channel: Channel,
        note: Note,
        step: usize,
    },
}

struct Ui {
    record: bool,
    clock: u16,

    focus: Focus,

    tracks: Tracks,
    undo: Vec<Tracks>,
    redo: Vec<Tracks>,

    seq_tx: Sender<Vec<(f64, seq::Event)>>,
}

impl Ui {
    fn push_tracks(&mut self) {
        // TODO(jack) Action enum to log messages of what's being done / undone?
        self.undo.push(self.tracks.clone());
        self.redo.clear();
    }
    fn move_left(&mut self) {
        match &mut self.focus {
            Focus::Step {
                channel,
                note,
                step,
            } => {
                if let Some(n) = self
                    .tracks
                    .0
                    .get(channel)
                    .and_then(|notes| notes.get(note))
                    .map(|steps| steps.len())
                    .filter(|&n| n > 0)
                {
                    *step = step.checked_sub(1).unwrap_or(n - 1);
                    self.print_tracks();
                }
            }
            _ => (),
        };
    }
    fn move_right(&mut self) {
        match &mut self.focus {
            Focus::Step {
                channel,
                note,
                step,
            } => {
                if let Some(n) = self
                    .tracks
                    .0
                    .get(channel)
                    .and_then(|notes| notes.get(note))
                    .map(|steps| steps.len())
                    .filter(|&n| n > 0)
                {
                    *step = (*step + 1) % n;
                    self.print_tracks();
                }
            }
            _ => (),
        };
        self.print_tracks();
    }
    fn move_down(&mut self) {
        match &mut self.focus {
            Focus::Channel(channel) => {
                if let Some(&c) = self.tracks.0.keys().filter(|&c| c < channel).last() {
                    *channel = c;
                }
            }
            Focus::Note { channel, note } | Focus::Step { channel, note, .. } => {
                if let Some((c, n)) = self
                    .tracks
                    .0
                    .iter()
                    .flat_map(|(&channel, notes)| notes.keys().map(move |&note| (channel, note)))
                    .filter(|(c, n)| (c, n) < (channel, note))
                    .last()
                {
                    *channel = c;
                    *note = n;
                }
            }
        };
        self.print_tracks();
    }
    fn move_up(&mut self) {
        match &mut self.focus {
            Focus::Channel(channel) => {
                if let Some(&c) = self.tracks.0.keys().filter(|&c| c > channel).next() {
                    *channel = c;
                }
            }
            Focus::Note { channel, note } | Focus::Step { channel, note, .. } => {
                if let Some((c, n)) = self
                    .tracks
                    .0
                    .iter()
                    .flat_map(|(&channel, notes)| notes.keys().map(move |&note| (channel, note)))
                    .filter(|(c, n)| (c, n) > (channel, note))
                    .next()
                {
                    *channel = c;
                    *note = n;
                }
            }
        };
        self.print_tracks();
    }
    fn step(&mut self, event: Event) -> bool {
        match event {
            Event::Key(Key::Ctrl('c')) => return false,
            Event::Key(Key::Backspace) => {
                self.push_tracks();
                match self.focus {
                    Focus::Channel(channel) => {
                        self.tracks.0.remove(&channel);
                    }
                    Focus::Note { channel, note } | Focus::Step { channel, note, .. } => {
                        if let Some(notes) = self.tracks.0.get_mut(&channel) {
                            notes.remove(&note);
                            if notes.is_empty() {
                                self.tracks.0.remove(&channel);
                            }
                        }
                    }
                }
                self.move_down();
                self.seq_tx.send(self.tracks.build_seq()).unwrap();
                self.print_tracks();
            }
            Event::Key(Key::Char('q')) => {
                self.push_tracks();
                // TODO(jack) This code is clearly very redundant. How can I refactor?
                // If I had a flat array to work with, I could use slices / slice iterators..
                match self.focus {
                    Focus::Channel(channel) => {
                        for notes in self.tracks.0.get_mut(&channel) {
                            for steps in notes.values_mut() {
                                for step in steps {
                                    if let Some(step) = step {
                                        step.delay = 0.0;
                                    }
                                }
                            }
                        }
                    }
                    Focus::Note { channel, note } => {
                        for notes in self.tracks.0.get_mut(&channel) {
                            for steps in notes.get_mut(&note) {
                                for step in steps {
                                    if let Some(step) = step {
                                        step.delay = 0.0;
                                    }
                                }
                            }
                        }
                    }
                    Focus::Step {
                        channel,
                        note,
                        step,
                    } => {
                        for notes in self.tracks.0.get_mut(&channel) {
                            for steps in notes.get_mut(&note) {
                                for step in steps.get_mut(step) {
                                    if let Some(step) = step {
                                        step.delay = 0.0;
                                    }
                                }
                            }
                        }
                    }
                }
                self.seq_tx.send(self.tracks.build_seq()).unwrap();
            }
            Event::Key(Key::Ctrl('l')) => {
                print!("{}", termion::clear::All);
                self.print_header();
                self.print_tracks();
                self.print_clock();
            }
            Event::Key(Key::Char('<')) => {
                self.push_tracks();
                self.tracks.transpose(-1);
                self.seq_tx.send(self.tracks.build_seq()).unwrap();
                self.print_tracks();
            }
            Event::Key(Key::Char('>')) => {
                self.push_tracks();
                self.tracks.transpose(1);
                self.seq_tx.send(self.tracks.build_seq()).unwrap();
                self.print_tracks();
            }
            Event::Key(Key::Char('g')) => {
                self.push_tracks();
                /*
                if let Some(steps) = self.tracks.0.values_mut().nth(self.focus_row) {
                    let n = steps.len();
                    for (i, step) in steps.iter_mut().enumerate() {
                        if let Some(step) = step {
                            step.velocity = i as f64 / n as f64;
                        }
                    }
                }
                */
                match self.focus {
                    Focus::Channel(channel) => {
                        for notes in self.tracks.0.get_mut(&channel) {
                            for steps in notes.values_mut() {
                                for step in steps {
                                    if let Some(step) = step {
                                        step.delay = 0.0;
                                    }
                                }
                            }
                        }
                    }
                    Focus::Note { channel, note } => {
                        for notes in self.tracks.0.get_mut(&channel) {
                            for steps in notes.get_mut(&note) {
                                for step in steps {
                                    if let Some(step) = step {
                                        step.delay = 0.0;
                                    }
                                }
                            }
                        }
                    }
                    Focus::Step {
                        channel,
                        note,
                        step,
                    } => {
                        for notes in self.tracks.0.get_mut(&channel) {
                            for steps in notes.get_mut(&note) {
                                for step in steps.get_mut(step) {
                                    if let Some(step) = step {
                                        step.delay = 0.0;
                                    }
                                }
                            }
                        }
                    }
                }
                self.seq_tx.send(self.tracks.build_seq()).unwrap();
                self.print_tracks();
            }
            Event::Key(Key::Char('r')) => {
                self.record = !self.record;
                RECORD.store(self.record, Ordering::Relaxed);
                self.print_header();
            }
            Event::Key(Key::Char('c')) => {
                self.focus = match self.focus {
                    Focus::Channel(channel)
                    | Focus::Note { channel, .. }
                    | Focus::Step { channel, .. } => Focus::Channel(channel),
                };
                self.print_header();
                self.print_tracks();
            }
            Event::Key(Key::Char('n')) => {
                self.focus = match self.focus {
                    Focus::Channel(channel) => Focus::Note {
                        channel,
                        note: self
                            .tracks
                            .0
                            .get(&channel)
                            .and_then(|notes| notes.keys().next())
                            .copied()
                            .unwrap_or(0),
                    },
                    Focus::Note { channel, note } | Focus::Step { channel, note, .. } => {
                        Focus::Note { channel, note }
                    }
                };
                self.print_header();
                self.print_tracks();
            }
            Event::Key(Key::Char('s')) => {
                self.focus = match self.focus {
                    Focus::Channel(channel) => Focus::Step {
                        channel,
                        note: self
                            .tracks
                            .0
                            .get(&channel)
                            .and_then(|notes| notes.keys().next())
                            .copied()
                            .unwrap_or(0),
                        step: 0,
                    },
                    Focus::Note { channel, note } => Focus::Step {
                        channel,
                        note,
                        step: 0,
                    },
                    step => step,
                };
                self.print_header();
                self.print_tracks();
            }
            Event::Key(Key::Char('h')) => self.move_left(),
            Event::Key(Key::Char('l')) => self.move_right(),
            Event::Key(Key::Char('k')) => self.move_down(),
            Event::Key(Key::Char('j')) => self.move_up(),
            Event::Key(Key::Char('u')) => {
                if let Some(tracks) = self.undo.pop() {
                    self.redo.push(replace(&mut self.tracks, tracks));
                    self.seq_tx.send(self.tracks.build_seq()).unwrap();
                    print!("{}", termion::clear::All);
                    self.print_header();
                    self.print_tracks();
                    self.print_clock();
                }
            }
            Event::Key(Key::Ctrl('r')) => {
                if let Some(tracks) = self.redo.pop() {
                    self.undo.push(replace(&mut self.tracks, tracks));
                    self.seq_tx.send(self.tracks.build_seq()).unwrap();
                    print!("{}", termion::clear::All);
                    self.print_header();
                    self.print_tracks();
                    self.print_clock();
                }
            }
            _ => (),
        };
        true
    }
    fn print_header(&self) {
        print!(
            "{}{}",
            termion::cursor::Goto(1, 1),
            termion::clear::CurrentLine
        );
        print!(
            "{}{}[{}]{}{}",
            termion::color::Bg(termion::color::Blue),
            termion::color::Fg(termion::color::Black),
            match self.focus {
                Focus::Channel { .. } => "focus channel",
                Focus::Note { .. } => "focus note",
                Focus::Step { .. } => "focus step",
            },
            termion::color::Bg(termion::color::Reset),
            termion::color::Fg(termion::color::Reset),
        );
        if self.record {
            print!(
                "{}{}[record]{}{}\r\n",
                termion::color::Bg(termion::color::Red),
                termion::color::Fg(termion::color::Black),
                termion::color::Bg(termion::color::Reset),
                termion::color::Fg(termion::color::Reset),
            );
        }
    }
    fn print_tracks(&self) {
        print!("{}", termion::cursor::Goto(1, 2));
        for (&channel, notes) in &self.tracks.0 {
            for ((&note, steps), first) in notes.iter().zip(once(true).chain(repeat(false))) {
                if first {
                    let highlight = match self.focus {
                        Focus::Channel(c) if c == channel => true,
                        _ => false,
                    };
                    if highlight {
                        print!(
                            "{}{}c{:02}{}{} ",
                            termion::color::Bg(termion::color::Blue),
                            termion::color::Fg(termion::color::Black),
                            channel,
                            termion::color::Bg(termion::color::Reset),
                            termion::color::Fg(termion::color::Reset),
                        );
                    } else {
                        print!("c{:02} ", channel);
                    }
                } else {
                    print!("    ");
                }
                let highlight = match self.focus {
                    Focus::Note {
                        channel: c,
                        note: n,
                    } if (c, n) == (channel, note) => true,
                    _ => false,
                };
                if highlight {
                    print!(
                        "{}{}n{:03}{}{} |",
                        termion::color::Bg(termion::color::Blue),
                        termion::color::Fg(termion::color::Black),
                        note,
                        termion::color::Bg(termion::color::Reset),
                        termion::color::Fg(termion::color::Reset),
                    );
                } else {
                    print!("n{:03} |", note);
                }
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
                    let highlight = match self.focus {
                        Focus::Step {
                            channel: c,
                            note: n,
                            step: j,
                        } if (c, n, j) == (channel, note, i) => true,
                        _ => false,
                    };
                    if highlight {
                        print!(
                            "{}{}",
                            termion::color::Bg(termion::color::Blue),
                            termion::color::Fg(termion::color::Black),
                        );
                    }
                    for _ in 0..spaces {
                        print!(" ");
                    }
                    let ch = if step.is_some() { "." } else { " " };
                    print!("{}", ch,);
                    if highlight {
                        print!(
                            "{}{}",
                            termion::color::Bg(termion::color::Reset),
                            termion::color::Fg(termion::color::Reset),
                        );
                    }
                }
                print!("|\r\n");
            }
        }

        // Print step information to the right.
        let x = 12 + MAX_STEPS as u16;
        let y = 2;
        match self.focus {
            Focus::Step {
                channel,
                note,
                step,
            } => {
                if let Some(step) = self
                    .tracks
                    .0
                    .get(&channel)
                    .and_then(|notes| notes.get(&note))
                    .and_then(|steps| steps.get(step).copied())
                    .flatten()
                {
                    print!(
                        "{}v={}{}l={}{}d={}",
                        termion::cursor::Goto(x, y),
                        step.velocity,
                        termion::cursor::Goto(x, y + 1),
                        step.length,
                        termion::cursor::Goto(x, y + 2),
                        step.delay,
                    );
                } else {
                    print!(
                        "{}{}{}{}{}{}",
                        termion::cursor::Goto(x, y),
                        termion::clear::UntilNewline,
                        termion::cursor::Goto(x, y + 1),
                        termion::clear::UntilNewline,
                        termion::cursor::Goto(x, y + 2),
                        termion::clear::UntilNewline,
                    );
                }
            }
            _ => (),
        };
    }
    fn print_clock(&self) {
        let row = 2 + self
            .tracks
            .0
            .values()
            .map(|notes| notes.len())
            .sum::<usize>() as u16;
        print!(
            "{}{}^\r\n",
            termion::cursor::Goto(10 + self.clock, row),
            termion::clear::CurrentLine,
        );
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
                held_incoming_midi_notes: HashMap::with_capacity(16 * 128),
                held_outgoing_midi_notes: HashMap::with_capacity(16 * 128),
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

    let stdin = stdin();
    let _stdout = {
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

        focus: Focus::Step {
            channel: 0,
            note: 0,
            step: 0,
        },

        tracks: Tracks::default(),
        undo: Vec::new(),
        redo: Vec::new(),

        seq_tx,
    };

    ui.print_header();

    'running: loop {
        select! {
            recv(shutdown_rx) -> _ => return,
            recv(seq_event_rx) -> result => {
                let (time, event) = result.unwrap();
                match event {
                    seq::Event::Note {channel, note, velocity, length} => {
                        ui.undo.push(ui.tracks.clone());
                        let steps = &mut ui.tracks.0.entry(channel).or_default().entry(note).or_insert((0..16).map(|_| None).collect());
                        let step = (time * steps.len() as f64) as usize;
                        let delay = time % (1.0 / steps.len() as f64);
                        steps[step] = Some(Step{velocity, length, delay});
                        ui.seq_tx.send(ui.tracks.build_seq()).unwrap();
                    },
                    _ => continue 'running,
                }
                ui.print_tracks();
            },
            recv(event_rx) -> result => {
                let event = result.unwrap();
                if !ui.step(event) {
                    break 'running;
                }
            },
            recv(state_rx) -> result => {
                let clock = result.unwrap();
                let step = (clock * 32 as f64) as u16;
                if ui.clock != step {
                    ui.clock = step;
                    ui.print_clock();
                }
            }
        }
    }

    FLUSH.store(true, Ordering::Relaxed);
    shutdown_rx.recv().unwrap();
}
