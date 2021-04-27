mod midi;
mod seq;

use std::io::{stdin, stdout, Write};
use std::thread::spawn;

use crossbeam::channel::{Receiver, Sender};
use crossbeam::select;
use termion::cursor::HideCursor;
use termion::event::{Event, Key};
use termion::input::{MouseTerminal, TermRead};
use termion::raw::IntoRawMode;
use termion::screen::AlternateScreen;

struct NotificationHandler {
    shutdown_tx: crossbeam::channel::Sender<()>,
}

impl jack::NotificationHandler for NotificationHandler {
    fn shutdown(&mut self, _status: jack::ClientStatus, _reason: &str) {
        self.shutdown_tx.send(()).unwrap();
    }
}

struct ProcessHandler {
    midi_in_port: jack::Port<jack::MidiIn>,
    midi_out_port: jack::Port<jack::MidiOut>,

    held_incoming_midi_notes: [[Option<(f64, f64)>; 128]; 16],
    held_outgoing_midi_notes: [[i32; 128]; 16],

    midi_event_queue: Vec<(f64, midi::Event)>,

    t: f64,

    // TODO(jack) Can we abstract a pattern for data that gets replaced through channels?
    // Possibly a crossbeam atomic?
    seq: Vec<(f64, seq::Event)>,
    seq_rx: Receiver<Vec<(f64, seq::Event)>>,

    seq_event_tx: Sender<(f64, seq::Event)>,
    state_tx: Sender<f64>,

    flush: bool,
    flush_rx: Receiver<()>,

    shutdown_tx: Sender<()>,
}
impl jack::ProcessHandler for ProcessHandler {
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
            t,
            seq,
            seq_rx,
            seq_event_tx,
            state_tx,
            flush,
            flush_rx,
            shutdown_tx,
        } = self;
        if let Some(new_seq) = seq_rx.try_iter().last() {
            *seq = new_seq;
        }

        let mut writer = midi_out_port.writer(process_scope);
        *flush = *flush || flush_rx.try_iter().next().is_some();
        if *flush {
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
            return jack::Control::Quit;
        }

        let bpm = 85.0;
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
                if let Some(event) = match event {
                    midi::Event::NoteOff { channel, note } => held_incoming_midi_notes
                        [channel as usize][note as usize]
                        .take()
                        .map(|(length, velocity)| seq::Event::Note {
                            channel,
                            note,
                            velocity,
                            length,
                        }),
                    midi::Event::NoteOn {
                        channel,
                        note,
                        velocity,
                    } => {
                        held_incoming_midi_notes[channel as usize][note as usize] =
                            Some((time, (velocity as f64 - 1.0) / 126.0));
                        None
                    }
                    midi::Event::ControlChange {
                        channel,
                        controller,
                        value,
                    } => Some(seq::Event::ControlChange {
                        channel,
                        controller,
                        value,
                    }),
                } {
                    seq_event_tx.send((*t + time, event)).unwrap();
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
        let t1 = *t;
        let t2 = *t + dt;
        *t = if t2 < 1.0 {
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
        state_tx.send(*t).unwrap();

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

        for (time, _) in held_incoming_midi_notes
            .iter_mut()
            .flat_map(|x| x.iter_mut())
            .filter_map(|x| x.as_mut())
        {
            *time += dt;
        }
        for (time, _) in midi_event_queue {
            *time -= dt;
        }

        jack::Control::Continue
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
    let (flush_tx, flush_rx) = crossbeam::channel::bounded(0);
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
                t: 0.0,
                seq: Vec::new(),
                seq_rx,
                seq_event_tx,
                state_tx,
                flush: false,
                flush_rx,
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

    #[derive(Copy, Clone, Debug, Default)]
    struct Step {
        velocity: f64,
        length: f64,
        delay: f64,
    }
    const NUM_STEPS: usize = 16;
    let mut tracks = [[[None; NUM_STEPS]; 128]; 16];

    fn print_tracks(
        stdout: &mut impl Write,
        tracks: &[[[Option<Step>; NUM_STEPS]; 128]; 16],
        clock: f64,
    ) -> Result<(), std::io::Error> {
        write!(
            stdout,
            "{}{}",
            termion::clear::All,
            termion::cursor::Goto(1, 1)
        )?;
        for (channel, notes) in tracks.iter().enumerate() {
            for (note, steps) in notes.iter().enumerate() {
                if steps.iter().any(|step| step.is_some()) {
                    write!(stdout, "c{:02} n{:03} |", channel, note)?;
                    for step in steps.iter() {
                        write!(stdout, "{}", if step.is_some() { "." } else { " " })?;
                    }
                    write!(stdout, "|\r\n")?;
                }
            }
        }
        let clock_step = (clock * NUM_STEPS as f64) as u16;
        write!(stdout, "{}^\r\n", termion::cursor::Right(10 + clock_step))?;
        Ok(())
    }

    fn tracks_to_seq(tracks: &[[[Option<Step>; NUM_STEPS]; 128]; 16]) -> Vec<(f64, seq::Event)> {
        let steps = tracks.iter().enumerate().flat_map(|(channel, notes)| {
            notes.iter().enumerate().flat_map(move |(note, steps)| {
                steps
                    .iter()
                    .enumerate()
                    .flat_map(move |(i, step)| match step {
                        Some(step) => Some((channel as u8, note as u8, i, step)),
                        None => None,
                    })
            })
        });
        let mut seq: Vec<_> = steps
            .map(
                |(
                    channel,
                    note,
                    i,
                    &Step {
                        velocity,
                        length,
                        delay,
                    },
                )| {
                    let t = (i as f64 + delay) / NUM_STEPS as f64;
                    (
                        t,
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

    let mut clock = 0.0;

    'running: loop {
        select! {
            recv(shutdown_rx) -> _ => return,
            recv(seq_event_rx) -> result => {
                let (time, event) = result.unwrap();
                match event {
                    seq::Event::Note {channel, note, velocity, length} => {
                        let step = (time * NUM_STEPS as f64) as usize;
                        let delay = (time * NUM_STEPS as f64) % 1.0;
                        tracks[channel as usize][note as usize][step] = Some(Step{velocity, length, delay});
                        seq_tx.send(tracks_to_seq(&tracks)).unwrap();
                    },
                    _ => continue 'running,
                }
                print_tracks(&mut stdout, &tracks, clock).unwrap();
            },
            recv(event_rx) -> result => {
                let event = result.unwrap();
                match event {
                    Event::Key(Key::Ctrl('c')) => break 'running,
                    Event::Key(Key::Delete) => tracks = [[[None; NUM_STEPS]; 128]; 16],
                    Event::Key(Key::Char('Q')) => {
                        let steps = tracks
                            .iter_mut()
                            .flat_map(|notes| notes.iter_mut())
                            .flat_map(|steps| steps.iter_mut())
                            .filter_map(|step| step.as_mut());
                        for step in steps {
                            step.delay = 0.0;
                        }
                        seq_tx.send(tracks_to_seq(&tracks)).unwrap();
                    },
                    _ => (),
                }
            },
            recv(state_rx) -> result => {
                clock = result.unwrap();
                print_tracks(&mut stdout, &tracks, clock).unwrap();
            }
        }
    }

    flush_tx.send(()).unwrap();
    shutdown_rx.recv().unwrap();
}
