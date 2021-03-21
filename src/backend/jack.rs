use crate::backend;

use backend::backend::{Backend, BackendChannels, Error, Event, EventFields, Song};
use crossbeam::{Receiver, Sender, TrySendError};
use rand::Rng;
use std::cmp::Ordering;
use std::collections::VecDeque;
use std::default::Default;
use std::f64::consts::PI;
use std::ops::Range;

#[derive(Debug, Copy, Clone, PartialEq, PartialOrd)]
pub enum MidiEventFields {
    NoteOn {
        channel: u8,
        note: u8,
        velocity: u8,
    },
    NoteOff {
        channel: u8,
        note: u8,
    },
    ControlChange {
        channel: u8,
        controller: u8,
        value: u8,
    },
    ProgramChange {
        channel: u8,
        program: u8,
    },
    ClockPulse,
    ClockStart,
    ClockStop,
    ClockContinue,
}

impl MidiEventFields {
    fn to_bytes<'a>(self, buf: &'a mut [u8; 3]) -> &'a [u8] {
        fn copy_from_slice<'a>(dest: &'a mut [u8], src: &[u8]) -> &'a [u8] {
            let dest = &mut dest[..src.len()];
            dest.copy_from_slice(src);
            dest
        }
        match self {
            Self::NoteOn {
                channel,
                note,
                velocity,
            } => copy_from_slice(buf, &[0x90 | channel, note, velocity]),
            Self::NoteOff { channel, note } => copy_from_slice(buf, &[0x80 | channel, note, 0]),
            Self::ControlChange {
                channel,
                controller,
                value,
            } => copy_from_slice(buf, &[0xb0 | channel, controller, value]),
            Self::ProgramChange { channel, program } => {
                copy_from_slice(buf, &[0xc0 | channel, program])
            }
            Self::ClockPulse => copy_from_slice(buf, &[0xf8]),
            Self::ClockStart => copy_from_slice(buf, &[0xfa]),
            Self::ClockContinue => copy_from_slice(buf, &[0xfb]),
            Self::ClockStop => copy_from_slice(buf, &[0xfc]),
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, PartialOrd)]
pub struct MidiEvent {
    time: f64,
    pub fields: MidiEventFields,
}

impl Eq for MidiEvent {}
impl Ord for MidiEvent {
    fn cmp(&self, other: &Self) -> Ordering {
        self.time.partial_cmp(&other.time).unwrap()
    }
}

pub struct Playback {
    pub song: Song,
    pub section: usize,
    pub clock: f64,
    pub midi_event_queue: VecDeque<MidiEvent>,
    pub midi_note_state: [[bool; 128]; 16],
}

impl Playback {
    fn schedule_midi_events(&mut self, clock_range: Range<f64>) {
        if self.song.sections.get(self.section).is_none() {
            return;
        }
        let events = self.song.sections[self.section]
            .events
            .iter()
            .skip_while(|&&Event { time, .. }| time < clock_range.start)
            .take_while(|&&Event { time, .. }| time < clock_range.end);
        for event in events {
            let &Event {
                time,
                channel,
                fields,
            } = event;
            let time = time - clock_range.start;
            let time = (time + 1.0) % 1.0;
            match fields {
                EventFields::Note {
                    note,
                    velocity,
                    duration,
                    chance_to_fire,
                } => {
                    if chance_to_fire < rand::random() {
                        continue;
                    }
                    let velocity = rand::thread_rng().gen_range(velocity.0..velocity.1);
                    self.midi_event_queue.push_back(MidiEvent {
                        time,
                        fields: MidiEventFields::NoteOn {
                            channel,
                            note,
                            velocity,
                        },
                    });
                    self.midi_event_queue.push_back(MidiEvent {
                        time: time + duration,
                        fields: MidiEventFields::NoteOff { channel, note },
                    });
                }
                EventFields::ControlChange { controller, value } => {
                    self.midi_event_queue.push_back(MidiEvent {
                        time,
                        fields: MidiEventFields::ControlChange {
                            channel,
                            controller,
                            value,
                        },
                    });
                }
            };
        }
    }
    fn apply_swing_to_clock(&self, clock: f64) -> f64 {
        // Compute the clock with swing.
        // Swing is a sinusoid that oscillates note displacement.
        let (beats_per_measure, _beats_per_note) = (4, 4); // Time signature.
        let cycles_per_section = beats_per_measure as f64 * 2.0;
        // If the swing is greater than max_swing, time will move backwards.
        // You can prove this by setting the derivative of our sinusoid to -1.
        let sinusoid = (2.0 * PI * cycles_per_section * clock).cos();
        let max_swing = 1.0 / (PI * cycles_per_section);
        let swing = self
            .song
            .sections
            .get(self.section)
            .map(|section| section.swing)
            .unwrap_or(0.0);
        let displacement = swing * max_swing * (0.5 - 0.5 * sinusoid);
        clock + displacement
    }
    pub fn write_midi_event(
        &mut self,
        writer: &mut jack::MidiWriter,
        midi_event: MidiEvent,
        time: u32,
    ) -> Result<(), jack::Error> {
        let mut buf = [0; 3];
        let bytes = midi_event.fields.to_bytes(&mut buf);
        let result = writer.write(&jack::RawMidi { bytes, time });
        if let Err(_) = result {
            self.midi_event_queue.push_front(midi_event);
        }
        result
    }
    pub fn step(
        &mut self,
        writer: &mut jack::MidiWriter,
        sample_rate: usize,
        buffer_size: u32,
    ) -> Result<(), jack::Error> {
        // Advance the clock and schedule events.
        let (beats_per_measure, _beats_per_note) = (4, 4); // Time signature.
        let bpm = self
            .song
            .sections
            .get(self.section)
            .map(|section| section.bpm)
            .unwrap_or(60);
        let seconds_per_measure = beats_per_measure as f64 * 60.0 / bpm as f64;
        let buffer_period = buffer_size as f64 / sample_rate as f64;
        let dt = buffer_period / seconds_per_measure;
        let new_clock = self.clock + dt;

        // Clock pulses.
        let pulses_per_measure = beats_per_measure as f64 * 24.0;
        let pulses = (new_clock * pulses_per_measure - self.clock * pulses_per_measure) as u32;
        for _ in 0..pulses {
            self.midi_event_queue.push_front(MidiEvent {
                time: 0.0,
                fields: MidiEventFields::ClockPulse,
            });
        }

        if !self.song.paused {
            if new_clock < 1.0 {
                self.schedule_midi_events(
                    self.apply_swing_to_clock(self.clock)..self.apply_swing_to_clock(new_clock),
                );
                self.clock = new_clock;
            } else {
                let new_clock = new_clock % 1.0;
                self.schedule_midi_events(self.apply_swing_to_clock(self.clock)..1.0);
                if self.song.sections.len() > 0 {
                    self.section = (self.section + 1) % self.song.sections.len();
                }
                self.schedule_midi_events(0.0..self.apply_swing_to_clock(new_clock));
                self.clock = new_clock;
            }
            self.midi_event_queue.make_contiguous().sort();
        }

        // Advance event timers.
        for MidiEvent { time, .. } in &mut self.midi_event_queue {
            *time -= dt;
        }
        // Send events.
        while let Some(midi_event) = self.midi_event_queue.pop_front() {
            let time = midi_event.time;
            if time > 0.0 {
                self.midi_event_queue.push_front(midi_event);
                break;
            }
            let time = ((1.0 + time / dt).max(0.0) * buffer_size as f64) as u32;
            self.write_midi_event(writer, midi_event, time)?
        }
        Ok(())
    }
}

pub struct JackBackend<N, P> {
    #[allow(dead_code)]
    async_client: jack::AsyncClient<N, P>,
    channels: BackendChannels,
}

impl<N, P> Backend for JackBackend<N, P> {
    fn channels(&self) -> &BackendChannels {
        &self.channels
    }
}

impl<N, P> JackBackend<N, P> {
    pub fn new(
    ) -> Result<JackBackend<impl jack::NotificationHandler, impl jack::ProcessHandler>, jack::Error>
    {
        let (song_tx, song_rx): (Sender<Song>, Receiver<Song>) = crossbeam::unbounded();
        let (quit_tx, quit_rx) = crossbeam::unbounded();
        let mut quit = false;
        let (playback_state_tx, playback_state_rx) = crossbeam::bounded(256);
        let (fatal_tx, fatal_rx): (Sender<Error>, Receiver<_>) = crossbeam::bounded(1);

        let (client, _) = jack::Client::new(
            "cord",
            jack::ClientOptions::NO_START_SERVER | jack::ClientOptions::USE_EXACT_NAME,
        )?;
        let mut output_port = client.register_port("out", jack::MidiOut::default())?;
        let mut song = Song::default();
        song.paused = true;
        let mut playback = Playback {
            song,
            section: 0,
            clock: 0.0,
            midi_event_queue: {
                let mut queue = VecDeque::with_capacity(32);
                queue.push_front(MidiEvent {
                    time: 0.0,
                    fields: MidiEventFields::ClockStart,
                });
                queue
            },
            midi_note_state: [[false; 128]; 16],
        };
        let process_handler =
            jack::ClosureProcessHandler::new(move |client, ps| -> jack::Control {
                if let Some(new_song) = song_rx.try_iter().last() {
                    let changed_programs = playback
                        .song
                        .programs
                        .iter()
                        .zip(new_song.programs.iter())
                        .enumerate()
                        .filter(|(_, (old, new))| old != new)
                        .map(|(channel, (_, new))| (channel, new));
                    for (channel, &program) in changed_programs {
                        playback.midi_event_queue.push_front(MidiEvent {
                            time: 0.0,
                            fields: MidiEventFields::ProgramChange {
                                channel: channel as u8,
                                program,
                            },
                        });
                    }
                    if new_song.paused && !playback.song.paused {
                        playback.midi_event_queue.push_front(MidiEvent {
                            time: 0.0,
                            fields: MidiEventFields::ClockStop,
                        });
                    } else if !new_song.paused && playback.song.paused {
                        playback.midi_event_queue.push_front(MidiEvent {
                            time: 0.0,
                            fields: MidiEventFields::ClockContinue,
                        });
                    }
                    playback.song = new_song;
                };
                quit = quit_rx.try_recv().is_ok() || quit;

                let mut writer = output_port.writer(ps);

                // If we're exiting, just drain the queue.
                if quit {
                    while let Some(midi_event) = playback.midi_event_queue.pop_front() {
                        if let MidiEventFields::NoteOff { .. } = midi_event.fields {
                            match playback.write_midi_event(&mut writer, midi_event, 0) {
                                Ok(()) => (),
                                Err(jack::Error::NotEnoughSpace) => return jack::Control::Continue,
                                Err(err) => {
                                    fatal_tx.send(err.into()).ok();
                                    return jack::Control::Quit;
                                }
                            };
                        }
                    }
                }

                // Otherwise, just step the playback.
                match playback.step(&mut writer, client.sample_rate(), client.buffer_size()) {
                    Ok(()) => (),
                    Err(jack::Error::NotEnoughSpace) => return jack::Control::Continue,
                    Err(err) => {
                        fatal_tx.send(err.into()).ok();
                        return jack::Control::Quit;
                    }
                }

                if let Err(TrySendError::Disconnected(_)) =
                    playback_state_tx.try_send(backend::PlaybackState {
                        clock: playback.apply_swing_to_clock(playback.clock),
                        section: playback.section,
                    })
                {
                    return jack::Control::Quit;
                }

                jack::Control::Continue
            });
        let async_client = client.activate_async((), process_handler)?;
        Ok(JackBackend {
            async_client,
            channels: BackendChannels {
                song_tx,
                quit_tx,
                playback_state_rx,
                fatal_rx,
            },
        })
    }
}
