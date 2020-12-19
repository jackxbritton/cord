use crossbeam::{Receiver, Sender, TrySendError};
use serde::{Deserialize, Serialize};
use std::cmp::{Ord, Ordering};
use std::collections::VecDeque;
use std::default::Default;
use std::error::Error;
use std::f64::consts::PI;
use std::io::Read;
use std::ops::Range;

#[derive(Debug, Copy, Clone, PartialEq, PartialOrd, Deserialize, Serialize)]
#[serde(untagged)]
pub enum EventFields {
    Note {
        note: u8,
        velocity: u8,
        duration: f64,
        chance_to_fire: f64,
    },
    ControlChange {
        controller: u8,
        value: u8,
    },
}

#[derive(Debug, Copy, Clone, PartialEq, PartialOrd, Deserialize, Serialize)]
pub struct Event {
    pub time: f64,
    pub channel: u8,
    #[serde(flatten)]
    pub fields: EventFields,
}

impl Eq for Event {}
impl Ord for Event {
    fn cmp(&self, other: &Self) -> Ordering {
        self.time.partial_cmp(&other.time).unwrap()
    }
}

#[derive(Debug, Clone, Default, PartialEq, Deserialize, Serialize)]
pub struct Section {
    pub next_section: Option<usize>,
    pub events: Vec<Event>,
}

#[derive(Debug, Clone, Default, PartialEq, Deserialize, Serialize)]
pub struct Song {
    pub bpm: u32,
    pub swing: f64,
    pub sections: Vec<Section>,
}

impl Song {
    pub fn from_reader<R>(reader: R) -> Result<Self, Box<dyn Error>>
    where
        R: Read,
    {
        let song = {
            let mut song: Song = serde_yaml::from_reader(reader)?;
            for section in &mut song.sections {
                section.events.sort_unstable();
            }
            song
        };
        if !(0.0..=1.0).contains(&song.swing) {
            Err("swing is not in the range 0.0..=1.0")?;
        }
        // Validate each section.
        for section in &song.sections {
            if let Some(index) = section.next_section {
                if !(0..song.sections.len()).contains(&index) {
                    Err("next section index outside of valid range")?
                }
            }
            for Event {
                time,
                channel,
                fields,
            } in &section.events
            {
                if !(0.0..1.0).contains(time) {
                    Err("section contains note with time not in the range 0.0..1.0")?;
                }
                if !(0..16).contains(channel) {
                    Err("section contains note with channel not in the range 0..16")?;
                }
                match fields {
                    EventFields::Note {
                        note,
                        velocity,
                        duration,
                        chance_to_fire,
                    } => {
                        if !(0..128).contains(note) {
                            Err("section contains note with note not in the range 0..128")?;
                        }
                        if !(1..128).contains(velocity) {
                            Err("section contains note with velocity not in the range 1..128")?;
                        }
                        if !(0.0..1.0).contains(duration) {
                            Err("section contains note with duration not in the range 0.0..1.0")?;
                        }
                        if !(0.0..=1.0).contains(chance_to_fire) {
                            Err("section contains note with chance_to_fire not in the range 0.0..=1.0")?;
                        }
                    }
                    EventFields::ControlChange { controller, value } => {
                        if !(0..120).contains(controller) {
                            Err("section contains control change with value not in the range 0..128")?;
                        }
                        if !(0..128).contains(value) {
                            Err("section contains control change with value not in the range 1..128")?;
                        }
                    }
                }
            }
        }
        Ok(song)
    }
}

#[derive(Debug, Copy, Clone, PartialEq, PartialOrd)]
pub enum MidiEventFields {
    NoteOn { note: u8, velocity: u8 },
    NoteOff { note: u8 },
    ControlChange { controller: u8, value: u8 },
}

#[derive(Debug, Copy, Clone, PartialEq, PartialOrd)]
pub struct MidiEvent {
    time: f64,
    channel: u8,
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
    pub current_section: usize,
    pub clock: f64,
    pub midi_event_queue: VecDeque<MidiEvent>,
    pub midi_note_state: [[bool; 128]; 16],
}

impl Playback {
    fn schedule_midi_events(&mut self, clock_range: Range<f64>) {
        if self.song.sections.get(self.current_section).is_none() {
            return;
        }
        let events = self.song.sections[self.current_section]
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
                    self.midi_event_queue.push_back(MidiEvent {
                        time,
                        channel,
                        fields: MidiEventFields::NoteOn { note, velocity },
                    });
                    self.midi_event_queue.push_back(MidiEvent {
                        time: time + duration,
                        channel,
                        fields: MidiEventFields::NoteOff { note },
                    });
                }
                EventFields::ControlChange { controller, value } => {
                    self.midi_event_queue.push_back(MidiEvent {
                        time,
                        channel,
                        fields: MidiEventFields::ControlChange { controller, value },
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
        let displacement = self.song.swing * max_swing * (0.5 - 0.5 * sinusoid);
        clock + displacement
    }
    pub fn write_midi_event(
        &mut self,
        writer: &mut jack::MidiWriter,
        midi_event: MidiEvent,
        time: u32,
    ) -> Result<(), jack::Error> {
        let MidiEvent {
            channel, fields, ..
        } = midi_event;
        match fields {
            MidiEventFields::NoteOn { note, velocity } => {
                if self.midi_note_state[channel as usize][note as usize] {
                    return Ok(());
                }
                let result = writer.write(&jack::RawMidi {
                    bytes: &[0x90 | channel, note, velocity],
                    time,
                });
                match result {
                    Ok(()) => self.midi_note_state[channel as usize][note as usize] = true,
                    Err(_) => self.midi_event_queue.push_front(midi_event),
                };
                result
            }
            MidiEventFields::NoteOff { note } => {
                if !self.midi_note_state[channel as usize][note as usize] {
                    return Ok(());
                }
                let result = writer.write(&jack::RawMidi {
                    bytes: &[0x80 | channel, note, 0],
                    time,
                });
                match result {
                    Ok(()) => self.midi_note_state[channel as usize][note as usize] = false,
                    Err(_) => self.midi_event_queue.push_front(midi_event),
                };
                result
            }
            MidiEventFields::ControlChange { controller, value } => {
                let result = writer.write(&jack::RawMidi {
                    bytes: &[0xb0 | channel, controller, value],
                    time,
                });
                match result {
                    Ok(()) => (),
                    Err(_) => self.midi_event_queue.push_front(midi_event),
                };
                result
            }
        }
    }
    pub fn step(
        &mut self,
        writer: &mut jack::MidiWriter,
        buffer_period: f64,
    ) -> Result<(), jack::Error> {
        // Advance the clock and schedule events.
        let (beats_per_measure, _beats_per_note) = (4, 4); // Time signature.
        let seconds_per_measure = beats_per_measure as f64 * 60.0 / self.song.bpm as f64;
        let dt = buffer_period / seconds_per_measure;
        let new_clock = self.clock + dt;
        if new_clock < 1.0 {
            self.schedule_midi_events(
                self.apply_swing_to_clock(self.clock)..self.apply_swing_to_clock(new_clock),
            );
            self.clock = new_clock;
        } else {
            let new_clock = new_clock % 1.0;
            self.schedule_midi_events(self.apply_swing_to_clock(self.clock)..1.0);
            if self.song.sections.len() > 0 {
                self.current_section = (self.current_section + 1) % self.song.sections.len();
            }
            self.schedule_midi_events(0.0..self.apply_swing_to_clock(new_clock));
            self.clock = new_clock;
        }
        self.midi_event_queue.make_contiguous().sort();
        // Advance event timers.
        for MidiEvent { time, .. } in &mut self.midi_event_queue {
            *time = *time - dt;
        }
        // Send events.
        while let Some(midi_event) = self.midi_event_queue.pop_front() {
            if midi_event.time > 0.0 {
                self.midi_event_queue.push_front(midi_event);
                break;
            }
            // TODO(jack) Check this math.
            let time = ((1.0 + midi_event.time / dt) / buffer_period) as u32;
            self.write_midi_event(writer, midi_event, time)?
        }
        Ok(())
    }
}

pub struct BackendChannels {
    pub song_tx: Sender<Song>,
    pub quit_tx: Sender<()>,
    pub clock_rx: Receiver<f64>,
    pub fatal_rx: Receiver<Box<dyn Error + Send + Sync>>,
}

pub trait Backend {
    fn channels(&self) -> &BackendChannels;
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
        let (song_tx, song_rx) = crossbeam::unbounded();
        let (quit_tx, quit_rx) = crossbeam::unbounded();
        let mut quit = false;
        let (clock_tx, clock_rx) = crossbeam::bounded(256);
        let (fatal_tx, fatal_rx): (Sender<Box<dyn Error + Send + Sync>>, Receiver<_>) =
            crossbeam::bounded(1);

        let (client, _) = jack::Client::new(
            "cord",
            jack::ClientOptions::NO_START_SERVER | jack::ClientOptions::USE_EXACT_NAME,
        )?;
        let mut output_port = client.register_port("out", jack::MidiOut::default())?;
        let buffer_period = client.buffer_size() as f64 / client.sample_rate() as f64;
        let mut playback = Playback {
            song: Song::default(),
            current_section: 0,
            clock: 0.0,
            midi_event_queue: VecDeque::with_capacity(32),
            midi_note_state: [[false; 128]; 16],
        };
        let process_handler =
            jack::ClosureProcessHandler::new(move |_client, ps| -> jack::Control {
                if let Some(new_song) = song_rx.try_iter().last() {
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
                match playback.step(&mut writer, buffer_period) {
                    Ok(()) => (),
                    Err(jack::Error::NotEnoughSpace) => return jack::Control::Continue,
                    Err(err) => {
                        fatal_tx.send(err.into()).ok();
                        return jack::Control::Quit;
                    }
                }

                if let Err(TrySendError::Disconnected(_)) =
                    clock_tx.try_send(playback.apply_swing_to_clock(playback.clock))
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
                clock_rx,
                fatal_rx,
            },
        })
    }
}
