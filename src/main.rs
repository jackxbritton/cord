use crossbeam::{bounded, Receiver, Sender};
use serde::{Deserialize, Serialize};
use std::cmp::{Ord, Ordering};
use std::collections::VecDeque;
use std::default::Default;
use std::error::Error;
use std::f64::consts::PI;
use std::io::Read;
use std::ops::Range;
use std::sync::atomic::AtomicBool;
use std::sync::atomic::Ordering::Relaxed;

#[derive(Debug, Copy, Clone, PartialEq, PartialOrd, Deserialize, Serialize)]
#[serde(untagged)]
enum EventFields {
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
struct Event {
    time: f64,
    channel: u8,
    #[serde(flatten)]
    fields: EventFields,
}

impl Eq for Event {}
impl Ord for Event {
    fn cmp(&self, other: &Self) -> Ordering {
        self.time.partial_cmp(&other.time).unwrap()
    }
}

#[derive(Debug, Clone, Default, PartialEq, Deserialize, Serialize)]
struct Section {
    next_section: Option<usize>,
    events: Vec<Event>,
}

#[derive(Debug, Clone, Default, PartialEq, Deserialize, Serialize)]
struct Song {
    rate: f64,
    swing: f64,
    swing_cycles_per_section: u8,
    sections: Vec<Section>,
}

impl Song {
    fn from_reader<R>(reader: R) -> Result<Song, Box<dyn Error>>
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
        if !(0.0..).contains(&song.rate) {
            Err("rate is not in the range 0.0..")?;
        }
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
enum MidiEventFields {
    NoteOn { note: u8, velocity: u8 },
    NoteOff { note: u8 },
    ControlChange { controller: u8, value: u8 },
}

#[derive(Debug, Copy, Clone, PartialEq, PartialOrd)]
struct MidiEvent {
    time: f64,
    channel: u8,
    fields: MidiEventFields,
}

impl Eq for MidiEvent {}
impl Ord for MidiEvent {
    fn cmp(&self, other: &Self) -> Ordering {
        self.time.partial_cmp(&other.time).unwrap()
    }
}

struct Playback {
    song: Song,
    current_section: usize,
    clock: f64,
    clock_with_swing: f64,
    midi_event_queue: VecDeque<MidiEvent>,
    midi_note_state: [[bool; 128]; 16],
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
    fn advance_clock(&mut self, dt: f64) {
        for MidiEvent { time, .. } in &mut self.midi_event_queue {
            *time = *time - dt;
        }
    }
    fn write_midi_event(
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
}

static RUNNING: AtomicBool = AtomicBool::new(true);

fn main() -> Result<(), Box<dyn Error>> {
    let (song_tx, song_rx): (Sender<Song>, Receiver<Song>) = bounded(0);

    // JACK audio.
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
        clock_with_swing: 0.0,
        midi_event_queue: VecDeque::with_capacity(32),
        midi_note_state: [[false; 128]; 16],
    };
    let _active_client = client.activate_async(
        (),
        jack::ClosureProcessHandler::new(move |client, ps| {
            if let Some(new_song) = song_rx.try_iter().last() {
                playback.song = new_song;
            };

            let mut writer = output_port.writer(ps);

            // If we're exiting, just drain the queue.
            if !RUNNING.load(Relaxed) {
                while let Some(midi_event) = playback.midi_event_queue.pop_front() {
                    if let MidiEventFields::NoteOff { .. } = midi_event.fields {
                        match playback.write_midi_event(&mut writer, midi_event, 0) {
                            Err(jack::Error::NotEnoughSpace) => return jack::Control::Continue,
                            Err(err) => {
                                eprintln!("{}", err);
                                RUNNING.store(false, Relaxed);
                                return jack::Control::Quit;
                            }
                            Ok(()) => (),
                        };
                    }
                }
            }

            // Advance the clock.
            playback.clock = playback.clock + playback.song.rate * buffer_period;
            let overflow = playback.clock >= 1.0;
            if overflow {
                // If the clock rolled over, schedule remaining events and advance the section.
                playback.schedule_midi_events(playback.clock_with_swing..1.0);
                playback.current_section =
                    (playback.current_section + 1) % playback.song.sections.len().max(1);
                playback.clock %= 1.0;
            }

            // Compute the clock with swing.
            // Swing is a sinusoid that oscillates note displacement.
            let old_clock_with_swing = playback.clock_with_swing;
            let cycles_per_section = playback.song.swing_cycles_per_section as f64;
            // If the swing is greater than max_swing, time will move backwards.
            // You can prove this by setting the derivative of our sinusoid to -1.
            let max_swing = 1.0 / (PI * cycles_per_section);
            playback.clock_with_swing = playback.clock
                + playback.song.swing
                    * max_swing
                    * (0.5 - 0.5 * (2.0 * PI * cycles_per_section * playback.clock).cos());
            let clock_range = if overflow {
                0.0..playback.clock_with_swing
            } else {
                // To avoid errors where quick changes in swing cause time to move backwards,
                // clamp the swing against the last swing.
                playback.clock_with_swing = playback.clock_with_swing.max(old_clock_with_swing);
                old_clock_with_swing..playback.clock_with_swing
            };

            playback.schedule_midi_events(clock_range);

            let dt = (playback.clock_with_swing + 1.0 - old_clock_with_swing) % 1.0;
            playback.advance_timers(dt);

            // Sort the event queue.
            playback.midi_event_queue.make_contiguous().sort();

            // Emit note events.
            while let Some(midi_event) = playback.midi_event_queue.pop_front() {
                if midi_event.time > 0.0 {
                    playback.midi_event_queue.push_front(midi_event);
                    break;
                }
                // println!("{:?}", midi_event.fields);
                let time = ((1.0 + midi_event.time / dt) * client.buffer_size() as f64) as u32;
                match playback.write_midi_event(&mut writer, midi_event, time) {
                    Err(jack::Error::NotEnoughSpace) => return jack::Control::Continue,
                    Err(err) => {
                        eprintln!("{}", err);
                        RUNNING.store(false, Relaxed);
                        return jack::Control::Quit;
                    }
                    Ok(()) => (),
                };
            }

            jack::Control::Continue
        }),
    )?;

    ctrlc::set_handler(|| {
        RUNNING.store(false, Relaxed);
    })?;

    let mut song = Song::default();
    while RUNNING.load(Relaxed) {
        let new_song = {
            let file = std::fs::File::open("x.yaml")?;
            Song::from_reader(file)
        };
        match new_song {
            Ok(new_song) if song != new_song => {
                song_tx.send(new_song.clone())?;
                song = new_song;
            }
            Ok(_) => (),
            Err(err) => eprintln!("{}", err),
        }
        std::thread::sleep(std::time::Duration::from_secs(1));
    }

    // Wait for the audio thread to clear MIDI events.
    std::thread::sleep(std::time::Duration::from_secs_f64(buffer_period));
    Ok(())
}
