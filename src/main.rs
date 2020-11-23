use crossbeam::{bounded, Receiver, Sender};
use rand::prelude::*;
use serde::{Deserialize, Serialize};
use std::default::Default;
use std::error::Error;
use std::io::Read;
use std::iter::once;
use std::sync::atomic::{AtomicBool, Ordering};

#[derive(Debug, Copy, Clone, PartialEq, Deserialize, Serialize)]
struct Note {
    time: f64,
    channel: u8,
    note: u8,
    velocity: u8,
    duration: f64,
}

#[derive(Debug, Clone, PartialEq, Deserialize, Serialize)]
struct MidiNote {
    time: f64,
    channel: u8,
    note: u8,
    velocity_and_duration: Option<(u8, f64)>,
}

#[derive(Debug, Clone, PartialEq, Deserialize, Serialize)]
struct Section {
    notes: Vec<Note>,
}

impl Section {
    fn render(&self) -> impl Iterator<Item = MidiNote> + '_ {
        self.notes.iter().flat_map(
            |&Note {
                 time,
                 channel,
                 note,
                 velocity,
                 duration,
             }| {
                once(MidiNote {
                    time,
                    channel,
                    note,
                    velocity_and_duration: Some((velocity, duration)),
                })
                .chain(once(MidiNote {
                    time: time + duration,
                    channel,
                    note,
                    velocity_and_duration: None,
                }))
            },
        )
    }
}

#[derive(Debug, Clone, Default, PartialEq, Deserialize, Serialize)]
struct Song {
    sections: Vec<Section>,
}

impl Song {
    fn from_reader<R>(reader: R) -> Result<Song, Box<dyn Error>>
    where
        R: Read,
    {
        let song: Song = serde_yaml::from_reader(reader)?;
        for section in &song.sections {
            let mut notes: Vec<MidiNote> = section.render().collect();
            for MidiNote {
                time,
                channel,
                note,
                velocity_and_duration,
            } in &notes
            {
                if !(0.0..1.0).contains(time) {
                    Err("section contains note with time not in the range 0.0..1.0")?;
                }
                if !(0..16).contains(channel) {
                    Err("section contains note with channel not in the range 0..16")?;
                }
                if !(0..128).contains(note) {
                    Err("section contains note with note not in the range 0..128")?;
                }
                if let Some((velocity, duration)) = velocity_and_duration {
                    if !(1..128).contains(velocity) {
                        Err("section contains note with velocity not in the range 1..128")?;
                    }
                    if *duration <= 0.0 {
                        Err("section contains note with non-positive duration")?;
                    }
                }
            }
            notes.sort_unstable_by(|MidiNote { time: t1, .. }, MidiNote { time: t2, .. }| {
                t1.partial_cmp(t2).unwrap()
            });
            let mut midi_state = [[false; 128]; 16];
            for MidiNote {
                time,
                channel,
                note,
                velocity_and_duration,
            } in notes
            {
                let on = velocity_and_duration.is_some();
                let slot = &mut midi_state[channel as usize][note as usize];
                if *slot == on {
                    Err("section sent redundant note event")?;
                }
                *slot = on;
            }
            if midi_state
                .iter()
                .flat_map(|notes| notes.iter())
                .any(|&on| on)
            {
                Err("section left notes on")?;
            }
        }
        Ok(song)
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
    let mut rendered_events = Vec::with_capacity(64);
    let mut midi_state = [[0; 128]; 16];
    let mut clock = 0.0;
    let _active_client = client.activate_async(
        (),
        jack::ClosureProcessHandler::new(move |client, ps| {
            let mut writer = output_port.writer(ps);

            if !RUNNING.load(Ordering::Relaxed) {
                let channels_and_notes =
                    (0..16).flat_map(|channel| (0..128).map(move |note| (channel, note)));
                let notes_to_turn_off =
                    channels_and_notes.filter(|&(channel, note)| midi_state[channel][note] > 0);
                for (channel, note) in notes_to_turn_off {
                    let bytes = [0x80 | channel as u8, note as u8, 0];
                    match writer.write(&jack::RawMidi {
                        bytes: &bytes,
                        time: 0,
                    }) {
                        Err(err) => {
                            eprintln!("{}", err);
                            return jack::Control::Continue;
                        }
                        _ => (),
                    };
                }
                midi_state = [[0; 128]; 16];
                return jack::Control::Quit;
            }

            if let Some(new_song) = song_rx.try_iter().last() {
                // Re-render.
                rendered_events.splice(
                    ..,
                    new_song
                        .sections
                        .get(0)
                        .unwrap_or(&Section { notes: Vec::new() })
                        .render(),
                );
                rendered_events.sort_unstable_by(
                    |MidiNote { time: t1, .. }, MidiNote { time: t2, .. }| {
                        t1.partial_cmp(t2).unwrap()
                    },
                );
            }

            let rate = 0.5;
            let dt = rate * buffer_period;
            let new_clock = (clock + dt) % 1.0;

            // According to the new clock, compute the desired MIDI state.
            let new_midi_state = {
                let mut midi_state = [[(0, 0.0); 128]; 16];
                for &MidiNote {
                    time,
                    channel,
                    note,
                    velocity_and_duration,
                } in rendered_events
                    .iter()
                    .take_while(|&&MidiNote { time, .. }| time < new_clock)
                {
                    let velocity = velocity_and_duration
                        .map(|(velocity, _duration)| velocity)
                        .unwrap_or(0);
                    midi_state[channel as usize][note as usize] = (velocity, time);
                }
                midi_state
            };

            // Next, resolve the differences with the present MIDI state.
            let channels_and_notes =
                (0..16).flat_map(|channel| (0..128).map(move |note| (channel, note)));
            let events_to_resolve = channels_and_notes.filter_map(|(channel, note)| {
                let (velocity, time) = new_midi_state[channel][note];
                if midi_state[channel][note] != velocity {
                    Some((time, channel, note, velocity))
                } else {
                    None
                }
            });
            for (time, channel, note, velocity) in events_to_resolve {
                let bytes = [
                    if velocity > 0 { 0x90 } else { 0x80 } | channel as u8,
                    note as u8,
                    velocity as u8,
                ];
                let sample = ((time - clock) / dt * client.buffer_size() as f64) as u32;
                match writer.write(&jack::RawMidi {
                    bytes: &bytes,
                    time: sample,
                }) {
                    Err(err) => {
                        eprintln!("{}", err);
                        return jack::Control::Continue;
                    }
                    _ => (),
                };
            }
            for (channel, notes) in midi_state.iter_mut().enumerate() {
                for (note, velocity) in notes.iter_mut().enumerate() {
                    *velocity = new_midi_state[channel][note].0;
                }
            }

            clock = new_clock;

            jack::Control::Continue
        }),
    )?;

    ctrlc::set_handler(|| {
        RUNNING.store(false, Ordering::Relaxed);
    })?;

    let mut song = Song::default();
    while RUNNING.load(Ordering::Relaxed) {
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
