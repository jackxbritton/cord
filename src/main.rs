use crossbeam::{bounded, Receiver, Sender};
use rand::prelude::*;
use serde::de::Error;
use serde::Deserializer;
use serde::{Deserialize, Serialize};
use std::default::Default;
use std::iter::once;
use std::sync::atomic::{AtomicBool, Ordering};

#[derive(Debug, Copy, Clone, PartialEq, Deserialize, Serialize)]
struct Step {
    #[serde(deserialize_with = "deserialize_u7")]
    note: u8,
    #[serde(deserialize_with = "deserialize_u7")]
    velocity: u8,
    #[serde(
        default = "default_duration",
        deserialize_with = "deserialize_unit_interval"
    )]
    duration: f64,
    #[serde(
        default = "default_chance_to_fire",
        deserialize_with = "deserialize_unit_interval"
    )]
    chance_to_fire: f64,
}

fn deserialize_u7<'de, D>(deserializer: D) -> Result<u8, D::Error>
where
    D: Deserializer<'de>,
{
    let unsigned = Deserialize::deserialize(deserializer)?;
    if (0..128).contains(&unsigned) {
        Ok(unsigned)
    } else {
        Err(D::Error::invalid_value(
            serde::de::Unexpected::Unsigned(unsigned as u64),
            &"an unsigned integer in the range 0 (inclusive) to 128 (exclusive)",
        ))
    }
}

fn default_duration() -> f64 {
    0.5
}

fn default_chance_to_fire() -> f64 {
    1.0
}

fn deserialize_unit_interval<'de, D>(deserializer: D) -> Result<f64, D::Error>
where
    D: Deserializer<'de>,
{
    let float = Deserialize::deserialize(deserializer)?;
    if (0.0..1.0).contains(&float) {
        Ok(float)
    } else {
        Err(D::Error::invalid_value(
            serde::de::Unexpected::Float(float),
            &"a float in the inclusive range 0 to 1",
        ))
    }
}

#[derive(Debug, Clone, PartialEq, Deserialize, Serialize)]
struct Track {
    channel: u8,
    #[serde(
        default = "default_swing",
        deserialize_with = "deserialize_unit_interval"
    )]
    swing: f64,
    steps: Vec<Step>,
}

fn default_swing() -> f64 {
    0.0
}

#[derive(Debug, Clone, PartialEq, Deserialize, Serialize)]
struct Section {
    tracks: Vec<Track>,
}
#[derive(Debug, Clone, Default, PartialEq, Deserialize, Serialize)]
struct Song {
    sections: Vec<Section>,
}

static RUNNING: AtomicBool = AtomicBool::new(true);

fn find_steps_to_cancel<'a>(
    old_tracks: &'a Vec<Track>,
    new_tracks: &'a Vec<Track>,
    clock: f64,
) -> impl Iterator<Item = Step> + 'a {
    old_tracks
        .iter()
        .enumerate()
        .filter_map(move |(track_index, track)| {
            if track.steps.len() == 0 {
                return None;
            }

            // Is the step currently playing?
            let clock = clock * track.steps.len() as f64;
            let step_index = clock as usize;
            let step = track.steps[step_index];
            let swing = (step_index % 2) as f64 * track.swing * 0.35;
            if clock % 1.0 >= (swing + 0.95 * step.duration).min(0.95) {
                return None;
            }

            // Is the track or step changed?
            if let Some(new_track) = new_tracks.get(track_index) {
                if new_track.steps.len() == track.steps.len()
                    && new_track.steps[step_index] == track.steps[step_index]
                {
                    return None;
                }
            }

            Some(step)
        })
}

fn render_events<'a>(tracks: &'a Vec<Track>) -> impl Iterator<Item = (f64, [u8; 3])> + 'a {
    tracks.iter().flat_map(|track| {
        track
            .steps
            .iter()
            .enumerate()
            .filter(|(_, &Step { chance_to_fire, .. })| {
                rand::thread_rng().gen::<f64>() < chance_to_fire
            })
            .flat_map(
                move |(
                    index,
                    &Step {
                        note,
                        velocity,
                        duration,
                        ..
                    },
                )| {
                    let swing = (index % 2) as f64 * track.swing * 0.35;
                    once((
                        (index as f64 + swing) / track.steps.len() as f64,
                        [0x90 | track.channel, note, velocity],
                    ))
                    .chain(once((
                        (index as f64 + (swing + 0.95 * duration).min(0.95))
                            / track.steps.len() as f64,
                        [0x80 | track.channel, note, 0],
                    )))
                },
            )
    })
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let (song_tx, song_rx): (Sender<Song>, Receiver<Song>) = bounded(0);

    // JACK audio.
    let (client, _) = jack::Client::new(
        "cord",
        jack::ClientOptions::NO_START_SERVER | jack::ClientOptions::USE_EXACT_NAME,
    )?;
    let mut output_port = client.register_port("out", jack::MidiOut::default())?;
    let buffer_period = client.buffer_size() as f64 / client.sample_rate() as f64;
    let mut song = Song::default();
    let mut rendered_events = Vec::with_capacity(64);
    let mut clock = 0.0;
    let mut current_section = 0;
    let _active_client = client.activate_async(
        (),
        jack::ClosureProcessHandler::new(move |client, ps| {
            let mut writer = output_port.writer(ps);

            if !RUNNING.load(Ordering::Relaxed) {
                let new_song = Song::default(); // This will drain all existing MIDI events.
                if song != new_song {
                    // Turn off notes of cancelled steps.
                    let empty = Vec::new();
                    let old_tracks = &song
                        .sections
                        .get(current_section)
                        .map(|section| &section.tracks)
                        .unwrap_or(&empty);
                    let new_tracks = &new_song
                        .sections
                        .get(current_section)
                        .map(|section| &section.tracks)
                        .unwrap_or(&empty);
                    for Step { note, velocity, .. } in
                        find_steps_to_cancel(old_tracks, new_tracks, clock)
                    {
                        writer
                            .write(&jack::RawMidi {
                                bytes: &[0x80, note, velocity],
                                time: 0,
                            })
                            .unwrap();
                    }
                    song = new_song;
                }
                return jack::Control::Quit;
            }

            if let Some(new_song) = song_rx.try_iter().last() {
                // Turn off notes of cancelled steps.
                let empty = Vec::new();
                let old_tracks = &song
                    .sections
                    .get(current_section)
                    .map(|section| &section.tracks)
                    .unwrap_or(&empty);
                let new_tracks = &new_song
                    .sections
                    .get(current_section)
                    .map(|section| &section.tracks)
                    .unwrap_or(&empty);
                for Step { note, velocity, .. } in
                    find_steps_to_cancel(old_tracks, new_tracks, clock)
                {
                    writer
                        .write(&jack::RawMidi {
                            bytes: &[0x80, note, velocity],
                            time: 0,
                        })
                        .unwrap();
                }

                // Render new notes.
                rendered_events.splice(.., render_events(new_tracks));

                // Move over the new song, and reset the sections counter in case it was cut off.
                song = new_song;
                current_section %= if song.sections.len() == 0 {
                    1
                } else {
                    song.sections.len()
                };
            }

            let rate = 0.5;
            let dt = rate * buffer_period;
            let new_clock = clock + dt;

            // If the clock has overflowed, increment the section and render the new notes.
            let ranges = if new_clock >= 1.0 {
                if song.sections.len() > 0 {
                    current_section = (current_section + 1) % song.sections.len();
                    rendered_events
                        .splice(.., render_events(&song.sections[current_section].tracks));
                }
                ((clock..1.0), (0.0..new_clock % 1.0))
            } else {
                ((clock..new_clock), (0.0..0.0))
            };
            clock = new_clock % 1.0;

            let rendered_events_of_interest = rendered_events
                .iter()
                .filter(|&(time, ..)| ranges.0.contains(time) || ranges.1.contains(time));

            for &(time, bytes) in rendered_events_of_interest {
                let sample = ((time - clock) / dt * client.buffer_size() as f64) as u32;
                writer
                    .write(&jack::RawMidi {
                        bytes: &bytes,
                        time: sample,
                    })
                    .unwrap();
            }

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
            serde_yaml::from_reader(file)
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
