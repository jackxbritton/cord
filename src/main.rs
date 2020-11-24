use crossbeam::{bounded, Receiver, Sender};
use serde::{Deserialize, Serialize};
use std::cmp::{Ord, Ordering};
use std::collections::VecDeque;
use std::default::Default;
use std::error::Error;
use std::f64::consts::PI;
use std::io::Read;
use std::sync::atomic;

// TODO(jack) What if the queue just keeps getting bigger?
// TODO(jack) Track MIDI state to ignore redundant NOTE OFFs, handle redundant
// NOTE ONs as NOTE OFF + NOTE ON.

#[derive(Debug, Copy, Clone, PartialEq, PartialOrd, Deserialize, Serialize)]
struct Note {
    time: f64,
    channel: u8,
    note: u8,
    velocity: u8,
    duration: f64,
}

impl Eq for Note {}
impl Ord for Note {
    fn cmp(&self, other: &Self) -> Ordering {
        self.time.partial_cmp(&other.time).unwrap()
    }
}

#[derive(Debug, Clone, Default, PartialEq, Deserialize, Serialize)]
struct Section {
    next_section: Option<usize>,
    notes: Vec<Note>,
}

#[derive(Debug, Clone, Default, PartialEq, Deserialize, Serialize)]
struct Song {
    rate: f64,
    swing: f64,
    sections: Vec<Section>,
}

impl Song {
    fn from_reader<R>(reader: R) -> Result<Song, Box<dyn Error>>
    where
        R: Read,
    {
        // Load the song and sort the notes.
        let song = {
            let mut song: Song = serde_yaml::from_reader(reader)?;
            for section in &mut song.sections {
                section.notes.sort_unstable();
            }
            song
        };
        // Validate.
        for section in &song.sections {
            if let Some(index) = section.next_section {
                if !(0..song.sections.len()).contains(&index) {
                    Err("next section index outside of valid range")?
                }
            }
            for Note {
                time,
                channel,
                note,
                velocity,
                duration,
            } in &section.notes
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
                if !(1..128).contains(velocity) {
                    Err("section contains note with velocity not in the range 1..128")?;
                }
                if *duration <= 0.0 {
                    Err("section contains note with non-positive duration")?;
                }
            }
        }
        Ok(song)
    }
}

static RUNNING: atomic::AtomicBool = atomic::AtomicBool::new(true);

fn main() -> Result<(), Box<dyn Error>> {
    let (song_tx, song_rx): (Sender<Song>, Receiver<Song>) = bounded(0);

    // JACK audio.
    let (client, _) = jack::Client::new(
        "cord",
        jack::ClientOptions::NO_START_SERVER | jack::ClientOptions::USE_EXACT_NAME,
    )?;
    let mut output_port = client.register_port("out", jack::MidiOut::default())?;
    let buffer_period = client.buffer_size() as f64 / client.sample_rate() as f64;
    let mut song = Song::default();
    let mut clock = 0.0;
    let mut clock_with_swing = 0.0;
    let mut current_section = 0;
    let mut event_queue: VecDeque<(f64, [u8; 3])> = VecDeque::with_capacity(32);
    let _active_client = client.activate_async(
        (),
        jack::ClosureProcessHandler::new(move |client, ps| {
            if let Some(new_song) = song_rx.try_iter().last() {
                song = new_song;
            };

            let mut writer = output_port.writer(ps);

            // If we're exiting, just drain the queue.
            if !RUNNING.load(atomic::Ordering::Relaxed) {
                while let Some((time, bytes)) = event_queue.pop_front() {
                    let result = writer.write(&jack::RawMidi {
                        bytes: &bytes,
                        time: 0,
                    });
                    match result {
                        Err(jack::Error::NotEnoughSpace) => {
                            event_queue.push_front((time, bytes));
                            return jack::Control::Continue;
                        }
                        Err(err) => eprintln!("{}", err),
                        Ok(()) => (),
                    };
                }
            }

            // Advance the clock.
            clock = clock + song.rate * buffer_period;
            if clock >= 1.0 {
                if song.sections.len() > 0 {
                    current_section = (current_section + 1) % song.sections.len();
                }
                clock %= 1.0;
            }

            // Compute the clock with swing.
            let old_clock_with_swing = clock_with_swing;
            let n = 8.0;
            let max_swing = 1.0 / (PI * n); // Maximum before the sinusoid beings bringing us backward in time.
            clock_with_swing =
                clock + song.swing * max_swing * (0.5 - 0.5 * (2.0 * PI * n * clock).cos()); // TODO(jack) What if the user changes the swing abruptly? Can we go back in time?

            // Schedule notes.
            for &Note {
                time,
                channel,
                note,
                velocity,
                duration,
            } in song
                .sections
                .get(current_section)
                .unwrap_or(&Section::default())
                .notes
                .iter()
                .filter(|&&Note { time, .. }| {
                    if old_clock_with_swing <= clock_with_swing {
                        old_clock_with_swing <= time && time < clock_with_swing
                    } else {
                        old_clock_with_swing <= time || time < clock_with_swing
                    }
                })
            {
                event_queue.push_back((
                    (time - old_clock_with_swing + 1.0) % 1.0,
                    [0x90 | channel, note, velocity],
                ));
                event_queue.push_back((
                    (time + duration - old_clock_with_swing + 1.0) % 1.0,
                    [0x80 | channel, note, 0],
                ));
            }

            // Advance timers.
            let dt = (clock_with_swing + 1.0 - old_clock_with_swing) % 1.0;
            for (time, _) in &mut event_queue {
                *time = *time - dt;
            }

            // Sort the event queue.
            event_queue
                .make_contiguous()
                .sort_by(|(t1, _), (t2, _)| t1.partial_cmp(t2).unwrap());

            // Emit note events.
            while let Some((time, bytes)) = event_queue.pop_front() {
                if time > 0.0 {
                    event_queue.push_front((time, bytes));
                    break;
                }
                let result = writer.write(&jack::RawMidi {
                    bytes: &bytes,
                    time: ((1.0 + time / dt) * client.buffer_size() as f64) as u32,
                });
                match result {
                    Err(jack::Error::NotEnoughSpace) => {
                        event_queue.push_front((time, bytes));
                        return jack::Control::Continue;
                    }
                    Err(err) => eprintln!("{}", err),
                    Ok(()) => (),
                };
            }

            jack::Control::Continue
        }),
    )?;

    ctrlc::set_handler(|| {
        RUNNING.store(false, atomic::Ordering::Relaxed);
    })?;

    let mut song = Song::default();
    while RUNNING.load(atomic::Ordering::Relaxed) {
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
