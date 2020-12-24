mod backend;
mod music;

use backend::jack::*;
use backend::*;
use music::*;

use crossbeam::select;
use crossbeam::{Receiver, SendError, Sender};
use std::error::Error;
use std::io::{stdin, stdout, Write};
use std::iter::repeat;
use std::thread::{sleep, spawn};
use std::time::Duration;
use termion::cursor::HideCursor;
use termion::event::{Event, Key};
use termion::input::{MouseTerminal, TermRead};
use termion::raw::IntoRawMode;
use termion::screen::AlternateScreen;
use termion::{clear, color, cursor};

// TODO(jack)
// sections
// undo / redo

enum FieldUpdateResult<T> {
    Ready(T),
    NotReady,
    Cancel,
}

trait Field<T> {
    fn update(&mut self, event: &Event) -> FieldUpdateResult<T>;
}

struct UnsignedField {
    radix: u32,
    max: u32,
    acc: u32,
}

impl UnsignedField {
    fn new(radix: u32, max: u32) -> Self {
        assert!(radix > 0);
        Self { radix, max, acc: 0 }
    }
}
impl Field<u32> for UnsignedField {
    fn update(&mut self, event: &Event) -> FieldUpdateResult<u32> {
        self.acc = match event {
            Event::Key(Key::Esc) => return FieldUpdateResult::Cancel,
            Event::Key(Key::Char('\n')) => return FieldUpdateResult::Ready(self.acc),
            Event::Key(Key::Char(ch @ '0'..='9')) => {
                let digit = ch.to_digit(self.radix).unwrap();
                self.radix
                    .checked_mul(self.acc)
                    .and_then(|tens| tens.checked_add(digit))
                    .filter(|&acc| acc <= self.max)
                    .unwrap_or(self.acc)
            }
            Event::Key(Key::Backspace) => self.acc / self.radix,
            _ => self.acc,
        };
        FieldUpdateResult::NotReady
    }
}

#[derive(Copy, Clone)]
struct KeyField {
    tonic: Option<char>,
    accidental: Option<Accidental>,
    mode: Mode,
    octave: Option<u8>,
}

impl KeyField {
    fn new() -> Self {
        Self {
            tonic: None,
            accidental: None,
            mode: Mode::Major,
            octave: None,
        }
    }
}
impl Field<music::Key> for KeyField {
    fn update(&mut self, event: &Event) -> FieldUpdateResult<music::Key> {
        match (*self, event) {
            (_, Event::Key(Key::Esc)) => return FieldUpdateResult::Cancel,

            (Self { tonic: None, .. }, &Event::Key(Key::Char(ch @ 'a'..='g'))) => {
                self.tonic = Some(ch)
            }
            (
                Self {
                    tonic: Some(_),
                    mode: Mode::Major,
                    ..
                },
                &Event::Key(Key::Char('b')),
            ) => {
                self.accidental = Some(Accidental::Flat);
            }
            (
                Self {
                    tonic: Some(_),
                    mode: Mode::Major,
                    ..
                },
                &Event::Key(Key::Char('#')),
            ) => {
                self.accidental = Some(Accidental::Sharp);
            }
            (
                Self {
                    tonic: Some(_),
                    octave: None,
                    ..
                },
                &Event::Key(Key::Char('m')),
            ) => {
                self.mode = Mode::Minor;
            }
            (Self { tonic: Some(_), .. }, &Event::Key(Key::Char(ch @ '0'..='9'))) => {
                self.octave = Some(ch.to_digit(10).unwrap() as u8);
            }

            // Backspace.
            (
                Self {
                    octave: Some(_), ..
                },
                Event::Key(Key::Backspace),
            ) => {
                self.octave = None;
            }
            (
                Self {
                    mode: Mode::Minor, ..
                },
                Event::Key(Key::Backspace),
            ) => {
                self.mode = Mode::Major;
            }
            (
                Self {
                    accidental: Some(_),
                    ..
                },
                Event::Key(Key::Backspace),
            ) => {
                self.accidental = None;
            }
            (Self { tonic: Some(_), .. }, Event::Key(Key::Backspace)) => {
                self.tonic = None;
            }

            (
                Self {
                    tonic: Some(tonic),
                    accidental,
                    mode,
                    octave: Some(octave),
                },
                Event::Key(Key::Char('\n')),
            ) => {
                return FieldUpdateResult::Ready(music::Key {
                    tonic,
                    accidental,
                    mode,
                    octave,
                });
            }
            _ => (),
        };
        FieldUpdateResult::NotReady
    }
}

enum UiState {
    EditSection,

    AddChannel(UnsignedField),
    EditChannel(UnsignedField),
    AddNote(UnsignedField),
    EditNote(UnsignedField),

    ConstrainToKey(KeyField),

    EditBpm(UnsignedField),
    EditSwing(UnsignedField),
    EditProgram(UnsignedField),

    Err(Box<dyn Error>),
}

#[derive(Copy, Clone)]
enum Step {
    Hold,
    Off,
    On,
}
#[derive(Copy, Clone)]
struct Track {
    steps: [Step; 16],
    mute: bool,
}

struct Section {
    tracks: [[Option<Track>; 128]; 16],
}

impl Section {
    fn iter(&self) -> impl DoubleEndedIterator<Item = (usize, usize, &Track)> {
        self.tracks
            .iter()
            .enumerate()
            .flat_map(move |(channel, tracks)| {
                tracks.iter().enumerate().filter_map(move |(note, track)| {
                    track.as_ref().map(|track| (channel, note, track))
                })
            })
    }
    fn iter_mut(&mut self) -> impl DoubleEndedIterator<Item = (usize, usize, &mut Track)> {
        self.tracks
            .iter_mut()
            .enumerate()
            .flat_map(move |(channel, tracks)| {
                tracks
                    .iter_mut()
                    .enumerate()
                    .filter_map(move |(note, track)| {
                        track.as_mut().map(|track| (channel, note, track))
                    })
            })
    }
    fn iter_channel(&self, channel: usize) -> impl DoubleEndedIterator<Item = (usize, &Track)> {
        self.tracks[channel]
            .iter()
            .enumerate()
            .filter_map(move |(note, track)| track.as_ref().map(|track| (note, track)))
    }
    fn iter_channel_mut(
        &mut self,
        channel: usize,
    ) -> impl DoubleEndedIterator<Item = (usize, &mut Track)> {
        self.tracks[channel]
            .iter_mut()
            .enumerate()
            .filter_map(move |(note, track)| track.as_mut().map(|track| (note, track)))
    }
}

struct Ui {
    bpm: u32,
    swing: u32,

    paused: bool,
    playback_state: backend::PlaybackState,
    backend: Option<Box<dyn Backend>>,

    state: UiState,
    sections: Vec<Section>,
    programs: [u8; 16],
    focused_section: usize,
    focused_channel: usize,
    focused_note: usize,
    focused_step: usize,
}

impl Ui {
    fn step(
        &mut self,
        event_rx: &Receiver<Event>,
        error_rx: &Receiver<Box<dyn Error + Send>>,
    ) -> bool {
        match self.step_helper(event_rx, error_rx) {
            Ok(quit) => quit,
            Err(err) => {
                self.state = UiState::Err(err);
                false
            }
        }
    }
    fn step_helper(
        &mut self,
        event_rx: &Receiver<Event>,
        error_rx: &Receiver<Box<dyn Error + Send>>,
    ) -> Result<bool, Box<dyn Error>> {
        let nevers = (crossbeam::channel::never(), crossbeam::channel::never());
        let (playback_state_rx, fatal_rx) = if let Some(backend) = &self.backend {
            (
                &backend.channels().playback_state_rx,
                &backend.channels().fatal_rx,
            )
        } else {
            (&nevers.0, &nevers.1)
        };
        select! {
            recv(event_rx) -> event => {
                if !self.handle_input(event?) {
                    if let Some(backend) = &self.backend {
                        backend.channels().quit_tx.send(()).ok();
                    }
                    return Ok(false);
                }

                self.render_to_backend()?;
            },
            recv(error_rx) -> err => return Err(err?),
            recv(playback_state_rx) -> playback_state => self.playback_state = playback_state?,
            recv(fatal_rx) -> err => {
                self.backend = None;
                return Err(err?);
            },
        };
        Ok(true)
    }
    fn handle_input(&mut self, event: Event) -> bool {
        let section = &mut self.sections[self.focused_section];
        let (focused_channel, focused_note) = (self.focused_channel, self.focused_note);
        match (&mut self.state, &event) {
            (_, Event::Key(Key::Ctrl('c'))) => return false,
            (_, Event::Key(Key::Char(' '))) => self.paused = !self.paused,

            (UiState::EditSection, Event::Key(Key::Char('C'))) => {
                self.state = UiState::AddChannel(UnsignedField::new(10, 15))
            }
            (UiState::AddChannel(field), event) => match field.update(event) {
                FieldUpdateResult::NotReady => (),
                FieldUpdateResult::Ready(channel) => {
                    self.focused_channel = channel as usize;
                    let first_track = section.iter_channel(self.focused_channel).next();
                    self.focused_note = if let Some((note, _)) = first_track {
                        note
                    } else {
                        section.tracks[self.focused_channel][0] = Some(Track {
                            mute: false,
                            steps: [Step::Off; 16],
                        });
                        0
                    };
                    self.state = UiState::EditSection;
                }
                FieldUpdateResult::Cancel => {
                    self.state = UiState::EditSection;
                }
            },

            (UiState::EditSection, Event::Key(Key::Char('c'))) => {
                if let Some(_) = self.sections[self.focused_section].tracks[self.focused_channel]
                    [self.focused_note]
                {
                    self.state = UiState::EditChannel(UnsignedField::new(10, 15));
                }
            }
            (UiState::EditChannel(field), event) => match field.update(event) {
                FieldUpdateResult::NotReady => (),
                FieldUpdateResult::Ready(channel) => {
                    let new_channel = channel as usize;
                    section.tracks[new_channel] = section.tracks[self.focused_channel];
                    section.tracks[self.focused_channel] = [None; 128];
                    self.focused_channel = new_channel;
                    self.state = UiState::EditSection;
                }
                FieldUpdateResult::Cancel => {
                    self.state = UiState::EditSection;
                }
            },

            (UiState::EditSection, Event::Key(Key::Char('N'))) => {
                if section.iter_channel(self.focused_channel).next().is_some() {
                    self.state = UiState::AddNote(UnsignedField::new(10, 127))
                }
            }
            (UiState::AddNote(field), event) => match field.update(event) {
                FieldUpdateResult::NotReady => (),
                FieldUpdateResult::Ready(note) => {
                    self.focused_note = note as usize;
                    if self.sections[self.focused_section].tracks[self.focused_channel]
                        [self.focused_note]
                        .is_none()
                    {
                        self.sections[self.focused_section].tracks[self.focused_channel]
                            [self.focused_note] = Some(Track {
                            mute: false,
                            steps: [Step::Off; 16],
                        });
                    }
                    self.state = UiState::EditSection;
                }
                FieldUpdateResult::Cancel => {
                    self.state = UiState::EditSection;
                }
            },

            (UiState::EditSection, Event::Key(Key::Char('n'))) => {
                if let Some(_) = section.tracks[self.focused_channel][self.focused_note] {
                    self.state = UiState::EditNote(UnsignedField::new(10, 127))
                }
            }
            (UiState::EditNote(field), event) => match field.update(event) {
                FieldUpdateResult::NotReady => (),
                FieldUpdateResult::Ready(note) => {
                    let new_note = note as usize;
                    section.tracks[self.focused_channel][new_note] =
                        section.tracks[self.focused_channel][self.focused_note].take();
                    self.focused_note = new_note;
                    self.state = UiState::EditSection;
                }
                FieldUpdateResult::Cancel => {
                    self.state = UiState::EditSection;
                }
            },

            (UiState::EditSection, Event::Key(Key::Char('p'))) => {
                self.state = UiState::EditProgram(UnsignedField::new(10, 127));
            }
            (UiState::EditProgram(field), event) => match field.update(event) {
                FieldUpdateResult::NotReady => (),
                FieldUpdateResult::Ready(program) => {
                    self.programs[self.focused_channel as usize] = program as u8;
                    self.state = UiState::EditSection;
                }
                FieldUpdateResult::Cancel => {
                    self.state = UiState::EditSection;
                }
            },

            // Mute / solo track / channel.
            (UiState::EditSection, Event::Key(Key::Char('m'))) => {
                if let Some(track) = &mut section.tracks[self.focused_channel][self.focused_note] {
                    track.mute = !track.mute;
                }
            }
            (UiState::EditSection, Event::Key(Key::Char('M'))) => {
                let mute = section
                    .iter_channel(self.focused_channel)
                    .any(|(_, track)| !track.mute);
                for (_, track) in section.iter_channel_mut(self.focused_channel) {
                    track.mute = mute;
                }
            }
            (UiState::EditSection, Event::Key(Key::Char('s'))) => {
                if section
                    .iter_channel(self.focused_channel)
                    .all(|(note, track)| track.mute == (note != focused_note))
                {
                    for (_, track) in section.iter_channel_mut(self.focused_channel) {
                        track.mute = false;
                    }
                } else {
                    for (note, track) in section.iter_channel_mut(self.focused_channel) {
                        track.mute = note != self.focused_note;
                    }
                }
            }
            (UiState::EditSection, Event::Key(Key::Char('S'))) => {
                let mute_all = section
                    .iter()
                    .all(|(channel, _, track)| track.mute == (channel != focused_channel));
                for (channel, _, track) in section.iter_mut() {
                    track.mute = !mute_all && channel != self.focused_channel;
                }
            }

            // Section controls.
            (UiState::EditSection, Event::Key(Key::Char('<'))) => {
                self.focused_section =
                    (self.focused_section + 1 + self.sections.len()) % self.sections.len();
            }
            (UiState::EditSection, Event::Key(Key::Char('>'))) => {
                self.focused_section = (self.focused_section + 1) % self.sections.len();
            }
            (UiState::EditSection, Event::Key(Key::Char('Z'))) => {
                self.sections.push(Section {
                    tracks: [[None; 128]; 16],
                });
                self.focused_section = self.sections.len() - 1;
            }

            (UiState::EditSection, Event::Key(Key::Char(ch @ '0'..='9'))) => {
                let index_to_focus = ['1', '2', '3', '4', '5', '6', '7', '8', '9', '0']
                    .iter()
                    .position(|other| other == ch)
                    .unwrap();
                if let Some((note, track)) = section
                    .iter_channel_mut(self.focused_channel)
                    .skip(index_to_focus)
                    .next()
                {
                    self.focused_note = note;
                    self.focused_step = self.focused_step.min(track.steps.len() - 1);
                    let step = &mut track.steps[self.focused_step];
                    *step = match *step {
                        Step::Off => Step::On,
                        Step::On => Step::Off,
                        _ => *step,
                    };
                    self.focused_step = (self.focused_step + 1) % track.steps.len();
                }
            }

            (UiState::EditSection, Event::Key(Key::Delete)) => {
                let track = section.tracks[self.focused_channel][self.focused_note].take();
                if track.is_some() {
                    let next_track = section
                        .iter()
                        .skip_while(|&(channel, note, _)| {
                            channel != focused_channel || note != focused_note
                        })
                        .skip(1)
                        .chain(section.iter().take_while(|&(channel, note, _)| {
                            channel != focused_channel || note != focused_note
                        }))
                        .next();
                    if let Some((channel, note, _)) = next_track {
                        self.focused_channel = channel;
                        self.focused_note = note;
                    }
                }
            }

            (UiState::EditSection, Event::Key(Key::Char('h'))) => {
                if let Some(track) = section.tracks[self.focused_channel][self.focused_note] {
                    self.focused_step = self
                        .focused_step
                        .checked_sub(1)
                        .unwrap_or(track.steps.len() - 1);
                }
            }
            (UiState::EditSection, Event::Key(Key::Char('l'))) => {
                if let Some(track) = section.tracks[self.focused_channel][self.focused_note] {
                    self.focused_step = (self.focused_step + 1) % track.steps.len();
                }
            }

            (UiState::EditSection, Event::Key(Key::Char('k'))) => {
                let previous_track = section
                    .iter()
                    .rev()
                    .skip_while(|&(channel, note, _)| {
                        channel != focused_channel || note != focused_note
                    })
                    .skip(1)
                    .next();
                if let Some((channel, note, _)) = previous_track {
                    self.focused_channel = channel;
                    self.focused_note = note;
                };
            }
            (UiState::EditSection, Event::Key(Key::Char('j'))) => {
                let next_track = section
                    .iter()
                    .skip_while(|&(channel, note, _)| {
                        channel != focused_channel || note != focused_note
                    })
                    .skip(1)
                    .next();
                if let Some((channel, note, _)) = next_track {
                    self.focused_channel = channel;
                    self.focused_note = note;
                };
            }

            (UiState::EditSection, Event::Key(Key::Char('g'))) => {
                let first_track = section.iter().next();
                if let Some((channel, note, _)) = first_track {
                    self.focused_channel = channel;
                    self.focused_note = note;
                };
            }
            (UiState::EditSection, Event::Key(Key::Char('G'))) => {
                let last_track = section.iter().last();
                if let Some((channel, note, _)) = last_track {
                    self.focused_channel = channel;
                    self.focused_note = note;
                };
            }

            (UiState::EditSection, Event::Key(Key::Char('.'))) => {
                if let Some(track) = &mut section.tracks[self.focused_channel][self.focused_note] {
                    track.steps[self.focused_step] = match track.steps[self.focused_step] {
                        Step::On => Step::Off,
                        _ => Step::On,
                    };
                    self.focused_step = (self.focused_step + 1) % track.steps.len();
                }
            }

            (UiState::EditSection, Event::Key(Key::Char('-'))) => {
                if let Some(track) = &mut section.tracks[self.focused_channel][self.focused_note] {
                    track.steps[self.focused_step] = match track.steps[self.focused_step] {
                        Step::Hold => Step::Off,
                        _ => Step::Hold,
                    };
                    self.focused_step = (self.focused_step + 1) % track.steps.len();
                }
            }

            (UiState::EditSection, Event::Key(Key::Char('K'))) => {
                self.state = UiState::ConstrainToKey(KeyField::new())
            }
            (UiState::ConstrainToKey(field), event) => match field.update(event) {
                FieldUpdateResult::NotReady => (),
                FieldUpdateResult::Ready(music::Key {
                    tonic,
                    accidental,
                    mode,
                    octave,
                }) => {
                    // Compute ten notes from the key.
                    let degree = match tonic {
                        ch @ 'c'..='g' => ch as i32 - 'c' as i32,
                        ch @ 'a'..='b' => ch as i32 - 'a' as i32 + 'g' as i32 - 'c' as i32,
                        _ => unreachable!(),
                    };
                    let root = Mode::Major.semitones(degree as u8);
                    let accidental = match accidental {
                        Some(Accidental::Flat) => -1,
                        Some(Accidental::Sharp) => 1,
                        _ => 0,
                    };
                    let notes = mode
                        .intervals()
                        .map(|interval| {
                            (12 * octave as i32 + root as i32 + accidental + interval as i32)
                                as usize
                        })
                        .take(10);

                    // Clone the channel's tracks and form an iterator over them.
                    let cloned_tracks = section.tracks[self.focused_channel].clone();
                    let tracks = cloned_tracks
                        .iter()
                        .copied()
                        .filter_map(|track| track)
                        .chain(repeat(Track {
                            mute: false,
                            steps: [Step::Off; 16],
                        }));

                    // Zero out the channel's tracks and populate for each note.
                    section.tracks[self.focused_channel] = [None; 128];
                    for (note, track) in notes.zip(tracks) {
                        section.tracks[self.focused_channel][note] = Some(track);
                    }

                    let (note, _) = section.iter_channel(self.focused_channel).last().unwrap();
                    self.focused_note = note;
                    self.state = UiState::EditSection;
                }
                FieldUpdateResult::Cancel => {
                    self.state = UiState::EditSection;
                }
            },

            (UiState::EditSection, Event::Key(Key::Char('b'))) => {
                self.state = UiState::EditBpm(UnsignedField::new(10, 300))
            }
            (UiState::EditBpm(field), event) => match field.update(event) {
                FieldUpdateResult::NotReady => (),
                FieldUpdateResult::Ready(bpm) => {
                    self.bpm = bpm;
                    self.state = UiState::EditSection;
                }
                FieldUpdateResult::Cancel => {
                    self.state = UiState::EditSection;
                }
            },

            (UiState::EditSection, Event::Key(Key::Char('s'))) => {
                self.state = UiState::EditSwing(UnsignedField::new(10, 9))
            }
            (UiState::EditSwing(field), event) => match field.update(event) {
                FieldUpdateResult::NotReady => (),
                FieldUpdateResult::Ready(swing) => {
                    self.swing = swing;
                    self.state = UiState::EditSection;
                }
                FieldUpdateResult::Cancel => {
                    self.state = UiState::EditSection;
                }
            },
            _ => (),
        };
        true
    }
    fn render_to_backend(&self) -> Result<(), SendError<backend::Song>> {
        let backend = match &self.backend {
            Some(backend) => backend,
            None => return Ok(()),
        };
        let sections =
            self.sections
                .iter()
                .map(|section| {
                    let mut events: Vec<_> =
                        section
                            .iter()
                            .filter(|(_, _, track)| !track.mute)
                            .flat_map(move |(channel, note, track)| {
                                track.steps.iter().enumerate().filter_map(
                                    move |(step_index, step)| match step {
                                        Step::Hold | Step::Off => None,
                                        Step::On => {
                                            let steps_to_hold = 1 + track
                                                .steps
                                                .iter()
                                                .cycle()
                                                .skip(step_index + 1)
                                                .take_while(|step| match step {
                                                    Step::Hold => true,
                                                    _ => false,
                                                })
                                                .count();
                                            Some(backend::Event {
                                                time: step_index as f64 / track.steps.len() as f64,
                                                channel: channel as u8,
                                                fields: EventFields::Note {
                                                    note: note as u8,
                                                    velocity: 127,
                                                    duration: (steps_to_hold as f64 - 0.5)
                                                        / track.steps.len() as f64,
                                                    chance_to_fire: 1.0,
                                                },
                                            })
                                        }
                                    },
                                )
                            })
                            .collect();
                    events.sort_unstable();
                    backend::Section {
                        bpm: self.bpm,
                        swing: self.swing as f64 / 9.0,
                        events,
                    }
                })
                .collect();
        backend.channels().song_tx.send(Song {
            paused: self.paused,
            programs: self.programs,
            sections,
        })?;
        Ok(())
    }
    fn write(&self, writer: &mut impl Write, width: u16, height: u16) -> std::io::Result<()> {
        let PlaybackState { clock, section } = self.playback_state;
        write!(
            writer,
            "{}{}{} clock={:.2} section={} {} {} {} ",
            cursor::Goto(1, 1),
            clear::CurrentLine,
            if self.paused { "||" } else { " >" },
            clock,
            section,
            self.focused_section,
            self.focused_channel,
            self.focused_note,
        )?;

        match &self.state {
            UiState::EditBpm(field) => {
                write!(
                    writer,
                    "bpm={}{}{:2}{}{}",
                    color::Bg(color::Red),
                    color::Fg(color::Black),
                    field.acc,
                    color::Bg(color::Reset),
                    color::Fg(color::Reset)
                )?;
            }
            UiState::EditSwing(field) => {
                write!(
                    writer,
                    "swing={}{}{:2}{}{}",
                    color::Bg(color::Red),
                    color::Fg(color::Black),
                    field.acc,
                    color::Bg(color::Reset),
                    color::Fg(color::Reset)
                )?;
            }
            UiState::AddChannel(field) => {
                write!(
                    writer,
                    "add channel={}{}{:2}{}{}",
                    color::Bg(color::Red),
                    color::Fg(color::Black),
                    field.acc,
                    color::Bg(color::Reset),
                    color::Fg(color::Reset)
                )?;
            }
            UiState::EditChannel(field) => {
                write!(
                    writer,
                    "edit channel={}{}{:2}{}{}",
                    color::Bg(color::Red),
                    color::Fg(color::Black),
                    field.acc,
                    color::Bg(color::Reset),
                    color::Fg(color::Reset)
                )?;
            }
            UiState::AddNote(field) => {
                write!(
                    writer,
                    "add note={}{}{:3}{}{}",
                    color::Bg(color::Red),
                    color::Fg(color::Black),
                    field.acc,
                    color::Bg(color::Reset),
                    color::Fg(color::Reset)
                )?;
            }
            UiState::EditNote(field) => {
                write!(
                    writer,
                    "edit note={}{}{:3}{}{}",
                    color::Bg(color::Red),
                    color::Fg(color::Black),
                    field.acc,
                    color::Bg(color::Reset),
                    color::Fg(color::Reset)
                )?;
            }
            UiState::EditProgram(field) => {
                write!(
                    writer,
                    "program={}{}{:3}{}{}",
                    color::Bg(color::Red),
                    color::Fg(color::Black),
                    field.acc,
                    color::Bg(color::Reset),
                    color::Fg(color::Reset)
                )?;
            }
            UiState::ConstrainToKey(KeyField {
                tonic,
                accidental,
                mode,
                octave,
            }) => {
                write!(
                    writer,
                    "key={}{}",
                    color::Bg(color::Red),
                    color::Fg(color::Black),
                )?;
                let spaces = if let Some(_) = tonic { 1 } else { 0 }
                    + if let Some(_) = accidental { 1 } else { 0 }
                    + if let Mode::Minor = mode { 1 } else { 0 }
                    + if let Some(_) = octave { 1 } else { 0 };
                for _ in 0..4 - spaces {
                    write!(writer, " ")?;
                }
                if let Some(tonic) = tonic {
                    write!(writer, "{}", tonic)?;
                }
                match accidental {
                    Some(Accidental::Flat) => write!(writer, "b")?,
                    Some(Accidental::Sharp) => write!(writer, "#")?,
                    None => (),
                };
                match mode {
                    Mode::Major => (),
                    Mode::Minor => write!(writer, "m")?,
                };
                if let Some(octave) = octave {
                    write!(writer, "{}", octave)?;
                }
                write!(
                    writer,
                    "{}{}",
                    color::Bg(color::Reset),
                    color::Fg(color::Reset)
                )?;
            }
            UiState::Err(err) => {
                write!(writer, "{}", err)?;
            }
            _ => (),
        };
        write!(writer, "\r\n\r\n")?;

        let section = &self.sections[self.focused_section];
        let mut last_channel = None;
        for (channel, note, track) in section.iter() {
            if last_channel != Some(channel) {
                write!(writer, "{:2} {:3}", channel, note)?;
                last_channel = Some(channel);
            } else {
                write!(writer, "   {:3}", note)?;
            }
            let mute = if track.mute { 'm' } else { ' ' };
            write!(writer, " {}", mute)?;
            let active_step = (clock * track.steps.len() as f64) as usize;
            for (step_index, step) in track.steps.iter().enumerate() {
                let ch = match step {
                    Step::Hold => '-',
                    Step::Off => ' ',
                    Step::On
                        if self.focused_section == self.playback_state.section
                            && !track.mute
                            && step_index == active_step =>
                    {
                        '*'
                    }
                    Step::On => '.',
                };
                match (
                    channel == self.focused_channel && note == self.focused_note,
                    step_index == self.focused_step,
                ) {
                    (true, true) => write!(
                        writer,
                        "{}{} {}{}{}",
                        color::Bg(color::Red),
                        color::Fg(color::Black),
                        ch,
                        color::Bg(color::Reset),
                        color::Fg(color::Reset)
                    )?,
                    (true, false) => write!(
                        writer,
                        "{}{} {}{}{}",
                        color::Bg(color::Blue),
                        color::Fg(color::Black),
                        ch,
                        color::Bg(color::Reset),
                        color::Fg(color::Reset)
                    )?,
                    _ => write!(writer, " {}", ch)?,
                };
            }
            write!(writer, "\r\n")?;
        }
        writer.flush()?;
        Ok(())
    }
}

fn main() -> Result<(), Box<dyn Error>> {
    // Set up panic hook.
    let default_hook = std::panic::take_hook();
    std::panic::set_hook(Box::new(move |info| {
        println!("{}", termion::screen::ToMainScreen);
        default_hook(info);
    }));

    let backend = JackBackend::<(), ()>::new()?;

    // Termion.
    let stdin = stdin();
    let stdout = stdout().into_raw_mode()?;
    let stdout = AlternateScreen::from(stdout);
    let stdout = MouseTerminal::from(stdout);
    let mut stdout = HideCursor::from(stdout);
    let (error_tx, error_rx): (Sender<Box<dyn Error + Send>>, Receiver<_>) = crossbeam::bounded(0);
    let (event_tx, event_rx) = crossbeam::bounded(0);
    spawn(move || {
        for event in stdin.events() {
            let event = match event {
                Ok(event) => event,
                Err(err) => {
                    error_tx.send(Box::new(err)).ok();
                    return;
                }
            };
            if let Err(err) = event_tx.send(event) {
                error_tx.send(Box::new(err)).ok();
                return;
            }
        }
    });
    let mut ui = Ui {
        bpm: 60,
        swing: 0,

        paused: true,
        playback_state: PlaybackState::default(),
        backend: Some(Box::new(backend)),

        state: UiState::EditSection,
        programs: [0; 16],
        sections: vec![Section {
            tracks: [[None; 128]; 16],
        }],
        focused_section: 0,
        focused_channel: 0,
        focused_note: 0,
        focused_step: 0,
    };
    let (mut width, mut height) = termion::terminal_size()?;
    while ui.step(&event_rx, &error_rx) {
        let (new_width, new_height) = termion::terminal_size()?;
        if (width, height) != (new_width, new_height) {
            write!(stdout, "{}", clear::All)?;
            width = new_width;
            height = new_height;
        }
        ui.write(&mut stdout, width, height)?;
    }

    // Wait for the audio thread to clear MIDI events.
    sleep(Duration::from_secs_f64(0.01));
    Ok(())
}
