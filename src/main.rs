mod backend;
mod music;

use backend::jack::*;
use backend::*;
use music::*;

use crossbeam::select;
use crossbeam::{Receiver, SendError, Sender};
use std::io::{stdin, stdout, Write};
use std::iter::repeat;
use std::ops::Range;
use std::thread::{sleep, spawn};
use std::time::Duration;
use termion::cursor::HideCursor;
use termion::event::{Event, Key};
use termion::input::{MouseTerminal, TermRead};
use termion::raw::IntoRawMode;
use termion::screen::AlternateScreen;
use termion::{clear, color, cursor};

enum FieldUpdateResult<T> {
    Ready(T),
    NotReady,
    Cancel,
}

trait Field<T> {
    fn update(&mut self, event: &Event) -> FieldUpdateResult<T>;
}

#[derive(Copy, Clone)]
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
struct UnsignedRangeField {
    radix: u32,
    max: u32,
    start: u32,
    end: Option<u32>,
}

impl UnsignedRangeField {
    fn new(radix: u32, max: u32) -> Self {
        assert!(radix > 0);
        Self {
            radix,
            max,
            start: 0,
            end: None,
        }
    }
}
impl Field<(u32, u32)> for UnsignedRangeField {
    fn update(&mut self, event: &Event) -> FieldUpdateResult<(u32, u32)> {
        if let Some(end) = self.end {
            match event {
                Event::Key(Key::Esc) => return FieldUpdateResult::Cancel,
                Event::Key(Key::Char('\n')) => {
                    return FieldUpdateResult::Ready((self.start, end.max(self.start + 1)))
                }
                Event::Key(Key::Char(ch @ '0'..='9')) => {
                    let digit = ch.to_digit(self.radix).unwrap();
                    self.end = Some(
                        self.radix
                            .checked_mul(end)
                            .and_then(|tens| tens.checked_add(digit))
                            .filter(|&acc| acc <= self.max)
                            .unwrap_or(end),
                    );
                }
                Event::Key(Key::Backspace) => {
                    self.end = if end == 0 {
                        None
                    } else {
                        Some(end / self.radix)
                    }
                }
                _ => (),
            };
        } else {
            match event {
                Event::Key(Key::Esc) => return FieldUpdateResult::Cancel,
                Event::Key(Key::Char('\n')) => {
                    return FieldUpdateResult::Ready((self.start, self.start + 1))
                }
                Event::Key(Key::Char(ch @ '0'..='9')) => {
                    let digit = ch.to_digit(self.radix).unwrap();
                    self.start = self
                        .radix
                        .checked_mul(self.start)
                        .and_then(|tens| tens.checked_add(digit))
                        .filter(|&acc| acc <= self.max)
                        .unwrap_or(self.start);
                }
                Event::Key(Key::Backspace) => self.start /= self.radix,
                Event::Key(Key::Char('-')) => self.end = Some(0),
                _ => (),
            };
        }
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

#[derive(Clone)]
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

    EditChanceToFire(UnsignedField),
    EditVelocity(UnsignedRangeField),

    Err(String),
}

#[derive(Copy, Clone)]
enum Step {
    Hold,
    Off,
    On {
        velocity: (u8, u8),
        chance_to_fire: u8,
    },
}

const NUM_STEPS: usize = 16;

#[derive(Copy, Clone)]
struct Track {
    steps: [Step; NUM_STEPS],
    mute: bool,
}

#[derive(Clone)]
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
}

#[derive(Clone)]
struct Song {
    bpm: u32,
    swing: u32,

    state: UiState,
    sections: Vec<Section>,
    programs: [u8; 16],
    focused_section: usize,
    cursor: (usize, usize),
    select_anchor: Option<(usize, usize)>,
}

struct Ui {
    event_rx: Receiver<Event>,
    error_rx: Receiver<Error>,

    paused: bool,

    playback_state: backend::PlaybackState,
    backend: Option<Box<dyn Backend>>,

    undo: Vec<Song>,
    redo: Vec<Song>,

    song: Song,
}

impl Ui {
    fn step(&mut self) -> bool {
        match self.step_helper() {
            Ok(true) => true,
            Ok(false) => {
                if let Some(backend) = &self.backend {
                    let BackendChannels { quit_tx, .. } = backend.channels();
                    quit_tx.send(()).ok();
                };
                false
            }
            Err(err) => {
                self.song.state = UiState::Err(err.to_string());
                false
            }
        }
    }
    fn step_helper(&mut self) -> Result<bool, Error> {
        if let Some(backend) = &self.backend {
            let BackendChannels {
                playback_state_rx,
                fatal_rx,
                ..
            } = backend.channels();
            select! {
                recv(self.event_rx) -> event => {
                    if !self.handle_input(event?) {
                        return Ok(false);
                    }
                    self.render_to_backend()?;
                },
                recv(self.error_rx) -> err => return Err(err?),
                recv(playback_state_rx) -> playback_state => self.playback_state = playback_state?,
                recv(fatal_rx) -> err => {
                    self.backend = None;
                    return Err(err?);
                },
            };
        } else {
            select! {
                recv(self.event_rx) -> event => {
                    if !self.handle_input(event?) {
                        return Ok(false);
                    }
                },
                recv(self.error_rx) -> err => return Err(err?),
            };
        }
        Ok(true)
    }
    fn render_to_backend(&self) -> Result<(), SendError<backend::Song>> {
        let backend = match &self.backend {
            Some(backend) => backend,
            None => return Ok(()),
        };
        let sections =
            self.song
                .sections
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
                                        &Step::On {
                                            chance_to_fire,
                                            velocity,
                                        } => {
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
                                                    velocity,
                                                    duration: (steps_to_hold as f64 - 0.5)
                                                        / track.steps.len() as f64,
                                                    chance_to_fire: chance_to_fire as f64 / 100.0,
                                                },
                                            })
                                        }
                                    },
                                )
                            })
                            .collect();
                    events.sort_unstable();
                    backend::Section {
                        bpm: self.song.bpm,
                        swing: self.song.swing as f64 / 9.0,
                        events,
                    }
                })
                .collect();
        backend.channels().song_tx.send(backend::Song {
            paused: self.paused,
            programs: self.song.programs,
            sections,
        })?;
        Ok(())
    }
    fn write(&self, writer: &mut impl Write) -> std::io::Result<()> {
        let clock = self.playback_state.clock;
        write!(
            writer,
            "{}{}{} clock={:.2} section={}/{} ",
            cursor::Goto(1, 1),
            clear::CurrentLine,
            if self.paused { "||" } else { " >" },
            clock,
            self.song.focused_section + 1,
            self.song.sections.len(),
        )?;
        let focus = self.song.sections[self.song.focused_section]
            .iter()
            .nth(self.song.cursor.0)
            .map(|(_, _, track)| track.steps[self.song.cursor.1]);
        if let Some(Step::On {
            chance_to_fire,
            velocity,
        }) = focus
        {
            write!(
                writer,
                "r={} v={}..{}",
                chance_to_fire, velocity.0, velocity.1
            )?;
        }

        match &self.song.state {
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
            UiState::EditVelocity(field) => {
                write!(
                    writer,
                    "velocity={}{}{}{}",
                    color::Bg(color::Red),
                    color::Fg(color::Black),
                    field.start,
                    if field.end.is_some() { "-" } else { "" },
                )?;
                if let Some(end) = field.end {
                    write!(writer, "{}", end)?;
                }
                write!(
                    writer,
                    "{}{}",
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

        let section = &self.song.sections[self.song.focused_section];
        let mut last_channel = None;
        let (select_rows, select_steps) = if let Some((row, step)) = self.song.select_anchor {
            (
                row.min(self.song.cursor.0)..row.max(self.song.cursor.0) + 1,
                step.min(self.song.cursor.1)..step.max(self.song.cursor.1) + 1,
            )
        } else {
            (
                self.song.cursor.0..self.song.cursor.0 + 1,
                self.song.cursor.1..self.song.cursor.1 + 1,
            )
        };
        for (row, (channel, note, track)) in section.iter().enumerate() {
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
                    Step::On { .. } => {
                        if self.song.focused_section == self.playback_state.section
                            && !track.mute
                            && step_index == active_step
                        {
                            '*'
                        } else {
                            '.'
                        }
                    }
                };
                if row == self.song.cursor.0 && step_index == self.song.cursor.1 {
                    write!(
                        writer,
                        "{}{} {}{}{}",
                        color::Bg(color::Red),
                        color::Fg(color::Black),
                        ch,
                        color::Bg(color::Reset),
                        color::Fg(color::Reset)
                    )?;
                } else if select_rows.contains(&row) && select_steps.contains(&step_index) {
                    write!(
                        writer,
                        "{}{} {}{}{}",
                        color::Bg(color::Blue),
                        color::Fg(color::Black),
                        ch,
                        color::Bg(color::Reset),
                        color::Fg(color::Reset)
                    )?;
                } else {
                    write!(writer, " {}", ch)?;
                }
            }
            write!(writer, "\r\n")?;
        }
        writer.flush()?;
        Ok(())
    }

    fn push_to_undo_stack(&mut self) {
        let mut song = self.song.clone();
        song.state = UiState::EditSection;
        self.undo.push(song);
        self.redo.clear();
    }

    fn get_selected_rows_and_steps_ranges(&self) -> (Range<usize>, Range<usize>) {
        if let Some((row, step)) = self.song.select_anchor {
            (
                row.min(self.song.cursor.0)..row.max(self.song.cursor.0) + 1,
                step.min(self.song.cursor.1)..step.max(self.song.cursor.1) + 1,
            )
        } else {
            (
                self.song.cursor.0..self.song.cursor.0 + 1,
                self.song.cursor.1..self.song.cursor.1 + 1,
            )
        }
    }

    fn iter_selected_steps(&self) -> impl Iterator<Item = &Step> {
        let (selected_rows, selected_steps) = self.get_selected_rows_and_steps_ranges();
        self.song.sections[self.song.focused_section]
            .tracks
            .iter()
            .flat_map(|track| track.iter())
            .filter_map(|track| track.as_ref())
            .skip(selected_rows.start)
            .take(selected_rows.end - selected_rows.start)
            .flat_map(move |track| track.steps[selected_steps.clone()].iter())
    }

    fn iter_mut_selected_steps(&mut self) -> impl Iterator<Item = &mut Step> {
        let (selected_rows, selected_steps) = self.get_selected_rows_and_steps_ranges();
        self.song.sections[self.song.focused_section]
            .tracks
            .iter_mut()
            .flat_map(|track| track.iter_mut())
            .filter_map(|track| track.as_mut())
            .skip(selected_rows.start)
            .take(selected_rows.end - selected_rows.start)
            .flat_map(move |track| track.steps[selected_steps.clone()].iter_mut())
    }

    fn handle_input(&mut self, event: Event) -> bool {
        match event {
            Event::Key(Key::Ctrl('c')) => return false,
            Event::Key(Key::Char(' ')) => {
                self.paused = !self.paused;
                return true;
            }
            _ => (),
        };
        let focus = self.song.sections[self.song.focused_section]
            .iter()
            .skip(self.song.cursor.0)
            .next()
            .map(|(channel, note, _)| (channel, note));
        match &mut self.song.state {
            UiState::EditSection => match event {
                Event::Key(Key::Char('C')) => {
                    self.song.state = UiState::AddChannel(UnsignedField::new(10, 15))
                }
                Event::Key(Key::Char('c')) => {
                    if let Some(_) = focus {
                        self.song.state = UiState::EditChannel(UnsignedField::new(10, 15));
                    }
                }
                Event::Key(Key::Char('N')) => {
                    self.song.state = UiState::AddNote(UnsignedField::new(10, 127))
                }
                Event::Key(Key::Char('n')) => {
                    if let Some(_) = focus {
                        self.song.state = UiState::EditNote(UnsignedField::new(10, 127));
                    }
                }
                Event::Key(Key::Char('p')) => {
                    self.song.state = UiState::EditProgram(UnsignedField::new(10, 127));
                }
                Event::Key(Key::Char('m')) => {
                    if let Some((channel, note)) = focus {
                        if let Some(track) =
                            &mut self.song.sections[self.song.focused_section].tracks[channel][note]
                        {
                            track.mute = !track.mute;
                        }
                    }
                }

                Event::Key(Key::Delete) => {
                    if let Some((channel, note)) = focus {
                        self.push_to_undo_stack();
                        self.song.sections[self.song.focused_section].tracks[channel][note] = None;
                        self.song.cursor.0 = self.song.sections[self.song.focused_section]
                            .iter()
                            .enumerate()
                            .skip(self.song.cursor.0)
                            .next()
                            .map(|(row, _)| row)
                            .unwrap_or(0);
                    }
                }

                Event::Key(Key::Char('h')) => {
                    if let Some(_) = focus {
                        self.song.cursor.1 = self.song.cursor.1.saturating_sub(1);
                    }
                }
                Event::Key(Key::Char('l')) => {
                    if let Some(_) = focus {
                        self.song.cursor.1 = (self.song.cursor.1 + 1).min(NUM_STEPS - 1);
                    }
                }

                Event::Key(Key::Char('k')) => {
                    if let Some(_) = focus {
                        self.song.cursor.0 = self.song.cursor.0.saturating_sub(1);
                    }
                }
                Event::Key(Key::Char('j')) => {
                    let rows = self.song.sections[self.song.focused_section].iter().count();
                    if rows != 0 {
                        self.song.cursor.0 = (self.song.cursor.0 + 1).min(rows - 1);
                    }
                }

                Event::Key(Key::Char('g')) => {
                    self.song.cursor.0 = 0;
                }
                Event::Key(Key::Char('G')) => {
                    self.song.cursor.0 = self.song.sections[self.song.focused_section]
                        .iter()
                        .enumerate()
                        .last()
                        .map(|(row, _)| row)
                        .unwrap_or(0);
                }

                Event::Key(Key::Char('^')) => {
                    self.song.cursor.1 = 0;
                }
                Event::Key(Key::Char('$')) => {
                    self.song.cursor.1 = NUM_STEPS - 1;
                }

                Event::Key(Key::Char('b')) => {
                    let steps_per_measure = 4;
                    self.song.cursor.1 = (self.song.cursor.1 / steps_per_measure + 3)
                        % steps_per_measure
                        * steps_per_measure;
                }
                Event::Key(Key::Char('w')) => {
                    let steps_per_measure = 4;
                    self.song.cursor.1 = (self.song.cursor.1 / steps_per_measure + 1)
                        % steps_per_measure
                        * steps_per_measure;
                }

                Event::Key(Key::Char('u')) => {
                    if let Some(song) = self.undo.pop() {
                        self.redo.push(self.song.clone());
                        self.song = song;
                    }
                }
                Event::Key(Key::Ctrl('r')) => {
                    if let Some(song) = self.redo.pop() {
                        self.undo.push(self.song.clone());
                        self.song = song;
                    }
                }

                Event::Key(Key::Char('r')) => {
                    if let Some((channel, note)) = focus {
                        if self.song.sections[self.song.focused_section].tracks[channel][note]
                            .is_some()
                        {
                            self.push_to_undo_stack();
                            self.song.state =
                                UiState::EditChanceToFire(UnsignedField::new(10, 100));
                        }
                    }
                }

                Event::Key(Key::Char('v')) => {
                    if let Some((channel, note)) = focus {
                        if self.song.sections[self.song.focused_section].tracks[channel][note]
                            .is_some()
                        {
                            self.push_to_undo_stack();
                            self.song.state =
                                UiState::EditVelocity(UnsignedRangeField::new(10, 127));
                        }
                    }
                }

                Event::Key(Key::Ctrl('n')) => self
                    .song
                    .sections
                    .push(self.song.sections[self.song.focused_section].clone()),
                Event::Key(Key::Char(ch @ '0'..='9')) => {
                    // 1-9,0 -> 0-9
                    let digit = ch.to_digit(10).unwrap().checked_sub(1).unwrap_or(9) as usize;
                    self.song.focused_section = digit.min(self.song.sections.len() - 1);
                }

                Event::Key(Key::Char('.')) => {
                    self.push_to_undo_stack();
                    let all_on = self.iter_selected_steps().all(|step| match step {
                        Step::On { .. } => true,
                        _ => false,
                    });
                    for step in self.iter_mut_selected_steps() {
                        *step = if all_on {
                            Step::Off
                        } else {
                            Step::On {
                                chance_to_fire: 100,
                                velocity: (127, 128),
                            }
                        };
                    }
                }

                Event::Key(Key::Char('-')) => {
                    self.push_to_undo_stack();
                    let all_hold = self.iter_selected_steps().all(|step| match step {
                        Step::Hold => true,
                        _ => false,
                    });
                    for step in self.iter_mut_selected_steps() {
                        *step = if all_hold { Step::Off } else { Step::Hold };
                    }
                }

                Event::Key(Key::Char('K')) => {
                    self.song.state = UiState::ConstrainToKey(KeyField::new())
                }
                Event::Key(Key::Char('B')) => {
                    self.song.state = UiState::EditBpm(UnsignedField::new(10, 240))
                }
                Event::Key(Key::Char('s')) => {
                    self.song.state = UiState::EditSwing(UnsignedField::new(10, 9))
                }

                Event::Key(Key::Char('V')) => {
                    self.song.select_anchor = match self.song.select_anchor {
                        Some(_) => None,
                        None => Some(self.song.cursor),
                    }
                }

                Event::Key(Key::Ctrl('a')) => {
                    let rows = self.song.sections[self.song.focused_section]
                        .tracks
                        .iter()
                        .flat_map(|track| track.iter())
                        .filter_map(|track| track.as_ref())
                        .count();
                    if self.song.cursor == (0, 0)
                        && self.song.select_anchor == Some((rows - 1, NUM_STEPS - 1))
                    {
                        self.song.select_anchor = None;
                    } else {
                        self.song.cursor = (0, 0);
                        self.song.select_anchor = Some((rows - 1, NUM_STEPS - 1));
                    }
                }
                _ => return true,
            },

            // TODO Abstraction to recognize keyboard input is captured, and is contributing to an edit.
            // Much of this code is very redundant.
            UiState::AddChannel(field) => match field.update(&event) {
                FieldUpdateResult::NotReady => (),
                FieldUpdateResult::Ready(channel) => {
                    let channel = channel as usize;
                    if !self.song.sections[self.song.focused_section].tracks[channel]
                        .iter()
                        .any(|track| track.is_some())
                    {
                        self.push_to_undo_stack();
                        self.song.sections[self.song.focused_section].tracks[channel][0] =
                            Some(Track {
                                mute: false,
                                steps: [Step::Off; 16],
                            });
                        self.song.cursor.0 = self.song.sections[self.song.focused_section]
                            .iter()
                            .enumerate()
                            .filter(|(_, (c, n, _))| c == &channel && n == &0)
                            .next()
                            .map(|(row, _)| row)
                            .unwrap();
                    }
                    self.song.state = UiState::EditSection;
                }
                FieldUpdateResult::Cancel => {
                    self.song.state = UiState::EditSection;
                }
            },
            UiState::EditChannel(field) => match field.update(&event) {
                FieldUpdateResult::NotReady => (),
                FieldUpdateResult::Ready(channel) => {
                    self.push_to_undo_stack();
                    let new_channel = channel as usize;
                    if let Some((channel, note)) = focus {
                        self.song.sections[self.song.focused_section].tracks[new_channel][0] =
                            self.song.sections[self.song.focused_section].tracks[channel][note]
                                .take();
                        self.song.cursor.0 = self.song.sections[self.song.focused_section]
                            .iter()
                            .enumerate()
                            .filter(|(_, (c, n, _))| c == &new_channel && n == &note)
                            .next()
                            .map(|(row, _)| row)
                            .unwrap();
                    }
                    self.song.state = UiState::EditSection;
                }
                FieldUpdateResult::Cancel => {
                    self.song.state = UiState::EditSection;
                }
            },

            UiState::AddNote(field) => match field.update(&event) {
                FieldUpdateResult::NotReady => (),
                FieldUpdateResult::Ready(note) => {
                    let note = note as usize;
                    let channel = if let Some((channel, _)) = focus {
                        channel
                    } else {
                        0
                    };
                    if self.song.sections[self.song.focused_section].tracks[channel][note].is_none()
                    {
                        self.push_to_undo_stack();
                        self.song.sections[self.song.focused_section].tracks[channel][note] =
                            Some(Track {
                                mute: false,
                                steps: [Step::Off; 16],
                            });
                    }
                    self.song.cursor.0 = self.song.sections[self.song.focused_section]
                        .iter()
                        .enumerate()
                        .filter(|(_, (c, n, _))| c == &channel && n == &note)
                        .map(|(row, _)| row)
                        .next()
                        .unwrap();
                    self.song.state = UiState::EditSection;
                }
                FieldUpdateResult::Cancel => {
                    self.song.state = UiState::EditSection;
                }
            },
            UiState::EditNote(field) => match field.update(&event) {
                FieldUpdateResult::NotReady => (),
                FieldUpdateResult::Ready(note) => {
                    self.push_to_undo_stack();
                    let new_note = note as usize;
                    if let Some((channel, note)) = focus {
                        self.song.sections[self.song.focused_section].tracks[channel][new_note] =
                            self.song.sections[self.song.focused_section].tracks[channel][note]
                                .take();
                        self.song.cursor.0 = self.song.sections[self.song.focused_section]
                            .iter()
                            .enumerate()
                            .filter(|(_, (c, n, _))| c == &channel && n == &new_note)
                            .map(|(row, _)| row)
                            .next()
                            .unwrap();
                    }
                    self.song.state = UiState::EditSection;
                }
                FieldUpdateResult::Cancel => {
                    self.song.state = UiState::EditSection;
                }
            },

            UiState::EditProgram(field) => match field.update(&event) {
                FieldUpdateResult::NotReady => (),
                FieldUpdateResult::Ready(program) => {
                    self.push_to_undo_stack();
                    if let Some((channel, _)) = focus {
                        self.song.programs[channel] = program as u8;
                    }
                    self.song.state = UiState::EditSection;
                }
                FieldUpdateResult::Cancel => {
                    self.song.state = UiState::EditSection;
                }
            },

            UiState::EditChanceToFire(field) => match field.update(&event) {
                FieldUpdateResult::NotReady => (),
                FieldUpdateResult::Cancel => {
                    self.song.state = UiState::EditSection;
                }
                FieldUpdateResult::Ready(chance_to_fire) => {
                    self.push_to_undo_stack();
                    for step in self.iter_mut_selected_steps() {
                        if let Step::On {
                            chance_to_fire: dest,
                            ..
                        } = step
                        {
                            *dest = chance_to_fire as u8;
                        }
                    }
                    self.song.state = UiState::EditSection;
                }
            },

            UiState::EditVelocity(field) => match field.update(&event) {
                FieldUpdateResult::NotReady => (),
                FieldUpdateResult::Cancel => {
                    self.song.state = UiState::EditSection;
                }
                FieldUpdateResult::Ready(velocity) => {
                    self.push_to_undo_stack();
                    for step in self.iter_mut_selected_steps() {
                        if let Step::On { velocity: dest, .. } = step {
                            *dest = (velocity.0 as u8, velocity.1 as u8);
                        }
                    }
                    self.song.state = UiState::EditSection;
                }
            },

            UiState::ConstrainToKey(field) => match field.update(&event) {
                FieldUpdateResult::NotReady => (),
                FieldUpdateResult::Ready(music::Key {
                    tonic,
                    accidental,
                    mode,
                    octave,
                }) => {
                    if let Some((channel, _)) = focus {
                        self.push_to_undo_stack();
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
                        let num_notes = 10;
                        let notes = mode
                            .intervals()
                            .map(|interval| {
                                (12 * octave as i32 + root as i32 + accidental + interval as i32)
                                    as usize
                            })
                            .take(num_notes);

                        // Clone the channel's tracks and form an iterator over them.
                        let cloned_tracks =
                            self.song.sections[self.song.focused_section].tracks[channel].clone();
                        let tracks = cloned_tracks
                            .iter()
                            .cloned()
                            .filter_map(|track| track)
                            .chain(repeat(Track {
                                mute: false,
                                steps: [Step::Off; 16],
                            }));

                        // Zero out the channel's tracks and populate for each note.
                        self.song.sections[self.song.focused_section].tracks[channel] = [None; 128];
                        for (note, track) in notes.zip(tracks) {
                            self.song.sections[self.song.focused_section].tracks[channel][note] =
                                Some(track);
                            self.song.cursor.0 = self.song.sections[self.song.focused_section]
                                .iter()
                                .enumerate()
                                .skip(self.song.cursor.0)
                                .next()
                                .map(|(row, _)| row)
                                .unwrap_or(num_notes - 1);
                        }
                    }
                    self.song.state = UiState::EditSection;
                }
                FieldUpdateResult::Cancel => {
                    self.song.state = UiState::EditSection;
                }
            },

            UiState::EditBpm(field) => match field.update(&event) {
                FieldUpdateResult::NotReady => (),
                FieldUpdateResult::Ready(bpm) => {
                    self.push_to_undo_stack();
                    self.song.bpm = bpm;
                    self.song.state = UiState::EditSection;
                }
                FieldUpdateResult::Cancel => {
                    self.song.state = UiState::EditSection;
                }
            },

            UiState::EditSwing(field) => match field.update(&event) {
                FieldUpdateResult::NotReady => (),
                FieldUpdateResult::Ready(swing) => {
                    self.push_to_undo_stack();
                    self.song.swing = swing;
                    self.song.state = UiState::EditSection;
                }
                FieldUpdateResult::Cancel => {
                    self.song.state = UiState::EditSection;
                }
            },

            UiState::Err(_) => (),
        };
        true
    }
}

fn main() -> Result<(), Error> {
    // Set up panic hook.
    let default_hook = std::panic::take_hook();
    std::panic::set_hook(Box::new(move |info| {
        println!("{}", termion::screen::ToMainScreen);
        default_hook(info);
    }));

    let backend = JackBackend::<(), ()>::new()?;

    // Termion.
    let stdin = stdin();
    let mut stdout = {
        let stdout = stdout().into_raw_mode()?;
        let stdout = AlternateScreen::from(stdout);
        let stdout = MouseTerminal::from(stdout);
        HideCursor::from(stdout)
    };
    let (error_tx, error_rx): (Sender<Error>, Receiver<_>) = crossbeam::bounded(0);
    let (event_tx, event_rx) = crossbeam::bounded(0);
    spawn(move || {
        let result = (|| -> Result<(), Error> {
            for event in stdin.events() {
                let event = event?;
                event_tx.send(event)?;
            }
            Ok(())
        })();
        if let Err(err) = result {
            error_tx.send(err).ok();
        }
    });

    let mut ui = Ui {
        event_rx,
        error_rx,

        paused: false,

        song: Song {
            bpm: 60,
            swing: 0,

            state: UiState::EditSection,
            programs: [0; 16],
            sections: vec![Section {
                tracks: [[None; 128]; 16],
            }],
            focused_section: 0,
            cursor: (0, 0),
            select_anchor: None,
        },

        undo: Vec::new(),
        redo: Vec::new(),

        playback_state: PlaybackState::default(),
        backend: Some(Box::new(backend)),
    };
    let (mut width, mut height) = termion::terminal_size()?;
    while ui.step() {
        let (new_width, new_height) = termion::terminal_size()?;
        if (width, height) != (new_width, new_height) {
            write!(stdout, "{}", clear::All)?;
            width = new_width;
            height = new_height;
        }
        ui.write(&mut stdout)?;
    }

    // Wait for the audio thread to clear MIDI events.
    sleep(Duration::from_secs_f64(0.01));
    Ok(())
}
