mod midi;
mod music;

use midi::*;
use music::*;

use crossbeam::select;
use crossbeam::{Receiver, SendError, Sender};
use std::error::Error;
use std::io::{stdin, stdout, Write};
use std::thread::{sleep, spawn};
use std::time::Duration;
use termion::cursor::HideCursor;
use termion::event::{Event, Key, MouseEvent};
use termion::input::{MouseTerminal, TermRead};
use termion::raw::IntoRawMode;
use termion::screen::AlternateScreen;
use termion::{clear, color, cursor, style};

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
}

impl KeyField {
    fn new() -> Self {
        Self {
            tonic: None,
            accidental: None,
            mode: Mode::Major,
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
            (Self { tonic: Some(_), .. }, &Event::Key(Key::Char('m'))) => {
                self.mode = Mode::Minor;
            }

            // Backspace.
            (
                Self {
                    tonic: Some(_),
                    mode: Mode::Minor,
                    ..
                },
                &Event::Key(Key::Backspace),
            ) => {
                self.mode = Mode::Major;
            }
            (
                Self {
                    tonic: Some(_),
                    accidental: Some(_),
                    ..
                },
                &Event::Key(Key::Backspace),
            ) => {
                self.accidental = None;
            }
            (Self { tonic: Some(_), .. }, &Event::Key(Key::Backspace)) => {
                self.tonic = None;
            }

            (
                Self {
                    tonic: Some(tonic),
                    accidental,
                    mode,
                },
                Event::Key(Key::Char('\n')),
            ) => {
                return FieldUpdateResult::Ready(music::Key {
                    tonic,
                    accidental,
                    mode,
                });
            }
            _ => (),
        };
        FieldUpdateResult::NotReady
    }
}

enum UiState {
    EditBpm(UnsignedField),
    EditSwing(UnsignedField),
    Err(Box<dyn Error>),

    EditSong,
}

#[derive(Copy, Clone)]
struct PercussionStep {
    on: bool,
}

struct PercussionTrack {
    note: u8,
    steps: [PercussionStep; 16],
}

enum PercussionState {
    EditSteps,
    EditNote(UnsignedField),
}

struct Percussion {
    tracks: Vec<PercussionTrack>,
    focused_track: usize,
    focused_step: usize,
    state: PercussionState,
}

trait Update {
    // Return true if the event was consumed.
    fn update(&mut self, event: &Event) -> bool;
}

impl Update for Percussion {
    fn update(&mut self, event: &Event) -> bool {
        match &mut self.state {
            PercussionState::EditSteps => match event {
                Event::Key(Key::Char('p')) => {
                    let track = PercussionTrack {
                        note: 0,
                        steps: [PercussionStep { on: false }; 16],
                    };
                    if self.focused_track + 1 >= self.tracks.len() {
                        self.tracks.push(track);
                        self.focused_track = self.tracks.len() - 1;
                    } else {
                        self.tracks.insert(self.focused_track + 1, track);
                        self.focused_track += 1;
                    }
                }
                _ if self.tracks.len() == 0 => return false,
                Event::Key(Key::Delete) => {
                    self.tracks.remove(self.focused_track);
                    let last_index = self.tracks.len().checked_sub(1);
                    self.focused_track = self.focused_track.min(last_index.unwrap_or(0))
                }
                Event::Key(Key::Char('j')) => {
                    self.focused_track = (self.focused_track + 1).min(self.tracks.len() - 1)
                }
                Event::Key(Key::Char('k')) => {
                    self.focused_track = self.focused_track.checked_sub(1).unwrap_or(0)
                }
                Event::Key(Key::Char('^')) => {
                    self.focused_step = 0;
                }
                Event::Key(Key::Char('$')) => {
                    self.focused_step = self.tracks[self.focused_track].steps.len() - 1;
                }
                Event::Key(Key::Char('l')) => {
                    self.focused_step =
                        (self.focused_step + 1).min(self.tracks[self.focused_track].steps.len() - 1)
                }
                Event::Key(Key::Char('h')) => {
                    self.focused_step = self.focused_step.checked_sub(1).unwrap_or(0)
                }
                Event::Key(Key::Char('g')) => {
                    self.focused_track = 0;
                }
                Event::Key(Key::Char('G')) => self.focused_track = self.tracks.len() - 1,
                Event::Key(Key::Char('.')) => {
                    let step = &mut self.tracks[self.focused_track].steps[self.focused_step];
                    step.on = !step.on;
                }
                Event::Key(Key::Char('>')) => {
                    let on = !self.tracks[self.focused_track]
                        .steps
                        .iter()
                        .any(|step| step.on);
                    for step in &mut self.tracks[self.focused_track].steps {
                        step.on = on;
                    }
                }
                Event::Key(Key::Char('n')) => {
                    self.state = PercussionState::EditNote(UnsignedField::new(10, 127))
                }
                _ => return false,
            },
            PercussionState::EditNote(acc) => match acc.update(event) {
                FieldUpdateResult::NotReady => (),
                FieldUpdateResult::Ready(note) => {
                    self.tracks[self.focused_track].note = note as u8;
                    self.state = PercussionState::EditSteps;
                }
                FieldUpdateResult::Cancel => {
                    self.state = PercussionState::EditSteps;
                }
            },
        };
        true
    }
}

struct MelodyTrack {
    channel: u8,
    steps: [MelodyStep; 16],
}

#[derive(Copy, Clone)]
enum MelodyStep {
    On { degree: u8 },
    Hold,
    Off,
}

enum MelodyState {
    EditSteps,
    EditChannel(UnsignedField),
    EditKey(KeyField),
}

struct Melody {
    state: MelodyState,
    tracks: Vec<MelodyTrack>,
    focused_track: usize,
    focused_step: usize,
    key: music::Key,
}

impl Update for Melody {
    fn update(&mut self, event: &Event) -> bool {
        match &mut self.state {
            MelodyState::EditSteps => match event {
                Event::Key(Key::Char('m')) => {
                    let track = MelodyTrack {
                        channel: 0,
                        steps: [MelodyStep::Off; 16],
                    };
                    if self.focused_track + 1 >= self.tracks.len() {
                        self.tracks.push(track);
                        self.focused_track = self.tracks.len() - 1;
                    } else {
                        self.tracks.insert(self.focused_track + 1, track);
                        self.focused_track += 1;
                    }
                }
                _ if self.tracks.len() == 0 => return false,
                Event::Key(Key::Delete) => {
                    self.tracks.remove(self.focused_track);
                    let last_index = self.tracks.len().checked_sub(1);
                    self.focused_track = self.focused_track.min(last_index.unwrap_or(0))
                }
                Event::Key(Key::Char('j')) => {
                    self.focused_track = (self.focused_track + 1).min(self.tracks.len() - 1)
                }
                Event::Key(Key::Char('k')) => {
                    self.focused_track = self.focused_track.checked_sub(1).unwrap_or(0)
                }
                Event::Key(Key::Char('^')) => {
                    self.focused_step = 0;
                }
                Event::Key(Key::Char('$')) => {
                    self.focused_step = self.tracks[self.focused_track].steps.len() - 1;
                }
                Event::Key(Key::Char('l')) => {
                    self.focused_step =
                        (self.focused_step + 1).min(self.tracks[self.focused_track].steps.len() - 1)
                }
                Event::Key(Key::Char('h')) => {
                    self.focused_step = self.focused_step.checked_sub(1).unwrap_or(0)
                }
                Event::Key(Key::Char('g')) => {
                    self.focused_track = 0;
                }
                Event::Key(Key::Char('G')) => self.focused_track = self.tracks.len() - 1,
                Event::Key(Key::Char('c')) => {
                    self.state = MelodyState::EditChannel(UnsignedField::new(10, 127))
                }
                &Event::Key(Key::Char(ch @ '0'..='9')) => {
                    let degree = ch.to_digit(10).unwrap() as u8;
                    self.tracks[self.focused_track].steps[self.focused_step] =
                        MelodyStep::On { degree };
                    self.focused_step = (self.focused_step + 1)
                        .min(self.tracks[self.focused_track].steps.len() - 1);
                }
                Event::Key(Key::Char('K')) => self.state = MelodyState::EditKey(KeyField::new()),
                &Event::Key(Key::Char('-')) => {
                    let steps = &mut self.tracks[self.focused_track].steps;
                    let previous_step_index =
                        self.focused_step.checked_sub(1).unwrap_or(steps.len() - 1);
                    match steps[previous_step_index] {
                        MelodyStep::Off => (),
                        _ => {
                            steps[self.focused_step] = MelodyStep::Hold;
                            self.focused_step = (self.focused_step + 1).min(steps.len() - 1);
                        }
                    };
                }
                &Event::Key(Key::Char('.')) => {
                    let step = &mut self.tracks[self.focused_track].steps[self.focused_step];
                    *step = match *step {
                        MelodyStep::Off => MelodyStep::On { degree: 0 },
                        _ => MelodyStep::Off,
                    };
                    self.focused_step = (self.focused_step + 1)
                        .min(self.tracks[self.focused_track].steps.len() - 1);
                }
                _ => return false,
            },
            MelodyState::EditChannel(acc) => match acc.update(event) {
                FieldUpdateResult::NotReady => (),
                FieldUpdateResult::Ready(channel) => {
                    self.tracks[self.focused_track].channel = channel as u8;
                    self.state = MelodyState::EditSteps;
                }
                FieldUpdateResult::Cancel => {
                    self.state = MelodyState::EditSteps;
                }
            },
            MelodyState::EditKey(acc) => match acc.update(event) {
                FieldUpdateResult::NotReady => (),
                FieldUpdateResult::Ready(key) => {
                    self.key = key;
                    self.state = MelodyState::EditSteps;
                }
                FieldUpdateResult::Cancel => {
                    self.state = MelodyState::EditSteps;
                }
            },
        };
        true
    }
}

enum SongState {
    EditPercussion,
    EditMelody,
}

struct Song {
    state: SongState,
    percussion: Percussion,
    melody: Melody,
}

impl Update for Song {
    fn update(&mut self, event: &Event) -> bool {
        let consumed = match &self.state {
            SongState::EditPercussion => self.percussion.update(&event),
            SongState::EditMelody => self.melody.update(&event),
        };
        if consumed {
            return true;
        }
        match &event {
            Event::Key(Key::Char('p')) => self.state = SongState::EditPercussion,
            Event::Key(Key::Char('m')) => self.state = SongState::EditMelody,
            _ => return false,
        }
        true
    }
}

struct Ui {
    paused: bool,
    bpm: u32,
    swing: u32,

    clock: f64,
    backend: Option<Box<dyn Backend>>,

    state: UiState,
    song: Song,
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
        let (clock_rx, fatal_rx) = if let Some(backend) = &self.backend {
            (&backend.channels().clock_rx, &backend.channels().fatal_rx)
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
            recv(clock_rx) -> clock => self.clock = clock?,
            recv(fatal_rx) -> err => {
                self.backend = None;
                return Err(err?);
            },
        };
        Ok(true)
    }
    fn handle_input(&mut self, event: Event) -> bool {
        if let Event::Key(Key::Ctrl('c')) = event {
            return false;
        }
        let consumed = match self.state {
            UiState::EditSong => self.song.update(&event),
            // TODO(jack) Update bpm, swing, here. How to propogate state change?
            // We need to signal from `update` that it's exited.
            _ => false,
        };
        if consumed {
            return true;
        }
        // Global controls.
        match (&mut self.state, &event) {
            (_, Event::Key(Key::Char(' '))) => self.paused = !self.paused,

            // Edit bpm.
            (_, Event::Key(Key::Char('b'))) => {
                self.state = UiState::EditBpm(UnsignedField::new(10, 300))
            }
            (UiState::EditBpm(acc), event) => match acc.update(event) {
                FieldUpdateResult::NotReady => (),
                FieldUpdateResult::Ready(bpm) => {
                    self.bpm = bpm;
                    self.state = UiState::EditSong;
                }
                FieldUpdateResult::Cancel => {
                    self.state = UiState::EditSong;
                }
            },

            (_, Event::Key(Key::Char('s'))) => {
                self.state = UiState::EditSwing(UnsignedField::new(10, 9))
            }
            (UiState::EditSwing(acc), event) => match acc.update(event) {
                FieldUpdateResult::NotReady => (),
                FieldUpdateResult::Ready(swing) => {
                    self.swing = swing;
                    self.state = UiState::EditSong;
                }
                FieldUpdateResult::Cancel => {
                    self.state = UiState::EditSong;
                }
            },
            _ => (),
        };
        true
    }
    fn render_to_backend(&self) -> Result<(), SendError<midi::Song>> {
        let backend = match &self.backend {
            Some(backend) => backend,
            None => return Ok(()),
        };
        let events =
            {
                let percussion = self.song.percussion.tracks.iter().flat_map(
                    |PercussionTrack { note, steps }| {
                        steps.iter().enumerate().filter(|(_, &step)| step.on).map(
                            move |(index, _)| midi::Event {
                                time: index as f64 / steps.len() as f64,
                                channel: 9,
                                fields: EventFields::Note {
                                    note: *note,
                                    velocity: 127,
                                    duration: 0.5 / steps.len() as f64,
                                    chance_to_fire: 1.0,
                                },
                            },
                        )
                    },
                );
                let melody =
                    self.song
                        .melody
                        .tracks
                        .iter()
                        .flat_map(|MelodyTrack { channel, steps }| {
                            steps
                                .iter()
                                .enumerate()
                                .filter_map(|(index, &step)| match step {
                                    MelodyStep::On { degree } => Some((index, degree)),
                                    _ => None,
                                })
                                .map(move |(index, degree)| {
                                    let music::Key {
                                        tonic,
                                        accidental,
                                        mode,
                                    } = &self.song.melody.key;
                                    let tonic = match *tonic {
                                        ch @ 'c'..='g' => ch as i32 - 'c' as i32,
                                        ch @ 'a'..='b' => {
                                            ch as i32 - 'a' as i32 + 'g' as i32 - 'c' as i32
                                        }
                                        _ => unreachable!(),
                                    } + match accidental {
                                        Some(Accidental::Flat) => -1,
                                        Some(Accidental::Sharp) => 1,
                                        None => 0,
                                    };
                                    let octave = 4;
                                    let note =
                                        (12 * octave + tonic + mode.semitones(degree as u8) as i32)
                                            as u8;

                                    // Next, measure the hold time.
                                    let steps_to_hold_for = steps
                                        .iter()
                                        .cycle()
                                        .enumerate()
                                        .skip(index + 1)
                                        .filter(|(_, step)| match step {
                                            MelodyStep::Hold => false,
                                            _ => true,
                                        })
                                        .map(|(index, _)| index)
                                        .next()
                                        .unwrap()
                                        - index;

                                    midi::Event {
                                        time: index as f64 / steps.len() as f64,
                                        channel: *channel,
                                        fields: EventFields::Note {
                                            note,
                                            velocity: 127,
                                            duration: (steps_to_hold_for as f64 - 0.5)
                                                / steps.len() as f64,
                                            chance_to_fire: 1.0,
                                        },
                                    }
                                })
                        });
                let mut events: Vec<_> = percussion.chain(melody).collect();
                events.sort_unstable();
                events
            };
        let sections = vec![Section {
            next_section: None,
            events,
        }];
        backend.channels().song_tx.send(midi::Song {
            bpm: if self.paused { 0 } else { self.bpm },
            swing: self.swing as f64 / 9.0,
            sections,
        })?;
        Ok(())
    }
    fn write(&self, writer: &mut impl Write, width: u16, height: u16) -> std::io::Result<()> {
        write!(writer, "{}", cursor::Goto(1, 1))?;

        // Percussion.
        let percussion = &self.song.percussion;
        let percussion_is_focused = match (&self.state, &self.song.state) {
            (UiState::EditSong, SongState::EditPercussion) => true,
            _ => false,
        };
        for (track_index, track) in percussion.tracks.iter().enumerate() {
            let note = match (percussion_is_focused, &percussion.state) {
                (true, PercussionState::EditNote(UnsignedField { acc, .. }))
                    if percussion.focused_track == track_index =>
                {
                    write!(
                        writer,
                        "{}{}",
                        color::Bg(color::Red),
                        color::Fg(color::Black)
                    )?;
                    *acc
                }
                _ => {
                    write!(
                        writer,
                        "{}{}",
                        color::Bg(color::Reset),
                        color::Fg(color::Reset)
                    )?;
                    track.note as u32
                }
            };
            write!(writer, "{:3}", note)?;

            let active_step = (self.clock * track.steps.len() as f64) as usize;
            for (step_index, step) in track.steps.iter().enumerate() {
                match (
                    percussion_is_focused,
                    percussion.focused_track == track_index,
                    percussion.focused_step == step_index,
                ) {
                    (true, true, true) => write!(
                        writer,
                        "{}{}",
                        color::Bg(color::Red),
                        color::Fg(color::Black)
                    ),
                    (true, true, false) => write!(
                        writer,
                        "{}{}",
                        color::Bg(color::Blue),
                        color::Fg(color::Black)
                    ),
                    _ => write!(
                        writer,
                        "{}{}",
                        color::Bg(color::Reset),
                        color::Fg(color::Reset)
                    ),
                }?;
                let ch = match (step.on, active_step == step_index) {
                    (true, true) => '*',
                    (true, false) => '.',
                    _ => ' ',
                };
                write!(writer, " {}", ch)?;
            }
            write!(writer, "\r\n")?;
        }

        // Melody.
        let melody = &self.song.melody;
        let melody_is_focused = match (&self.state, &self.song.state) {
            (UiState::EditSong, SongState::EditMelody) => true,
            _ => false,
        };
        for (track_index, track) in melody.tracks.iter().enumerate() {
            let channel = match (melody_is_focused, &melody.state) {
                (true, MelodyState::EditChannel(UnsignedField { acc, .. }))
                    if melody.focused_track == track_index =>
                {
                    write!(
                        writer,
                        "{}{}",
                        color::Bg(color::Red),
                        color::Fg(color::Black)
                    )?;
                    *acc
                }
                _ => {
                    write!(
                        writer,
                        "{}{}",
                        color::Bg(color::Reset),
                        color::Fg(color::Reset)
                    )?;
                    track.channel as u32
                }
            };
            write!(writer, "{:3}", channel)?;

            let active_step = (self.clock * track.steps.len() as f64) as usize;
            for (step_index, step) in track.steps.iter().enumerate() {
                match (
                    melody_is_focused,
                    melody.focused_track == track_index,
                    melody.focused_step == step_index,
                ) {
                    (true, true, true) => write!(
                        writer,
                        "{}{}",
                        color::Bg(color::Red),
                        color::Fg(color::Black)
                    ),
                    (true, true, false) => write!(
                        writer,
                        "{}{}",
                        color::Bg(color::Blue),
                        color::Fg(color::Black)
                    ),
                    _ => write!(
                        writer,
                        "{}{}",
                        color::Bg(color::Reset),
                        color::Fg(color::Reset)
                    ),
                }?;
                match step {
                    MelodyStep::On { degree } => write!(writer, " {}", degree)?,
                    MelodyStep::Hold => write!(writer, " -")?,
                    MelodyStep::Off => write!(writer, " .")?,
                };
            }
            write!(writer, "\r\n")?;
        }
        write!(
            writer,
            "{}{}{} {} {:.2}",
            color::Fg(color::Reset),
            color::Bg(color::Reset),
            cursor::Goto(width / 2, height / 2),
            if self.paused { "||" } else { " >" },
            self.clock,
        )?;
        let bpm = if let UiState::EditBpm(UnsignedField { acc, .. }) = self.state {
            write!(
                writer,
                "{}{}",
                color::Fg(color::Black),
                color::Bg(color::Red)
            )?;
            acc
        } else {
            self.bpm
        };
        write!(
            writer,
            " bpm={:3}{}{}",
            bpm,
            color::Fg(color::Reset),
            color::Bg(color::Reset),
        )?;
        let swing = if let UiState::EditSwing(UnsignedField { acc, .. }) = self.state {
            write!(
                writer,
                "{}{}",
                color::Fg(color::Black),
                color::Bg(color::Red)
            )?;
            acc
        } else {
            self.swing
        };
        write!(
            writer,
            " swing={}{}{}",
            swing,
            color::Fg(color::Reset),
            color::Bg(color::Reset),
        )?;
        write!(writer, " key=")?;
        match &self.song.melody.state {
            MelodyState::EditKey(acc) => {
                write!(
                    writer,
                    "{}{}",
                    color::Fg(color::Black),
                    color::Bg(color::Red),
                )?;
                let &KeyField {
                    tonic,
                    accidental,
                    mode,
                } = acc;
                if let Some(tonic) = tonic {
                    let key = music::Key {
                        tonic,
                        accidental,
                        mode,
                    };
                    write!(writer, "{}", key)?;
                } else {
                    write!(writer, "   ")?;
                }
                write!(
                    writer,
                    "{}{}",
                    color::Fg(color::Reset),
                    color::Bg(color::Reset),
                )?;
            }
            _ => {
                write!(writer, "{}", self.song.melody.key)?;
            }
        };
        if let UiState::Err(err) = &self.state {
            write!(writer, "{}{}", cursor::Goto(width / 2, height / 2 + 1), err)?;
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
        paused: true,
        bpm: 60,
        swing: 0,

        clock: 0.0,
        backend: Some(Box::new(backend)),

        state: UiState::EditSong,

        song: Song {
            state: SongState::EditPercussion,
            percussion: Percussion {
                state: PercussionState::EditSteps,
                tracks: Vec::new(),
                focused_track: 0,
                focused_step: 0,
            },
            melody: Melody {
                state: MelodyState::EditSteps,
                tracks: Vec::new(),
                focused_track: 0,
                focused_step: 0,
                key: music::Key {
                    tonic: 'c',
                    accidental: None,
                    mode: Mode::Major,
                },
            },
        },
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
