mod midi;

use crossbeam::select;
use crossbeam::{Receiver, SendError, Sender};
use midi::*;
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

enum UIState {
    EditNote { acc: u32 },
    EditSteps,
    EditBpm { acc: u32 },
    EditSwing { acc: u32 },
    Err(Box<dyn Error>),
}

type Track = (u8, Vec<bool>);

struct UI<'a> {
    paused: bool,
    bpm: u32,
    swing: u32,
    tracks: Vec<Track>,
    clock: f64,
    state: UIState,
    focused_track: usize,
    focused_step: usize,
    backend: Option<&'a dyn Backend>,
}

impl<'a> UI<'a> {
    fn step(
        &mut self,
        event_rx: &Receiver<Event>,
        error_rx: &Receiver<Box<dyn Error + Send>>,
    ) -> bool {
        match self.step_helper(event_rx, error_rx) {
            Ok(quit) => quit,
            Err(err) => {
                self.state = UIState::Err(err);
                false
            }
        }
    }
    fn step_helper(
        &mut self,
        event_rx: &Receiver<Event>,
        error_rx: &Receiver<Box<dyn Error + Send>>,
    ) -> Result<bool, Box<dyn Error>> {
        let never = crossbeam::channel::never();
        let clock_rx = self
            .backend
            .map(|backend| backend.clock_rx())
            .unwrap_or(&never);
        let never = crossbeam::channel::never();
        let fatal_rx = self
            .backend
            .map(|backend| backend.fatal_rx())
            .unwrap_or(&never);
        select! {
            recv(event_rx) -> event => {
                if !self.handle_input(event?) {
                    if let Some(backend) = self.backend {
                        backend.quit_tx().send(()).ok();
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
        let step_chars = [
            '1', '!', '2', '@', '3', '#', '4', '$', '5', '%', '6', '^', '7', '&', '8', '*',
        ];
        match (&mut self.state, self.tracks.len(), event) {
            (_, _, Event::Key(Key::Ctrl('c'))) => return false,
            (_, _, Event::Key(Key::Char(' '))) => self.paused = !self.paused,
            (UIState::EditSteps { .. }, _, Event::Key(Key::Char('='))) => {
                self.bpm = (self.bpm + 1).min(300)
            }
            (UIState::EditSteps { .. }, _, Event::Key(Key::Char('-'))) => {
                self.bpm = self.bpm.checked_sub(1).unwrap_or(0)
            }
            (&mut UIState::EditSteps, len, Event::Key(Key::Insert)) => {
                let track = (0, (0..16).map(|_| false).collect());
                match len {
                    len if len == 0 || self.focused_track == len - 1 => {
                        self.focused_track = len;
                        self.tracks.push(track);
                    }
                    _ => {
                        self.focused_track += 1;
                        self.tracks.insert(self.focused_track, track);
                    }
                };
            }
            (&mut UIState::EditSteps, 0, _) => (), // If zero tracks, there's not much we can do here.
            (UIState::EditSteps, _, Event::Key(Key::Delete)) => {
                self.tracks.remove(self.focused_track);
                self.focused_track = if self.tracks.len() == 0 {
                    0
                } else {
                    self.focused_track.min(self.tracks.len() - 1)
                };
            }
            (&mut UIState::EditSteps, _, Event::Key(Key::Char('j'))) => {
                self.focused_track = (self.focused_track + 1) % self.tracks.len();
            }
            (&mut UIState::EditSteps, _, Event::Key(Key::Char('k'))) => {
                self.focused_track =
                    (self.focused_track + self.tracks.len() - 1) % self.tracks.len();
            }
            (&mut UIState::EditSteps, _, Event::Key(Key::Char(ch))) if step_chars.contains(&ch) => {
                let step = step_chars.iter().position(|&other| other == ch).unwrap();
                if let Some(on) = self.tracks[self.focused_track].1.get_mut(step) {
                    self.focused_step = step;
                    *on = !*on;
                }
            }

            // Edit note.
            (&mut UIState::EditSteps, _, Event::Key(Key::Char('n'))) => {
                self.state = UIState::EditNote { acc: 0 };
            }
            (UIState::EditNote { acc }, _, Event::Key(Key::Char(ch @ '0'..='9'))) => {
                let new_acc = 10 * *acc + ch.to_digit(10).unwrap();
                if new_acc <= 999 {
                    *acc = new_acc;
                }
            }
            (UIState::EditNote { acc, .. }, _, Event::Key(Key::Backspace)) => *acc /= 10,
            (&mut UIState::EditNote { acc }, _, Event::Key(Key::Char('\n'))) => {
                self.tracks[self.focused_track].0 = acc.min(127) as u8;
                self.state = UIState::EditSteps;
            }

            // Edit bpm.
            (&mut UIState::EditSteps { .. }, _, Event::Key(Key::Char('b'))) => {
                self.state = UIState::EditBpm { acc: 0 }
            }
            (UIState::EditBpm { acc }, _, Event::Key(Key::Char(ch @ '0'..='9'))) => {
                let new_acc = 10 * *acc + ch.to_digit(10).unwrap();
                if new_acc <= 999 {
                    *acc = new_acc;
                }
            }
            (UIState::EditBpm { acc }, _, Event::Key(Key::Backspace)) => *acc /= 10,
            (&mut UIState::EditBpm { acc }, _, Event::Key(Key::Char('\n'))) => {
                self.bpm = acc.min(300);
                self.state = UIState::EditSteps;
            }

            // Edit swing.
            (&mut UIState::EditSteps { .. }, _, Event::Key(Key::Char('s'))) => {
                self.state = UIState::EditSwing { acc: 0 }
            }
            (UIState::EditSwing { acc }, _, Event::Key(Key::Char(ch @ '0'..='9'))) => {
                let new_acc = 10 * *acc + ch.to_digit(10).unwrap();
                if new_acc <= 9 {
                    *acc = new_acc;
                }
            }
            (UIState::EditSwing { acc }, _, Event::Key(Key::Backspace)) => *acc /= 10,
            (&mut UIState::EditSwing { acc }, _, Event::Key(Key::Char('\n'))) => {
                self.swing = acc.min(9);
                self.state = UIState::EditSteps;
            }

            _ => (),
        };
        true
    }
    fn render_to_backend(&self) -> Result<(), SendError<Song>> {
        if let Some(backend) = self.backend {
            let events = {
                let mut events: Vec<_> = self
                    .tracks
                    .iter()
                    .flat_map(move |(note, steps)| {
                        steps
                            .iter()
                            .enumerate()
                            .filter(|(_index, step)| **step)
                            .map(move |(index, _step)| midi::Event {
                                time: index as f64 / steps.len() as f64,
                                channel: 9,
                                fields: EventFields::Note {
                                    note: *note,
                                    velocity: 127,
                                    duration: 0.5 / steps.len() as f64,
                                    chance_to_fire: 1.0,
                                },
                            })
                    })
                    .collect();
                events.sort_unstable();
                events
            };
            let sections = vec![Section {
                next_section: None,
                events,
            }];
            backend.song_tx().send(Song {
                bpm: if self.paused { 0 } else { self.bpm },
                swing: self.swing as f64 / 9.0,
                sections,
            })?;
        }
        Ok(())
    }
    fn write<W>(&self, writer: &mut W, width: u16, height: u16) -> std::io::Result<()>
    where
        W: Write,
    {
        // Render state.
        write!(writer, "{}", cursor::Goto(1, 1))?;
        for (track, (note, steps)) in self.tracks.iter().enumerate() {
            let track_is_focused = self.focused_track == track;
            let note = match self.state {
                UIState::EditNote { acc } if track_is_focused => {
                    write!(
                        writer,
                        "{}{}",
                        color::Fg(color::Black),
                        color::Bg(color::Red),
                    )?;
                    acc
                }
                _ => *note as u32,
            };
            write!(
                writer,
                "{:3}{}{}",
                note,
                color::Fg(color::Reset),
                color::Bg(color::Reset),
            )?;
            let active_step = (self.clock * steps.len() as f64) as usize;
            if track_is_focused {
                write!(
                    writer,
                    "{}{}",
                    color::Fg(color::Black),
                    color::Bg(color::Blue)
                )?;
            }
            for (step, &on) in steps.iter().enumerate() {
                let ch = match (step == active_step, on) {
                    (true, true) => '*',
                    (false, true) => '.',
                    (_, false) => ' ',
                };
                if track_is_focused && self.focused_step == step {
                    write!(
                        writer,
                        "{}{} {}{}",
                        color::Fg(color::Black),
                        color::Bg(color::Red),
                        ch,
                        color::Bg(color::Blue)
                    )?;
                } else {
                    write!(writer, " {}", ch)?;
                }
            }
            write!(
                writer,
                "{}{}",
                color::Fg(color::Reset),
                color::Bg(color::Reset),
            )?;
            write!(writer, "\r\n")?;
        }
        write!(
            writer,
            "{}{} {:.2}",
            cursor::Goto(width / 2, height / 2),
            if self.paused { "||" } else { " >" },
            self.clock,
        )?;
        let bpm = if let UIState::EditBpm { acc } = self.state {
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
        let swing = if let UIState::EditSwing { acc } = self.state {
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
        if let UIState::Err(err) = &self.state {
            write!(
                writer,
                "{}ERROR -> {}",
                cursor::Goto(width / 2, height / 2 + 1),
                err
            )?;
        }
        writer.flush()?;
        Ok(())
    }
}

fn main() -> Result<(), Box<dyn Error>> {
    let backend = JackBackend::<(), ()>::new()?;

    /*
    let song = {
        let file = std::fs::File::open("x.yaml")?;
        Song::from_reader(file)?
    };
    backend.song_tx.send(song)?;
    */

    // Termion.
    let stdin = stdin();
    let mut stdout = {
        let stdout = stdout().into_raw_mode()?;
        let stdout = AlternateScreen::from(stdout);
        let stdout = MouseTerminal::from(stdout);
        let stdout = HideCursor::from(stdout);
        stdout
    };
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
    let mut ui = UI {
        paused: true,
        bpm: 60,
        swing: 0,
        tracks: Vec::new(),
        state: UIState::EditSteps,
        focused_track: 0,
        focused_step: 0,
        clock: 0.0,
        backend: Some(&backend),
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
