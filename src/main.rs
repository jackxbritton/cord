mod midi;

use crossbeam::select;
use crossbeam::{Receiver, Sender};
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

// TODO(jack) Lots to do, but let's start by rendering to a song.

enum UIState {
    EditNote { track: usize, acc: u32 },
    EditSteps { track: usize },
    Err(Box<dyn Error>),
}

struct UI {
    bpm: u32,
    tracks: Vec<(u8, Vec<bool>)>,
    clock: f64,
    state: UIState,
}

impl UI {
    fn step(
        &mut self,
        event_rx: &Receiver<Event>,
        error_rx: &Receiver<Box<dyn Error + Send>>,
        backend: &dyn Backend,
    ) -> bool {
        match self.step_helper(event_rx, error_rx, backend) {
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
        backend: &dyn Backend,
    ) -> Result<bool, Box<dyn Error>> {
        select! {
            recv(event_rx) -> event => {
                if !self.handle_input(event?) {
                    backend.quit_tx().send(()).ok();
                    return Ok(false);
                }

                // TODO(jack) Render to song and send to the backend.
                let events = self .tracks.iter().flat_map(move |(note, steps)| steps.iter() .enumerate() .filter(|(_index, step)| **step).map(move |(index, _step)| midi::Event{time: index as f64 / steps.len() as f64, channel: 9, fields: EventFields::Note{note: *note, velocity: 127, duration: 0.5 / steps.len() as f64, chance_to_fire: 1.0}})).collect();
                backend.song_tx().send(Song{
                    bpm: self.bpm,
                    swing: 0.0,
                    sections: vec![Section{next_section: None, events}],
                })?;
            },
            recv(error_rx) -> err => return Err(err?),
            recv(backend.clock_rx()) -> clock => self.clock = clock?,
        };
        Ok(true)
    }
    fn handle_input(&mut self, event: Event) -> bool {
        match (&mut self.state, event) {
            (_, Event::Key(Key::Ctrl('c'))) => return false,
            (UIState::EditSteps { .. }, Event::Key(Key::Char('='))) => {
                self.bpm = (self.bpm + 1).min(300)
            }
            (UIState::EditSteps { .. }, Event::Key(Key::Char('-'))) => {
                self.bpm = self.bpm.checked_sub(1).unwrap_or(0)
            }
            (UIState::EditSteps { .. }, Event::Key(Key::Insert)) => {
                self.tracks.push((0, (0..8).map(|_| false).collect()));
            }
            (UIState::EditSteps { track }, Event::Key(Key::Delete)) => {
                self.tracks.remove(*track);
                *track = (*track).min(self.tracks.len() - 1);
            }
            (&mut UIState::EditSteps { track }, Event::Key(Key::Char('j'))) => {
                if self.tracks.len() > 0 {
                    self.state = UIState::EditSteps {
                        track: (track + 1) % self.tracks.len(),
                    };
                }
            }
            (&mut UIState::EditSteps { track }, Event::Key(Key::Char('k'))) => {
                if self.tracks.len() > 0 {
                    self.state = UIState::EditSteps {
                        track: (track + self.tracks.len() - 1) % self.tracks.len(),
                    };
                }
            }
            (&mut UIState::EditSteps { track }, Event::Key(Key::Char(ch @ '0'..='9'))) => {
                let step = ch.to_digit(10).unwrap() as usize;
                if let Some(step) = self.tracks[track].1.get_mut(step) {
                    *step = !*step;
                }
            }
            (&mut UIState::EditSteps { track }, Event::Key(Key::Char('n'))) => {
                self.state = UIState::EditNote { track, acc: 0 }
            }
            (UIState::EditNote { acc, .. }, Event::Key(Key::Char(ch @ '0'..='9'))) => {
                let new_acc = 10 * *acc + ch.to_digit(10).unwrap();
                if new_acc <= 999 {
                    *acc = new_acc;
                }
            }
            (UIState::EditNote { acc, .. }, Event::Key(Key::Backspace)) => *acc /= 10,
            (&mut UIState::EditNote { track, acc }, Event::Key(Key::Char('\n'))) => {
                self.tracks[track].0 = acc.min(127) as u8;
                self.state = UIState::EditSteps { track };
            }
            _ => (),
        };
        true
    }
    fn write<W>(&self, writer: &mut W, width: u16, height: u16) -> std::io::Result<()>
    where
        W: Write,
    {
        // Render state.
        write!(writer, "{}", cursor::Goto(1, 1))?;
        for (track, (note, steps)) in self.tracks.iter().enumerate() {
            let note = match self.state {
                UIState::EditNote {
                    track: focused_track,
                    acc,
                } if track == focused_track => {
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
            let focused = match self.state {
                UIState::EditSteps {
                    track: focused_track,
                    ..
                } => focused_track == track,
                _ => false,
            };
            if focused {
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
                write!(writer, " {}", ch)?;
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
            "{}{} - {:.2}",
            cursor::Goto(width / 2, height / 2),
            self.bpm,
            self.clock,
        )?;
        if let UIState::Err(err) = &self.state {
            write!(writer, "{}{}", cursor::Goto(width / 2, height / 2 + 1), err)?;
        }
        writer.flush()?;
        Ok(())
    }
}

fn main() -> Result<(), Box<dyn Error>> {
    let (error_tx, error_rx): (Sender<Box<dyn Error + Send>>, Receiver<_>) = crossbeam::bounded(0);
    let backend = JackBackend::<(), ()>::new(error_tx.clone())?;

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
    let (event_tx, event_rx) = crossbeam::unbounded();
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
        bpm: 60,
        tracks: Vec::new(),
        state: UIState::EditSteps { track: 0 },
        clock: 0.0,
    };
    let (mut width, mut height) = termion::terminal_size()?;
    while ui.step(&event_rx, &error_rx, &backend) {
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
