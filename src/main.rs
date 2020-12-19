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
    EditPercussion,
    EditPercussionNote { acc: u32 },
    EditBpm { acc: u32 },
    EditSwing { acc: u32 },
    Err(Box<dyn Error>),
}

#[derive(Copy, Clone)]
struct PercussionStep {
    on: bool,
}

struct PercussionTrack {
    note: u8,
    steps: Vec<PercussionStep>,
}

struct MelodyTrack {
    channel: u8,
    steps: Vec<MelodyStep>,
}

#[derive(Copy, Clone)]
enum MelodyStep {
    Note { note: u8 },
    Hold,
    None,
}

struct UI {
    paused: bool,
    bpm: u32,
    swing: u32,
    percussion: Vec<PercussionTrack>,
    percussion_focused_track: usize,
    percussion_focused_step: usize,
    clock: f64,
    state: UIState,
    backend: Option<Box<dyn Backend>>,
}

impl UI {
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
        // Global controls.
        match (&mut self.state, event) {
            (_, Event::Key(Key::Ctrl('c'))) => return false,
            (_, Event::Key(Key::Char(' '))) => self.paused = !self.paused,

            // Edit bpm.
            (_, Event::Key(Key::Char('b'))) => self.state = UIState::EditBpm { acc: 0 },
            (UIState::EditBpm { acc }, Event::Key(Key::Char(ch @ '0'..='9'))) => {
                let new_acc = 10 * *acc + ch.to_digit(10).unwrap();
                if new_acc <= 999 {
                    *acc = new_acc;
                }
            }
            (UIState::EditBpm { acc }, Event::Key(Key::Backspace)) => *acc /= 10,
            (UIState::EditBpm { acc }, Event::Key(Key::Char('\n'))) => {
                self.bpm = (*acc).min(300);
                self.state = UIState::EditPercussion;
            }

            // Edit swing.
            (_, Event::Key(Key::Char('s'))) => self.state = UIState::EditSwing { acc: 0 },
            (UIState::EditSwing { acc }, Event::Key(Key::Char(ch @ '0'..='9'))) => {
                let new_acc = 10 * *acc + ch.to_digit(10).unwrap();
                if new_acc <= 9 {
                    *acc = new_acc;
                }
            }
            (UIState::EditSwing { acc }, Event::Key(Key::Backspace)) => *acc /= 10,
            (UIState::EditSwing { acc }, Event::Key(Key::Char('\n'))) => {
                self.swing = (*acc).min(9);
                self.state = UIState::EditPercussion;
            }

            // Edit percussion note.
            (UIState::EditPercussionNote { acc }, Event::Key(Key::Char(ch @ '0'..='9'))) => {
                let new_acc = 10 * *acc + ch.to_digit(10).unwrap();
                if new_acc <= 999 {
                    *acc = new_acc;
                }
            }
            (UIState::EditPercussionNote { acc }, Event::Key(Key::Backspace)) => *acc /= 10,
            (UIState::EditPercussionNote { acc }, Event::Key(Key::Char('\n'))) => {
                self.percussion[self.percussion_focused_track].note = (*acc).min(127) as u8;
                self.state = UIState::EditPercussion;
            }

            // Percussion.
            (_, Event::Key(Key::Char('p'))) => self.state = UIState::EditPercussion,
            (UIState::EditPercussion, event) => match event {
                Event::Key(Key::Insert) if self.percussion.len() < 10 => {
                    let track = PercussionTrack {
                        note: 0,
                        steps: (0..16).map(|_| PercussionStep { on: false }).collect(),
                    };
                    if self.percussion_focused_track + 1 >= self.percussion.len() {
                        self.percussion.push(track);
                        self.percussion_focused_track += 1;
                    } else {
                        self.percussion
                            .insert(self.percussion_focused_track + 1, track);
                        self.percussion_focused_track += 1;
                    }
                }
                Event::Key(Key::Delete) if self.percussion.len() > 1 => {
                    self.percussion.remove(self.percussion_focused_track);
                    self.percussion_focused_track =
                        self.percussion_focused_track.min(self.percussion.len() - 1)
                }
                Event::Key(Key::Char('j')) => {
                    self.percussion_focused_track =
                        (self.percussion_focused_track + 1).min(self.percussion.len() - 1)
                }
                Event::Key(Key::Char('k')) => {
                    self.percussion_focused_track =
                        self.percussion_focused_track.checked_sub(1).unwrap_or(0)
                }
                Event::Key(Key::Char('0')) | Event::Key(Key::Char('^')) => {
                    self.percussion_focused_step = 0;
                }
                Event::Key(Key::Char('$')) => {
                    self.percussion_focused_step =
                        self.percussion[self.percussion_focused_track].steps.len() - 1;
                }
                Event::Key(Key::Char('l')) => {
                    self.percussion_focused_step = (self.percussion_focused_step + 1)
                        .min(self.percussion[self.percussion_focused_track].steps.len() - 1)
                }
                Event::Key(Key::Char('h')) => {
                    self.percussion_focused_step =
                        self.percussion_focused_step.checked_sub(1).unwrap_or(0)
                }
                Event::Key(Key::Char('g')) => {
                    self.percussion_focused_track = 0;
                }
                Event::Key(Key::Char('G')) => {
                    self.percussion_focused_track = self.percussion.len() - 1
                }
                Event::Key(Key::Char('.')) => {
                    if let Some(step) = self.percussion[self.percussion_focused_track]
                        .steps
                        .get_mut(self.percussion_focused_step)
                    {
                        step.on = !step.on;
                    }
                }
                Event::Key(Key::Char('>')) => {
                    let on = !self.percussion[self.percussion_focused_track]
                        .steps
                        .iter()
                        .any(|step| step.on);
                    for step in &mut self.percussion[self.percussion_focused_track].steps {
                        step.on = on;
                    }
                }
                Event::Key(Key::Char('n')) => self.state = UIState::EditPercussionNote { acc: 0 },
                _ => (),
            },

            _ => (),
        };
        true
    }
    fn render_to_backend(&self) -> Result<(), SendError<Song>> {
        let backend = match &self.backend {
            Some(backend) => backend,
            None => return Ok(()),
        };
        let events = {
            let mut events: Vec<_> =
                self.percussion
                    .iter()
                    .flat_map(|PercussionTrack { note, steps }| {
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
                    })
                    .collect();
            events.sort_unstable();
            events
        };
        let sections = vec![Section {
            next_section: None,
            events,
        }];
        backend.channels().song_tx.send(Song {
            bpm: if self.paused { 0 } else { self.bpm },
            swing: self.swing as f64 / 9.0,
            sections,
        })?;
        Ok(())
    }
    fn write<W>(&self, writer: &mut W, width: u16, height: u16) -> std::io::Result<()>
    where
        W: Write,
    {
        // Render state.
        for (track_index, track) in self.percussion.iter().enumerate() {
            let note = match self.state {
                UIState::EditPercussionNote { acc }
                    if self.percussion_focused_track == track_index =>
                {
                    write!(
                        writer,
                        "{}{}",
                        color::Bg(color::Red),
                        color::Fg(color::Black)
                    )?;
                    acc
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
            write!(
                writer,
                "{}{:3}",
                cursor::Goto(1, 1 + track_index as u16),
                note
            )?;

            let active_step = (self.clock * track.steps.len() as f64) as usize;
            for (step_index, step) in track.steps.iter().enumerate() {
                match (
                    self.percussion_focused_track == track_index,
                    self.percussion_focused_step == step_index,
                ) {
                    (true, true) => write!(
                        writer,
                        "{}{}",
                        color::Bg(color::Red),
                        color::Fg(color::Black)
                    ),
                    (true, false) => write!(
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
                let goto = cursor::Goto(1 + 3 + step_index as u16 * 2, 1 + track_index as u16);
                let ch = match (step.on, active_step == step_index) {
                    (true, true) => '*',
                    (true, false) => '.',
                    _ => ' ',
                };
                write!(writer, "{} {}", goto, ch)?;
            }
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

    /*
    let song = {
        let file = std::fs::File::open("x.yaml")?;
        Song::from_reader(file)?
    };
    backend.song_tx.send(song)?;
    */

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
    let mut ui = UI {
        paused: true,
        bpm: 60,
        swing: 0,
        percussion: vec![PercussionTrack {
            note: 0,
            steps: (0..16).map(|_| PercussionStep { on: false }).collect(),
        }],
        percussion_focused_step: 0,
        percussion_focused_track: 0,
        state: UIState::EditPercussion,
        clock: 0.0,
        backend: Some(Box::new(backend)),
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
