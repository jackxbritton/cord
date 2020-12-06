mod midi;

use crossbeam::select;
use midi::*;
use std::default::Default;
use std::error::Error;
use std::io::{stdin, stdout, Write};
use std::sync::atomic::AtomicBool;
use std::sync::atomic::Ordering::Relaxed;
use std::thread::{sleep, spawn};
use std::time::Duration;
use termion::event::{Event, Key, MouseEvent};
use termion::input::{MouseTerminal, TermRead};
use termion::raw::IntoRawMode;
use termion::{clear, color, cursor, style};

static RUNNING: AtomicBool = AtomicBool::new(true);

fn main() -> Result<(), Box<dyn Error>> {
    let backend = JackBackend::<()>::new(&RUNNING)?;

    let song = {
        let file = std::fs::File::open("x.yaml")?;
        Song::from_reader(file)?
    };
    backend.song_tx.send(song)?;

    // TODO(jack) Try out termion.
    let stdin = stdin();
    let mut stdout = MouseTerminal::from(stdout().into_raw_mode()?);
    let (event_tx, event_rx) = crossbeam::unbounded();

    spawn(move || {
        || -> Result<(), ()> {
            for event in stdin.events() {
                let event = event.map_err(|_| ())?;
                event_tx.send(event).map_err(|_| ())?;
            }
            Ok(())
        }()
    });

    write!(stdout, "{}{}", cursor::Hide, clear::All)?;
    let (width, height) = termion::terminal_size()?;
    for y in 0..4 {
        write!(stdout, "{}{:3} ", cursor::Goto(1, 1 + y), y)?;
        for _ in 0..8 {
            write!(stdout, " *")?;
        }
    }
    write!(
        stdout,
        "{}{}, {}",
        cursor::Goto(width / 2, height / 2),
        width,
        height,
    )?;
    stdout.flush()?;
    loop {
        select! {
            recv(event_rx) -> event => match event {
                Ok(Event::Key(Key::Ctrl('c'))) => break,
                Err(err) => write!(stdout, "{}{}", cursor::Goto(width / 2, height / 2), err)?,
                _ => (),
            },
            recv(backend.error_rx) -> err => match err {
                Ok(err) => write!(stdout, "{}{}", cursor::Goto(width / 2, height / 2), err)?,
                Err(err) => write!(stdout, "{}{}", cursor::Goto(width / 2, height / 2), err)?,
            },
            recv(backend.clock_rx) -> clock => match clock {
                Ok(clock) => {
                    let clock = backend.clock_rx.try_iter().last().unwrap_or(clock);
                    write!(stdout, "{}{:.2}", cursor::Goto(width / 2, height / 2), clock)?;
                },
                Err(err) => write!(stdout, "{}{}", cursor::Goto(width / 2, height / 2), err)?,
            },
        };
        stdout.flush()?;
    }

    /*
    let mut song = Song::default();
    while RUNNING.load(Relaxed) {
        let new_song = {
            let file = std::fs::File::open("x.yaml")?;
            Song::from_reader(file)
        };
        match new_song {
            Ok(new_song) => {
                if new_song != song {
                    backend.song_tx.send(new_song.clone())?;
                    song = new_song;
                }
            }
            Err(err) => eprintln!("{}", err),
        }
        sleep(Duration::from_secs(1));
    }
    */

    // Wait for the audio thread to clear MIDI events.
    sleep(Duration::from_secs_f64(0.01));
    write!(stdout, "{}\n", cursor::Show)?;
    stdout.flush()?;
    Ok(())
}
