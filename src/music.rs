use std::iter::once;

#[derive(Copy, Clone)]
pub enum Accidental {
    Flat,
    Sharp,
}

#[derive(Copy, Clone)]
pub enum Mode {
    Major,
    Minor,
}

impl Mode {
    const DIATONIC_INTERVALS: [u8; 7] = [2, 2, 1, 2, 2, 2, 1];
    pub fn intervals(&self) -> impl Iterator<Item = u8> {
        let skip = match self {
            Mode::Major => 0,
            Mode::Minor => 5,
        };
        once(0).chain(Self::DIATONIC_INTERVALS.iter().cycle().skip(skip).scan(
            0,
            |acc, interval| {
                *acc += interval;
                Some(*acc)
            },
        ))
    }
    pub fn semitones(&self, degree: u8) -> u8 {
        self.intervals().skip(degree as usize).next().unwrap()
    }
}

pub struct Key {
    pub tonic: char,
    pub accidental: Option<Accidental>,
    pub mode: Mode,
    pub octave: u8,
}

/*
impl std::fmt::Display for Key {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let Key {
            tonic,
            accidental,
            mode,
        } = self;

        let spaces = 1
            + if let Some(_) = accidental { 1 } else { 0 }
            + if let Mode::Minor = mode { 1 } else { 0 };
        for _ in 0..3 - spaces {
            write!(f, " ")?;
        }

        write!(f, "{}", tonic)?;
        match accidental {
            Some(Accidental::Flat) => write!(f, "b")?,
            Some(Accidental::Sharp) => write!(f, "#")?,
            None => (),
        };
        match mode {
            Mode::Major => (),
            Mode::Minor => write!(f, "m")?,
        }
        Ok(())
    }
}
*/
