use crossbeam::{Receiver, Sender};
use std::cmp::Ordering;
use std::error::Error;

#[derive(Debug, Copy, Clone, PartialEq, PartialOrd)]
pub enum EventFields {
    Note {
        note: u8,
        velocity: u8,
        duration: f64,
        chance_to_fire: f64,
    },
    ControlChange {
        controller: u8,
        value: u8,
    },
}

#[derive(Debug, Copy, Clone, PartialEq, PartialOrd)]
pub struct Event {
    pub time: f64,
    pub channel: u8,
    pub fields: EventFields,
}

impl Eq for Event {}
impl Ord for Event {
    fn cmp(&self, other: &Self) -> Ordering {
        self.time.partial_cmp(&other.time).unwrap()
    }
}

#[derive(Debug, Clone, Default, PartialEq)]
pub struct Section {
    pub bpm: u32,
    pub swing: f64,
    pub events: Vec<Event>,
}

#[derive(Debug, Clone, Default, PartialEq)]
pub struct Song {
    pub paused: bool,
    pub programs: [u8; 16],
    pub sections: Vec<Section>,
}

#[derive(Default)]
pub struct PlaybackState {
    pub clock: f64,
    pub section: usize,
}

pub struct BackendChannels {
    pub song_tx: Sender<Song>,
    pub quit_tx: Sender<()>,
    pub playback_state_rx: Receiver<PlaybackState>,
    pub fatal_rx: Receiver<Box<dyn Error + Send + Sync>>,
}

pub trait Backend {
    fn channels(&self) -> &BackendChannels;
}