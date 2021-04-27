#[derive(Copy, Clone, Debug, PartialEq, PartialOrd)]
pub enum Event {
    Note {
        channel: u8,
        note: u8,
        velocity: f64,
        length: f64,
    },
    ControlChange {
        channel: u8,
        controller: u8,
        value: u8,
    },
}
