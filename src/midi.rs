use arrayvec::ArrayVec;

#[derive(Copy, Clone, Debug, PartialEq, PartialOrd)]
pub enum Event {
    NoteOff {
        channel: u8,
        note: u8,
    },
    NoteOn {
        channel: u8,
        note: u8,
        velocity: u8,
    },
    ControlChange {
        channel: u8,
        controller: u8,
        value: u8,
    },
}

impl Event {
    pub fn from_bytes(bytes: &[u8]) -> Option<Self> {
        if bytes.len() == 0 {
            return None;
        }
        let kind = bytes[0] & 0xf0;
        match (kind, bytes) {
            (0x80, &[status, note, _velocity]) => Some(Self::NoteOff {
                channel: status & 0x0f,
                note,
            }),
            (0x90, &[status, note, velocity]) if velocity > 0 => Some(Self::NoteOn {
                channel: status & 0x0f,
                note,
                velocity,
            }),
            (0xb0, &[status, controller, value]) => Some(Self::ControlChange {
                channel: status & 0x0f,
                controller,
                value,
            }),
            _ => None,
        }
    }
    pub fn to_bytes(self) -> ArrayVec<u8, 4> {
        let mut out = ArrayVec::new();
        match self {
            Event::NoteOff { channel, note } => {
                out.push(0x80 | channel);
                out.push(note);
                out.push(0);
            }
            Event::NoteOn {
                channel,
                note,
                velocity,
            } => {
                out.push(0x90 | channel);
                out.push(note);
                out.push(velocity);
            }
            Event::ControlChange {
                channel,
                controller,
                value,
            } => {
                out.push(0xb0 | channel);
                out.push(controller);
                out.push(value);
            }
        };
        out
    }
}
