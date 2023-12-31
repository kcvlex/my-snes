#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Button {
    B,
    Y,
    Select,
    Start,
    Up,
    Down,
    Left,
    Right,
    A,
    X,
    L,
    R,
}

const BUTTON_NUM: usize = 12;

#[derive(Default, Copy, Clone)]
pub struct Joypad {
    states: [bool; BUTTON_NUM],
    snapshot: u16,
    latch: bool,
    auto_read: AutoRead,
}

#[derive(Clone, Copy)]
enum AutoRead {
    Finish(u16),
    InProgress(usize),
}

impl Default for AutoRead {
    fn default() -> Self {
        AutoRead::Finish(0)
    }
}

impl Joypad {
    pub fn tick(&mut self, v: u8) {
        let next = match self.auto_read {
            v @ AutoRead::Finish(_) => v.clone(),
            AutoRead::InProgress(rest) => {
                let v = v as usize;
                if rest <= v {
                    self.take_snapshot();
                    let mut v = 0;
                    for _ in 0..BUTTON_NUM {
                        v = (v << 1) | (self.read() as u16);
                    }
                    v <<= 4;
                    AutoRead::Finish(v)
                } else {
                    AutoRead::InProgress(rest - v)
                }
            }
        };
        self.auto_read = next;
    }

    pub fn press(&mut self, button: Button) {
        self.states[button as usize] = true;
    }

    pub fn release(&mut self, button: Button) {
        self.states[button as usize] = false;
    }

    fn take_snapshot(&mut self) {
        self.snapshot = 0;
        for i in (0..BUTTON_NUM).rev() {
            self.snapshot = (self.snapshot << 1) | (self.states[i] as u16);
        }
    }

    pub fn set_latch(&mut self, v: bool) {
        let pre = self.latch;
        self.latch = v;
        if pre && !v {
            self.take_snapshot();
        }
    }

    // NES-style read
    pub fn read(&mut self) -> bool {
        let res = (self.snapshot & 1) == 1;
        if !self.latch {
            self.snapshot >>= 1;
        }
        res
    }

    pub fn start_auto_read(&mut self) {
        self.auto_read = AutoRead::InProgress(4224);
    }

    pub fn auto_read_in_progress(&self) -> bool {
        match self.auto_read {
            AutoRead::InProgress(_) => true,
            AutoRead::Finish(_) => false,
        }
    }

    pub fn auto_read_result(&self) -> Option<u16> {
        match self.auto_read {
            AutoRead::Finish(x) => Some(x),
            AutoRead::InProgress(_) => None,
        }
    }
}
