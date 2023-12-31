use crate::ppu::mmio;

pub struct Window {
    left: u16,
    right: u16,
    enable: bool,
    invert: bool,
}

impl Window {
    fn hide(&self, x: u16) -> Option<bool> {
        if !self.enable {
            return None;
        }
        let res = self.left <= x && x <= self.right;
        let res = if self.invert { !res } else { res };
        Some(res)
    }
}

pub struct Windows {
    win1: Window,
    win2: Window,
    logic: mmio::WindowMaskLogic,
}

impl Windows {
    pub fn make(wh: &[u8; 4], wsel: mmio::WSEL, logic: mmio::WindowMaskLogic) -> Self {
        let win1 = Window {
            left: wh[0] as u16,
            right: wh[1] as u16,
            enable: wsel.enable_win1(),
            invert: wsel.invert_win1(),
        };
        let win2 = Window {
            left: wh[2] as u16,
            right: wh[3] as u16,
            enable: wsel.enable_win2(),
            invert: wsel.invert_win2(),
        };
        Self { win1, win2, logic }
    }

    pub fn hide(&self, x: u16) -> bool {
        match (self.win1.hide(x), self.win2.hide(x)) {
            (None, None) => false,
            (Some(x), None) => x,
            (None, Some(x)) => x,
            (Some(x), Some(y)) => match self.logic {
                mmio::WindowMaskLogic::OR => x || y,
                mmio::WindowMaskLogic::AND => x && y,
                mmio::WindowMaskLogic::XOR => x ^ y,
                mmio::WindowMaskLogic::XNOR => !(x ^ y),
            },
        }
    }
}
