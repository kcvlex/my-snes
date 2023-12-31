use modular_bitfield::prelude::*;
use sdl2::pixels::Color;

#[derive(Default)]
pub struct Registers {
    pub inidisp: INIDISP,
    pub objsel: OBJSEL,
    pub oamadd: OAMADD,
    pub bgmode: BGMODE,
    pub mosaic: MOSAIC,
    pub bg_sc: [BGnSC; 4],
    pub bg_nba: [u8; 4],
    pub bg_hofs: [u16; 4],
    pub bg_vofs: [u16; 4],
    pub m7hofs: u16,
    pub m7vofs: u16,
    pub vmain: VMAIN,
    pub vmadd: u16,
    pub m7sel: M7SEL,
    pub m7matrix: [u16; 4],
    pub m7center: [M7CENTER; 2],
    pub cgadd: u8,
    pub w_bg_sel: [WSEL; 4],
    pub w_obj_sel: WSEL,
    pub w_color_sel: WSEL,
    pub wh: [u8; 4],
    pub w_bg_log: [WindowMaskLogic; 4],
    pub w_obj_log: WindowMaskLogic,
    pub w_color_log: WindowMaskLogic,
    pub tm: ScreenDesignate,
    pub ts: ScreenDesignate,
    pub tmw: WindowArea,
    pub tsw: WindowArea,
    pub cgwsel: CGWSEL,
    pub cgadsub: CGADSUB,
    pub setini: SETINI,
    pub stat77: STAT77,
    pub stat78: STAT78,
}

#[bitfield]
#[repr(u8)]
#[derive(Default)]
pub struct INIDISP {
    brightness_bin: B4,
    #[skip]
    __: B3,
    pub forced_blanking: bool,
}

impl INIDISP {
    pub fn brighten(&self, c0: CGDATA) -> CGDATA {
        let brig = self.brightness_bin() as u32;
        if brig == 0 {
            0.into()
        } else {
            let mut c: CGDATA = 0.into();
            c.set_r(((c0.r() as u32) * (brig + 1) / 16) as u8);
            c.set_g(((c0.g() as u32) * (brig + 1) / 16) as u8);
            c.set_b(((c0.b() as u32) * (brig + 1) / 16) as u8);
            c
        }
    }
}

#[bitfield]
#[repr(u8)]
#[derive(Default, Copy, Clone)]
pub struct OBJSEL {
    pub name_base_addr: B3,
    pub name_select: B2,
    obj_size_bin: B3,
}

impl OBJSEL {
    pub fn obj_size(&self) -> ((u8, u8), (u8, u8)) {
        match self.obj_size_bin() {
            0 => ((8, 8), (16, 16)),
            1 => ((8, 8), (32, 32)),
            2 => ((8, 8), (64, 64)),
            3 => ((16, 16), (32, 32)),
            4 => ((16, 16), (64, 64)),
            5 => ((32, 32), (64, 64)),
            6 => ((16, 32), (32, 64)),
            7 => ((16, 32), (32, 32)),
            _ => unreachable!(),
        }
    }
}

#[bitfield]
#[repr(u16)]
#[derive(Clone, Copy, Default)]
pub struct OAMADD {
    pub addr: B9,
    #[skip]
    __: B6,
    pub priority_rotation: bool,
}

impl OAMADD {
    pub fn set(&mut self, v: u8, hi: bool) {
        self.bytes[hi as usize] = v;
    }
}

#[derive(Clone, Copy, Default)]
pub enum ChrSize {
    #[default]
    Normal = 8,
    Twice = 16,
}

impl ChrSize {
    pub fn rdiv(self, v: usize) -> usize {
        match self {
            ChrSize::Normal => v >> 3,
            ChrSize::Twice => v >> 4,
        }
    }

    pub fn rmod(self, v: usize) -> usize {
        match self {
            ChrSize::Normal => v & 7,
            ChrSize::Twice => v & 15,
        }
    }

    pub fn mul(self, v: usize) -> usize {
        match self {
            ChrSize::Normal => v << 3,
            ChrSize::Twice => v << 4,
        }
    }

    pub fn get_offset(self, v: usize, flip: bool) -> usize {
        let v = self.rmod(v);
        if flip {
            (self as usize) - 1 - v
        } else {
            v
        }
    }
}

#[bitfield]
#[repr(u8)]
#[derive(Default, Debug)]
pub struct BGMODE {
    pub bg_mode: B3,
    pub bg3_priority: bool,
    chr_size_bin: B4,
}

impl BGMODE {
    pub fn chr_size(&self, bg: u8) -> (ChrSize, ChrSize) {
        let tmp = self.chr_size_bin();
        let tmp = tmp >> bg;
        let mode = self.bg_mode();
        match (tmp & 1, mode) {
            (0, 5) | (0, 6) => (ChrSize::Twice, ChrSize::Normal),
            (0, _) => (ChrSize::Normal, ChrSize::Normal),
            (1, _) => (ChrSize::Twice, ChrSize::Twice),
            _ => unreachable!(),
        }
    }
}

#[bitfield]
#[repr(u8)]
#[derive(Default)]
pub struct MOSAIC {
    enable_bin: B4,
    pub size: B4,
}

impl MOSAIC {
    pub fn enable(&self, bg: u8) -> bool {
        ((self.enable_bin() >> bg) & 1) == 1
    }
}

#[bitfield]
#[repr(u8)]
#[derive(Default, Debug)]
pub struct BGnSC {
    horizontal_tilemap_count: bool,
    vertical_tilemap_count: bool,
    pub tilemap_vram_addr: B6,
}

impl BGnSC {
    pub fn screen_size(&self) -> [[u8; 2]; 2] {
        match (self.horizontal_tilemap_count(), self.vertical_tilemap_count()) {
            (false, false) => [[0, 0], [0, 0]],
            (false, true) => [[0, 0], [1, 1]],
            (true, false) => [[0, 1], [0, 1]],
            (true, true) => [[0, 1], [2, 3]],
        }
    }
}

#[bitfield]
#[repr(u8)]
#[derive(Default)]
pub struct VMAIN {
    increment_size: B2,
    remapping: B2,
    #[skip]
    __: B3,
    pub increment_mode: bool,
}

impl VMAIN {
    // FIXME : bytes? words?
    pub fn increment_amount(&self) -> u16 {
        match self.increment_size() {
            0 => 1,
            1 => 32,
            2 => 128,
            3 => 128,
            _ => unreachable!(),
        }
    }

    // https://snes.nesdev.org/wiki/PPU_registers#VMAIN
    //
    // 0: None
    // 1: Remap rrrrrrrr YYYccccc -> rrrrrrrr cccccYYY (2bpp)
    // 2: Remap rrrrrrrY YYcccccP -> rrrrrrrc ccccPYYY (4bpp)
    // 3: Remap rrrrrrYY YcccccPP -> rrrrrrcc cccPPYYY (8bpp)
    pub fn remap(&self, addr: u16) -> u16 {
        match self.remapping() {
            0 => addr,
            v @ _ => {
                let p_len = v - 1;
                let p = addr & ((1 << p_len) - 1);
                let ccccc = (addr >> p_len) & 0x1F;
                let yyy = (addr >> (p_len + 5)) & 0x7;
                let mut res = (addr >> (p_len + 8)) << (p_len + 8);
                res |= ccccc << (p_len + 3);
                res |= p << 3;
                res |= yyy;
                // println!("v={} addr={:#018b} res={:#018b}", v, addr, res);
                res
            }
        }
    }
}

#[bitfield]
#[repr(u8)]
#[derive(Default)]
pub struct M7SEL {
    pub flip_horizontal: bool,
    pub flip_vertical: bool,
    #[skip]
    __: B4,
    pub bit_screen_over: B2,
}

pub enum M7ScreenOver {
    Wrap = 0,
    Transparent = 1,
    Tile00 = 2,
}

impl M7SEL {
    pub fn screen_over(&self) -> M7ScreenOver {
        match self.bit_screen_over() {
            0 | 1 => M7ScreenOver::Wrap,
            2 => M7ScreenOver::Transparent,
            3 => M7ScreenOver::Tile00,
            _ => unreachable!(),
        }
    }
}

#[bitfield]
#[repr(u16)]
#[derive(Default)]
pub struct M7CENTER {
    pub center: B13,
    #[skip]
    __: B3,
}

#[bitfield]
#[repr(u16)]
#[derive(Clone, Copy, Default, Debug)]
pub struct CGDATA {
    pub r: B5,
    pub g: B5,
    pub b: B5,
    #[skip]
    __: bool,
}

impl Into<Color> for CGDATA {
    fn into(self) -> Color {
        Color::RGB(self.r() << 3, self.g() << 3, self.b() << 3)
    }
}

#[bitfield(bits = 4)]
#[derive(Default, Clone, Copy, Debug)]
pub struct WSEL {
    pub invert_win1: bool,
    pub enable_win1: bool,
    pub invert_win2: bool,
    pub enable_win2: bool,
}

impl WSEL {
    pub fn set(&mut self, v: u8) {
        self.bytes[0] = v;
    }
}

#[derive(BitfieldSpecifier)]
#[bits = 2]
#[derive(Default, Clone, Copy, Debug)]
pub enum WindowMaskLogic {
    #[default]
    OR = 0,
    AND = 1,
    XOR = 2,
    XNOR = 3,
}

#[bitfield(bits = 4)]
#[derive(Default)]
pub struct WOBJLOG {
    pub obj: WindowMaskLogic,
    pub color: WindowMaskLogic,
}

#[derive(BitfieldSpecifier)]
#[bits = 2]
#[derive(Default, Debug)]
pub enum RegionTypes {
    #[default]
    Nowhere = 0,
    OutsideColorWindow = 1,
    InsideColorWindow = 2,
    Everywhere = 3,
}

impl RegionTypes {
    pub fn in_region(&self, in_win: bool) -> bool {
        match self {
            RegionTypes::Nowhere => false,
            RegionTypes::OutsideColorWindow => !in_win,
            RegionTypes::InsideColorWindow => in_win,
            RegionTypes::Everywhere => true,
        }
    }
}

// TODO: direct_color_mode
#[bitfield]
#[repr(u8)]
#[derive(Default, Debug)]
pub struct CGWSEL {
    pub direct_color_mode: bool,
    pub addend: bool,
    #[skip]
    __: B2,
    pub color_math_enable_region: RegionTypes,
    pub main_screen_black_region: RegionTypes,
}

#[derive(BitfieldSpecifier)]
#[bits = 1]
#[derive(Default)]
pub enum OperatorType {
    #[default]
    Add = 0,
    Sub = 1,
}

#[bitfield]
#[repr(u8)]
#[derive(Default)]
pub struct CGADSUB {
    enable_color_math_bg_bin: B4,
    pub enable_color_math_obj: bool,
    pub enable_backdrop_color_math: bool,
    pub half_color_math: bool,
    operator_type: OperatorType,
}

impl CGADSUB {
    pub fn enable_color_math_bg(&self, bg: usize) -> bool {
        ((self.enable_color_math_bg_bin() >> bg) & 1) == 1
    }

    pub fn op(&self, x: CGDATA, y: CGDATA, div: u8) -> CGDATA {
        match self.operator_type() {
            OperatorType::Add => {
                let r = std::cmp::min(0x1F, (x.r() + y.r()) / div);
                let g = std::cmp::min(0x1F, (x.g() + y.g()) / div);
                let b = std::cmp::min(0x1F, (x.b() + y.b()) / div);
                let mut color: CGDATA = 0.into();
                color.set_r(r);
                color.set_g(g);
                color.set_b(b);
                color
            }
            OperatorType::Sub => {
                let r = (x.r() as u8).saturating_sub(y.r()) / div;
                let g = (x.g() as u8).saturating_sub(y.g()) / div;
                let b = (x.b() as u8).saturating_sub(y.b()) / div;
                let mut color: CGDATA = 0.into();
                color.set_r(r);
                color.set_g(g);
                color.set_b(b);
                color
            }
        }
    }
}

#[bitfield]
#[repr(u8)]
#[derive(Default)]
pub struct COLDATA {
    pub color: B5,
    pub r: bool,
    pub g: bool,
    pub b: bool,
}

#[bitfield]
#[repr(u8)]
#[derive(Default)]
pub struct SETINI {
    pub screen_interlacing: bool,
    pub obj_interlacing: bool,
    pub overscan_mode: bool,
    pub high_res_mode: bool,
    #[skip]
    __: B2,
    pub extbg_mode: bool,
    pub external_sync: bool, // 0=Non Interlace, 1=Interlace
}

#[bitfield]
#[repr(u8)]
#[derive(Default, Clone, Copy)]
pub struct STAT77 {
    pub ppu1_version: B4,
    pub ppu1_open_bus: bool,
    pub master_slave_mode: bool,
    pub range_overflow: bool,
    pub time_overflow: bool,
}

#[bitfield]
#[repr(u8)]
#[derive(Default, Clone, Copy)]
pub struct STAT78 {
    pub ppu2_version: B4,
    pub ntsc_pal_mode: bool,
    pub ppu2_open_bus: bool,
    pub counter_latch_value: bool,
    pub interlace_field: bool, // 0=1st, 1=2nd Frame
}

#[bitfield]
#[repr(u8)]
#[derive(Default)]
pub struct ScreenDesignate {
    enable_bg_bin: B4,
    pub enable_obj: bool,
    #[skip]
    __: B3,
}

impl ScreenDesignate {
    fn enable_bg(&self, bg: usize) -> bool {
        ((self.enable_bg_bin() >> bg) & 1) == 1
    }
}

#[bitfield]
#[repr(u8)]
#[derive(Default)]
pub struct WindowArea {
    apply_win_bg_bin: B4,
    pub apply_win_obj: bool,
    #[skip]
    __: B3,
}

impl WindowArea {
    fn apply_win_bg(&self, bg: usize) -> bool {
        ((self.apply_win_bg_bin() >> bg) & 1) == 1
    }
}

impl Registers {
    pub fn is_main_pixel_enable_bg(&self, bg: usize, hide: bool) -> bool {
        self.tm.enable_bg(bg) && !(self.tmw.apply_win_bg(bg) && hide)
    }

    pub fn is_sub_pixel_enable_bg(&self, bg: usize, hide: bool) -> bool {
        self.ts.enable_bg(bg) && !(self.tsw.apply_win_bg(bg) && hide)
    }

    pub fn is_main_pixel_enable_obj(&self, hide: bool) -> bool {
        self.tm.enable_obj() && !(self.tmw.apply_win_obj() && hide)
    }

    pub fn is_sub_pixel_enable_obj(&self, hide: bool) -> bool {
        self.ts.enable_obj() && !(self.tsw.apply_win_obj() && hide)
    }

    pub fn is_high_resolution(&self) -> bool {
        let mode = self.bgmode.bg_mode();
        if mode == 5 || mode == 6 {
            true
        } else {
            self.setini.high_res_mode()
        }
    }
}
