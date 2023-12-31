#![allow(dead_code)]

use crate::emulator::PendingEvents;
use crate::memory::{HV_TimerIRQ, Memory};
use media;
use modular_bitfield::prelude::*;
use sdl2::pixels::Color;

mod mmio;
mod sprite;
mod tile;
mod windows;

#[derive(Default)]
pub struct PPU {
    regs: mmio::Registers,

    mpy: i32,
    fixed_color: mmio::CGDATA,

    bgofs_latch: u8,
    bghofs_latch: u8,
    vram_latch: u16,
    mode7_latch: u8,
    cgram_latch: u8,
    cgram_byte: bool,
    counter_latch: bool,
    oam_latch: u8,
    internal_oamadd: u16,
    ophct_byte: bool, // OutPut Horizontal Counter Byte
    opvct_byte: bool, // OutPut Vertical Counter Byte

    horizontal: u16,
    vertical: u16,
    frame: u32,

    vram: Vec<u8>,
    oam: Vec<u8>,
    cgram: Vec<mmio::CGDATA>,

    ppu1_open_bus: u8,
    ppu2_open_bus: u8,

    renderer: Option<media::render::Renderer>,
    line_buffer: Vec<(Color, Color)>,
    sprites_line_buffer: Vec<Option<ObjPixel>>,
    main_line_buffer: Vec<Option<LinePixel>>,
    sub_line_buffer: Vec<Option<LinePixel>>,

    cycles: u64,
}

#[derive(Clone, Copy, Debug)]
struct BGPixel {
    bg: u8, // FIXME: necessary?
    color: mmio::CGDATA,
}

#[derive(Clone, Copy, Debug)]
struct ObjPixel {
    priority: u8,
    palette: u8,
    color: mmio::CGDATA,
}

#[derive(Clone, Copy, Debug)]
enum LinePixel {
    BG(BGPixel),
    OBJ(ObjPixel),
}

impl LinePixel {
    fn color(&self) -> mmio::CGDATA {
        match self {
            LinePixel::BG(BGPixel { color, .. }) => *color,
            LinePixel::OBJ(ObjPixel { color, .. }) => *color,
        }
    }
}

#[bitfield]
#[repr(u8)]
#[derive(Default)]
struct WLOGINPUT {
    wlog1: mmio::WindowMaskLogic,
    wlog2: mmio::WindowMaskLogic,
    wlog3: mmio::WindowMaskLogic,
    wlog4: mmio::WindowMaskLogic,
}

#[derive(Clone, Copy, PartialEq)]
enum InterlaceState {
    Normal,
    Interlace1,
    Interlace2,
}

impl PPU {
    pub fn init(renderer: media::render::Renderer) -> Self {
        Self {
            vram: vec![0; 0x10000],
            oam: vec![0; 544],
            cgram: vec![0.into(); 256],
            line_buffer: vec![(Color::BLACK, Color::BLACK); 256],
            sprites_line_buffer: vec![None; 256],
            main_line_buffer: vec![None; 256],
            sub_line_buffer: vec![None; 256],
            renderer: Some(renderer),
            ..Default::default()
        }
    }

    fn interlace_state(&self) -> InterlaceState {
        let mode = self.regs.bgmode.bg_mode();
        if (mode != 5 && mode != 6) || !self.regs.setini.screen_interlacing() {
            InterlaceState::Normal
        } else if !self.regs.stat78.interlace_field() {
            InterlaceState::Interlace1
        } else {
            InterlaceState::Interlace2
        }
    }

    fn send_to_renderer(&mut self, y: i16) {
        if self.regs.inidisp.forced_blanking() {
            return;
        }
        let interlace = self.interlace_state();
        let renderer = self.renderer.as_mut().unwrap();
        let w = 1;
        let h = match interlace {
            InterlaceState::Normal => 2,
            InterlaceState::Interlace1 | InterlaceState::Interlace2 => 1,
        };
        let frame = self.frame;
        let y = 2 * y + if interlace == InterlaceState::Interlace2 { 1 } else { 0 };
        let mut push = |color: Color, x: i16| {
            renderer.push(media::render::Pixel { x, y, w, h, color, frame });
        };
        for (x, data) in self.line_buffer.iter().enumerate() {
            let x = x as i16;
            let (c0, c1) = data;
            push(*c0, 2 * x + 0);
            push(*c1, 2 * x + 1);
        }
    }

    pub fn tick(&mut self, master: u64, memory: &Memory, events: &mut PendingEvents) -> Result<(), String> {
        while self.cycles < master {
            self.tick_aux(memory, events)?;
        }
        Ok(())
    }

    fn tick_aux(&mut self, memory: &Memory, events: &mut PendingEvents) -> Result<(), String> {
        // long line is a feature for 50hz (i.e. pal), which we don't assume
        let is_short_line = self.regs.stat78.interlace_field() && self.regs.setini.external_sync() && self.vertical == 240;
        let spend = if is_short_line {
            4
        } else if 336 <= self.horizontal {
            5
        } else {
            4
        };

        let max_dots: u16 = 339; // TODO: 340 if long line
        let max_lines: u16 = 261; // TODO: 311 when pal
        let max_lines = max_lines + if self.regs.stat78.interlace_field() { 1 } else { 0 }; // extra scanline
        let max_visible_lines: u16 = (if self.regs.setini.overscan_mode() { 240 } else { 224 }) + 1; // TODO?: is +1 necessary?
        self.renderer.as_ref().expect("renderer is uninitialized");

        let is_visible = 1 <= self.vertical && self.vertical <= max_visible_lines;

        // ref: https://problemkaputt.de/fullsnes.htm#snestiminghvevents
        //
        // SNES Timing H/V Events
        // Detailed List (H=Horizontal, V=Vertical, F=Field)
        //
        // H=0, V=0, F=0         SNES starts at this time after /RESET
        // H=0, V=0              clear Vblank flag, and reset NMI flag (auto ack)
        // H=0, V=225            set Vblank flag
        // H=0.5, V=225          set NMI flag
        // H=1                   clear hblank flag
        // H=1, V=0              toggle interlace FIELD flag
        // H=HTIME+3.5           H-IRQ
        // H=2.5, V=VTIME        V-IRQ   (or HV-IRQ with HTIME=0)
        // H=HTIME+3.5, V=VTIME  HV-IRQ  (when HTIME=1..339)
        // H=6, V=0              reload HDMA registers
        // H=10, V=225           reload OAMADD
        // H=22-277(?), V=1-224  draw picture
        // H=32.5..95.5, V=225   around here, joypad read begins (duration 4224 clks)
        // H=133.5               around here, REFRESH begins (duration 40 clks/10 dots)
        // H=274                 set hblank flag
        // H=278, V=0..224       perform HDMA transfers
        // H=323,327             seen as long-PPU-dots (but not as long-CPU-dots)
        // H=323,327, V=240, F=1 seen as normal-PPU-dots (in short scanline 240) (60Hz)
        // H=339                 this is last PPU-dot (in normal and short scanlines)
        // H=340, V=311, F=1     this is last PPU-dot in long scanlines (50Hz+Interlace)
        // CPU.H=339             this is last CPU-dot (in normal scanlines)
        // CPU.H=338, V=240, F=1 this is last CPU-dot (in short scanlines)
        // CPU.H=340, V=311, F=1 this is last CPU-dot (in long scanlines)
        // H=0?, V=0             reset OBJ overflow flags in 213Eh (only if not f-blank)
        // H=0?+INDEX*2, V=YLOC  set OBJ overflow bit6 (too many OBJs in next line)
        // H=0?, V=YLOC+1        set OBJ overflow bit7 (too many OBJ pix in this line)
        match (self.horizontal, self.vertical) {
            (0, 0) => {
                events.enable_clear_vblank();
                events.enable_reset_nmi();
                if !self.regs.inidisp.forced_blanking() {
                    self.regs.stat77.set_time_overflow(false);
                    self.regs.stat77.set_range_overflow(false);
                }
            }
            (0, 225) => {
                events.enable_set_vblank();
                events.enable_set_vblank_nmi();
            }
            (1, _) => {
                events.enable_clear_hblank();
                if self.vertical == 0 {
                    self.regs.stat78.set_interlace_field(!self.regs.stat78.interlace_field());
                }
            }
            (6, 0) => {
                events.enable_reload_hdma();
            }
            (10, 255) => {
                // Reload OAMADD
                self.internal_oamadd = self.regs.oamadd.addr() << 1;
            }
            (22, _) if is_visible => {
                self.main_line_buffer.fill(None);
                self.sub_line_buffer.fill(None);
                self.line_buffer.fill((Color::BLACK, Color::BLACK));
                self.draw_line(self.vertical as u8);
                self.do_color_math();

                self.send_to_renderer(self.vertical as i16);

                // Prepare next sprites
                // Note that sprites appear 1 line lower than their Y value (like NES)
                self.search_sprites_for_line(self.vertical.try_into().unwrap());
            }
            (74, 225) => {
                // ref: https://problemkaputt.de/fullsnes.htm#snescontrollersioportsautomaticreading
                //
                // When enabled, the SNES will read 16 bits from each of the 4 controller port data lines into registers $4218-f.
                // This begins between H=32.5 and H=95.5 of the first V-Blank scanline, and ends 4224 master cycles later.
                // Register $4212 bit 0 is set during this time. Specifically, it begins at H=74.5 on the first frame,
                // and thereafter some multiple of 256 cycles after the start of the previous read that falls within the observed range.
                events.enable_joypad_auto_read();
            }
            (274, _) => {
                events.enable_set_hblank();
            }
            (278, _) if self.vertical <= 224 => {
                events.enable_perform_hdma();
            }

            _ => (),
        }

        // H/V Timer IRQ
        let htime = memory.htime();
        let vtime = memory.vtime();
        match memory.hv_timer_irq() {
            HV_TimerIRQ::Disable => (),
            HV_TimerIRQ::H_Counter => {
                if self.horizontal == htime + 3 {
                    events.enable_hv_irq();
                }
            }
            HV_TimerIRQ::V_Counter => {
                if (self.horizontal, self.vertical) == (2, vtime) {
                    events.enable_hv_irq();
                }
            }
            HV_TimerIRQ::HV_Counter => {
                if (self.horizontal, self.vertical) == (htime + 3, vtime) {
                    events.enable_hv_irq();
                }
            }
        }

        // Increment
        self.horizontal += 1;
        if max_dots < self.horizontal {
            self.horizontal = 0;
            self.vertical += 1;
            if max_lines < self.vertical {
                self.vertical = 0;

                // Rendering
                if self.interlace_state() != InterlaceState::Interlace1 {
                    self.renderer.as_mut().unwrap().draw(self.frame)?;
                }
                self.frame = self.frame + 1;
            }
        }

        self.cycles += spend;
        Ok(())
    }

    // addr is Word Address
    fn read_vram(&self, addr: u16) -> u16 {
        let addr = 2 * addr as usize;
        let lo = self.vram[addr + 0] as u16;
        let hi = self.vram[addr + 1] as u16;
        (hi << 8) | lo
    }

    // internal_oamadd is 10bit register
    fn incr_internal_oamadd(&mut self) {
        self.internal_oamadd = (self.internal_oamadd + 1) & 0x3FF;
    }

    pub fn write(&mut self, addr: u16, data: u8) {
        match addr {
            0x2100 => self.regs.inidisp = data.into(),
            0x2101 => self.regs.objsel = data.into(),
            0x2102 | 0x2103 => {
                self.regs.oamadd.set(data, addr == 0x2103);
                self.internal_oamadd = self.regs.oamadd.addr() << 1;
            }
            0x2104 => {
                if self.internal_oamadd < 0x200 {
                    if (self.internal_oamadd & 1) == 0 {
                        self.oam_latch = data
                    } else {
                        self.oam[(self.internal_oamadd - 1) as usize] = self.oam_latch;
                        self.oam[self.internal_oamadd as usize] = data;
                    }
                }
                if 0x200 <= self.internal_oamadd {
                    self.oam[self.internal_oamadd as usize & 0x21F] = data
                }
                self.incr_internal_oamadd();
            }
            0x2105 => self.regs.bgmode = data.into(),
            0x2106 => self.regs.mosaic = data.into(),
            0x2107..=0x210A => {
                let idx = addr - 0x2107;
                self.regs.bg_sc[idx as usize] = data.into()
            }
            0x210B | 0x210C => {
                let idx = (addr - 0x210B) as usize;
                self.regs.bg_nba[2 * idx + 0] = (data >> 0) & 0xF;
                self.regs.bg_nba[2 * idx + 1] = (data >> 4) & 0xF;
            }
            0x210D..=0x2114 => {
                let hori = (addr & 1) == 1;
                let idx = ((addr - 0x210D) / 2) as usize;

                if hori {
                    let v = ((data as u16) << 8) | ((self.bgofs_latch & !7) as u16) | ((self.bghofs_latch & 7) as u16);
                    self.regs.bg_hofs[idx] = v;
                    if addr == 0x210D {
                        self.regs.m7hofs = ((data as u16) << 8) | (self.mode7_latch as u16);
                        self.regs.m7hofs &= 0x1FFF;
                        self.mode7_latch = data;
                    }
                    self.regs.bg_hofs[idx] &= 0x3FF;
                    self.bgofs_latch = data;
                    self.bghofs_latch = data
                } else {
                    let v = ((data as u16) << 8) | (self.bgofs_latch as u16);
                    self.regs.bg_vofs[idx] = v;
                    if addr == 0x210E {
                        self.regs.m7vofs = ((data as u16) << 8) | (self.mode7_latch as u16);
                        self.regs.m7vofs &= 0x1FFF;
                        self.mode7_latch = data;
                    }
                    self.regs.bg_vofs[idx] &= 0x3FF;
                    self.bgofs_latch = data
                }
            }
            0x2115 => self.regs.vmain = data.into(),
            0x2116 | 0x2117 => {
                if addr == 0x2116 {
                    self.regs.vmadd = (self.regs.vmadd & 0xFF00) | (data as u16);
                } else {
                    let data = (data & 0x7F) as u16; // ignore msb
                    self.regs.vmadd = (self.regs.vmadd & 0x00FF) | (data << 8);
                }
                // TODO: don't reload the latch if horizontal-blank or active-display
                // TODO? bytes -> words?
                self.vram_latch = self.read_vram(self.regs.vmadd)
            }
            0x2118 | 0x2119 => {
                // TODO: ignore writing if horizontal-blank or active-display
                let is_high = addr == 0x2119;
                let addr = 2 * (self.regs.vmain.remap(self.regs.vmadd) as usize) + (is_high as usize);
                self.vram[addr] = data;
                if self.regs.vmain.increment_mode() == is_high {
                    // VRAM Size is 64KB, i.e., 32K Words
                    self.regs.vmadd = (self.regs.vmadd + self.regs.vmain.increment_amount()) & 0x7FFF;
                }
            }
            0x211A => self.regs.m7sel = data.into(),
            0x211B..=0x211E => {
                let idx = (addr - 0x211B) as usize;
                self.regs.m7matrix[idx] = ((data as u16) << 8) | (self.mode7_latch as u16);
                self.mode7_latch = data;
                if addr == 0x211B || addr == 0x211C {
                    let a = self.regs.m7matrix[0] as i16 as i32;
                    let b = (self.regs.m7matrix[1] & 0xFF) as i8 as i32;
                    self.mpy = a * b;
                }
            }
            0x211F | 0x2120 => {
                let idx = (addr - 0x211F) as usize;
                self.regs.m7center[idx] = (((data as u16) << 8) | (self.mode7_latch as u16)).into();
                self.mode7_latch = data
            }
            0x2121 => {
                self.regs.cgadd = data;
                self.cgram_byte = false
            }
            0x2122 => {
                if !self.cgram_byte {
                    self.cgram_latch = data
                } else {
                    self.cgram[self.regs.cgadd as usize] = (((data as u16) << 8) | (self.cgram_latch as u16)).into();
                    self.regs.cgadd = self.regs.cgadd.wrapping_add(1);
                }
                self.cgram_byte = !self.cgram_byte;
            }
            0x2123 | 0x2124 => {
                let idx = (addr - 0x2123) as usize;
                self.regs.w_bg_sel[2 * idx + 0].set(data & 0xF);
                self.regs.w_bg_sel[2 * idx + 1].set(data >> 4);
            }
            0x2125 => {
                self.regs.w_obj_sel.set(data & 0xF);
                self.regs.w_color_sel.set(data >> 4);
            }
            0x2126..=0x2129 => self.regs.wh[(addr - 0x2126) as usize] = data,
            0x212A => {
                let data = WLOGINPUT::from(data);
                self.regs.w_bg_log[0] = data.wlog1();
                self.regs.w_bg_log[1] = data.wlog2();
                self.regs.w_bg_log[2] = data.wlog3();
                self.regs.w_bg_log[3] = data.wlog4();
            }
            0x212B => {
                let data = WLOGINPUT::from(data);
                self.regs.w_obj_log = data.wlog1();
                self.regs.w_color_log = data.wlog2();
            }
            0x212C => self.regs.tm = data.into(),
            0x212D => self.regs.ts = data.into(),
            0x212E => self.regs.tmw = data.into(),
            0x212F => self.regs.tsw = data.into(),
            0x2130 => self.regs.cgwsel = data.into(),
            0x2131 => self.regs.cgadsub = data.into(),

            // COLDATA
            0x2132 => {
                let coldata: mmio::COLDATA = data.into();
                let color = coldata.color();
                if coldata.r() {
                    self.fixed_color.set_r(color);
                }
                if coldata.g() {
                    self.fixed_color.set_g(color);
                }
                if coldata.b() {
                    self.fixed_color.set_b(color);
                }
            }
            0x2133 => self.regs.setini = data.into(),
            _ => unimplemented!(),
        }
    }

    // TODO: update open_bus
    pub fn read(&mut self, addr: u16) -> u8 {
        match addr {
            0x2134 => ((self.mpy >> 0) & 0xFF) as u8,
            0x2135 => ((self.mpy >> 8) & 0xFF) as u8,
            0x2136 => ((self.mpy >> 16) & 0xFF) as u8,

            0x2137 => {
                let res: u8 = 0; // TODO : open bus
                self.counter_latch = true;
                res
            }
            0x2138 => {
                let addr = if self.internal_oamadd < 0x200 { self.internal_oamadd } else { self.internal_oamadd & 0x21F };
                let res = self.oam[addr as usize];
                self.incr_internal_oamadd();
                res
            }
            0x2139 | 0x213A => {
                let is_high = addr == 0x213A;
                let shift = if is_high { 8 } else { 0 };
                let res = ((self.vram_latch >> shift) & 0xFF) as u8;
                if self.regs.vmain.increment_mode() == is_high {
                    self.vram_latch = self.read_vram(self.regs.vmain.remap(self.regs.vmadd))
                }
                res
            }
            0x213B => {
                let raw = u16::from(self.cgram[self.regs.cgadd as usize]);
                let res = if self.cgram_byte {
                    let tmp = ((raw >> 8) & 0xFF) as u8;
                    tmp | (self.ppu2_open_bus & 0x80)
                } else {
                    (raw & 0xFF) as u8
                };
                self.cgram_byte = !self.cgram_byte;
                res
            }
            0x213C | 0x213D => {
                let is_hori = addr == 0x213C;
                let counter = if is_hori { self.horizontal } else { self.vertical };
                let latch = if is_hori { &mut self.ophct_byte } else { &mut self.opvct_byte };
                let res = if *latch {
                    let tmp = ((counter >> 8) & 0xFF) as u8;
                    tmp | (self.ppu2_open_bus & 0xFE)
                } else {
                    (counter & 0xFF) as u8
                };
                *latch = !*latch;
                res
            }
            0x213E => {
                // FIXME?: ppu1_version, master_slave_mode
                self.regs.stat77.set_ppu1_version(1);
                self.regs.stat77.set_ppu1_open_bus((self.ppu1_open_bus >> 4) == 1);
                self.regs.stat77.set_master_slave_mode(true);
                self.regs.stat77.clone().into()
            }
            0x213F => {
                // TODO: counter latch value
                // TODO: Clear op{h,v}ct_byte?
                self.regs.stat78.set_ppu2_version(1);
                self.regs.stat78.set_ntsc_pal_mode(false);
                self.regs.stat78.set_ppu2_open_bus((self.ppu2_open_bus >> 5) == 1);
                self.regs.stat78.clone().into()
            }
            _ => unimplemented!(),
        }
    }

    // https://snes.nesdev.org/wiki/PPU_registers#BGMODE
    //
    // Mode| BG bit depth  |Offsets |     Priorities (front -> back)       |                     Notes
    //     |BG1 BG2 BG3 BG4|per tile|                                      |
    //  0  | 2   2   2   2 |   No   |   S3 1H 2H S2 1L 2L S1 3H 4H S0 3L 4L|
    //  1  | 4   4   2     |   No   |   S3 1H 2H S2 1L 2L S1 3H    S0 3L   |BG3 priority = 0
    //     |               |        |3H S3 1H 2H S2 1L 2L S1       S0 3L   |BG3 priority = 1
    //  2  | 4   4         |  Yes   |   S3 1H    S2 2H    S1 1L    S0 2L   |
    //  3  | 8   4         |   No   |   S3 1H    S2 2H    S1 1L    S0 2L   |
    //  4  | 8   2         |  Yes   |   S3 1H    S2 2H    S1 1L    S0 2L   |
    //  5  | 4   2         |   No   |   S3 1H    S2 2H    S1 1L    S0 2L   |Fixed 16 pixel char width. Forced high-res mode.
    //  6  | 4             |  Yes   |   S3 1H    S2       S1 1L    S0      |Fixed 16 pixel char width. Forced high-res mode.
    //  7  | 8             |   No   |   S3       S2       S1 1L    S0      |Fixed 8x8 char size.
    // 7EXT| 8   7         |   No   |   S3       S2 2H    S1 1L    S0 2L   |Fixed 8x8 char size. BG2 bit 7 acts as priority.
    fn draw_line(&mut self, y: u8) {
        enum Priority {
            BG(u8, bool),
            OBJ(u8),
        }

        const S0: Priority = Priority::OBJ(0);
        const S1: Priority = Priority::OBJ(1);
        const S2: Priority = Priority::OBJ(2);
        const S3: Priority = Priority::OBJ(3);
        const H1: Priority = Priority::BG(0, true);
        const H2: Priority = Priority::BG(1, true);
        const H3: Priority = Priority::BG(2, true);
        const H4: Priority = Priority::BG(3, true);
        const L1: Priority = Priority::BG(0, false);
        const L2: Priority = Priority::BG(1, false);
        const L3: Priority = Priority::BG(2, false);
        const L4: Priority = Priority::BG(3, false);

        // TODO: high resolution mode
        let priorities: &'static [Priority] = match self.regs.bgmode.bg_mode() {
            0 => &[S3, H1, H2, S2, L1, L2, S1, H3, H4, S0, L3, L4],
            1 => {
                if !self.regs.bgmode.bg3_priority() {
                    &[S3, H1, H2, S2, L1, L2, S1, H3, S0, L3]
                } else {
                    &[H3, S3, H1, H2, S2, L1, L2, S1, S0, L3]
                }
            }
            2..=5 => &[S3, H1, S2, H2, S1, L1, S0, L2],
            6 => &[S3, H1, S2, S1, L1, S0],
            7 => &[S3, S2, S1, L1, S0],
            _ => panic!("unknown mode"),
        };

        for priority in priorities.iter().rev() {
            match priority {
                Priority::BG(bg, priority) => self.draw_bg_line(*bg, y, *priority),
                Priority::OBJ(priority) => self.draw_sprites_line(*priority),
            }
        }
    }

    fn draw_bg_line(&mut self, bg: u8, y: u8, priority: bool) {
        let mode = self.regs.bgmode.bg_mode();

        if mode == 7 {
            self.draw_bg_line_mode7(y);
            return;
        }

        let bpp = match (mode, bg + 1) {
            // Mode 0
            (0, 1) => 2,
            (0, 2) => 2,
            (0, 3) => 2,
            (0, 4) => 2,

            // Mode 1
            (1, 1) => 4,
            (1, 2) => 4,
            (1, 3) => 2,

            // Mode 2
            (2, 1) => 4,
            (2, 2) => 4,

            // Mode 3
            (3, 1) => 8,
            (3, 2) => 4,

            // Mode 4
            (4, 1) => 8,
            (4, 2) => 2,

            // Mode 5
            (5, 1) => 4,
            (5, 2) => 2,

            // Mode 6
            (6, 1) => 4,

            _ => panic!("unknown combination of mode and background"),
        };

        let win = self.get_windows(self.regs.w_bg_sel[bg as usize].clone(), self.regs.w_bg_log[bg as usize].clone());
        let is_hr = self.regs.is_high_resolution();
        let y = {
            let y = y as u16;
            match self.interlace_state() {
                InterlaceState::Normal => y,
                InterlaceState::Interlace1 => 2 * y,
                InterlaceState::Interlace2 => 2 * y + 1,
            }
        };

        let mut entry_cache = tile::BGEntryCache::default();
        let mut pixel_cache = tile::BGPixelCache::default();
        let mut get_pixel = |ppu: &PPU, x: u16| {
            ppu.calc_bg_pixel_fast(bg, bpp, x, y, priority, mode, &mut entry_cache, &mut pixel_cache)
                .map(|color| BGPixel { bg, color })
                .map(|pixel| LinePixel::BG(pixel))
        };
        for x in 0..=255u8 {
            let main = {
                let x = x as u16;
                let x = if is_hr { 2 * x } else { x };
                self.regs.is_main_pixel_enable_bg(bg as usize, win.hide(x))
            };
            let sub = {
                let x = x as u16;
                let x = if is_hr { 2 * x + 1 } else { x };
                self.regs.is_sub_pixel_enable_bg(bg as usize, win.hide(x))
            };
            if !is_hr {
                if let Some(c) = get_pixel(&self, x as u16) {
                    if main {
                        self.main_line_buffer[x as usize] = Some(c.clone());
                    }
                    if sub {
                        self.sub_line_buffer[x as usize] = Some(c.clone());
                    }
                }
            } else {
                if main {
                    let c = get_pixel(&self, 2 * x as u16 + 0);
                    if c.is_some() {
                        self.main_line_buffer[x as usize] = c;
                    }
                }
                if sub {
                    let c = get_pixel(&self, 2 * x as u16 + 1);
                    if c.is_some() {
                        self.sub_line_buffer[x as usize] = c;
                    }
                }
            }
        }
    }

    // TODO: EXT
    // ref: https://problemkaputt.de/fullsnes.htm#snesppurotationscaling
    //
    // To calculate VRAM coordinates for any SCREEN coordinates:
    //
    // IF xflip THEN SCREEN.X=((0..255) XOR FFh), ELSE SCREEN.X=(0..255)
    // IF yflip THEN SCREEN.Y=((1..224/239) XOR FFh), ELSE SCREEN.Y=(1..224/239)
    // ORG.X = (M7HOFS-M7X) AND NOT 1C00h, IF ORG.X<0 THEN ORG.X=ORG.X OR 1C00h
    // ORG.Y = (M7VOFS-M7Y) AND NOT 1C00h, IF ORG.Y<0 THEN ORG.Y=ORG.Y OR 1C00h
    // VRAM.X = ((M7A*ORG.X) AND NOT 3Fh) + ((M7B*ORG.Y) AND NOT 3Fh) + M7X*100h
    // VRAM.Y = ((M7C*ORG.X) AND NOT 3Fh) + ((M7D*ORG.Y) AND NOT 3Fh) + M7Y*100h
    // VRAM.X = VRAM.X + ((M7B*SCREEN.Y) AND NOT 3Fh) + (M7A*SCREEN.X)
    // VRAM.Y = VRAM.Y + ((M7D*SCREEN.Y) AND NOT 3Fh) + (M7C*SCREEN.X)
    //
    //
    // After calculating the left-most pixel of a scanline, the following pixels on that scanline can be also calculated by increasing VRAM coordinates as so:
    //
    // IF xflip THEN VRAM.X=VRAM.X-M7A, ELSE VRAM.X=VRAM.X+M7A
    // IF xflip THEN VRAM.Y=VRAM.Y-M7C, ELSE VRAM.Y=VRAM.Y+M7C
    // (The result is same as on hardware, although the real hardware doesn't seem
    // to use that method, instead it seems to contain an excessively fast multiply
    // unit that recalculates (M7A*SCREEN.X) and (M7C*SCREEN.X) on every pixel.)
    fn draw_bg_line_mode7(&mut self, y: u8) {
        let u13_as_signed = |x: u16| (((x as i16) << 3) >> 3) as i32;

        let xflip = self.regs.m7sel.flip_horizontal();
        let yflip = self.regs.m7sel.flip_vertical();
        let m7a = self.regs.m7matrix[0] as i16 as i32;
        let m7b = self.regs.m7matrix[1] as i16 as i32;
        let m7c = self.regs.m7matrix[2] as i16 as i32;
        let m7d = self.regs.m7matrix[3] as i16 as i32;
        let m7x = u13_as_signed(self.regs.m7center[0].center());
        let m7y = u13_as_signed(self.regs.m7center[1].center());
        let m7hofs = u13_as_signed(self.regs.m7hofs);
        let m7vofs = u13_as_signed(self.regs.m7vofs);

        let screen_x = if xflip { 0xFF } else { 0 };
        let screen_y = (y as i32) ^ (if yflip { 0xFF } else { 0 });
        let org_x = {
            let res = (m7hofs - m7x) & !0x1C00;
            if res < 0 {
                res | 0x1C00
            } else {
                res
            }
        };
        let org_y = {
            let res = (m7vofs - m7y) & !0x1C00;
            if res < 0 {
                res | 0x1C00
            } else {
                res
            }
        };
        let vram_x = ((m7a * org_x) & !0x3F) + ((m7b * org_y) & !0x3F) + m7x * 0x100;
        let vram_y = ((m7c * org_x) & !0x3F) + ((m7d * org_y) & !0x3F) + m7y * 0x100;
        let vram_x = vram_x + ((m7b * screen_y) & !0x3F) + (m7a * screen_x);
        let vram_y = vram_y + ((m7d * screen_y) & !0x3F) + (m7c * screen_x);

        let win = self.get_windows(self.regs.w_bg_sel[0].clone(), self.regs.w_bg_log[0].clone());
        for x in 0..=255 {
            let vram_x = vram_x + x * (if xflip { -m7a } else { m7a });
            let vram_y = vram_y + x * (if xflip { -m7c } else { m7c });
            let vram_x = vram_x >> 8;
            let vram_y = vram_y >> 8;

            let (main, sub) = {
                let hide = win.hide(x as u16);
                (self.regs.is_main_pixel_enable_bg(0, hide), self.regs.is_sub_pixel_enable_bg(0, hide))
            };
            let coor = {
                let x = vram_x;
                let y = vram_y;
                let is_overflow = !(0 <= x && x < 1024 && 0 <= y && y < 1024);
                if !is_overflow {
                    Some((x as usize, y as usize))
                } else {
                    match self.regs.m7sel.screen_over() {
                        mmio::M7ScreenOver::Wrap => Some((x.rem_euclid(1024) as usize, y.rem_euclid(1024) as usize)),
                        mmio::M7ScreenOver::Transparent => None,
                        mmio::M7ScreenOver::Tile00 => Some((0, 0)),
                    }
                }
            };
            coor.map(|xy| {
                let (x, y) = xy;
                let tile_num = {
                    let index = ((y & !7) << 4) + (x >> 3);
                    self.vram[2 * index] as usize
                };
                let pixel = {
                    let index = (tile_num << 6) + ((y & 7) << 3) + (x & 7);
                    self.vram[2 * index + 1] as usize
                };
                if pixel == 0 {
                    None
                } else {
                    Some(self.cgram[pixel].clone())
                }
            })
            .flatten()
            .map(|color| LinePixel::BG(BGPixel { bg: 0, color }))
            .map(|pixel| {
                if main {
                    self.main_line_buffer[x as usize] = Some(pixel.clone());
                }
                if sub {
                    self.sub_line_buffer[x as usize] = Some(pixel)
                }
            });
        }
    }

    // TODO: Interlace
    fn draw_sprites_line(&mut self, priority: u8) {
        let win = self.get_windows(self.regs.w_obj_sel, self.regs.w_obj_log);
        for x in 0..=255 {
            let (main, sub) = {
                let hide = win.hide(x);
                (self.regs.is_main_pixel_enable_obj(hide), self.regs.is_sub_pixel_enable_obj(hide))
            };
            let pixel = self.sprites_line_buffer[x as usize]
                .map(|pixel| if pixel.priority == priority { Some(LinePixel::OBJ(pixel)) } else { None })
                .flatten();
            match pixel {
                Some(_) => {
                    if main {
                        self.main_line_buffer[x as usize] = pixel.clone();
                    };
                    if sub {
                        self.sub_line_buffer[x as usize] = pixel.clone();
                    }
                }
                None => (),
            };
        }
    }

    fn bg_entry(&self, bg: u8, x: usize, y: usize) -> (tile::BGMapEntry, u16) {
        let (chr_w, chr_h) = self.regs.bgmode.chr_size(bg);

        let screen_idx = {
            let sz = self.regs.bg_sc[bg as usize].screen_size();
            let vidx = chr_h.rdiv(y >> 5) & 1; // (y / (32 * chr_h)) & 1
            let hidx = chr_w.rdiv(x >> 5) & 1; // (x / (32 * chr_w)) & 1
            sz[vidx][hidx] as usize
        };

        // Normalize to 32x32 tiles
        let x = chr_w.rdiv(x) % 32; // x / chr_w % 32
        let y = chr_h.rdiv(y) % 32; // y / chr_y % 32
        let tile_idx = y * 32 + x;

        let addr = (self.regs.bg_sc[bg as usize].tilemap_vram_addr() as usize) << 10; // words
        let addr = addr + 32 * 32 * screen_idx; // Each tilemap consists of 32x32 entries, even if a tilemap size is larger than 8x8 (e.g. 16x16)
        let addr = addr + tile_idx;
        let addr = (addr & 0x7FFF) as u16;
        (self.read_vram(addr).into(), addr)
    }

    fn bg_entry_fast(&self, bg: u8, x: usize, y: usize, cache: &mut tile::BGEntryCache) -> (tile::BGMapEntry, u16) {
        let ctx = tile::BGContext {
            x,
            y,
            chr_size: self.regs.bgmode.chr_size(bg),
        };
        if !cache.is_consistent(ctx) {
            let entry = self.bg_entry(bg, x, y);
            cache.burst(entry, x, y);
        }
        cache.value()
    }

    fn scroll(&self, bg: u8, x: usize, mode: u8) -> (usize, usize) {
        #[bitfield]
        #[repr(u16)]
        struct OffsetPerTile {
            scroll: B10,
            #[skip]
            __: B3,
            enable_bg1: bool,
            enable_bg2: bool,
            is_vertical: bool,
        }

        impl OffsetPerTile {
            fn override_hofs(&self, bg: u8, mode: u8) -> bool {
                let prevent = mode == 4 && self.is_vertical();
                match bg {
                    0 => self.enable_bg1() && !prevent,
                    1 => self.enable_bg2() && !prevent,
                    _ => unreachable!(),
                }
            }

            fn override_vofs(&self, bg: u8, mode: u8) -> bool {
                let prevent = mode == 4 && !self.is_vertical();
                match bg {
                    0 => self.enable_bg1() && !prevent,
                    1 => self.enable_bg2() && !prevent,
                    _ => unreachable!(),
                }
            }
        }

        let hofs = self.regs.bg_hofs[bg as usize] as usize;
        let vofs = self.regs.bg_vofs[bg as usize] as usize;
        let vofs = match self.interlace_state() {
            InterlaceState::Normal => vofs,
            InterlaceState::Interlace1 | InterlaceState::Interlace2 => vofs / 2,
        };
        let is_leftmost_tile = {
            let second = (hofs / 8 + 1) * 8;
            x + hofs < second
        };
        if (mode != 2 && mode != 4 && mode != 6) || is_leftmost_tile {
            return (hofs, vofs);
        }

        // https://wiki.superfamicom.org/backgrounds
        let hofs = x + hofs + 1; // TODO: What's this "1"?
        let bg3hofs = self.regs.bg_hofs[2] as usize;
        let bg3vofs = self.regs.bg_vofs[2] as usize;
        let bg3x = (hofs & 7) | (((x - 8) & !7) + (bg3hofs & !7));
        let hoffset: OffsetPerTile = u16::from(self.bg_entry(2, bg3x, bg3vofs).0).into();
        let voffset: OffsetPerTile = u16::from(self.bg_entry(2, bg3x, bg3vofs + if mode == 4 { 0 } else { 8 }).0).into();
        let hofs = if hoffset.override_hofs(bg, mode) {
            (hofs & 7) | ((x & !7) + ((hoffset.scroll() as usize) & !7))
        } else {
            hofs
        };
        let vofs = if voffset.override_vofs(bg, mode) { voffset.scroll() as usize } else { vofs };
        (hofs - x, vofs)
    }

    fn adjust_xy(&self, bg: u8, x: u16, y: u16, mode: u8) -> (usize, usize) {
        let (hofs, vofs) = self.scroll(bg, x as usize, mode);
        let x = {
            let mx = if self.regs.mosaic.enable(bg) { self.regs.mosaic.size() + 1 } else { 1 } as u16;
            let sub = (x - x / mx * mx) as usize;
            (x as usize) + hofs * (if self.regs.is_high_resolution() { 2 } else { 1 }) - sub
        };
        let y = {
            let my = if self.regs.mosaic.enable(bg) { self.regs.mosaic.size() + 1 } else { 1 } as u16;
            let sub = (y - y / my * my) as usize;
            (y as usize) + vofs - sub
        };
        (x, y)
    }

    fn calc_bg_pixel(&self, bg: u8, bpp: u8, x: u16, y: u16, priority: bool, mode: u8) -> Option<mmio::CGDATA> {
        let (x, y) = self.adjust_xy(bg, x, y, mode);
        let entry = self.bg_entry(bg, x, y).0;
        if entry.priority() != priority {
            return None;
        }

        let (chr_w, chr_h) = self.regs.bgmode.chr_size(bg);
        let addr = (self.regs.bg_nba[bg as usize] as usize) << 12; // words
        let tile_index = entry.character_num() as usize;
        let x = chr_w.get_offset(x, entry.x_flip());
        let y = chr_h.get_offset(y, entry.y_flip());
        let tile_index = tile_index + (x / 8) + ((y / 8) * 16);
        let tile_index = tile_index & 0x3FF; // 32x32 tiles
        let x = x % 8;
        let y = y % 8;
        let tile_size = (bpp as usize) * 4; // words
        let addr = addr + tile_index * tile_size + y;
        let addr = addr * 2;
        let color = {
            let mut c: u8 = 0;
            for i in 0..(bpp / 2) {
                let addr = (addr + (16 * i) as usize) & 0xFFFE;
                let lo = (self.vram[addr + 0] >> (7 - x)) & 1;
                let hi = (self.vram[addr + 1] >> (7 - x)) & 1;
                c |= ((hi << 1) | lo) << (i * 2);
            }
            c
        };

        if color == 0 {
            None
        } else {
            let addr = entry.palette_num() as usize;
            let addr = addr << bpp;
            let addr = addr + color as usize;
            let addr = addr +
                if mode == 0 {
                    // TODO: 0x10?
                    0x20 * bg as usize
                } else {
                    0usize
                };
            let addr = addr & 0xFF;
            Some(self.cgram[addr].clone())
        }
    }

    // TODO: make faster
    fn calc_bg_pixel_fast(&self, bg: u8, bpp: u8, x: u16, y: u16, priority: bool, mode: u8, entry_cache: &mut tile::BGEntryCache, pixel_cache: &mut tile::BGPixelCache) -> Option<mmio::CGDATA> {
        let (x, y) = self.adjust_xy(bg, x, y, mode);
        let (entry, entry_addr) = self.bg_entry_fast(bg, x, y, entry_cache);
        if entry.priority() != priority {
            return None;
        }

        let chr_size = self.regs.bgmode.chr_size(bg);
        if !pixel_cache.is_consistent(entry_addr, x, y) {
            let addr = (self.regs.bg_nba[bg as usize] as usize) << 12; // words
            let ctx = tile::BGContext { x, y, chr_size };
            pixel_cache.burst((entry, entry_addr), ctx, bpp, addr, &self.vram[..]);
        }
        let x = x % 8;
        let x = if entry.x_flip() { 7 - x } else { x };
        let color = pixel_cache.get_cgram_addr(x);
        if color == 0 {
            None
        } else {
            let addr = entry.palette_num() as usize;
            let addr = addr << bpp;
            let addr = addr + color as usize;
            let addr = addr +
                if mode == 0 {
                    // TODO: 0x10?
                    0x20 * bg as usize
                } else {
                    0usize
                };
            let addr = addr & 0xFF;
            Some(self.cgram[addr].clone())
        }
    }

    fn read_sprite(&self, obj_index: u8) -> sprite::Sprite {
        sprite::Sprite::read(&self.oam, obj_index, self.regs.objsel.clone())
    }

    // TODO: interlace
    fn search_sprites_for_line(&mut self, y: u8) {
        self.sprites_line_buffer.fill(None);
        let y = y + 1;
        let first_sprite = if self.regs.oamadd.priority_rotation() { ((self.regs.oamadd.addr() & 0xFE) as u8) / 2 } else { 0 };

        let mut sprites_vec = Vec::new();
        for i in (0..128).rev() {
            let sprite = self.read_sprite((first_sprite + i) & 0x7F);
            if sprite.occur(y as i16) {
                if sprites_vec.len() == 32 {
                    self.regs.stat77.set_time_overflow(true);
                    break;
                } else {
                    sprites_vec.push(sprite);
                }
            }
        }

        let mut tiles_vec = Vec::new();
        for sprite in sprites_vec.iter() {
            let tiles = sprite.calc_tiles(y, self.regs.objsel.clone());
            let mut overflow = false;
            for tile in tiles {
                if tiles_vec.len() == 34 {
                    overflow = true;
                    break;
                }
                tiles_vec.push((sprite.priority, tile))
            }
            if overflow {
                self.regs.stat77.set_range_overflow(true);
                break;
            }
        }

        for (priority, tile) in tiles_vec.iter().rev() {
            for ofx in 0..8 {
                match tile.pixel(ofx, y, &self.vram[..], &self.cgram[(self.cgram.len() / 2)..]) {
                    Some(sprite::SpritePixel { x, palette, color }) => {
                        if !(0 <= x && x < 256) {
                            continue;
                        }
                        self.sprites_line_buffer[x as usize] = Some(ObjPixel { priority: *priority, palette, color });
                    }
                    None => (),
                }
            }
        }
    }

    fn get_windows(&self, wsel: mmio::WSEL, logic: mmio::WindowMaskLogic) -> windows::Windows {
        windows::Windows::make(&self.regs.wh, wsel, logic)
    }

    fn meld_two_colors(&self, main: Option<LinePixel>, sub: Option<LinePixel>, inside_win: bool) -> mmio::CGDATA {
        let apply_color_math = if self.regs.cgwsel.color_math_enable_region().in_region(inside_win) {
            false
        } else {
            match main {
                Some(pixel) => match pixel {
                    LinePixel::BG(BGPixel { bg, .. }) => self.regs.cgadsub.enable_color_math_bg(bg as usize),
                    LinePixel::OBJ(ObjPixel { palette, .. }) => 4 <= palette && palette <= 7 && self.regs.cgadsub.enable_color_math_obj(),
                },
                None => self.regs.cgadsub.enable_backdrop_color_math(),
            }
        };

        let main = main.map(|c| c.color());
        let sub = sub.map(|c| c.color());
        let region = self.regs.cgwsel.main_screen_black_region();

        let is_black = region.in_region(inside_win);
        let color = if is_black { Some(0.into()) } else { main };

        let res = if !apply_color_math {
            color.unwrap_or(self.cgram[0])
        } else {
            let x = color.unwrap_or(self.cgram[0]);
            let y = sub.unwrap_or(self.fixed_color);
            let div = {
                let half = self.regs.cgadsub.half_color_math();
                if half && !sub.is_none() && !is_black {
                    2
                } else {
                    1
                }
            };
            self.regs.cgadsub.op(x, y, div)
        };

        self.regs.inidisp.brighten(res)
    }

    fn do_color_math(&mut self) {
        let windows = self.get_windows(self.regs.w_color_sel.clone(), self.regs.w_color_log.clone());
        for x in 0..=255u8 {
            let main = self.main_line_buffer[x as usize];
            let sub = self.sub_line_buffer[x as usize];
            let inside_win = windows.hide(x as u16);

            let c0 = self.meld_two_colors(main, sub, inside_win);
            self.line_buffer[x as usize] = if self.regs.is_high_resolution() {
                let c1 = self.meld_two_colors(sub, sub, inside_win); // TODO: correct?
                (c0.into(), c1.into())
            } else {
                (c0.into(), c0.into())
            };
        }
    }

    pub fn frame(&self) -> u32 {
        self.frame
    }
}
