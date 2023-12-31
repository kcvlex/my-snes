#![allow(dead_code)]

use crate::apu::APU;
use crate::emulator::{PendingEvents, NMI};
use crate::joypad::Joypad;
use crate::ppu::PPU;
use crate::rom::Cartridge;
use modular_bitfield::prelude::*;

// Overall
//
//                0x3F     0x80     0xBF     0xFF
// 0x0000 +--------+------+-+--------+--------+
//        |        |      |W|        |        |
//        | System | WS1  |R| System |  WS2   |
//        |  Area  |HiROM |A|  Area  | HiROM  |
//        |        |      |M|        |        |
// 0x8000 +--------+      | +--------+        |
//        |        |      | |        |        |
//        |  WS1   |      | |  WS2   |        |
//        | LoROM  |      | | LoROM  |        |
//        |        |      | |        |        |
// 0xFFFF +--------+------+-+--------+--------+
//
//
pub struct Memory {
    wram: Vec<u8>,

    open_bus: u8,
    memsel: MEMSEL,
    nmitimen: NMITIMEN,
    mdmaen: [bool; 8],
    hdmaen: [bool; 8],

    htime: TwoWords,
    vtime: TwoWords,

    wmadd: u32,

    dma: [DMA; 8],

    hblank: bool,
    vblank: bool,

    wrmpya: u8,
    wrmpyb: u8,
    rdmpy: u16,
    wrdiv: u16,
    wrdivb: u8,
    rddiv: u16,
}

pub trait Resources {
    fn tick(&mut self, v: u8);

    fn ppu_mut(&mut self) -> &mut PPU;
    fn apu_mut(&mut self) -> &mut APU;
    fn cartridge(&self) -> &Cartridge;
    fn pending_events_mut(&mut self) -> &mut PendingEvents;
    fn nmi_mut(&mut self) -> &mut NMI;
    fn joypad(&self, i: usize) -> &Joypad;
    fn joypad_mut(&mut self, i: usize) -> &mut Joypad;
}

#[derive(Clone, Copy, Debug)]
struct DMA {
    // MMIO Registers
    dmap: DMAPn,
    bbad: u8,
    a1t: TwoWords,
    a1b: u8,
    das: TwoWords,
    dasb: u8,
    a2a: TwoWords,
    nltr: NLTRn,
    unused: u8,

    // Internal
    in_progress: bool,
    unit_index: usize,
    hdma_do_transfer: bool,
    hdma_finish_scanline: bool,
    hdma_terminate: bool,
}

impl Default for DMA {
    fn default() -> Self {
        Self {
            dmap: DMAPn::default(),
            bbad: 0xFF,
            a1t: TwoWords::ffff(),
            a1b: 0xFF,
            das: TwoWords::ffff(),
            dasb: 0xFF,
            a2a: TwoWords::ffff(),
            nltr: NLTRn::default(),
            unused: 0xFF,

            in_progress: false,
            unit_index: 0,
            hdma_do_transfer: false,
            hdma_finish_scanline: false,
            hdma_terminate: false,
        }
    }
}

#[derive(Default, Debug, Clone, Copy)]
struct TwoWords {
    lo: u8,
    hi: u8,
}

impl Into<u16> for TwoWords {
    fn into(self) -> u16 {
        ((self.hi as u16) << 8) | (self.lo as u16)
    }
}

impl From<u16> for TwoWords {
    fn from(v: u16) -> Self {
        Self {
            lo: (v & 0xFF) as u8,
            hi: (v >> 8) as u8,
        }
    }
}

impl TwoWords {
    fn incr(&mut self) {
        let v: u16 = (*self).into();
        let v = v.wrapping_add(1);
        *self = v.into();
    }

    fn decr(&mut self) {
        let v: u16 = (*self).into();
        let v = v.wrapping_sub(1);
        *self = v.into();
    }

    fn with_bank(&self, bank: u8) -> u32 {
        let offset: u16 = (*self).into();
        ((bank as u32) << 16) | (offset as u32)
    }

    fn ffff() -> TwoWords {
        0xFFFF.into()
    }
}

#[bitfield]
#[repr(u8)]
#[derive(Default)]
struct MEMSEL {
    enable_fast_rom: bool,
    #[skip]
    __: B7,
}

#[bitfield]
#[repr(u8)]
#[derive(Default, Debug)]
struct NMITIMEN {
    joypad_auto_read_enable: bool,
    #[skip]
    __: B3,
    hv_timer_irq: HV_TimerIRQ,
    #[skip]
    __: bool,
    vblank_nmi_enable: bool,
}

#[allow(non_camel_case_types)]
#[derive(BitfieldSpecifier)]
#[bits = 2]
#[derive(Default, Clone, Copy, Debug)]
pub enum HV_TimerIRQ {
    #[default]
    Disable = 0,
    H_Counter = 1,
    V_Counter = 2,
    HV_Counter = 3,
}

#[bitfield]
#[repr(u8)]
#[derive(Clone, Copy, Debug)]
struct DMAPn {
    transfer_unit_selection_bin: B3,
    a_bus_addr_step_bin: B2,
    #[skip]
    __: bool,
    addr_mode: DMA_AddrMode,
    transfer_dir: DMA_TransferDir,
}

impl Default for DMAPn {
    fn default() -> Self {
        0xFF.into()
    }
}

#[allow(non_camel_case_types)]
enum A_BusAddressStep {
    Increment,
    Decrement,
    Fixed,
}

impl DMAPn {
    fn transfer_unit_selection(&self) -> &'static [u8] {
        match self.transfer_unit_selection_bin() {
            0 => &[0],
            1 => &[0, 1],
            2 | 6 => &[0, 0],
            3 | 7 => &[0, 0, 1, 1],
            4 => &[0, 1, 2, 3],
            5 => &[0, 1, 0, 1],
            _ => unreachable!(),
        }
    }

    fn a_bus_addr_step(&self) -> A_BusAddressStep {
        match self.a_bus_addr_step_bin() {
            0 => A_BusAddressStep::Increment,
            2 => A_BusAddressStep::Decrement,
            1 | 3 => A_BusAddressStep::Fixed,
            _ => unreachable!(),
        }
    }
}

#[allow(non_camel_case_types)]
#[derive(BitfieldSpecifier)]
#[bits = 1]
#[derive(Default, Clone, Copy, Debug, PartialEq)]
enum DMA_AddrMode {
    #[default]
    DirectTable = 0,
    IndirectTable = 1,
}

#[allow(non_camel_case_types)]
#[derive(BitfieldSpecifier)]
#[bits = 1]
#[derive(Default, Clone, Copy, Debug)]
enum DMA_TransferDir {
    #[default]
    CPU2IO = 0,
    IO2CPU = 1,
}

#[bitfield]
#[repr(u8)]
#[derive(Clone, Copy, Debug)]
struct NLTRn {
    scanline_counter: B7,
    hdma_repeat_flag: bool,
}

impl Default for NLTRn {
    fn default() -> Self {
        0xFF.into()
    }
}

const FAST_CYCLES: u8 = 6;
const SLOW_CYCLES: u8 = 8;
const XSLOW_CYCLES: u8 = 12;
const DMA_TRANSFER_CYCLES: u8 = 8;

#[allow(non_camel_case_types)]
#[derive(PartialEq)]
pub enum HowToAccess {
    CPU,
    DMA_A_Bus,
    DMA_B_Bus,
    Debugger,
}

impl Default for Memory {
    fn default() -> Self {
        Self {
            wram: vec![0x00; 0x20000],
            open_bus: 0,
            memsel: 0.into(),
            nmitimen: 0.into(),
            mdmaen: [false; 8],
            hdmaen: [false; 8],
            htime: TwoWords::ffff(),
            vtime: TwoWords::ffff(),
            wmadd: 0,
            dma: [DMA::default(); 8],
            hblank: false,
            vblank: false,
            wrmpya: 0xFF,
            wrmpyb: 0x00,
            wrdiv: 0xFFFF,
            wrdivb: 0x00,
            rddiv: 0x0000,
            rdmpy: 0x0000,
        }
    }
}

const UNMAPPED_ROM_VALUE: u8 = 0;

impl Memory {
    pub fn tick(&mut self, rsrc: &mut impl Resources) {
        if rsrc.pending_events_mut().consume_set_hblank() {
            self.hblank = true;
        }
        if rsrc.pending_events_mut().consume_clear_hblank() {
            self.hblank = false;
        }
        if rsrc.pending_events_mut().consume_set_vblank() {
            self.vblank = true;
        }
        if rsrc.pending_events_mut().consume_clear_vblank() {
            self.vblank = false;
        }
        // Set NMI flag is 0.5 clock after setting VBlank
        if rsrc.pending_events_mut().consume_set_vblank_nmi() {
            rsrc.nmi_mut().set();
        }
        if rsrc.pending_events_mut().consume_reset_nmi() {
            rsrc.nmi_mut().reset();
        }

        if rsrc.pending_events_mut().consume_joypad_auto_read() {
            if self.nmitimen.joypad_auto_read_enable() {
                for i in 0..4 {
                    rsrc.joypad_mut(i).start_auto_read();
                }
            }
        }

        self.dma_step(rsrc);
    }

    pub fn vblank(&self) -> bool {
        self.vblank
    }

    pub fn in_dma(&self) -> bool {
        for ch in 0..8 {
            if self.dma[ch].in_progress {
                return true;
            }
            if self.hdmaen[ch] && !self.dma[ch].hdma_finish_scanline && !self.dma[ch].hdma_terminate {
                return true;
            }
        }
        return false;
    }

    pub fn htime(&self) -> u16 {
        let res: u16 = self.htime.into();
        res & 0x1FF
    }

    pub fn vtime(&self) -> u16 {
        let res: u16 = self.vtime.into();
        res & 0x1FF
    }

    pub fn hv_timer_irq(&self) -> HV_TimerIRQ {
        self.nmitimen.hv_timer_irq()
    }

    fn incr_wmadd(&mut self) {
        self.wmadd += 1;
        self.wmadd &= 0x1FFFF;
    }

    pub fn read(&mut self, addr: u32, how_to_access: HowToAccess, rsrc: &mut impl Resources) -> u8 {
        let bank = (addr >> 16) as u8;
        let offset = (addr & 0xFFFF) as u16;

        let rom_cycles = if 0x80 <= bank && self.memsel.enable_fast_rom() { FAST_CYCLES } else { SLOW_CYCLES };

        let mut tick_aux = |cycles: u8| {
            if how_to_access == HowToAccess::CPU {
                rsrc.tick(cycles)
            }
        };

        macro_rules! wrap_io_port {
            ($e: expr) => {{
                tick_aux(FAST_CYCLES);
                match how_to_access {
                    HowToAccess::DMA_A_Bus => self.open_bus,
                    HowToAccess::Debugger => unimplemented!(),
                    _ => $e,
                }
            }};
        }

        // TODO: SRAM
        let res = match bank {
            0x00..=0x3F | 0x80..=0xBF => match offset {
                // System Area -- Mirror of WRAM
                0x0000..=0x1FFF => {
                    tick_aux(SLOW_CYCLES);
                    self.wram[offset as usize]
                }

                // System Area -- Unused
                0x2000..=0x20FF => {
                    tick_aux(FAST_CYCLES);
                    self.open_bus
                }

                // System Area -- I/O Ports (B-Bus)
                0x2100..=0x2133 => {
                    tick_aux(FAST_CYCLES);
                    self.open_bus
                }

                // System Area -- I/O Ports (B-Bus)
                0x2134..=0x213F => wrap_io_port!(rsrc.ppu_mut().read(offset)),
                0x2140..=0x217F => wrap_io_port!({
                    let idx = (offset & 3) as u8;
                    rsrc.apu_mut().port_read(idx)
                }),
                0x2180 => wrap_io_port!({
                    let res = self.wram[self.wmadd as usize];
                    self.incr_wmadd();
                    res
                }),
                0x2181..=0x21FF => wrap_io_port!(self.open_bus),

                // System Area -- Unused
                0x2200..=0x3FFF => {
                    tick_aux(FAST_CYCLES);
                    self.open_bus
                }

                // System Area -- I/O Ports (manual joypad access)
                0x4000..=0x4015 => {
                    tick_aux(XSLOW_CYCLES);
                    // TODO
                    self.open_bus
                }

                0x4016 | 0x4017 => {
                    tick_aux(XSLOW_CYCLES);
                    match how_to_access {
                        HowToAccess::DMA_A_Bus => self.open_bus, // TODO
                        HowToAccess::Debugger => unimplemented!(),
                        _ => {
                            let idx0 = (offset - 0x4016) as usize;
                            let idx1 = idx0 + 2;
                            let mut res = self.open_bus & 0b11;
                            res |= (rsrc.joypad_mut(idx0).read() as u8) << 0;
                            res |= (rsrc.joypad_mut(idx1).read() as u8) << 1;
                            res
                        }
                    }
                }

                // Write-only
                0x4018..=0x420F => {
                    tick_aux(FAST_CYCLES);
                    self.open_bus
                }

                // RDNMI
                0x4210 => wrap_io_port!({
                    let mut res = 0u8;
                    // TODO: CPU Version
                    res |= self.open_bus & 0x70;
                    res |= (rsrc.nmi_mut().read_and_clear() as u8) << 7;
                    res
                }),

                // TIMEUP
                0x4211 => wrap_io_port!({
                    let mut res = self.open_bus & 0x7F;
                    res |= (rsrc.pending_events_mut().consume_hv_irq() as u8) << 7;
                    res
                }),

                // HVBJOY
                0x4212 => wrap_io_port!({
                    let mut res = self.open_bus & 0x3E;
                    res |= (rsrc.joypad(0).auto_read_in_progress() as u8) << 0;
                    res |= (self.hblank as u8) << 6;
                    res |= (self.vblank as u8) << 7;
                    res
                }),

                // TODO: RDIO
                0x4213 => wrap_io_port!(self.open_bus),

                // RDDIVL
                0x4214 => wrap_io_port!((self.rddiv & 0x00FF) as u8),

                // RDDIVH
                0x4215 => wrap_io_port!((self.rddiv >> 8) as u8),

                // RDMPYL
                0x4216 => wrap_io_port!((self.rdmpy & 0x00FF) as u8),

                // RDMPYH
                0x4217 => wrap_io_port!((self.rdmpy >> 8) as u8),

                0x4218..=0x421F => wrap_io_port!({
                    let idx = ((offset - 0x4218) / 2) as usize;

                    // FIXME?:
                    // https://problemkaputt.de/fullsnes.htm#snescontrollersioportsautomaticreading
                    //
                    // Reading $4218-f during this time will read back incorrect values.
                    // The only reliable value is that no buttons pressed will return 0
                    // (however, if buttons are pressed 0 could still be returned incorrectly).
                    // Presumably reading $4016/7 or writing $4016 during this time will also screw things up.
                    let v = rsrc.joypad_mut(idx).auto_read_result().unwrap();
                    if (offset & 1) == 0 {
                        (v & 0xFF) as u8
                    } else {
                        (v >> 8) as u8
                    }
                }),

                // Unused region
                0x4220..=0x42FF => wrap_io_port!(self.open_bus),

                // DMA
                0x4300..=0x437F => wrap_io_port!({
                    let idx = ((offset >> 4) & 7) as usize;
                    match offset & 0xF {
                        0x0 => self.dma[idx].dmap.clone().into(),
                        0x1 => self.dma[idx].bbad,
                        0x2 => self.dma[idx].a1t.lo,
                        0x3 => self.dma[idx].a1t.hi,
                        0x4 => self.dma[idx].a1b,
                        0x5 => self.dma[idx].das.lo,
                        0x6 => self.dma[idx].das.hi,
                        0x7 => self.dma[idx].dasb,
                        0x8 => self.dma[idx].a2a.lo,
                        0x9 => self.dma[idx].a2a.hi,
                        0xA => self.dma[idx].nltr.into(),
                        0xB | 0xF => self.dma[idx].unused,
                        0xC..=0xE => self.open_bus,
                        _ => unreachable!(),
                    }
                }),

                0x4380..=0x5FFF => wrap_io_port!(self.open_bus),

                // System Area -- Expansion
                0x6000..=0x7FFF => {
                    tick_aux(SLOW_CYCLES);
                    self.open_bus // TODO?
                }
                // WS{1,2} LoROM
                0x8000..=0xFFFF => {
                    tick_aux(rom_cycles);
                    rsrc.cartridge().mapped(bank, offset).unwrap_or(UNMAPPED_ROM_VALUE)
                }
            },
            // WS{1,2} HiROM
            0x40..=0x7D | 0xC0..=0xFF => {
                tick_aux(rom_cycles);
                rsrc.cartridge().mapped(bank, offset).unwrap_or(UNMAPPED_ROM_VALUE)
            }
            // WRAM
            0x7E | 0x7F => {
                tick_aux(rom_cycles);
                let addr = (bank as usize - 0x7E) * 0x10000 + (offset as usize);
                self.wram[addr]
            }
        };

        if how_to_access != HowToAccess::Debugger {
            self.open_bus = res;
        }

        res
    }

    pub fn write(&mut self, addr: u32, value: u8, how_to_access: HowToAccess, rsrc: &mut impl Resources) {
        let bank = (addr >> 16) as u8;
        let offset = (addr & 0xFFFF) as u16;

        let rom_cycles = if 0x80 <= bank && self.memsel.enable_fast_rom() { FAST_CYCLES } else { SLOW_CYCLES };

        let mut tick_aux = |cycles: u8| {
            if how_to_access == HowToAccess::CPU {
                rsrc.tick(cycles)
            }
        };

        self.open_bus = value;

        macro_rules! wrap_io_port {
            ($e: expr) => {{
                tick_aux(FAST_CYCLES);
                if how_to_access != HowToAccess::DMA_A_Bus {
                    $e
                }
            }};
        }

        match bank {
            0x00..=0x3F | 0x80..=0xBF => match offset {
                // System Area -- Mirror of WRAM
                0x0000..=0x1FFF => {
                    tick_aux(SLOW_CYCLES);
                    self.wram[offset as usize] = value;
                }
                // System Area -- Unused
                0x2000..=0x20FF => {
                    tick_aux(FAST_CYCLES);
                }

                // System Area -- I/O Ports (B-Bus)
                0x2100..=0x2133 => wrap_io_port!(rsrc.ppu_mut().write(offset, value)),

                // Read-Only
                0x2134..=0x213F => wrap_io_port!(()),

                0x2140..=0x217F => wrap_io_port!({
                    let port = (offset & 3) as u8;
                    rsrc.apu_mut().port_write(port, value);
                }),

                // WMDATA
                0x2180 => wrap_io_port!({
                    // println!("wram[{:#08X}] <- {:#06X}", self.wmadd, value);
                    self.wram[self.wmadd as usize] = value;
                    self.incr_wmadd();
                }),

                // WMADD
                0x2181 => wrap_io_port!(self.wmadd = (self.wmadd & 0x1FF00) | (((value & 0xFF) as u32) << 0)),
                0x2182 => wrap_io_port!(self.wmadd = (self.wmadd & 0x100FF) | (((value & 0xFF) as u32) << 8)),
                0x2183 => wrap_io_port!(self.wmadd = (self.wmadd & 0x0FFFF) | (((value & 0x01) as u32) << 16)),

                0x2184..=0x21FF => wrap_io_port!(()),

                // System Area -- Unused
                0x2200..=0x3FFF => {
                    tick_aux(FAST_CYCLES);
                }

                // System Area -- I/O Ports
                0x4000..=0x4015 | 0x4017..=0x41FF => {
                    tick_aux(XSLOW_CYCLES);
                }

                0x4016 => {
                    tick_aux(XSLOW_CYCLES);
                    if how_to_access != HowToAccess::DMA_A_Bus {
                        let v = (value & 1) == 1;
                        for i in 0..4 {
                            rsrc.joypad_mut(i).set_latch(v);
                        }
                    }
                }

                // System Area -- I/O Ports
                // NMITIMEN
                0x4200 => wrap_io_port!({
                    self.nmitimen = value.into();
                    rsrc.nmi_mut().set_enable(self.nmitimen.vblank_nmi_enable());
                }),

                // WRIO
                0x4201 => wrap_io_port!({ /* TODO */ }),

                // WRMPYA
                0x4202 => wrap_io_port!(self.wrmpya = value),

                // WRMPYB
                0x4203 => wrap_io_port!({
                    self.wrmpyb = value;

                    // TODO: This should take 8 CPU cycles
                    // (But I believe that decent games don't read this value until the calculus isn't completed)
                    self.rdmpy = (self.wrmpya as u16) * (self.wrmpyb as u16);
                    self.rddiv = self.wrmpyb as u16;
                }),

                // WRDIVL
                0x4204 => wrap_io_port!(self.wrdiv = (self.wrdiv & 0xFF00) | (value as u16)),

                // WRDIVH
                0x4205 => wrap_io_port!(self.wrdiv = (self.wrdiv & 0x00FF) | ((value as u16) << 8)),

                // WRDIVB
                0x4206 => wrap_io_port!({
                    // TODO: Take 16 CPU cycles
                    self.wrdivb = value;
                    if self.wrdivb == 0 {
                        self.rddiv = 0xFFFF;
                        self.rdmpy = self.wrdiv;
                    } else {
                        self.rddiv = self.wrdiv / (self.wrdivb as u16);
                        self.rdmpy = self.wrdiv % (self.wrdivb as u16);
                    }
                }),

                // HTIME
                0x4207 => wrap_io_port!(self.htime.lo = value),
                0x4208 => wrap_io_port!(self.htime.hi = value & 0x01),

                // VTIME
                0x4209 => wrap_io_port!(self.vtime.lo = value),
                0x420A => wrap_io_port!(self.vtime.hi = value & 0x01),

                // MDMAEN / HDMAEN
                0x420B | 0x420C => wrap_io_port!({
                    for i in 0..8 {
                        let v = ((value >> i) & 1) == 1;
                        if offset == 0x420B {
                            self.mdmaen[i] = v;
                        } else {
                            self.hdmaen[i] = v;
                        }
                    }
                }),

                // MEMSEL
                0x420D => wrap_io_port!(self.memsel = value.into()),

                // DMA
                0x4300..=0x437F => wrap_io_port!({
                    let idx = ((offset >> 4) & 7) as usize;
                    match offset & 0xF {
                        0x0 => self.dma[idx].dmap = value.into(),
                        0x1 => self.dma[idx].bbad = value,
                        0x2 => self.dma[idx].a1t.lo = value,
                        0x3 => self.dma[idx].a1t.hi = value,
                        0x4 => self.dma[idx].a1b = value,
                        0x5 => self.dma[idx].das.lo = value,
                        0x6 => self.dma[idx].das.hi = value,
                        0x7 => self.dma[idx].dasb = value,
                        0x8 => self.dma[idx].a2a.lo = value,
                        0x9 => self.dma[idx].a2a.hi = value,
                        0xA => self.dma[idx].nltr = value.into(),
                        0xB | 0xF => self.dma[idx].unused = value,
                        _ => (),
                    }
                }),

                0x420E..=0x42FF | 0x4380..=0x5FFF => wrap_io_port!(()),

                // System Area -- Expansion
                0x6000..=0x7FFF => {
                    tick_aux(SLOW_CYCLES);
                }
                // WS{1,2} LoROM
                0x8000..=0xFFFF => {
                    tick_aux(rom_cycles);
                    // FIXME?: Ignoring is correct?
                }
            },
            // WS{1,2} HiROM
            0x40..=0x7D | 0xC0..=0xFF => {
                tick_aux(rom_cycles);
                // FIXME?: Ignoring is correct?
            }
            // WRAM
            0x7E | 0x7F => {
                tick_aux(rom_cycles);
                let addr = (bank as usize - 0x7E) * 0x10000 + (offset as usize);
                self.wram[addr] = value;
            }
        };
    }

    // General-Purpose DMA
    fn gpdma_step(&mut self, ch: usize, rsrc: &mut impl Resources) {
        // Take 8 clocks when starting
        if !self.dma[ch].in_progress {
            self.dma[ch].in_progress = true;
            self.dma[ch].unit_index = 0;
            rsrc.tick(8);
        }

        let a_addr = self.dma[ch].a1t.with_bank(self.dma[ch].a1b);
        let transfer_unit = self.dma[ch].dmap.transfer_unit_selection();
        let b_addr = 0x2100 | (self.dma[ch].bbad.wrapping_add(transfer_unit[self.dma[ch].unit_index]) as u16);

        match self.dma[ch].dmap.transfer_dir() {
            DMA_TransferDir::CPU2IO => {
                let byte = self.read(a_addr, HowToAccess::DMA_A_Bus, rsrc);
                self.write(b_addr as u32, byte, HowToAccess::DMA_B_Bus, rsrc);
            }
            DMA_TransferDir::IO2CPU => {
                let byte = self.read(b_addr as u32, HowToAccess::DMA_B_Bus, rsrc);
                self.write(a_addr, byte, HowToAccess::DMA_A_Bus, rsrc);
            }
        }

        rsrc.tick(DMA_TRANSFER_CYCLES);

        match self.dma[ch].dmap.a_bus_addr_step() {
            A_BusAddressStep::Increment => self.dma[ch].a1t.incr(),
            A_BusAddressStep::Decrement => self.dma[ch].a1t.decr(),
            A_BusAddressStep::Fixed => (),
        };

        self.dma[ch].unit_index += 1;
        if self.dma[ch].unit_index == transfer_unit.len() {
            self.dma[ch].unit_index = 0;
        }

        let byte_counter: u16 = self.dma[ch].das.into();
        let byte_counter = byte_counter.wrapping_sub(1);
        self.dma[ch].das = TwoWords::from(byte_counter);
        if byte_counter == 0 {
            self.dma[ch].in_progress = false;
            self.mdmaen[ch] = false;
        }
    }

    fn dma_step(&mut self, rsrc: &mut impl Resources) {
        if rsrc.pending_events_mut().consume_reload_hdma() {
            self.hdma_reload(rsrc);
        }

        if rsrc.pending_events_mut().consume_perform_hdma() {
            for ch in 0..8 {
                if self.hdmaen[ch] && !self.dma[ch].hdma_terminate {
                    self.dma[ch].unit_index = 0;
                    self.dma[ch].hdma_finish_scanline = false;
                }
            }

            // https://wiki.superfamicom.org/dma-and-hdma
            //
            // For each scanline during which HDMA is active (i.e. at least one channel is not paused and has not terminated yet for the frame), there are ~18 master cycles overhead.
            for ch in 0..8 {
                if self.dma[ch].hdma_do_transfer {
                    rsrc.tick(18);
                    break;
                }
            }
        }

        for ch in 0..8 {
            if !self.hdmaen[ch] || self.dma[ch].hdma_finish_scanline || self.dma[ch].hdma_terminate {
                continue;
            }
            self.hdma_step(ch, rsrc);
            return;
        }

        for ch in 0..8 {
            if !self.mdmaen[ch] {
                continue;
            }
            self.gpdma_step(ch, rsrc);
            return;
        }
    }

    fn hdma_read_table(&mut self, ch: usize, rsrc: &mut impl Resources) -> u8 {
        let addr = self.dma[ch].a2a.with_bank(self.dma[ch].a1b);
        let byte = self.read(addr, HowToAccess::DMA_A_Bus, rsrc);
        self.dma[ch].a2a.incr();
        byte
    }

    // At begining of a frame
    fn hdma_reload(&mut self, rsrc: &mut impl Resources) {
        for ch in 0..8 {
            self.dma[ch].hdma_do_transfer = false;
            self.dma[ch].hdma_terminate = false;
            if !self.hdmaen[ch] {
                continue;
            }
            self.dma[ch].a2a = self.dma[ch].a1t.clone();

            // https://wiki.superfamicom.org/dma-and-hdma
            //
            // Load $43xA (Line Counter and Repeat) from the table. I believe $00 will terminate this channel immediately.
            let table0 = self.hdma_read_table(ch, rsrc);
            if table0 == 0 {
                continue;
            }
            self.dma[ch].nltr = table0.into();

            match self.dma[ch].dmap.addr_mode() {
                DMA_AddrMode::DirectTable => {
                    rsrc.tick(8);
                }
                DMA_AddrMode::IndirectTable => {
                    self.dma[ch].das.lo = self.hdma_read_table(ch, rsrc);
                    self.dma[ch].das.hi = self.hdma_read_table(ch, rsrc);
                    rsrc.tick(24);
                }
            }

            self.dma[ch].hdma_do_transfer = true;
            self.dma[ch].hdma_finish_scanline = true; // To prevent HDMA run immediately
            self.dma[ch].hdma_terminate = false;
        }
    }

    // TODO: What cause if mdmaen[ch] is also true
    // Assume !self.hdma_terminate_transfer[ch] && !self.hdma_finish_scanlne[ch]
    fn hdma_step(&mut self, ch: usize, rsrc: &mut impl Resources) {
        assert!(!self.dma[ch].hdma_terminate && !self.dma[ch].hdma_finish_scanline);
        if self.dma[ch].hdma_do_transfer {
            let data = match self.dma[ch].dmap.addr_mode() {
                DMA_AddrMode::DirectTable => self.hdma_read_table(ch, rsrc),
                DMA_AddrMode::IndirectTable => {
                    let addr = self.dma[ch].das.with_bank(self.dma[ch].dasb);
                    self.dma[ch].das.incr();
                    self.read(addr, HowToAccess::DMA_A_Bus, rsrc)
                }
            };
            let transfer_unit = self.dma[ch].dmap.transfer_unit_selection();
            let b_addr = 0x2100 | (self.dma[ch].bbad.wrapping_add(transfer_unit[self.dma[ch].unit_index]) as u16);
            self.write(b_addr.into(), data, HowToAccess::DMA_B_Bus, rsrc);

            rsrc.tick(DMA_TRANSFER_CYCLES);

            self.dma[ch].unit_index += 1;
            if self.dma[ch].unit_index < transfer_unit.len() {
                return;
            }
        }

        self.dma[ch].hdma_finish_scanline = true;
        self.dma[ch].nltr = {
            let cur: u8 = self.dma[ch].nltr.into();
            let cur = cur.wrapping_sub(1);
            cur.into()
        };
        self.dma[ch].hdma_do_transfer = self.dma[ch].nltr.hdma_repeat_flag();
        if self.dma[ch].nltr.scanline_counter() == 0 {
            let table0 = self.hdma_read_table(ch, rsrc);
            self.dma[ch].nltr = table0.into();
            rsrc.tick(8);
            if self.dma[ch].dmap.addr_mode() == DMA_AddrMode::IndirectTable {
                let mut is_last = true;
                for nch in (ch + 1)..8 {
                    if self.hdmaen[nch] {
                        is_last = false;
                        break;
                    }
                }
                if table0 == 0 && is_last {
                    // One oddity: if $43xA is 0 and this is the last active HDMA channel for this scanline,
                    // only load one byte for Address, and use the $00 for the low byte.
                    // So Address ends up incremented one less than otherwise expected, and one less CPU Cycle is used.
                    self.dma[ch].das.lo = 0;
                    self.dma[ch].das.hi = self.hdma_read_table(ch, rsrc);
                    rsrc.tick(8);
                } else {
                    self.dma[ch].das.lo = self.hdma_read_table(ch, rsrc);
                    self.dma[ch].das.hi = self.hdma_read_table(ch, rsrc);
                    rsrc.tick(16);
                }
            }
            if table0 == 0 {
                self.dma[ch].hdma_terminate = true;
            }
            self.dma[ch].hdma_do_transfer = true;
        }
    }
}
