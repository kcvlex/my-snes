use crate::apu;
use crate::cpu;
use crate::debug;
use crate::joypad;
use crate::memory;
use crate::ppu;
use crate::rom;
use macros::SNES_Events;
use media::audio::AudioPlayer;
use media::render::Renderer;

use sdl2::event;
use sdl2::keyboard;

// use log::debug;
use std::io::{self, stdout, BufRead, Write};

pub struct Emulator {
    memory: memory::Memory,
    cpu: cpu::CPU,
    ppu: ppu::PPU,
    apu: apu::APU,
    joypads: [joypad::Joypad; 4],
    cartridge: rom::Cartridge,
    events: PendingEvents,
    nmi: NMI,
    clock: Clock,
    debugger: debug::Debugger,

    // DEBUG
    pause: bool,
}

#[derive(Default, SNES_Events)]
pub struct PendingEvents {
    set_hblank: bool,
    set_vblank: bool,
    set_vblank_nmi: bool,
    reset_nmi: bool,
    clear_hblank: bool,
    clear_vblank: bool,

    hv_irq: bool,

    reload_hdma: bool,
    perform_hdma: bool,

    joypad_auto_read: bool,
}

// NMI happens only when VBlank
#[derive(Default)]
pub struct NMI {
    flag: bool,
    enable: bool,
    detect_nmi: bool,
}

impl NMI {
    pub fn set(&mut self) {
        let pre = self.flag && self.enable;
        self.flag = true;
        let cur = self.flag && self.enable;
        self.detect_nmi = !pre && cur;
    }

    pub fn reset(&mut self) {
        self.flag = false;
    }

    pub fn enable(&mut self) {
        let pre = self.flag && self.enable;
        self.enable = true;
        let cur = self.flag && self.enable;
        self.detect_nmi = !pre && cur;
    }

    pub fn disable(&mut self) {
        self.enable = false;
    }

    pub fn set_enable(&mut self, v: bool) {
        if v {
            self.enable()
        } else {
            self.disable()
        }
    }

    // FIXME?: Wait clear for a while?
    pub fn consume_detected(&mut self) -> bool {
        let res = self.detect_nmi;
        self.detect_nmi = false;
        res
    }

    pub fn read_and_clear(&mut self) -> bool {
        let res = self.flag;
        self.flag = false;
        res
    }
}

#[derive(Default)]
pub struct Clock {
    count: u64,
}

impl Clock {
    pub fn add_cycles(&mut self, v: u8) {
        self.count += v as u64;
    }
}

struct Resources<'a> {
    ppu: &'a mut ppu::PPU,
    apu: &'a mut apu::APU,
    cartridge: &'a mut rom::Cartridge,
    joypads: &'a mut [joypad::Joypad; 4],
    events: &'a mut PendingEvents,
    nmi: &'a mut NMI,
    clock: &'a mut Clock,
}

struct MemWithResources<'a> {
    mem: &'a mut memory::Memory,
    debugger: &'a mut debug::Debugger,
    rsrc: &'a mut Resources<'a>,
}

struct EmulatorForDebug<'a> {
    cpu: &'a cpu::CPU,
    mem: &'a mut memory::Memory,
    rsrc: &'a mut Resources<'a>,
}

macro_rules! memory_resources {
    ($self: ident) => {{
        Resources {
            ppu: &mut $self.ppu,
            apu: &mut $self.apu,
            cartridge: &mut $self.cartridge,
            joypads: &mut $self.joypads,
            events: &mut $self.events,
            nmi: &mut $self.nmi,
            clock: &mut $self.clock,
        }
    }};
}

macro_rules! cpu_resources {
    ($self: ident) => {{
        MemWithResources {
            mem: &mut $self.memory,
            debugger: &mut $self.debugger,
            rsrc: &mut memory_resources!($self),
        }
    }};
}

macro_rules! debugger_emulator {
    ($self: ident) => {{
        EmulatorForDebug {
            cpu: &$self.cpu,
            mem: &mut $self.memory,
            rsrc: &mut memory_resources!($self),
        }
    }};
}

fn tick_with_joypads(v: u8, clock: &mut Clock, joypads: &mut [joypad::Joypad; 4]) {
    clock.add_cycles(v);
    for joypad in joypads.iter_mut() {
        joypad.tick(v)
    }
}

impl<'a> memory::Resources for Resources<'a> {
    fn tick(&mut self, v: u8) {
        tick_with_joypads(v, self.clock, self.joypads);
    }

    fn ppu_mut(&mut self) -> &mut ppu::PPU {
        self.ppu
    }

    fn apu_mut(&mut self) -> &mut apu::APU {
        self.apu
    }

    fn cartridge(&self) -> &rom::Cartridge {
        self.cartridge
    }

    fn pending_events_mut(&mut self) -> &mut PendingEvents {
        self.events
    }

    fn nmi_mut(&mut self) -> &mut NMI {
        self.nmi
    }

    fn joypad(&self, i: usize) -> &joypad::Joypad {
        &self.joypads[i]
    }

    fn joypad_mut(&mut self, i: usize) -> &mut joypad::Joypad {
        &mut self.joypads[i]
    }
}

impl<'a> cpu::Resources for MemWithResources<'a> {
    fn tick(&mut self, v: u8) {
        tick_with_joypads(v, self.rsrc.clock, self.rsrc.joypads);
    }

    fn read(&mut self, addr: u32) -> u8 {
        self.mem.read(addr, memory::HowToAccess::CPU, self.rsrc)
    }

    fn write(&mut self, addr: u32, v: u8) {
        self.mem.write(addr, v, memory::HowToAccess::CPU, self.rsrc);
    }

    fn pending_events_mut(&mut self) -> &mut PendingEvents {
        &mut self.rsrc.events
    }

    fn nmi_mut(&mut self) -> &mut NMI {
        &mut self.rsrc.nmi
    }

    fn debugger_mut(&mut self) -> &mut debug::Debugger {
        &mut self.debugger
    }
}

impl<'a> debug::Emulator for EmulatorForDebug<'a> {
    fn cpu_regs(&self) -> cpu::Regfile {
        self.cpu.reg.clone()
    }

    fn read_mem(&mut self, addr: u32) -> u8 {
        self.mem.read(addr, memory::HowToAccess::Debugger, self.rsrc)
    }
}

impl Emulator {
    pub fn make(raw_rom: &[u8], renderer: Renderer, audio_player: AudioPlayer) -> Result<Self, String> {
        let cartridge = rom::Cartridge::make(raw_rom)?;
        let ppu = ppu::PPU::init(renderer);
        let apu = apu::APU::init(audio_player);
        Ok(Self {
            memory: memory::Memory::default(),
            cpu: cpu::CPU::default(),
            ppu,
            apu,
            joypads: [joypad::Joypad::default(); 4],
            cartridge,
            events: PendingEvents::default(),
            nmi: NMI::default(),
            clock: Clock::default(),
            debugger: debug::Debugger::default(),
            pause: false,
        })
    }

    pub fn reset(&mut self) {
        {
            let mut rsrc = cpu_resources!(self);
            self.cpu.reset(&mut rsrc);
        }
        self.apu.reset();
    }

    pub fn step(&mut self) {
        if self.pause {
            return;
        }

        if !self.memory.in_dma() {
            if self.debugger.is_interrupting() {
                if let Some(instr) = self.debugger.get_instr() {
                    println!("{}", instr);
                }
                let mut rsrc = debugger_emulator!(self);
                loop {
                    let mut line = String::new();
                    print!("(debugger) >> ");
                    stdout().flush().unwrap();
                    io::stdin().lock().read_line(&mut line).unwrap();
                    match self.debugger.exec(&line[..(line.len() - 1)], &mut rsrc) {
                        debug::Control::Print(s) => println!("{}", s),
                        debug::Control::Exit => break,
                    }
                }
            }
            {
                let mut rsrc = cpu_resources!(self);
                self.cpu.tick(&mut rsrc);
            }
        }

        {
            let mut rsrc = memory_resources!(self);
            self.memory.tick(&mut rsrc);
        }

        {
            match self.ppu.tick(self.clock.count, &self.memory, &mut self.events) {
                Ok(()) => (),
                Err(e) => panic!("{}", e),
            }
        }

        {
            self.apu.tick(self.clock.count);
            self.apu.send_audio(false).unwrap();
        }
    }

    pub fn cpu_debug(&mut self, debug: bool) {
        self.cpu.enable_debug = debug;
    }

    // TODO: JOY2 ~ JOY4
    pub fn handle_event<F>(&mut self, event: &event::Event, key_mapping: F)
    where
        F: FnOnce(keyboard::Keycode) -> Option<joypad::Button>,
    {
        match event {
            event::Event::KeyDown {
                keycode: Some(keyboard::Keycode::P), ..
            } => {
                self.pause = !self.pause;
            }
            event::Event::KeyDown { keycode: Some(key), .. } => {
                if let Some(button) = key_mapping(*key) {
                    self.joypads[0].press(button);
                }
            }
            event::Event::KeyUp { keycode: Some(key), .. } => {
                if let Some(button) = key_mapping(*key) {
                    self.joypads[0].release(button);
                }
            }
            _ => (),
        }
    }

    pub fn resume_audio(&mut self) {
        self.apu.resume_audio()
    }

    pub fn frame(&self) -> u32 {
        self.ppu.frame()
    }
}
