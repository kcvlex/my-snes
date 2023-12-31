mod decode;
mod dsp;
mod memory;
mod spc700;

use media;

#[derive(Default)]
pub struct APU {
    cpu: spc700::SPC700,
    mem: memory::Memory,
    dsp: dsp::DSP,
    timer0: Timer0,
    timer1: Timer1,
    timer2: Timer2,
    clock: Clock,

    audio_player: Option<media::audio::AudioPlayer>,
}

#[derive(Default)]
pub struct Timer<const FREQ: u64> {
    div: u8,
    out: u8,
    counter: u64,
    enabled: bool,

    last_master: u64,
}

impl<const FREQ: u64> Timer<FREQ> {
    const RATE: u64 = APU_CLOCK / FREQ;

    fn tick(&mut self, clock: &Clock) {
        let div = if self.div == 0 { 256 } else { self.div as u64 };
        while self.last_master < clock.master {
            self.last_master += Self::RATE;
            self.counter += 1;
            if self.counter == div {
                self.counter = 0;
                self.out += 1;
                self.out &= 0xF;
            }
        }
    }

    fn set_enabled(&mut self, enabled: bool) {
        if !self.enabled && enabled {
            self.out = 0;
            self.counter = 0;
        }
        self.enabled = enabled;
    }

    fn read(&mut self) -> u8 {
        let res = self.out;
        self.out = 0;
        res
    }

    fn write_div(&mut self, v: u8) {
        self.div = v;
    }
}

const TIMER0_FREQ: u64 = 8000;
// const TIMER0_FREQ: u64 = 8000<<8;  // For DEBUG
pub type Timer0 = Timer<TIMER0_FREQ>;
pub type Timer1 = Timer<8000>;
pub type Timer2 = Timer<64000>;

#[derive(Default)]
pub struct Clock {
    master: u64,
    spc_count: u64,
    dsp_count: u64,
}

const MASTER_CLOCK: u64 = 21_477_270; // Hz
const APU_CLOCK: u64 = 24_576_000; // Hz
const SPC_CLOCK: u64 = 1_024_000; // Hz
const DSP_CLOCK: u64 = 32_000; // Hz

const SPC_STEP: u64 = APU_CLOCK / SPC_CLOCK;
const DSP_STEP: u64 = APU_CLOCK / DSP_CLOCK;

struct Resources<'a> {
    dsp: &'a mut dsp::DSP,
    timer0: &'a mut Timer0,
    timer1: &'a mut Timer1,
    timer2: &'a mut Timer2,
    clock: &'a mut Clock,
}

struct MemWithResources<'a> {
    mem: &'a mut memory::Memory,
    rsrc: &'a mut Resources<'a>,
}

macro_rules! spc_resources {
    ($self: ident) => {{
        MemWithResources {
            mem: &mut $self.mem,
            rsrc: &mut Resources {
                dsp: &mut $self.dsp,
                timer0: &mut $self.timer0,
                timer1: &mut $self.timer1,
                timer2: &mut $self.timer2,
                clock: &mut $self.clock,
            },
        }
    }};
}

impl<'a> memory::Resources for Resources<'a> {
    fn add_cycles(&mut self, v: u8) {
        self.clock.spc_count += v as u64 * SPC_STEP;
    }

    fn dsp_read(&self, addr: u8) -> u8 {
        self.dsp.read(addr)
    }

    fn dsp_write(&mut self, addr: u8, v: u8) {
        self.dsp.write(addr, v)
    }

    fn timer0_mut(&mut self) -> &mut Timer0 {
        self.timer0
    }

    fn timer1_mut(&mut self) -> &mut Timer1 {
        self.timer1
    }

    fn timer2_mut(&mut self) -> &mut Timer2 {
        self.timer2
    }
}

impl<'a> spc700::Resources for MemWithResources<'a> {
    fn add_cycles(&mut self, v: u8) {
        self.rsrc.clock.spc_count += v as u64 * SPC_STEP;
    }

    fn read(&mut self, addr: u16) -> u8 {
        self.mem.read(addr, self.rsrc)
    }

    fn write(&mut self, addr: u16, v: u8) {
        self.mem.write(addr, v, self.rsrc);
    }
}

impl APU {
    pub fn init(audio_player: media::audio::AudioPlayer) -> Self {
        Self {
            audio_player: Some(audio_player),
            ..Default::default()
        }
    }

    pub fn reset(&mut self) {
        self.mem = memory::Memory::default();
        let mut rsrc = spc_resources!(self);
        self.cpu.reset(&mut rsrc);
        self.dsp.reset();
    }

    pub fn tick(&mut self, master: u64) {
        let master = master * APU_CLOCK / MASTER_CLOCK;
        while self.clock.master < master {
            self.clock.master += 1;
            self.timer0.tick(&self.clock);
            self.timer1.tick(&self.clock);
            self.timer2.tick(&self.clock);
            while self.clock.spc_count < self.clock.master {
                let mut rsrc = spc_resources!(self);
                self.cpu.tick(&mut rsrc);
            }
            while self.clock.dsp_count < self.clock.master {
                let mem = self.mem.raw_mut();
                let sample = self.dsp.tick(mem);
                self.audio_player.as_mut().unwrap().push(sample[0], sample[1]);
                self.clock.dsp_count += DSP_STEP;
            }
        }
    }

    pub fn resume_audio(&mut self) {
        self.audio_player.as_mut().unwrap().resume();
    }

    pub fn send_audio(&mut self, force: bool) -> Result<(), String> {
        let player = self.audio_player.as_mut().ok_or("uninitialized audio_player".to_owned())?;
        if force {
            player.send_force()?;
        } else {
            player.send()?;
        }
        Ok(())
    }

    pub fn wait_finish(&self) {
        self.audio_player.as_ref().unwrap().wait_finish();
    }

    pub fn port_read(&self, port: u8) -> u8 {
        self.mem.cpu_port_read(port)
    }

    pub fn port_write(&mut self, port: u8, v: u8) {
        self.mem.cpu_port_write(port, v);
    }
}
