#![allow(dead_code)]

use modular_bitfield::prelude::*;

#[bitfield]
#[repr(u8)]
#[derive(Clone, Copy, Default)]
struct BRRControl {
    end: bool,
    do_loop: bool,
    filter_bin: B2,
    left_shift: B4,
}

impl BRRControl {
    fn filter(&self, sample: i16, old: i16, older: i16) -> i16 {
        let sample = sample as i32;
        let old = old as i32;
        let older = older as i32;
        let res = match self.filter_bin() {
            0 => sample,
            1 => sample + old * 1 + ((-old * 1) >> 4),
            2 => sample + old * 2 + ((-old * 3) >> 5) - older + ((older * 1) >> 4),
            3 => sample + old * 2 + ((-old * 13) >> 6) - older + ((older * 3) >> 4),
            _ => unreachable!(),
        };
        res.clamp(-0x8000, 0x7FFF) as i16
    }
}

#[derive(Default, PartialEq)]
enum ADSR {
    #[default]
    Attack,
    Decay,
    Sustain,
    Release,
}

#[bitfield]
#[repr(u16)]
#[derive(Default)]
struct VxADSR {
    attack_rate: B4,
    decay_rate: B3,
    use_adsr: bool,
    sustain_rate: B5,
    sustain_level: B3,
}

#[bitfield]
#[repr(u8)]
#[derive(Clone, Copy, Default)]
struct VxGAIN {
    rate: B5,
    mode: GainMode,
    is_custom: bool,
}

#[derive(BitfieldSpecifier)]
#[bits = 2]
#[derive(Clone, Copy)]
enum GainMode {
    LinearDecrease = 0,
    ExpDecrease = 1,
    LinearIncrease = 2,
    BentIncrease = 3,
}

impl GainMode {
    fn apply(&self, v: u16) -> u16 {
        match self {
            GainMode::LinearDecrease => v.saturating_sub(32),
            GainMode::ExpDecrease => v.saturating_sub(((v - 1) >> 8) + 1),
            GainMode::LinearIncrease => v + 32,
            GainMode::BentIncrease => v + if v < 0x600 { 32 } else { 8 },
        }
    }
}

impl VxGAIN {
    fn fixed_volume(&self) -> u16 {
        let tmp: u8 = self.clone().into();
        let tmp = tmp as u16;
        tmp * 16
    }
}

#[derive(Default)]
struct RateClock {
    clock: u16,
}

impl RateClock {
    fn tick(&mut self) {
        self.clock += 1;
    }

    fn consume_trigger(&mut self, rate: usize) -> bool {
        if rate == 0 {
            false
        } else {
            let period = PERIOD_TABLE[rate] as u32;
            let offset = PERIOD_OFFSET[rate] as u32;
            let res = period <= (self.clock as u32 + offset);
            if res {
                self.clock = 0;
            }
            res
        }
    }
}

#[derive(Default)]
struct Envelope {
    value: u16,
    clock: RateClock,
    adsr: VxADSR,
    gain: VxGAIN,
    mode: ADSR,
}

impl Envelope {
    fn tick(&mut self) {
        self.clock.tick();

        let res = if self.adsr.use_adsr() || self.mode == ADSR::Release {
            // TODO?
            match self.mode {
                ADSR::Attack => {
                    let rate = self.adsr.attack_rate() as usize;
                    let rate = rate * 2 + 1;
                    let step = if self.clock.consume_trigger(rate) {
                        if rate == 31 {
                            1024
                        } else {
                            32
                        }
                    } else {
                        0
                    };
                    let res = self.value.saturating_add(step);
                    if 0x7E0 <= res {
                        self.mode = ADSR::Decay;
                    }
                    res
                }
                ADSR::Decay => {
                    let rate = self.adsr.decay_rate() as usize;
                    let rate = rate * 2 + 16;
                    let step = if self.clock.consume_trigger(rate) { ((self.value - 1) >> 8) + 1 } else { 0 };
                    let res = self.value.saturating_sub(step);
                    let boundary = self.adsr.sustain_level() as u16;
                    let boundary = (boundary + 1) * 0x100;
                    if res <= boundary {
                        self.mode = ADSR::Sustain;
                    }
                    res
                }
                ADSR::Sustain => {
                    let rate = self.adsr.sustain_rate() as usize;
                    let step = if self.clock.consume_trigger(rate) { ((self.value - 1) >> 8) + 1 } else { 0 };
                    self.value.saturating_sub(step)
                }
                ADSR::Release => {
                    let rate = 31usize;
                    let step = if self.clock.consume_trigger(rate) { 8 } else { 0 }; // TODO: -0x800 if BRREnd
                    self.value.saturating_sub(step)
                }
            }
        } else {
            if !self.gain.is_custom() {
                self.gain.fixed_volume()
            } else {
                let rate = self.gain.rate();
                if !self.clock.consume_trigger(rate as usize) {
                    self.value
                } else {
                    self.gain.mode().apply(self.value)
                }
            }
        };
        self.value = res.clamp(0, 0x7FF);
    }

    fn envx(&self) -> u8 {
        ((self.value >> 4) & 0x7F) as u8
    }
}

#[derive(Default)]
struct Noise {
    clock: RateClock,
    value: i16,
}

impl Noise {
    fn reset(&mut self) {
        self.value = -0x4000;
    }

    fn tick(&mut self, ctx: &Context) {
        self.clock.tick();
        let rate = ctx.flg.noise_freq();
        if self.clock.consume_trigger(rate as usize) {
            let b0 = (self.value >> 0) & 1;
            let b1 = (self.value >> 1) & 1;
            self.value = ((self.value >> 1) & 0x3FFF) | (((b0 ^ b1) << 15) >> 1);
        }
    }
}

#[derive(Default)]
struct Context {
    dir: u8,
    non: u8,
    flg: FLG,
}

impl Context {
    fn enable_noise(&self, i: usize) -> bool {
        ((self.non >> i) & 1) == 1
    }
}

#[derive(Default)]
struct BRREntry {
    start_addr: u16,
    restart_loop_addr: u16,
}

#[derive(Default)]
struct BRRDecoder {
    srcn: u8,

    entry: BRREntry,
    data: [i8; 16],
    addr: u16,
}

impl BRRDecoder {
    fn key_on(&mut self, ctx: &Context, ram: &[u8]) -> BRRControl {
        self._fetch_entry(ctx, ram, true);
        self._fetch_block(ram)
    }

    // FIXME?: Use current entry's loop address?
    fn fetch_loop(&mut self, ctx: &Context, ram: &[u8]) -> BRRControl {
        self._fetch_entry(ctx, ram, false);
        self._fetch_block(ram)
    }

    fn fetch_next(&mut self, ram: &[u8]) -> BRRControl {
        self._fetch_block(ram)
    }

    fn _fetch_entry(&mut self, ctx: &Context, ram: &[u8], is_start: bool) {
        let addr = (ctx.dir as usize * 0x100) + (self.srcn as usize * 4);
        self.entry.start_addr = u16::from_le_bytes(ram[addr..(addr + 2)].try_into().unwrap());
        let addr = (addr as u16).wrapping_add(2) as usize;
        self.entry.restart_loop_addr = u16::from_le_bytes(ram[addr..(addr + 2)].try_into().unwrap());
        self.addr = if is_start { self.entry.start_addr } else { self.entry.restart_loop_addr };
    }

    fn _fetch_block(&mut self, ram: &[u8]) -> BRRControl {
        let res: BRRControl = self._fetch_byte(ram).into();

        for i in 0..8 {
            let data = self._fetch_byte(ram);
            let d0 = ((((data >> 4) & 0xF) << 4) as i8) >> 4;
            let d1 = ((((data >> 0) & 0xF) << 4) as i8) >> 4;
            self.data[(2 * i + 0) as usize] = d0;
            self.data[(2 * i + 1) as usize] = d1;
        }

        res
    }

    fn _fetch_byte(&mut self, ram: &[u8]) -> u8 {
        let res = ram[self.addr as usize];
        self.addr = self.addr.wrapping_add(1);
        res
    }
}

#[derive(Default)]
enum KeyState {
    #[default]
    Nothing,
    KeyOn,
    KeyOff,
}

#[derive(Default)]
struct Voice {
    out: i16,
    pitch: u16,

    endx: bool,

    pitch_counter: u16,
    samples: [i16; 4], // 0 is the latest value

    noise: Noise,
    envelope: Envelope,

    brr_ctrl: BRRControl,
    brr_decoder: BRRDecoder,

    key_state: KeyState,
}

// https://problemkaputt.de/fullsnes.htm#snesapublockdiagram
//
// DSP Voice Block Diagram (n=voice, 0..7)
//
//                     OUTx(n-1)   PITCHn
//                      PMON         |    ADSRn/ENV
//                        |____MUL___|     |
//   DIR*256                    |          +-------------------------------> ENVxn
//   +SRCn*4                    |          |             .-----------------> OUTxn
//    _____      _______      __V___       |             |        _____
//   |     |    |  BRR  |    | BRR  |      |    _____    |       |VOLnL|
//   | RAM |--->|Decoder|--->| Time |---o  '-->|     |   |   .-->| MUL |---> Ln
//   |_____|    |_______|    |______|    \     | MUL |   |   |   |_____|
//                            ______ NONn o--->|     |---+---+    _____
//                           | Noise|          |_____|       |   |VOLnR|
//                           | Time |---o                    '-->| MUL |---> Rn
//                           |______|                            |_____|
//                              ^
//                              |
//                             FLG
//
impl Voice {
    fn reset(&mut self) {
        self.noise.reset();
    }

    fn tick(&mut self, ctx: &Context, raw: &[u8], pre: Option<i16>, enable_noise: bool) {
        //// Consume key_state
        match self.key_state {
            KeyState::KeyOn => {
                // TODO? Append 5 'empty' sample
                self.envelope.value = 0;
                self.envelope.mode = ADSR::Attack;
                self.endx = false;
                self.brr_ctrl = self.brr_decoder.key_on(ctx, raw);
            }
            KeyState::KeyOff => {
                self.envelope.mode = ADSR::Release;
            }
            KeyState::Nothing => (),
        };
        self.key_state = KeyState::Nothing;

        //// BRR Time
        self.brr_time(ctx, raw, pre);

        //// MUL
        self.noise.tick(ctx);
        self.envelope.tick();
        let out = if enable_noise {
            self.noise.value
        } else {
            let i = ((self.pitch_counter >> 4) & 0xFF) as usize;
            let add0 = (GAUSS[0x0FF - i] as i32) * (self.samples[3] as i32);
            let add1 = (GAUSS[0x1FF - i] as i32) * (self.samples[2] as i32);
            let add2 = (GAUSS[0x100 + i] as i32) * (self.samples[1] as i32);
            let add3 = (GAUSS[0x000 + i] as i32) * (self.samples[0] as i32);
            let out = 0 as i16;
            let out = out.wrapping_add((add0 >> 10) as i16);
            let out = out.wrapping_add((add1 >> 10) as i16);
            let out = out.wrapping_add((add2 >> 10) as i16);
            let out = out.saturating_add((add3 >> 10) as i16);
            let out = out >> 1;
            out
        };
        let out = (out as i32) * (self.envelope.value as i32); // 15bit * 11bit -> 26 bit
        self.out = (out >> 11).try_into().unwrap();
    }

    fn brr_time(&mut self, ctx: &Context, raw: &[u8], pre: Option<i16>) {
        let mut cur = (self.pitch_counter >> 12) & 0xF;
        let overflow = {
            let mut step = self.pitch;
            if let Some(factor) = pre {
                let factor = factor >> 4;
                let factor = u16::try_from(factor + 0x400).unwrap(); // TODO
                step = (((step as u32) * (factor as u32)) >> 10) as u16;
            }
            step = step.min(0x3FFF); // TODO?
            let (counter, overflow) = self.pitch_counter.overflowing_add(step);
            self.pitch_counter = counter;
            overflow
        };
        if overflow {
            for i in cur..16 {
                self.append_new_sample(i as usize);
            }

            if !self.brr_ctrl.end() {
                self.brr_ctrl = self.brr_decoder.fetch_next(raw);
            } else {
                if !self.brr_ctrl.do_loop() {
                    self.envelope.value = 0x0000;
                    self.envelope.mode = ADSR::Release;
                }
                self.brr_ctrl = self.brr_decoder.fetch_loop(ctx, raw);
                self.endx = true;
            }
            cur = 0;
        }

        let next = (self.pitch_counter >> 12) & 0xF;
        for i in cur..next {
            self.append_new_sample(i as usize);
        }
    }

    fn append_new_sample(&mut self, i: usize) {
        let sample = {
            let shift = self.brr_ctrl.left_shift();
            let (nibble, shift) = if shift <= 12 { (self.brr_decoder.data[i], shift) } else { (self.brr_decoder.data[i] >> 3, 12) };
            let nibble = nibble as i16;
            (nibble << shift) >> 1
        };

        let old = self.samples[0];
        let older = self.samples[1];
        let sample = self.brr_ctrl.filter(sample, old, older);
        self.samples[3] = self.samples[2];
        self.samples[2] = self.samples[1];
        self.samples[1] = self.samples[0];
        self.samples[0] = sample;
    }

    fn outx(&self) -> u8 {
        (self.out >> 7) as u8
    }
}

#[bitfield]
#[repr(u8)]
#[derive(Clone, Copy, Debug)]
struct FLG {
    noise_freq: B5,
    echo_buffer_write_disable: bool,
    mute_amp: bool,
    soft_reset: bool,
}

impl Default for FLG {
    fn default() -> Self {
        0xE0.into()
    }
}

#[bitfield]
#[repr(u8)]
#[derive(Default, Clone, Copy)]
struct EDL {
    echo_buffer_size_bin: B4,
    #[skip]
    __: B4,
}

impl EDL {
    // bytes
    fn echo_buffer_size(&self) -> usize {
        let res = self.echo_buffer_size_bin() as usize;
        if res == 0 {
            4
        } else {
            res << 11 // 2K-byte steps
        }
    }
}

#[derive(Default)]
struct Mixer {
    mvol: [i8; 2],
    v_vol: [[i8; 8]; 2],

    // Echo
    evol: [i8; 2],
    efb: i8,
    eon: u8,
    esa: u8,
    edl: EDL,
    fir: [i8; 8],

    // Internal
    fir_buf: [[i16; 8]; 2],
    ram_index: usize,
    fir_index: usize,
    remain: usize,
}

// https://problemkaputt.de/fullsnes.htm#snesapublockdiagram
//
// DSP Mixer/Reverb Block Diagram (c=channel, L/R)
//
//           ________                        _____                   _____
//   c0 --->| ADD    |                      |MVOLc| Master Volume   |     |
//   c1 --->| Output |--------------------->| MUL |---------------->|     |
//   c2 --->| Mixing |                      |_____|                 |     |
//   c3 --->|        |                       _____                  | ADD |--> c
//   c4 --->|        |                      |EVOLc| Echo Volume     |     |
//   c5 --->|        |   Feedback   .------>| MUL |---------------->|     |
//   c6 --->|        |   Volume     |       |_____|                 |_____|
//   c7 --->|________|    _____     |                      _________________
//                       | EFB |    |                     |                 |
//      EON  ________    | MUL |<---+---------------------|   Add FIR Sum   |
//   c0 -:->|        |   |_____|                          |_________________|
//   c1 -:->|        |      |                              _|_|_|_|_|_|_|_|_
//   c2 -:->|        |      |                             |   MUL FIR7..0   |
//   c3 -:->|        |      |         ESA=Addr, EDL=Len   |_7_6_5_4_3_2_1_0_|
//   c4 -:->| ADD    |    __V__  FLG   _______________     _|_|_|_|_|_|_|_|_
//   c5 -:->| Echo   |   |     | ECEN | Echo Buffer c |   | FIR Buffer c    |
//   c6 -:->| Mixing |-->| ADD |--:-->|   RAM         |-->| (Hardware regs) |
//   c7 -:->|________|   |_____|      |_______________|   |_________________|
//                                    Newest --> Oldest    Newest --> Oldest
impl Mixer {
    // TODO: unnecessary?
    fn reset(&mut self) {
        self.remain = 4;
    }

    fn mix(&self, c: [i16; 8]) -> ([i16; 2], [i16; 2]) {
        let mut output = [0i16, 0i16];
        let mut echo = [0i16, 0i16];
        for lr in 0..2 {
            for i in 0..8 {
                let add = (c[i] as i32) * (self.v_vol[lr][i] as i32);
                let add: i16 = (add >> 6).try_into().unwrap();
                output[lr] = output[lr].saturating_add(add);
                if ((self.eon >> i) & 1) == 1 {
                    echo[lr] = echo[lr].saturating_add(add);
                }
            }
        }
        (output, echo)
    }

    fn fir_filter(&mut self, ram: &[u8], addr: usize) -> [i16; 2] {
        let mut res = [0i16; 2];
        for lr in 0..2 {
            let addr = addr + lr * 2;
            let buf = &mut self.fir_buf[lr];
            buf[self.fir_index & 7] = i16::from_le_bytes([ram[addr], ram[addr + 1]]) >> 1;
            let mut sum = 0i16;
            for x in 0..8 {
                let add = buf[self.fir_index.wrapping_add(x + 1) & 7] as i32;
                let add = add * self.fir[x] as i32;
                let add: i16 = (add >> 6).try_into().unwrap();
                sum = if x != 7 { sum.wrapping_add(add) } else { sum.saturating_add(add) };
            }
            res[lr] = sum;
        }
        res
    }

    fn feedback(&mut self, ram: &mut [u8], addr: usize, mixed: [i16; 2], fir: [i16; 2]) {
        for lr in 0..2 {
            let addr = addr + lr * 2;
            let echo_input = (fir[lr] as i32) * (self.efb as i32);
            let echo_input: i16 = (echo_input >> 7).try_into().unwrap();
            let echo_input = mixed[lr].saturating_add(echo_input);
            let echo_input = if (echo_input & 1) == 1 { echo_input ^ 1 } else { echo_input };
            let echo_input = i16::to_le_bytes(echo_input);
            ram[addr + 0] = echo_input[0];
            ram[addr + 1] = echo_input[1];
        }
    }

    fn exec(&mut self, c: [i16; 8], ram: &mut [u8], ctx: &Context) -> [i16; 2] {
        let (output_mixed, echo_mixed) = self.mix(c);
        let addr = (self.esa as usize) * 0x100 + self.ram_index * 4;
        let addr = addr & 0xFFFF;
        let fir_out = self.fir_filter(ram, addr);
        let mut audio_output = [0i16, 0i16];
        if !ctx.flg.mute_amp() {
            for lr in 0..2 {
                let master = (output_mixed[lr] as i32) * (self.mvol[lr] as i32);
                let master: i16 = (master >> 7).try_into().unwrap();
                let echo = (fir_out[lr] as i32) * (self.evol[lr] as i32);
                let echo: i16 = (echo >> 7).try_into().unwrap();
                audio_output[lr] = master.saturating_add(echo);
            }
        }

        if !ctx.flg.echo_buffer_write_disable() {
            self.feedback(ram, addr, echo_mixed, fir_out);
        }

        self.fir_index = self.fir_index.wrapping_add(1);
        self.ram_index = self.ram_index + 1;
        self.remain = self.remain.saturating_sub(4);
        if self.remain == 0 {
            self.remain = self.edl.echo_buffer_size();
            self.ram_index = 0;
        }

        audio_output
    }
}

#[derive(Default)]
pub struct DSP {
    voices: [Voice; 8],
    mixer: Mixer,

    kon: u8,
    kof: u8,
    pmon: u8,
    flg: FLG,
    non: u8,
    dir: u8,

    unused_a: [u8; 16],
    unused_b: [u8; 16],
    unused_1d: u8,
    unused_e: [u8; 16],
}

impl DSP {
    pub fn reset(&mut self) {
        self.mixer.reset();
        for i in 0..8 {
            self.voices[i].reset();
        }
    }

    fn make_ctx(&self) -> Context {
        Context {
            dir: self.dir,
            non: self.non,
            flg: self.flg,
        }
    }

    pub fn tick(&mut self, ram: &mut [u8]) -> [i16; 2] {
        let ctx = self.make_ctx();
        let mut c = [0i16; 8];
        for i in 0..8 {
            let pre = if 0 < i && ((self.pmon >> i) & 1) == 1 { Some(self.voices[i - 1].out) } else { None };
            let enable_noise = ctx.enable_noise(i);
            self.voices[i].tick(&ctx, ram, pre, enable_noise);
            c[i] = self.voices[i].out;
        }
        self.mixer.exec(c, ram, &ctx)
    }

    pub fn read(&self, addr: u8) -> u8 {
        // assert((addr & 0x80) == 0)
        let lo = (addr >> 0) & 0xF;
        let hi = (addr >> 4) & 0xF;
        let x = hi as usize;
        match lo {
            0x0 | 0x1 => self.mixer.v_vol[lo as usize][x] as u8,
            0x2 => ((self.voices[x].pitch >> 0) & 0xFF) as u8,
            0x3 => ((self.voices[x].pitch >> 8) & 0x3F) as u8,
            0x4 => self.voices[x].brr_decoder.srcn,
            0x5 | 0x6 => self.voices[x].envelope.adsr.bytes[(lo - 0x5) as usize],
            0x7 => self.voices[x].envelope.gain.into(),
            0x8 => self.voices[x].envelope.envx(),
            0x9 => self.voices[x].outx(),
            0xA => self.unused_a[x],
            0xB => self.unused_b[x],
            0xC => match hi {
                0x0 | 0x1 => self.mixer.mvol[x] as u8,
                0x2 | 0x3 => self.mixer.evol[x - 2] as u8,
                0x4 => self.kon,
                0x5 => self.kof,
                0x6 => self.flg.clone().into(),
                0x7 => {
                    let mut res: u8 = 0;
                    for i in (0..8).rev() {
                        res = (res << 1) | (self.voices[i].endx as u8);
                    }
                    res
                }
                _ => unreachable!(),
            },
            0xD => match hi {
                0x0 => self.mixer.efb as u8,
                0x1 => self.unused_1d,
                0x2 => self.pmon,
                0x3 => self.non,
                0x4 => self.mixer.eon,
                0x5 => self.dir,
                0x6 => self.mixer.esa,
                0x7 => self.mixer.edl.into(),
                _ => unreachable!(),
            },
            0xE => self.unused_e[x],
            0xF => self.mixer.fir[x] as u8,
            _ => unreachable!(),
        }
    }

    pub fn write(&mut self, addr: u8, v: u8) {
        // assert((addr & 0x80) == 0)
        let lo = (addr >> 0) & 0xF;
        let hi = (addr >> 4) & 0xF;
        let x = hi as usize;
        match lo {
            0x0 | 0x1 => self.mixer.v_vol[lo as usize][x] = v as i8,
            0x2 => self.voices[x].pitch = (self.voices[x].pitch & 0xFF00) | (v as u16),
            0x3 => self.voices[x].pitch = (self.voices[x].pitch & 0x00FF) | (((v & 0x3F) as u16) << 8),
            0x4 => self.voices[x].brr_decoder.srcn = v,
            0x5 | 0x6 => self.voices[x].envelope.adsr.bytes[(lo - 0x5) as usize] = v,
            0x7 => self.voices[x].envelope.gain = v.into(),
            0x8 | 0x9 => (),
            0xA => self.unused_a[x] = v,
            0xB => self.unused_b[x] = v,
            0xC => match hi {
                0x0 | 0x1 => self.mixer.mvol[x] = v as i8,
                0x2 | 0x3 => self.mixer.evol[x - 2] = v as i8,
                0x4 => {
                    self.kon = v;
                    for i in 0..8 {
                        if ((v >> i) & 1) == 1 {
                            self.voices[i].key_state = KeyState::KeyOn;
                        }
                    }
                }
                0x5 => {
                    self.kof = v;
                    for i in 0..8 {
                        if ((v >> i) & 1) == 1 {
                            self.voices[i].key_state = KeyState::KeyOff;
                        }
                    }
                }
                0x6 => {
                    self.flg = v.into();
                    if self.flg.soft_reset() {
                        for i in 0..8 {
                            self.voices[i].key_state = KeyState::KeyOff;
                            self.voices[i].envelope.value = 0;
                        }
                    }
                }
                0x7 => {
                    // TODO?: Also clear them when reset
                    for i in 0..8 {
                        self.voices[i].endx = false;
                    }
                }
                _ => unreachable!(),
            },
            0xD => match hi {
                0x0 => self.mixer.efb = v as i8,
                0x1 => self.unused_1d = v,
                0x2 => self.pmon = v,
                0x3 => self.non = v,
                0x4 => self.mixer.eon = v,
                0x5 => self.dir = v,
                0x6 => self.mixer.esa = v,
                0x7 => self.mixer.edl = v.into(),
                _ => unreachable!(),
            },
            0xE => self.unused_e[x] = v,
            0xF => self.mixer.fir[x] = v as i8,
            _ => unreachable!(),
        }
    }
}

const PERIOD_TABLE: [u16; 32] = [
    0, 2048, 1536, 1280, 1024, 768, 640, 512, 384, 320, 256, 192, 160, 128, 96, 80, 64, 48, 40, 32, 24, 20, 16, 12, 10, 8, 6, 5, 4, 3, 2, 1,
];

const PERIOD_OFFSET: [u16; 32] = [
    0, 0, 1040, 536, 0, 1040, 536, 0, 1040, 536, 0, 1040, 536, 0, 1040, 536, 0, 1040, 536, 0, 1040, 536, 0, 1040, 536, 0, 1040, 536, 0, 1040, 536, 0,
];

const GAUSS: [u16; 0x200] = [
    0x000, 0x000, 0x000, 0x000, 0x000, 0x000, 0x000, 0x000, 0x000, 0x000, 0x000, 0x000, 0x000, 0x000, 0x000, 0x000, 0x001, 0x001, 0x001, 0x001, 0x001, 0x001, 0x001, 0x001, 0x001, 0x001, 0x001, 0x002,
    0x002, 0x002, 0x002, 0x002, 0x002, 0x002, 0x003, 0x003, 0x003, 0x003, 0x003, 0x004, 0x004, 0x004, 0x004, 0x004, 0x005, 0x005, 0x005, 0x005, 0x006, 0x006, 0x006, 0x006, 0x007, 0x007, 0x007, 0x008,
    0x008, 0x008, 0x009, 0x009, 0x009, 0x00A, 0x00A, 0x00A, 0x00B, 0x00B, 0x00B, 0x00C, 0x00C, 0x00D, 0x00D, 0x00E, 0x00E, 0x00F, 0x00F, 0x00F, 0x010, 0x010, 0x011, 0x011, 0x012, 0x013, 0x013, 0x014,
    0x014, 0x015, 0x015, 0x016, 0x017, 0x017, 0x018, 0x018, 0x019, 0x01A, 0x01B, 0x01B, 0x01C, 0x01D, 0x01D, 0x01E, 0x01F, 0x020, 0x020, 0x021, 0x022, 0x023, 0x024, 0x024, 0x025, 0x026, 0x027, 0x028,
    0x029, 0x02A, 0x02B, 0x02C, 0x02D, 0x02E, 0x02F, 0x030, 0x031, 0x032, 0x033, 0x034, 0x035, 0x036, 0x037, 0x038, 0x03A, 0x03B, 0x03C, 0x03D, 0x03E, 0x040, 0x041, 0x042, 0x043, 0x045, 0x046, 0x047,
    0x049, 0x04A, 0x04C, 0x04D, 0x04E, 0x050, 0x051, 0x053, 0x054, 0x056, 0x057, 0x059, 0x05A, 0x05C, 0x05E, 0x05F, 0x061, 0x063, 0x064, 0x066, 0x068, 0x06A, 0x06B, 0x06D, 0x06F, 0x071, 0x073, 0x075,
    0x076, 0x078, 0x07A, 0x07C, 0x07E, 0x080, 0x082, 0x084, 0x086, 0x089, 0x08B, 0x08D, 0x08F, 0x091, 0x093, 0x096, 0x098, 0x09A, 0x09C, 0x09F, 0x0A1, 0x0A3, 0x0A6, 0x0A8, 0x0AB, 0x0AD, 0x0AF, 0x0B2,
    0x0B4, 0x0B7, 0x0BA, 0x0BC, 0x0BF, 0x0C1, 0x0C4, 0x0C7, 0x0C9, 0x0CC, 0x0CF, 0x0D2, 0x0D4, 0x0D7, 0x0DA, 0x0DD, 0x0E0, 0x0E3, 0x0E6, 0x0E9, 0x0EC, 0x0EF, 0x0F2, 0x0F5, 0x0F8, 0x0FB, 0x0FE, 0x101,
    0x104, 0x107, 0x10B, 0x10E, 0x111, 0x114, 0x118, 0x11B, 0x11E, 0x122, 0x125, 0x129, 0x12C, 0x130, 0x133, 0x137, 0x13A, 0x13E, 0x141, 0x145, 0x148, 0x14C, 0x150, 0x153, 0x157, 0x15B, 0x15F, 0x162,
    0x166, 0x16A, 0x16E, 0x172, 0x176, 0x17A, 0x17D, 0x181, 0x185, 0x189, 0x18D, 0x191, 0x195, 0x19A, 0x19E, 0x1A2, 0x1A6, 0x1AA, 0x1AE, 0x1B2, 0x1B7, 0x1BB, 0x1BF, 0x1C3, 0x1C8, 0x1CC, 0x1D0, 0x1D5,
    0x1D9, 0x1DD, 0x1E2, 0x1E6, 0x1EB, 0x1EF, 0x1F3, 0x1F8, 0x1FC, 0x201, 0x205, 0x20A, 0x20F, 0x213, 0x218, 0x21C, 0x221, 0x226, 0x22A, 0x22F, 0x233, 0x238, 0x23D, 0x241, 0x246, 0x24B, 0x250, 0x254,
    0x259, 0x25E, 0x263, 0x267, 0x26C, 0x271, 0x276, 0x27B, 0x280, 0x284, 0x289, 0x28E, 0x293, 0x298, 0x29D, 0x2A2, 0x2A6, 0x2AB, 0x2B0, 0x2B5, 0x2BA, 0x2BF, 0x2C4, 0x2C9, 0x2CE, 0x2D3, 0x2D8, 0x2DC,
    0x2E1, 0x2E6, 0x2EB, 0x2F0, 0x2F5, 0x2FA, 0x2FF, 0x304, 0x309, 0x30E, 0x313, 0x318, 0x31D, 0x322, 0x326, 0x32B, 0x330, 0x335, 0x33A, 0x33F, 0x344, 0x349, 0x34E, 0x353, 0x357, 0x35C, 0x361, 0x366,
    0x36B, 0x370, 0x374, 0x379, 0x37E, 0x383, 0x388, 0x38C, 0x391, 0x396, 0x39B, 0x39F, 0x3A4, 0x3A9, 0x3AD, 0x3B2, 0x3B7, 0x3BB, 0x3C0, 0x3C5, 0x3C9, 0x3CE, 0x3D2, 0x3D7, 0x3DC, 0x3E0, 0x3E5, 0x3E9,
    0x3ED, 0x3F2, 0x3F6, 0x3FB, 0x3FF, 0x403, 0x408, 0x40C, 0x410, 0x415, 0x419, 0x41D, 0x421, 0x425, 0x42A, 0x42E, 0x432, 0x436, 0x43A, 0x43E, 0x442, 0x446, 0x44A, 0x44E, 0x452, 0x455, 0x459, 0x45D,
    0x461, 0x465, 0x468, 0x46C, 0x470, 0x473, 0x477, 0x47A, 0x47E, 0x481, 0x485, 0x488, 0x48C, 0x48F, 0x492, 0x496, 0x499, 0x49C, 0x49F, 0x4A2, 0x4A6, 0x4A9, 0x4AC, 0x4AF, 0x4B2, 0x4B5, 0x4B7, 0x4BA,
    0x4BD, 0x4C0, 0x4C3, 0x4C5, 0x4C8, 0x4CB, 0x4CD, 0x4D0, 0x4D2, 0x4D5, 0x4D7, 0x4D9, 0x4DC, 0x4DE, 0x4E0, 0x4E3, 0x4E5, 0x4E7, 0x4E9, 0x4EB, 0x4ED, 0x4EF, 0x4F1, 0x4F3, 0x4F5, 0x4F6, 0x4F8, 0x4FA,
    0x4FB, 0x4FD, 0x4FF, 0x500, 0x502, 0x503, 0x504, 0x506, 0x507, 0x508, 0x50A, 0x50B, 0x50C, 0x50D, 0x50E, 0x50F, 0x510, 0x511, 0x511, 0x512, 0x513, 0x514, 0x514, 0x515, 0x516, 0x516, 0x517, 0x517,
    0x517, 0x518, 0x518, 0x518, 0x518, 0x518, 0x519, 0x519,
];
