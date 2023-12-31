#![allow(dead_code)]

use crate::apu::decode::decode;
use modular_bitfield::prelude::*;

#[derive(Default)]
pub struct SPC700 {
    regs: Registers,
    halt: bool,
}

#[bitfield]
#[repr(u8)]
#[derive(Clone, Copy, Default, Debug)]
struct ProgramStatusWord {
    c: bool, // Carry Flag
    z: bool, // Zero Flag
    i: bool, // Interrupt Enable (no function in SNES APU)
    h: bool, // Half-carry
    b: bool, // Break Flag
    p: bool, // Zero Page Location
    v: bool, // Overflow Flag
    n: bool, // Sign Flag
}

// TODO: sp's default value
#[derive(Debug)]
struct Registers {
    a: u8,
    x: u8,
    y: u8,
    pc: u16,
    sp: u8,
    psw: ProgramStatusWord,
}

impl Default for Registers {
    fn default() -> Self {
        Self {
            a: 0,
            x: 0,
            y: 0,
            pc: 0,
            sp: 0xFF,
            psw: ProgramStatusWord::default(),
        }
    }
}

impl Registers {
    fn set_p_nz(&mut self, v: u8) {
        self.psw.set_n(((v >> 7) & 1) == 1);
        self.psw.set_z(v == 0);
    }

    fn get_ya(&self) -> u16 {
        ((self.y as u16) << 8) | (self.a as u16)
    }

    fn set_ya(&mut self, v: u16) {
        self.a = ((v >> 0) & 0xFF) as u8;
        self.y = ((v >> 8) & 0xFF) as u8;
    }
}

#[derive(Debug)]
enum AddrMode {
    DP(u8),
    IndexedDP { addr: u8, i: u8 },
    ABS(u16),
    IndexedABS { addr: u16, i: u8 },
    Index(u8),
    IndexInc(u8),
    IndirectIndexed { addr: u8, i: u8 }, // [addr] + i
    IndexedIndirect { addr: u8, i: u8 }, // [addr + i]
}

#[derive(Debug, Clone, Copy)]
enum Operand {
    Imm(u8),
    Addr(u16),
    A,
    X,
    Y,
    SP,
    PSW,
}

pub trait Resources {
    fn add_cycles(&mut self, v: u8);
    fn read(&mut self, addr: u16) -> u8;
    fn write(&mut self, addr: u16, v: u8);
}

impl SPC700 {
    pub fn reset(&mut self, rsrc: &mut impl Resources) {
        self.regs.psw.set_i(false);
        self.regs.psw.set_b(false);
        self.regs.psw.set_p(false);
        self.regs.pc = self.read16(0xFFFE, rsrc);
        self.halt = false;
    }

    // https://problemkrsrctt.de/fullsnes.htm#snesapuspc700cpujumpcontrolcommands
    //
    // Note: The SNES APU doesn't have any interrupt sources,
    // so SLEEP/STOP will hang the CPU forever, and DI/EI have no effect (other than changing to I-flag in PSW).
    pub fn tick(&mut self, rsrc: &mut impl Resources) {
        if self.halt {
            rsrc.add_cycles(1);
            return;
        }

        self.exec_instr(rsrc);
    }

    fn read8(&self, addr: u16, rsrc: &mut impl Resources) -> u8 {
        rsrc.read(addr)
    }

    // For 16-bit operations
    fn read16(&self, addr: u16, rsrc: &mut impl Resources) -> u16 {
        let lo = self.read8(addr, rsrc) as u16;
        let hi = self.read8((addr & 0xFF00) | (addr.wrapping_add(1) & 0x00FF), rsrc) as u16;
        (hi << 8) | lo
    }

    fn write8(&self, addr: u16, data: u8, rsrc: &mut impl Resources) {
        rsrc.write(addr, data);
    }

    fn write16(&self, addr: u16, data: u16, rsrc: &mut impl Resources) {
        let lo = ((data >> 0) & 0xFF) as u8;
        let hi = ((data >> 8) & 0xFF) as u8;
        self.write8(addr, lo, rsrc);
        self.write8(addr.wrapping_add(1), hi, rsrc); // TODO? wrap in 8-bit?
    }

    fn fetch(&mut self, rsrc: &mut impl Resources) -> u8 {
        let res = self.read8(self.regs.pc, rsrc);
        self.regs.pc = self.regs.pc.wrapping_add(1);
        res
    }

    fn push8(&mut self, v: u8, rsrc: &mut impl Resources) {
        self.write8(0x100 + (self.regs.sp as u16), v, rsrc);
        self.regs.sp = self.regs.sp.wrapping_sub(1);
    }

    fn push16(&mut self, v: u16, rsrc: &mut impl Resources) {
        let lo = ((v >> 0) & 0xFF) as u8;
        let hi = ((v >> 8) & 0xFF) as u8;
        self.push8(hi, rsrc);
        self.push8(lo, rsrc);
    }

    fn pull8(&mut self, rsrc: &mut impl Resources) -> u8 {
        self.regs.sp = self.regs.sp.wrapping_add(1);
        self.read8(0x100 + (self.regs.sp as u16), rsrc)
    }

    fn pull16(&mut self, rsrc: &mut impl Resources) -> u16 {
        let lo = self.pull8(rsrc) as u16;
        let hi = self.pull8(rsrc) as u16;
        (hi << 8) | lo
    }

    fn calc_effaddr(&mut self, addr: AddrMode, rsrc: &mut impl Resources) -> u16 {
        let page = if self.regs.psw.p() { 0x100 } else { 0 };
        match addr {
            // [aa]
            AddrMode::DP(addr) => addr as u16 + page,

            // [aa+X] / [aa+Y]
            AddrMode::IndexedDP { addr, i } => {
                let addr = addr.wrapping_add(i) as u16 + page;
                rsrc.add_cycles(1);
                addr
            }

            // [aaaa]
            AddrMode::ABS(addr) => addr,

            // [aaaa+X] / [aaaa+Y]
            AddrMode::IndexedABS { addr, i } => {
                let addr = addr.wrapping_add(i as u16);
                rsrc.add_cycles(1);
                addr
            }

            // [X] / [Y]
            AddrMode::Index(addr) => {
                rsrc.add_cycles(1);
                page + addr as u16
            }
            AddrMode::IndexInc(addr) => {
                rsrc.add_cycles(1);
                page + addr as u16
            }

            // [[aa]+Y]
            AddrMode::IndirectIndexed { addr, i } => {
                let lo = self.read8(page + addr as u16, rsrc) as u16;
                let hi = self.read8(page + addr.wrapping_add(1) as u16, rsrc) as u16; // FIXME? Wrapping??
                let addr = (hi << 8) | lo;
                let addr = addr.wrapping_add(i as u16);
                rsrc.add_cycles(1);
                addr
            }

            // [[aa+X]]
            AddrMode::IndexedIndirect { addr, i } => {
                let addr = addr.wrapping_add(i) as u16;
                rsrc.add_cycles(1);
                let lo = self.read8(page + addr as u16, rsrc) as u16;
                let hi = self.read8(page + addr.wrapping_add(1) as u16, rsrc) as u16; // FIXME?: Wrapping??
                (hi << 8) | lo
            }
        }
    }

    fn read_operand(&self, op: Operand, rsrc: &mut impl Resources) -> u8 {
        match op {
            Operand::A => self.regs.a,
            Operand::X => self.regs.x,
            Operand::Y => self.regs.y,
            Operand::SP => self.regs.sp,
            Operand::PSW => u8::from(self.regs.psw),
            Operand::Imm(v) => v,
            Operand::Addr(addr) => self.read8(addr, rsrc),
        }
    }

    fn write_operand(&mut self, op: Operand, v: u8, rsrc: &mut impl Resources) {
        match op {
            Operand::A => self.regs.a = v,
            Operand::X => self.regs.x = v,
            Operand::Y => self.regs.y = v,
            Operand::SP => self.regs.sp = v,
            Operand::PSW => self.regs.psw = v.into(),
            Operand::Addr(addr) => self.write8(addr, v, rsrc),
            Operand::Imm(_) => panic!("write to imm"),
        }
    }

    fn exec_instr(&mut self, rsrc: &mut impl Resources) {
        macro_rules! addr {
            (X ind) => { AddrMode::Index(self.regs.x) };
            (Y ind) => { AddrMode::Index(self.regs.y) };
            (X ind inc) => {{
                let addr = AddrMode::IndexInc(self.regs.x);
                self.regs.x = self.regs.x.wrapping_add(1);
                addr
            }};
            (dp) => {{
                let v = self.fetch(rsrc);
                AddrMode::DP(v)
            }};
            (dp X p) => {{
                let addr = self.fetch(rsrc);
                let i = self.regs.x;
                AddrMode::IndexedDP { addr, i }
            }};
            (dp Y p) => {{
                let addr = self.fetch(rsrc);
                let i = self.regs.y;
                AddrMode::IndexedDP { addr, i }
            }};
            (abs) => {{
                let lo = self.fetch(rsrc) as u16;
                let hi = self.fetch(rsrc) as u16;
                AddrMode::ABS((hi << 8) | lo)
            }};
            (abs X p) => {{
                let lo = self.fetch(rsrc) as u16;
                let hi = self.fetch(rsrc) as u16;
                let addr = (hi << 8) | lo;
                let i = self.regs.x;
                AddrMode::IndexedABS { addr, i }
            }};
            (abs Y p) => {{
                let lo = self.fetch(rsrc) as u16;
                let hi = self.fetch(rsrc) as u16;
                let addr = (hi << 8) | lo;
                let i = self.regs.y;
                AddrMode::IndexedABS { addr, i }
            }};
            (dp X p ind) => {{
                let addr = self.fetch(rsrc);
                let i = self.regs.x;
                AddrMode::IndexedIndirect { addr, i }
            }};
            (dp ind Y p) => {{
                let addr = self.fetch(rsrc);
                let i = self.regs.y;
                AddrMode::IndirectIndexed { addr, i }
            }};
        }

        macro_rules! addr_jmp {
            (abs X p ind) => {{
                let addr = addr!(abs X p);
                let addr = self.calc_effaddr(addr, rsrc);
                rsrc.add_cycles(1);
                self.read16(addr, rsrc)
            }};
            (abs) => {{
                let addr = addr!(abs);
                self.calc_effaddr(addr, rsrc)
            }};
        }

        macro_rules! operand {
            (A) => { Operand::A };
            (X) => { Operand::X };
            (Y) => { Operand::Y };
            (SP) => { Operand::SP };
            (PSW) => { Operand::PSW };
            (imm) => { Operand::Imm(self.fetch(rsrc)) };
            ($( $x: ident )*) => {{
                let addr = addr!($($x)*);
                let addr = self.calc_effaddr(addr, rsrc);
                Operand::Addr(addr)
            }};
        }

        macro_rules! read_w {
            (dp) => {{
                let addr = addr!(dp);
                let addr = self.calc_effaddr(addr, rsrc);
                self.read16(addr, rsrc)
            }};
        }

        // https://problemkrsrctt.de/fullsnes.htm#snesapuspc700cpualucommands
        //
        // For ADDW/SUBW, H is carry from bit11 to bit12.
        macro_rules! add_sub {
            (OP2, ADD, $v: expr, u8) => { $v.wrapping_add(self.regs.psw.c() as u32) };
            (OP2, SUB, $v: expr, u8) => { $v.wrapping_add(1).wrapping_sub(self.regs.psw.c() as u32) };
            (OP2, $_: ident, $v: expr, u16) => { $v };

            (ADD, $x: expr, $y: expr, $ty: ident) => {{
                let x = $x as u32;
                let y = $y as u32;
                let res = x + add_sub!(OP2, ADD, y, $ty);
                let mask = ((<$ty>::MAX as u32) + 1) / 2;
                let h_shift = <$ty>::BITS / 2 - 1;
                self.regs.psw.set_n((res & mask) != 0);
                self.regs.psw.set_v(((x ^ res) & (y ^ res) & mask) != 0);
                self.regs.psw.set_h(10 <= ((x >> h_shift) & 0xF) + ((y >> h_shift) & 0xF));
                self.regs.psw.set_z((res & <$ty>::MAX as u32) == 0);
                self.regs.psw.set_c((res & (1 << <$ty>::BITS)) != 0);
                res as $ty
            }};
            (SUB, $x: expr, $y: expr, $ty: ident) => {{
                let x = $x as u32;
                let y = add_sub!(OP2, SUB, $y as u32, $ty);
                let res = x.wrapping_sub(y);
                let mask = 1u32 << (<$ty>::BITS - 1);
                let h_shift = <$ty>::BITS / 2 - 1;
                self.regs.psw.set_n((res & mask) != 0);
                self.regs.psw.set_v(((x ^ res) & (!y ^ res) & mask) != 0);
                self.regs.psw.set_h(((y >> h_shift) & 0xF) <= ((x >> h_shift) & 0xF));
                self.regs.psw.set_z(res == 0);
                self.regs.psw.set_c(y <= x);
                res as $ty
            }};
        }

        macro_rules! logic {
            (INTERNAL, AND, $x: expr, $y: expr) => { $x & $y };
            (INTERNAL,  OR, $x: expr, $y: expr) => { $x | $y };
            (INTERNAL, EOR, $x: expr, $y: expr) => { $x ^ $y };

            ($op: ident, $( $x: ident )*, $( $y: ident )*) => {{
                let op2 = operand!($($y)*);
                let op1 = operand!($($x)*);
                let v1 = self.read_operand(op1, rsrc);
                let v2 = self.read_operand(op2, rsrc);
                let res = logic!(INTERNAL, $op, v1, v2);
                self.regs.set_p_nz(res);
                self.write_operand(op1, res, rsrc);
            }};
        }

        macro_rules! inc_dec {
            (INTERNAL, INC, $x: expr) => { $x.wrapping_add(1) };
            (INTERNAL, DEC, $x: expr) => { $x.wrapping_sub(1) };

            ($op: ident, $( $x: ident )*) => {{
                let op = operand!($($x)*);
                let x = self.read_operand(op, rsrc);
                let x = inc_dec!(INTERNAL, $op, x);
                self.regs.set_p_nz(x);
                self.write_operand(op, x, rsrc);
                rsrc.add_cycles(1);
            }};
        }

        macro_rules! shift_rot {
            (INTERNAL, ASL, $x: expr) => {{
                let msb = (($x >> 7) & 1) == 1;
                let x = $x << 1;
                self.regs.psw.set_c(msb);
                x as u8
            }};
            (INTERNAL, LSR, $x: expr) => {{
                let lsb = ($x & 1) == 1;
                let x = $x >> 1;
                self.regs.psw.set_c(lsb);
                x
            }};
            (INTERNAL, ROL, $x: expr) => {{
                let msb = (($x >> 7) & 1) == 1;
                let x = ($x << 1) | (self.regs.psw.c() as u8);
                self.regs.psw.set_c(msb);
                x as u8
            }};
            (INTERNAL, ROR, $x: expr) => {{
                let lsb = ($x & 1) == 1;
                let x = ($x >> 1) | ((self.regs.psw.c() as u8) << 7);
                self.regs.psw.set_c(lsb);
                x
            }};

            ($op: ident, $( $x: ident )*) => {{
                let op = operand!($($x)*);
                let x = self.read_operand(op, rsrc);
                let x = shift_rot!(INTERNAL, $op, x);
                self.regs.set_p_nz(x);
                self.write_operand(op, x, rsrc);
            }};
        }

        macro_rules! inc_dec_w {
            (INTERNAL, INCW, $x: expr) => { $x.wrapping_add(1) };
            (INTERNAL, DECW, $x: expr) => { $x.wrapping_sub(1) };

            // Only `dp` is correct
            ($op: ident, dp) => {{
                let addr = addr!(dp);
                let addr = self.calc_effaddr(addr, rsrc);
                let v = self.read16(addr, rsrc);
                let v = inc_dec_w!(INTERNAL, $op, v);
                self.regs.psw.set_z(v == 0);
                self.regs.psw.set_n(((v >> 15) & 1) == 1);
                self.write16(addr, v, rsrc);
            }};
        }

        macro_rules! branch {
            ($b: expr) => {{
                let offset = (self.fetch(rsrc) as i8) as u16;
                if $b {
                    rsrc.add_cycles(2);
                    self.regs.pc = self.regs.pc.wrapping_add(offset);
                }
            }};
        }

        macro_rules! comp {
            ($op1: expr, $op2: expr, $ty: ident) => {{
                let op1 = $op1 as $ty;
                let op2 = $op2 as $ty;
                let res = op1.wrapping_sub(op2);
                self.regs.psw.set_n((res & (1 << (<$ty>::BITS - 1))) != 0);
                self.regs.psw.set_z(res == 0);
                self.regs.psw.set_c(op2 <= op1);
            }};
        }

        macro_rules! call {
            ($addr: expr) => {{
                self.push16(self.regs.pc, rsrc);
                self.regs.pc = $addr;
            }};
        }

        macro_rules! set1_clr1 {
            (INTERNAL, SET1, $v: expr, $bit: expr) => { $v |  (1 << $bit) };
            (INTERNAL, CLR1, $v: expr, $bit: expr) => { $v & !(1 << $bit) };

            ($op: ident, dp, $bit: expr) => {{
                let addr = addr!(dp);
                let addr = self.calc_effaddr(addr, rsrc);
                let v = self.read8(addr, rsrc);
                let v = set1_clr1!(INTERNAL, $op, v, $bit);
                self.write8(addr, v, rsrc);
            }};
        }

        macro_rules! tset1_tclr1 {
            (INTERNAL, TSET1, $v: expr) => { $v |  self.regs.a };
            (INTERNAL, TCLR1, $v: expr) => { $v & !self.regs.a };

            ($op: ident, abs) => {{
                let addr = addr!(abs);
                let addr = self.calc_effaddr(addr, rsrc);
                let v = self.read8(addr, rsrc);
                comp!(self.regs.a, v, u8);  // To call set_p_nz
                let v = tset1_tclr1!(INTERNAL, $op, v);
                self.write8(addr, v, rsrc);
            }};
        }

        macro_rules! addr_aaab {
            (aaab) => {{
                let aa = self.fetch(rsrc) as u16;
                let ba = self.fetch(rsrc) as u16;
                let addr = AddrMode::ABS(((ba & 0x1F) << 8) | aa);
                let addr = self.calc_effaddr(addr, rsrc);
                let bit = ba >> 5;
                (addr, bit, false)
            }};
            (not aaab) => {{
                let (addr, bit, _) = addr_aaab!(aaab);
                (addr, bit, true)
            }};
        }

        macro_rules! mem_bit_op {
            (INTERNAL, $( $x: ident )*, bit) => {{
                let (addr, bit, not) = addr_aaab!($($x)*);
                let v = self.read8(addr, rsrc);
                let v = ((v >> bit) & 1) != 0;
                if not { !v } else { v }
            }};

            (INTERNAL, AND1, $v: expr) => { self.regs.psw.c() && $v };
            (INTERNAL,  OR1, $v: expr) => { self.regs.psw.c() || $v };
            (INTERNAL, EOR1, $v: expr) => { self.regs.psw.c() != $v };

            ($op: ident, $( $x: ident )*, bit) => {{
                let v = mem_bit_op!(INTERNAL, $($x)*, bit);
                let v = mem_bit_op!(INTERNAL, $op, v);
                self.regs.psw.set_c(v);
            }};
        }

        macro_rules! add_sub_wrap {
            ($op: ident, $( $x: ident )*, $( $y: ident )*) => {{
                let op2 = operand!($($y)*);
                let op1 = operand!($($x)*);
                let v1 = self.read_operand(op1.clone(), rsrc);
                let v2 = self.read_operand(op2.clone(), rsrc);
                let res = add_sub!($op, v1, v2, u8);
                self.write_operand(op1, res, rsrc);
            }};
        }

        macro_rules! binop {
            (MOV, $( $dst: ident )*, $( $src: ident )*) => {{
                let src = operand!($($src)*);
                let dst = operand!($($dst)*);
                let src = self.read_operand(src, rsrc);
                match dst {
                    Operand::A | Operand::X | Operand::Y => self.regs.set_p_nz(src),
                    _ => (),
                };
                self.write_operand(dst, src, rsrc);
            }};

            (ADC, $( $x: ident )*, $( $y: ident )*) => { add_sub_wrap!(ADD, $($x)*, $($y)*) };
            (SBC, $( $x: ident )*, $( $y: ident )*) => { add_sub_wrap!(SUB, $($x)*, $($y)*) };

            (CMP, $( $x: ident )*, $( $y: ident )*) => {{
                let y = operand!($($y)*);
                let y = self.read_operand(y, rsrc);
                let x = operand!($($x)*);
                let x = self.read_operand(x, rsrc);
                comp!(x, y, u8);
            }};

            (AND, $( $x: ident )*, $( $y: ident )*) => { logic!(AND, $($x)*, $($y)*) };
            (OR, $( $x: ident )*, $( $y: ident )*) => { logic!(OR, $($x)*, $($y)*) };
            (EOR, $( $x: ident )*, $( $y: ident )*) => { logic!(EOR, $($x)*, $($y)*) };
        }

        // TODO? It seems that there is one additional cycle if a first operand is dp
        macro_rules! exec {
            (MOV, $( $x: ident )*, $( $y: ident )*) => { binop!(MOV, $($x)*, $($y)*) };
            (ADC, $( $x: ident )*, $( $y: ident )*) => { binop!(ADC, $($x)*, $($y)*) };
            (SBC, $( $x: ident )*, $( $y: ident )*) => { binop!(SBC, $($x)*, $($y)*) };
            (CMP, $( $x: ident )*, $( $y: ident )*) => { binop!(CMP, $($x)*, $($y)*) };
            (AND, $( $x: ident )*, $( $y: ident )*) => { binop!(AND, $($x)*, $($y)*) };
            (EOR, $( $x: ident )*, $( $y: ident )*) => { binop!(EOR, $($x)*, $($y)*) };
            (OR, $( $x: ident )*, $( $y: ident )*) => { binop!(OR, $($x)*, $($y)*) };

            (INC, $( $x: ident )*) => { inc_dec!(INC, $($x)*) };
            (DEC, $( $x: ident )*) => { inc_dec!(DEC, $($x)*) };

            (ASL, $( $x: ident )*) => { shift_rot!(ASL, $($x)*) };
            (LSR, $( $x: ident )*) => { shift_rot!(LSR, $($x)*) };
            (ROL, $( $x: ident )*) => { shift_rot!(ROL, $($x)*) };
            (ROR, $( $x: ident )*) => { shift_rot!(ROR, $($x)*) };
            (XCN, A) => {{
                rsrc.add_cycles(4);
                let lo = (self.regs.a >> 0) & 0xF;
                let hi = (self.regs.a >> 4) & 0xF;
                self.regs.a = (lo << 4) | hi;
                self.regs.set_p_nz(self.regs.a);
            }};

            // YA=Word[dp] (4 cycles)
            (MOVW, YA, dp) => {{
                let v = read_w!(dp);
                self.regs.set_ya(v);
                self.regs.psw.set_n(((v >> 15) & 1) == 1);
                self.regs.psw.set_z(v == 0);
            }};
            // Word[dp]=YA (5 cycles)
            (MOVW, dp, YA) => {{
                let addr = addr!(dp);
                let addr = self.calc_effaddr(addr, rsrc);
                self.write16(addr, self.regs.get_ya(), rsrc);
                rsrc.add_cycles(1);
            }};

            (INCW, dp) => { inc_dec_w!(INCW, dp) };
            (DECW, dp) => { inc_dec_w!(DECW, dp) };

            (ADDW, YA, dp) => {{
                let x = self.regs.get_ya();
                let y = read_w!(dp);
                let res = add_sub!(ADD, x, y, u16);
                self.regs.set_ya(res);
            }};
            (SUBW, YA, dp) => {{
                let x = self.regs.get_ya();
                let y = read_w!(dp);
                let res = add_sub!(SUB, x, y, u16);
                self.regs.set_ya(res);
            }};
            (CMPW, YA, dp) => {{
                let x = self.regs.get_ya();
                let y = read_w!(dp);
                comp!(x, y, u16);
            }};
            (MUL, YA) => {{
                let a = self.regs.a as u16;
                let y = self.regs.y as u16;
                let res = a * y;
                rsrc.add_cycles(8);
                let a = ((res >> 0) & 0xFF) as u8;
                let y = ((res >> 8) & 0xFF) as u8;
                self.regs.a = a;
                self.regs.y = y;
                self.regs.set_p_nz(y);
            }};
            // https://problemkrsrctt.de/fullsnes.htm#snesunpredictablethings
            (DIV, YA, X) => {{
                self.regs.psw.set_h((self.regs.x & 0x0F) <= (self.regs.y & 0x0F));
                let x = self.regs.x as u32;
                let mut tmp = self.regs.get_ya() as u32;
                for _ in 0..9 {
                    tmp = tmp * 2;
                    if (tmp & 0x20000) != 0 {
                        tmp = tmp ^ 0x20001;
                    }
                    if x * 0x200 <= tmp {
                        tmp = tmp ^ 1;
                    }
                    if (tmp & 1) != 0 {
                        tmp = tmp.wrapping_sub(x * 0x200) & 0x1FFFF;
                    }
                }
                self.regs.a = (tmp & 0xFF) as u8;
                self.regs.y = (tmp / 0x200) as u8;
                self.regs.psw.set_v((tmp & (1 << 8)) != 0);
                self.regs.set_p_nz(self.regs.a);
            }};

            (DAA, A) => {{
                let mut tmp = self.regs.a as u16;
                if self.regs.psw.h() {
                    tmp = tmp.wrapping_add(6);
                }
                rsrc.add_cycles(2);
                self.regs.set_p_nz(tmp as u8);
                self.regs.psw.set_c((tmp & (1 << 8)) != 0);
                self.regs.a = tmp as u8;
            }};
            (DAS, A) => {{
                let sub = if self.regs.psw.h() { 6 } else { 0 };
                rsrc.add_cycles(2);
                self.regs.psw.set_c(sub <= self.regs.a);
                let a = self.regs.a.wrapping_sub(sub);
                self.regs.a = a;
                self.regs.set_p_nz(a);
            }};

            (BRA, rel) => { branch!(true) };
            (BEQ, rel) => { branch!(self.regs.psw.z()) };
            (BNE, rel) => { branch!(!self.regs.psw.z()) };
            (BCS, rel) => { branch!(self.regs.psw.c()) };
            (BCC, rel) => { branch!(!self.regs.psw.c()) };
            (BVS, rel) => { branch!(self.regs.psw.v()) };
            (BVC, rel) => { branch!(!self.regs.psw.v()) };
            (BMI, rel) => { branch!(self.regs.psw.n()) };
            (BPL, rel) => { branch!(!self.regs.psw.n()) };
            (BBS, dp, $bit: expr, rel) => {{
                let v = operand!(dp);
                let v = self.read_operand(v, rsrc);
                branch!((v & (1 << $bit)) != 0);
            }};
            (BBC, dp, $bit: expr, rel) => {{
                let v = operand!(dp);
                let v = self.read_operand(v, rsrc);
                branch!((v & (1 << $bit)) == 0);
            }};
            (CBNE, $( $x: ident )*, rel) => {{
                let a = self.regs.a;
                let b = operand!($($x)*);
                let b = self.read_operand(b, rsrc);
                branch!(a != b);
            }};
            (DBNZ, dp, rel) => {{
                let addr = addr!(dp);
                let addr = self.calc_effaddr(addr, rsrc);
                let v = self.read8(addr, rsrc).wrapping_sub(1);
                self.write8(addr, v, rsrc);
                branch!(v != 0);
            }};
            (DBNZ, Y, rel) => {{
                let y = self.regs.y.wrapping_sub(1);
                self.regs.y = y;
                branch!(y != 0);
            }};
            (JMP, $( $x: ident )*) => {{
                let addr = addr_jmp!($($x)*);
                self.regs.pc = addr;
            }};

            (CALL, abs) => {{
                let addr = addr!(abs);
                let addr = self.calc_effaddr(addr, rsrc);
                call!(addr);
                rsrc.add_cycles(3);
            }};
            (TCALL, $n: expr) => {{
                let addr = self.read16(0xFFDE - 2 * ($n as u16), rsrc);
                call!(addr);
                rsrc.add_cycles(3);
            }};
            (PCALL, up) => {{
                let up = self.fetch(rsrc) as u16;
                let addr = 0xFF00 | up;
                call!(addr);
                rsrc.add_cycles(2);
            }};

            (BRK) => {{
                self.regs.psw.set_i(false);
                self.regs.psw.set_b(true);
                self.push16(self.regs.pc, rsrc);
                self.push8(self.regs.psw.clone().into(), rsrc);
                self.regs.pc = self.read16(0xFFDE, rsrc);
                rsrc.add_cycles(2);
            }};
            (RET) => {{
                self.regs.pc = self.pull16(rsrc);
                rsrc.add_cycles(2);
            }};
            (RETI) => {{
                self.regs.psw = self.pull8(rsrc).into();
                self.regs.pc = self.pull16(rsrc);
                rsrc.add_cycles(2);
            }};

            (PUSH, $x: ident) => {{
                let x = operand!($x);
                let x = self.read_operand(x, rsrc);
                self.push8(x, rsrc);
                rsrc.add_cycles(1);
            }};
            (POP, $x: ident) => {{
                let v = self.pull8(rsrc);
                let x = operand!($x);
                self.write_operand(x, v, rsrc);
                rsrc.add_cycles(1);
            }};

            (SET1, $x: ident, $bit: expr) => { set1_clr1!(SET1, $x, $bit) };
            (CLR1, $x: ident, $bit: expr) => { set1_clr1!(CLR1, $x, $bit) };

            (TSET1, $x: ident) => { tset1_tclr1!(TSET1, $x) };
            (TCLR1, $x: ident) => { tset1_tclr1!(TCLR1, $x) };

            (AND1, C, $( $x: ident )*, bit) => { mem_bit_op!(AND1, $($x)*, bit) };
            (OR1, C, $( $x: ident )*, bit) => { mem_bit_op!(OR1, $($x)*, bit) };
            (EOR1, C, $( $x: ident )*, bit) => { mem_bit_op!(EOR1, $($x)*, bit) };

            (NOT1, aaab, bit) => {{
                let (addr, bit, _) = addr_aaab!(aaab);
                let v = self.read8(addr, rsrc);
                let v = v ^ (1 << bit);
                self.write8(addr, v, rsrc);
            }};
            (MOV1, C, aaab, bit) => {{
                let (addr, bit, _) = addr_aaab!(aaab);
                let v = self.read8(addr, rsrc);
                self.regs.psw.set_c(((v >> bit) & 1) == 1);
            }};
            (MOV1, aaab, bit, C) => {{
                let (addr, bit, _) = addr_aaab!(aaab);
                let v = self.read8(addr, rsrc);
                let v = if self.regs.psw.c() {
                    v | (1 << bit)
                } else {
                    v & !(1 << bit)
                };
                self.write8(addr, v, rsrc);
            }};

            (CLRC) => {{
                self.regs.psw.set_c(false);
                rsrc.add_cycles(1);
            }};
            (SETC) => {{
                self.regs.psw.set_c(true);
                rsrc.add_cycles(1);
            }};
            (NOTC) => {{
                let c = self.regs.psw.c();
                self.regs.psw.set_c(!c);
                rsrc.add_cycles(2);
            }};
            (CLRV) => {{
                self.regs.psw.set_v(false);
                self.regs.psw.set_h(false);
                rsrc.add_cycles(1);
            }};
            (CLRP) => {{
                self.regs.psw.set_p(false);
                rsrc.add_cycles(1);
            }};
            (SETP) => {{
                self.regs.psw.set_p(true);
                rsrc.add_cycles(1);
            }};
            (EI) => {{
                self.regs.psw.set_i(true);
                rsrc.add_cycles(1);
            }};
            (DI) => {{
                self.regs.psw.set_i(false);
                rsrc.add_cycles(1);
            }};

            (NOP) => {{
                rsrc.add_cycles(1);
            }};
            (SLEEP) => {{
                rsrc.add_cycles(2);
                self.halt = true;
            }};
            (STOP) => {{
                rsrc.add_cycles(1);
                self.halt = true;
            }};
        }

        let opcode = self.fetch(rsrc);
        decode!(opcode, exec);
    }
}
