use crate::debug::Debugger;
use crate::emulator::{PendingEvents, NMI};
use crate::opcode::opcode_to_ident;
use modular_bitfield::bitfield;
use std::cmp::Ordering;

#[bitfield]
#[repr(u8)]
#[derive(Copy, Clone, Debug)]
pub struct StatusRegister {
    c: bool, // carry flag
    z: bool, // zero flag
    i: bool, // interrupt disabled flag
    d: bool, // decimal mode
    x: bool, // index registers' width flag (16-bit or 8-bit)
    m: bool, // accumulator's width flag (16-bit or 8-bit)
    v: bool, // overflow flag
    n: bool, // negative flag
}

impl Default for StatusRegister {
    fn default() -> Self {
        let mut res: Self = 0x00.into();
        res.set_i(true);
        res.set_x(true);
        res.set_m(true);
        res
    }
}

#[derive(Debug, Clone, Copy)]
pub struct Regfile {
    pub pc: u16,
    pub p: StatusRegister,
    a: u16,
    x: u16,
    y: u16,
    pub d: u16,
    s: u16,
    pub pb: u8,  // program bank
    pub db: u8,  // data bank
    pub e: bool, // emulation mode flag
}

impl Default for Regfile {
    fn default() -> Self {
        Self {
            pc: 0,
            p: StatusRegister::default(),
            a: 0,
            x: 0,
            y: 0,
            d: 0,
            s: 0x1FF,
            pb: 0,
            db: 0,
            e: false,
        }
    }
}

#[derive(Default)]
pub struct CPU {
    pub reg: Regfile,
    shutdown: bool,
    wait: bool,

    pub enable_debug: bool,
}

pub trait Resources {
    fn tick(&mut self, v: u8);
    fn read(&mut self, addr: u32) -> u8;
    fn write(&mut self, addr: u32, data: u8);

    fn pending_events_mut(&mut self) -> &mut PendingEvents;
    fn nmi_mut(&mut self) -> &mut NMI;
    fn debugger_mut(&mut self) -> &mut Debugger;
}

#[derive(Clone, Copy, Debug)]
enum ComputationValue {
    Value8(u32),
    Value16(u32),
}

enum Interrupt {
    COP,
    BRK,
    NMI,
    IRQ,
}

impl ComputationValue {
    fn value(&self) -> u16 {
        match self {
            ComputationValue::Value8(x) => (x & 0xFF) as u16,
            ComputationValue::Value16(x) => (x & 0xFFFF) as u16,
        }
    }

    fn detect_carry(&self) -> bool {
        match self {
            ComputationValue::Value8(x) => (u8::MAX as u32) < *x,
            ComputationValue::Value16(x) => (u16::MAX as u32) < *x,
        }
    }

    fn is_neg(&self) -> bool {
        match self {
            ComputationValue::Value8(x) => ((x >> 7) & 1) != 0,
            ComputationValue::Value16(x) => ((x >> 15) & 1) != 0,
        }
    }

    fn is_zero(&self) -> bool {
        match self {
            ComputationValue::Value8(x) => (x & 0xFF) == 0,
            ComputationValue::Value16(x) => (x & 0xFFFF) == 0,
        }
    }

    fn get_msb(&self) -> bool {
        self.is_neg()
    }

    fn get_second_high_bit(&self) -> bool {
        match self {
            ComputationValue::Value8(x) => ((x >> 6) & 1) != 0,
            ComputationValue::Value16(x) => ((x >> 14) & 1) != 0,
        }
    }

    fn with_msb(&self, b: bool) -> Self {
        match self {
            ComputationValue::Value8(x) => ComputationValue::Value8(if b { x | ((1 << 7) as u32) } else { x & !((1 << 7) as u32) }),
            ComputationValue::Value16(x) => ComputationValue::Value16(if b { x | (1 << 15) as u32 } else { x & !((1 << 15) as u32) }),
        }
    }

    fn write_mem(&self, addr: MemAddr, rsrc: &mut impl Resources) {
        match self {
            ComputationValue::Value8(x) => addr.write8(*x as u8, rsrc),
            ComputationValue::Value16(x) => addr.write16(*x as u16, rsrc),
        }
    }
}

impl PartialEq for ComputationValue {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (&ComputationValue::Value8(x), &ComputationValue::Value8(y)) => x == y,
            (&ComputationValue::Value16(x), &ComputationValue::Value16(y)) => x == y,
            _ => false,
        }
    }
}
impl Eq for ComputationValue {}

impl StatusRegister {
    fn value(&self) -> u8 {
        u8::from(*self)
    }
}

impl Regfile {
    fn dh(&self) -> u8 {
        ((self.d >> 8) & 0xFF) as u8
    }

    fn dl(&self) -> u8 {
        (self.d & 0xFF) as u8
    }

    fn sl(&self) -> u8 {
        ((self.s >> 8) & 0xFF) as u8
    }

    fn norm_index(&mut self) {
        self.x &= 0xFF;
        self.y &= 0xFF;
    }

    fn norm_stack(&mut self) {
        let lo = self.s & 0xFF;
        self.s = 0x0100;
        self.s |= lo;
    }

    fn set_a(&mut self, v: ComputationValue) {
        if self.p.m() {
            let hi = self.a & 0xFF00;
            self.a = v.value() & 0x00FF;
            self.a += hi;
        } else {
            self.a = v.value()
        }
    }

    fn set_x(&mut self, v: ComputationValue) {
        self.x = v.value();
        if self.p.x() {
            self.norm_index();
        }
    }

    fn set_y(&mut self, v: ComputationValue) {
        self.y = v.value();
        if self.p.x() {
            self.norm_index();
        }
    }

    fn set_s(&mut self, v: ComputationValue) {
        self.s = v.value();
        if self.e {
            self.norm_stack();
        }
    }

    fn get_a(&self) -> ComputationValue {
        if self.p.m() {
            ComputationValue::Value8(self.a as u32)
        } else {
            ComputationValue::Value16(self.a as u32)
        }
    }

    fn set_c(&mut self, v: u16) {
        self.a = v
    }

    fn get_c(&self) -> u16 {
        self.a
    }

    fn get_x(&self) -> ComputationValue {
        if self.p.x() {
            ComputationValue::Value8(self.x as u32)
        } else {
            ComputationValue::Value16(self.x as u32)
        }
    }

    fn get_y(&self) -> ComputationValue {
        if self.p.x() {
            ComputationValue::Value8(self.y as u32)
        } else {
            ComputationValue::Value16(self.y as u32)
        }
    }

    fn get_s(&self) -> ComputationValue {
        if self.e {
            ComputationValue::Value8(self.s as u32)
        } else {
            ComputationValue::Value16(self.s as u32)
        }
    }

    fn get_s16(&self) -> u16 {
        self.s
    }

    fn set_p(&mut self, v: u8) {
        self.p = StatusRegister::from(v);
        if self.p.x() {
            self.norm_index()
        }
    }

    fn set_p_c(&mut self, v: ComputationValue) {
        self.p.set_c(v.detect_carry());
    }

    fn set_p_n(&mut self, v: ComputationValue) {
        self.p.set_n(v.is_neg());
    }

    fn set_p_z(&mut self, v: ComputationValue) {
        self.p.set_z(v.is_zero());
    }

    fn set_p_x(&mut self, v: bool) {
        self.p.set_x(v);
        if v {
            self.norm_index()
        }
    }

    fn set_p_m(&mut self, v: bool) {
        self.p.set_m(v);
    }

    fn incr_s(&mut self) {
        if self.e {
            let sh = self.s & 0xFF00;
            let sl = self.sl().wrapping_add(1);
            self.s = sh | (sl as u16);
        } else {
            self.s = self.s.wrapping_add(1)
        }
    }

    fn decr_s(&mut self) {
        if self.e {
            let sh = self.s & 0xFF00;
            let sl = self.sl().wrapping_sub(1);
            self.s = sh | (sl as u16);
        } else {
            self.s = self.s.wrapping_sub(1)
        }
    }

    fn norm_by_e(&mut self) {
        if self.e {
            let sl = self.sl();
            self.s = 0x100 | (sl as u16);
            self.set_p_m(true);
            self.set_p_x(true);
        }
    }
}

#[derive(Clone, Copy, Debug)]
enum MemAddr {
    Addr8_8_8 { hi: u8, mi: u8, lo: u8 },
    Addr8_16(u8, u16),
    Addr24(u32),
}

impl MemAddr {
    fn raw(&self) -> u32 {
        match self {
            MemAddr::Addr8_8_8 { hi, mi, lo } => ((*hi as u32) << 16) | ((*mi as u32) << 8) | (*lo as u32),
            MemAddr::Addr8_16(hi, lo) => ((*hi as u32) << 16) | (*lo as u32),
            MemAddr::Addr24(x) => *x,
        }
    }

    fn add8(&self, v: u8) -> MemAddr {
        match self {
            MemAddr::Addr8_8_8 { hi, mi, lo } => MemAddr::Addr8_8_8 {
                hi: *hi,
                mi: *mi,
                lo: lo.wrapping_add(v),
            },
            MemAddr::Addr8_16(hi, lo) => MemAddr::Addr8_16(*hi, lo.wrapping_add(v.into())),
            MemAddr::Addr24(x) => MemAddr::Addr24(x.wrapping_add(v.into())),
        }
    }

    fn add16(&self, v: u16) -> MemAddr {
        match self {
            MemAddr::Addr8_8_8 { .. } => panic!("Addr8_8_8::add16"),
            MemAddr::Addr8_16(hi, lo) => MemAddr::Addr8_16(*hi, lo.wrapping_add(v.into())),
            MemAddr::Addr24(x) => MemAddr::Addr24(x.wrapping_add(v.into())),
        }
    }

    fn read8(&self, rsrc: &mut impl Resources) -> u8 {
        rsrc.read(self.raw())
    }

    fn read16(&self, rsrc: &mut impl Resources) -> u16 {
        let a = rsrc.read(self.raw()) as u16;
        let b = rsrc.read(self.add8(1).raw()) as u16;
        (b << 8) | a
    }

    fn read24(&self, rsrc: &mut impl Resources) -> u32 {
        let a = rsrc.read(self.raw()) as u32;
        let b = rsrc.read(self.add8(1).raw()) as u32;
        let c = rsrc.read(self.add8(2).raw()) as u32;
        (c << 16) | (b << 8) | a
    }

    fn write8(&self, v: u8, rsrc: &mut impl Resources) {
        rsrc.write(self.raw(), v)
    }

    fn write16(&self, v: u16, rsrc: &mut impl Resources) {
        rsrc.write(self.raw(), (v & 0xFF) as u8);
        rsrc.write(self.add8(1).raw(), (v >> 8) as u8)
    }
}

struct BcdN<const N: usize>([u8; N]);

impl<const N: usize> BcdN<N> {
    const ZERO: BcdN<N> = Self([0; N]);

    const ONE: BcdN<N> = Self({
        let mut arr: [u8; N] = [0; N];
        arr[0] = 1;
        arr
    });

    fn wrapping_add(&self, other: &Self) -> (Self, bool) {
        let mut arr: [u8; N] = [0; N];
        let mut c = false;
        for i in 0..N {
            arr[i] = self.0[i] + other.0[i] + (c as u8);
            c = 10 <= arr[i];
            if 10 <= arr[i] {
                arr[i] -= 10;
            }
        }
        (Self(arr), c)
    }

    fn neg(&self) -> Self {
        let mut arr = self.0.clone();
        for i in 0..N {
            arr[i] = 9 - arr[i]
        }
        Self(arr).wrapping_add(&Self::ONE).0
    }
}

impl<const N: usize> Ord for BcdN<N> {
    fn cmp(&self, other: &Self) -> Ordering {
        let arr0 = self.0;
        let arr1 = other.0;
        for i in (0..N).rev() {
            if arr0[i] < arr1[i] {
                return Ordering::Less;
            } else if arr0[i] > arr1[i] {
                return Ordering::Greater;
            }
        }
        Ordering::Equal
    }
}

impl<const N: usize> PartialEq for BcdN<N> {
    fn eq(&self, other: &Self) -> bool {
        self.cmp(other) == Ordering::Equal
    }
}

impl<const N: usize> PartialOrd for BcdN<N> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl<const N: usize> Eq for BcdN<N> {}

type BCD2 = BcdN<2>;
type BCD4 = BcdN<4>;

macro_rules! bcd_into_impl {
    ($arr: expr, $ty: ident) => {{
        let mut res: $ty = 0;
        for i in (0..$arr.len()).rev() {
            res = (res << 4) | ($arr[i] as $ty)
        }
        res
    }};
}

macro_rules! bcd_from_impl {
    ($len: expr, $v: expr) => {{
        let mut arr: [u8; $len] = [0; $len];
        let mut ok = true;
        for i in 0..$len {
            let x = (($v >> (i * 4)) & 0xF) as u8;
            if 10 <= x {
                ok = false;
                break;
            }
            arr[i] = x;
        }
        if ok {
            Ok(arr)
        } else {
            Err("Invalid as BCD")
        }
    }};
}

impl BCD2 {
    fn to_binary(&self) -> u8 {
        bcd_into_impl!(self.0, u8)
    }
}

impl TryFrom<u8> for BCD2 {
    type Error = &'static str;

    fn try_from(v: u8) -> Result<Self, Self::Error> {
        bcd_from_impl!(2, v).map(|x| Self(x))
    }
}

impl BCD4 {
    fn to_binary(&self) -> u16 {
        bcd_into_impl!(self.0, u16)
    }
}

impl TryFrom<u16> for BCD4 {
    type Error = &'static str;

    fn try_from(v: u16) -> Result<Self, Self::Error> {
        bcd_from_impl!(4, v).map(|x| Self(x))
    }
}

const CPU_CYCLES: u8 = 6;

#[derive(Debug, PartialEq)]
enum EffAddrType {
    Abs,
    AbsX,
    AbsY,
    IndAbs,
    IndAbsLong,
    IndAbsX,
    Direct,
    DirectX,
    DirectY,
    Indirect,
    IndirectLong,
    IndirectX,
    IndirectY,
    IndirectYLong,
    Long,
    LongX,
    StackS,
    StackSY,
}

#[derive(Clone, Copy, Debug, PartialEq)]
enum RegType {
    A,
    X,
    Y,
    C,
}

enum TransferRegType {
    Normal(RegType),
    S,
    D,
}

trait TypeOps {
    type BcdType;
    fn read_from_addr(addr: &MemAddr, rsrc: &mut impl Resources) -> Self;
    fn as_computation(v: u32) -> ComputationValue;
    fn detect_overflow(v: i32) -> bool;
    fn to_bcd(v: Self) -> Self::BcdType;
}

impl TypeOps for u8 {
    type BcdType = BCD2;

    fn read_from_addr(addr: &MemAddr, rsrc: &mut impl Resources) -> Self {
        addr.read8(rsrc)
    }
    fn as_computation(v: u32) -> ComputationValue {
        ComputationValue::Value8(v)
    }
    fn detect_overflow(v: i32) -> bool {
        !((Self::MIN as i32) <= v && v <= (Self::MAX as i32))
    }
    fn to_bcd(v: Self) -> Self::BcdType {
        Self::BcdType::try_from(v).unwrap()
    }
}

impl TypeOps for u16 {
    type BcdType = BCD4;

    fn read_from_addr(addr: &MemAddr, rsrc: &mut impl Resources) -> Self {
        addr.read16(rsrc)
    }
    fn as_computation(v: u32) -> ComputationValue {
        ComputationValue::Value16(v)
    }
    fn detect_overflow(v: i32) -> bool {
        !((Self::MIN as i32) <= v && v <= (Self::MAX as i32))
    }
    fn to_bcd(v: Self) -> Self::BcdType {
        Self::BcdType::try_from(v).unwrap()
    }
}

impl CPU {
    fn additional_cycle_w(&self, rsrc: &mut impl Resources) {
        if self.reg.dl() != 0 {
            rsrc.tick(CPU_CYCLES);
        }
    }

    fn fetch8(&mut self, rsrc: &mut impl Resources) -> u8 {
        let res = rsrc.read(((self.reg.pb as u32) << 16) | (self.reg.pc as u32));
        self.reg.pc += 1;
        if self.enable_debug {
            rsrc.debugger_mut().tell_fetched(res);
        }
        res
    }

    fn fetch16(&mut self, rsrc: &mut impl Resources) -> u16 {
        let ll = self.fetch8(rsrc);
        let hh = self.fetch8(rsrc);
        ((hh as u16) << 8) | (ll as u16)
    }

    fn push8(&mut self, v: u8, rsrc: &mut impl Resources) {
        rsrc.write(self.reg.get_s16() as u32, v);
        self.reg.decr_s();
    }

    fn push16(&mut self, v: u16, rsrc: &mut impl Resources) {
        let lo = (v & 0xFF) as u8;
        let hi = ((v >> 8) & 0xFF) as u8;
        self.push8(hi, rsrc);
        self.push8(lo, rsrc);
    }

    fn pull8(&mut self, rsrc: &mut impl Resources) -> u8 {
        self.reg.incr_s();
        rsrc.read(self.reg.get_s16() as u32)
    }

    fn pull16(&mut self, rsrc: &mut impl Resources) -> u16 {
        let lo = self.pull8(rsrc);
        let hi = self.pull8(rsrc);
        ((hi as u16) << 8) | (lo as u16)
    }

    pub fn reset(&mut self, rsrc: &mut impl Resources) {
        self.wait = false;
        self.shutdown = false;
        self.reg = Regfile::default();

        // RESET doesn't save current context on the stack
        let vector = 0xFFFC;
        let l = rsrc.read(vector) as u16;
        let h = rsrc.read(vector + 1) as u16;
        self.reg.pc = (h << 8) | l;
    }

    pub fn tick(&mut self, rsrc: &mut impl Resources) {
        if self.shutdown {
            rsrc.tick(CPU_CYCLES);
            return;
        }

        if rsrc.nmi_mut().consume_detected() {
            rsrc.read(((self.reg.pb as u32) << 16) | (self.reg.pc as u32));
            self.wait = false;
            self.interrupt(rsrc, Interrupt::NMI);
            return;
        }

        if rsrc.pending_events_mut().get_hv_irq() {
            if self.wait && self.reg.p.i() {
                self.wait = false;
            } else if !self.reg.p.i() {
                rsrc.read(((self.reg.pb as u32) << 16) | (self.reg.pc as u32));
                self.interrupt(rsrc, Interrupt::IRQ);
                return;
            }
        }

        if self.wait {
            rsrc.tick(CPU_CYCLES);
            return;
        }

        if self.enable_debug {
            rsrc.debugger_mut().instr_prolog(self.reg.pb, self.reg.pc);
        }
        self.exec_instr(rsrc);
    }

    fn effaddr(&mut self, tp: EffAddrType, bank: u8, rsrc: &mut impl Resources) -> MemAddr {
        match tp {
            EffAddrType::Abs => {
                let addr = self.fetch16(rsrc) as u32;
                MemAddr::Addr24(((bank as u32) << 16) | addr)
            }
            EffAddrType::AbsX | EffAddrType::AbsY => {
                let reg = if tp == EffAddrType::AbsX { self.reg.get_x() } else { self.reg.get_y() };
                let addr = self.fetch16(rsrc) as u32;
                MemAddr::Addr24(((bank as u32) << 16) | addr).add16(reg.value())
            }
            EffAddrType::IndAbs => {
                let addr = self.fetch16(rsrc);
                let ptr = MemAddr::Addr8_16(0 as u8, addr);
                MemAddr::Addr8_16(bank, ptr.read16(rsrc))
            }
            EffAddrType::IndAbsLong => {
                let addr = self.fetch16(rsrc);
                let paddr = MemAddr::Addr8_16(0 as u8, addr);
                let dst = paddr.read24(rsrc);
                MemAddr::Addr8_16(((dst >> 16) & 0xFF) as u8, (dst & 0xFFFF) as u16)
            }
            EffAddrType::IndAbsX => {
                let addr = self.fetch16(rsrc);
                let ptr = MemAddr::Addr8_16(bank, addr).add16(self.reg.get_x().value());
                MemAddr::Addr8_16(bank, ptr.read16(rsrc))
            }
            EffAddrType::Direct => {
                let ll = self.fetch8(rsrc);
                if self.reg.e && self.reg.dl() == 0 {
                    MemAddr::Addr8_8_8 { hi: 0, mi: self.reg.dh(), lo: ll }
                } else {
                    self.additional_cycle_w(rsrc);
                    MemAddr::Addr8_16(0, self.reg.d).add8(ll)
                }
            }
            EffAddrType::DirectX | EffAddrType::DirectY => {
                let reg = if tp == EffAddrType::DirectX { self.reg.get_x() } else { self.reg.get_y() };
                let ll = self.fetch8(rsrc);
                rsrc.tick(CPU_CYCLES);
                self.additional_cycle_w(rsrc);
                if self.reg.e && self.reg.dl() == 0 {
                    // if self.reg.p.e, self.reg.x's high 8 bit is 0
                    MemAddr::Addr8_8_8 { hi: 0, mi: self.reg.dh(), lo: ll }.add8(self.reg.get_x().value() as u8)
                } else {
                    MemAddr::Addr8_16(0, self.reg.d).add16(ll.into()).add16(reg.value())
                }
            }
            EffAddrType::Indirect => {
                let ll = self.fetch8(rsrc);
                let ptr = if self.reg.e && self.reg.dl() == 0 {
                    MemAddr::Addr8_8_8 { hi: 0, mi: self.reg.dh(), lo: ll }
                } else {
                    MemAddr::Addr8_16(0, self.reg.d).add8(ll)
                };
                MemAddr::Addr24(((bank as u32) << 16) | ptr.read16(rsrc) as u32)
            }
            EffAddrType::IndirectLong => {
                let ll = self.fetch8(rsrc);
                self.additional_cycle_w(rsrc);
                let ptr = MemAddr::Addr8_16(0, self.reg.d).add8(ll);
                MemAddr::Addr24(ptr.read24(rsrc))
            }
            EffAddrType::IndirectX => {
                let ll = self.fetch8(rsrc);
                rsrc.tick(CPU_CYCLES);
                self.additional_cycle_w(rsrc);
                let paddr = if self.reg.e && self.reg.dl() == 0 {
                    // if self.reg.p.e, self.reg.x's high 8 bit is 0
                    MemAddr::Addr8_8_8 { hi: 0, mi: self.reg.dh(), lo: ll }.add8(self.reg.get_x().value() as u8)
                } else {
                    MemAddr::Addr8_16(0, self.reg.d).add8(ll).add16(self.reg.get_x().value() as u16)
                };
                let ptr = paddr.read16(rsrc);
                MemAddr::Addr24(((bank as u32) << 16) | (ptr as u32))
            }
            EffAddrType::IndirectY => {
                // TODO : detect page boundary wrapping
                let ll = self.fetch8(rsrc);
                rsrc.tick(CPU_CYCLES); // TODO : necessary?
                self.additional_cycle_w(rsrc);
                let paddr = if self.reg.e && self.reg.dl() == 0 {
                    MemAddr::Addr8_8_8 { hi: 0, mi: self.reg.dh(), lo: ll }
                } else {
                    MemAddr::Addr8_16(0, self.reg.d).add8(ll)
                };
                let ptr = paddr.read16(rsrc);
                MemAddr::Addr24(((self.reg.db as u32) << 16) | (ptr as u32)).add16(self.reg.get_y().value())
            }
            EffAddrType::IndirectYLong => {
                let ll = self.fetch8(rsrc);
                self.additional_cycle_w(rsrc);
                let ptr = MemAddr::Addr8_16(0, self.reg.d).add16(ll.into());
                MemAddr::Addr24(ptr.read24(rsrc)).add16(self.reg.get_y().value())
            }
            EffAddrType::Long => {
                let ll = self.fetch8(rsrc);
                let mm = self.fetch8(rsrc);
                let hh = self.fetch8(rsrc);
                MemAddr::Addr24(((hh as u32) << 16) | ((mm as u32) << 8) | (ll as u32))
            }
            EffAddrType::LongX => {
                let ll = self.fetch8(rsrc);
                let mm = self.fetch8(rsrc);
                let hh = self.fetch8(rsrc);
                MemAddr::Addr24(((hh as u32) << 16) | ((mm as u32) << 8) | (ll as u32)).add16(self.reg.get_x().value())
            }
            EffAddrType::StackS => {
                let ll = self.fetch8(rsrc);
                MemAddr::Addr8_16(0, ll as u16).add16(self.reg.s)
            }
            EffAddrType::StackSY => {
                let ll = self.fetch8(rsrc);
                rsrc.tick(CPU_CYCLES);
                let paddr = MemAddr::Addr8_16(0, ll as u16).add16(self.reg.s);
                let ptr = paddr.read16(rsrc);
                MemAddr::Addr24(((bank as u32) << 16) | (ptr as u32)).add16(self.reg.get_y().value() as u16)
            }
        }
    }

    fn get_reg(&self, reg: RegType) -> ComputationValue {
        match reg {
            RegType::A => self.reg.get_a(),
            RegType::X => self.reg.get_x(),
            RegType::Y => self.reg.get_y(),
            RegType::C => ComputationValue::Value16(self.reg.get_c().into()),
        }
    }

    fn set_reg(&mut self, reg: RegType, v: ComputationValue) {
        match reg {
            RegType::A => self.reg.set_a(v),
            RegType::X => self.reg.set_x(v),
            RegType::Y => self.reg.set_y(v),
            RegType::C => self.reg.set_c(v.value()),
        }
    }

    fn instr_push(&mut self, flag: bool, v: ComputationValue, rsrc: &mut impl Resources) {
        if flag {
            self.push8(v.value() as u8, rsrc)
        } else {
            self.push16(v.value(), rsrc)
        }
    }

    fn instr_pull(&mut self, flag: bool, reg: RegType, rsrc: &mut impl Resources) {
        let v = if flag {
            ComputationValue::Value8(self.pull8(rsrc).into())
        } else {
            ComputationValue::Value16(self.pull16(rsrc).into())
        };
        self.reg.set_p_n(v);
        self.reg.set_p_z(v);
        self.set_reg(reg, v);
        rsrc.tick(CPU_CYCLES)
    }

    fn instr_transfer(&mut self, dst: TransferRegType, src: ComputationValue, rsrc: &mut impl Resources) {
        match dst {
            TransferRegType::S => {
                rsrc.tick(CPU_CYCLES);
                self.reg.set_s(src);
            }
            TransferRegType::D => {
                rsrc.tick(CPU_CYCLES);
                self.reg.d = src.value();
                let val = ComputationValue::Value16(self.reg.d.into());
                self.reg.set_p_n(val);
                self.reg.set_p_z(val);
            }
            TransferRegType::Normal(reg) => {
                rsrc.tick(CPU_CYCLES);
                self.set_reg(reg, src);
                let res = self.get_reg(reg);
                self.reg.set_p_n(res);
                self.reg.set_p_z(res);
            }
        }
    }

    fn instr_branch8(&mut self, cond: bool, offset: u8, rsrc: &mut impl Resources) {
        if cond {
            rsrc.tick(CPU_CYCLES);
            let offset = (offset as i8) as u16;
            let tmp = self.reg.pc;
            self.reg.pc = self.reg.pc.wrapping_add(offset);

            // page boundary crossed
            let tmp = ((tmp & self.reg.pc) >> 8) & 0xFF;
            if tmp != 0 && self.reg.e {
                rsrc.tick(CPU_CYCLES);
            }
        }
    }

    fn instr_move_block(&mut self, dst: u8, src: u8, dir: i8, rsrc: &mut impl Resources) {
        let x = self.reg.get_x().value();
        let y = self.reg.get_y().value();
        self.reg.db = dst;
        let src = MemAddr::Addr8_16(src, x);
        let dst = MemAddr::Addr8_16(dst, y);
        let v = src.read8(rsrc);
        dst.write8(v, rsrc);

        let advance = |x: &u16| match dir {
            1 => x.wrapping_add(1),
            -1 => x.wrapping_sub(1),
            _ => unreachable!(),
        };
        self.reg.set_x(ComputationValue::Value16(advance(&x).into()));
        self.reg.set_y(ComputationValue::Value16(advance(&y).into()));
        let na = self.reg.get_a().value().wrapping_sub(1);
        self.reg.set_a(ComputationValue::Value16(na.into()));
        if na != 0xFFFF {
            self.reg.pc = self.reg.pc.wrapping_sub(3);
        }
        rsrc.tick(CPU_CYCLES);
        rsrc.tick(CPU_CYCLES);
    }

    fn instr_comp<T: TypeOps>(&mut self, a: ComputationValue, b: ComputationValue) {
        let a = a.value() as u32;
        let b = (b.value() as u16) as u32;
        let res_u32 = a.wrapping_sub(b);
        let res = T::as_computation(res_u32.into());
        self.reg.p.set_c(b <= a);
        self.reg.set_p_n(res);
        self.reg.set_p_z(res);
    }

    fn instr_inc<T: TypeOps>(&mut self, a: ComputationValue) -> ComputationValue {
        let res = a.value().wrapping_add(1);
        let res = T::as_computation(res.into());
        self.reg.set_p_n(res);
        self.reg.set_p_z(res);
        res
    }

    fn instr_dec<T: TypeOps>(&mut self, a: ComputationValue) -> ComputationValue {
        let res = a.value().wrapping_sub(1);
        let res = T::as_computation(res.into());
        self.reg.set_p_n(res);
        self.reg.set_p_z(res);
        res
    }

    fn instr_and<T: TypeOps>(&mut self, a: ComputationValue, b: ComputationValue) -> ComputationValue {
        let res = a.value() & b.value();
        let res = T::as_computation(res.into());
        self.reg.set_p_n(res);
        self.reg.set_p_z(res);
        res
    }

    fn instr_eor<T: TypeOps>(&mut self, a: ComputationValue, b: ComputationValue) -> ComputationValue {
        let res = a.value() ^ b.value();
        let res = T::as_computation(res.into());
        self.reg.set_p_n(res);
        self.reg.set_p_z(res);
        res
    }

    fn instr_ora<T: TypeOps>(&mut self, a: ComputationValue, b: ComputationValue) -> ComputationValue {
        let res = a.value() | b.value();
        let res = T::as_computation(res.into());
        self.reg.set_p_n(res);
        self.reg.set_p_z(res);
        res
    }

    fn instr_asl<T: TypeOps>(&mut self, a: ComputationValue) -> ComputationValue {
        let res = T::as_computation(((a.value() as u32) << 1).into());
        self.reg.set_p_c(res);
        self.reg.set_p_n(res);
        self.reg.set_p_z(res);
        res
    }

    fn instr_lsr<T: TypeOps>(&mut self, a: ComputationValue) -> ComputationValue {
        let lsb = (a.value() & 1) == 1;
        let res = T::as_computation((a.value() >> 1).into());
        self.reg.p.set_c(lsb);
        self.reg.set_p_n(res);
        self.reg.set_p_z(res);
        res
    }

    fn instr_rol<T: TypeOps>(&mut self, a: ComputationValue) -> ComputationValue {
        let res = (a.value() << 1) | (self.reg.p.c() as u16);
        let res = T::as_computation(res.into());
        self.reg.p.set_c(a.get_msb());
        self.reg.set_p_n(res);
        self.reg.set_p_z(res);
        res
    }

    fn instr_ror<T: TypeOps>(&mut self, a: ComputationValue) -> ComputationValue {
        let lsb = (a.value() & 1) == 1;
        let res = T::as_computation((a.value() >> 1).into()).with_msb(self.reg.p.c());
        self.reg.p.set_c(lsb);
        self.reg.set_p_n(res);
        self.reg.set_p_z(res);
        res
    }

    fn instr_load(&mut self, reg: RegType, v: ComputationValue) {
        self.reg.set_p_n(v);
        self.reg.set_p_z(v);
        self.set_reg(reg, v);
    }

    fn exec_instr(&mut self, rsrc: &mut impl Resources) {
        macro_rules! fetch_imm {
            (RAW, u8) => { self.fetch8(rsrc) };
            (RAW, u16) => { self.fetch16(rsrc) };

            ($ty: ident) => {{
                let imm = fetch_imm!(RAW, $ty);
                <$ty>::as_computation(imm.into())
            }};
        }

        macro_rules! get_bank {
            (JMP) => { self.reg.pb };
            (JSL) => { self.reg.pb };
            (JSR) => { self.reg.pb };

            ($_: ident) => { self.reg.db };
        }

        macro_rules! fetch_effaddr_aux {
            (ABS) => { EffAddrType::Abs };
            (ABS_X) => { EffAddrType::AbsX };
            (ABS_Y) => { EffAddrType::AbsY };
            (ABS_IND) => { EffAddrType::IndAbs };
            (ABS_IND_LONG) => { EffAddrType::IndAbsLong };
            (ABS_IND_X) => { EffAddrType::IndAbsX };
            (DIR) => { EffAddrType::Direct };
            (DIR_X) => { EffAddrType::DirectX };
            (DIR_Y) => { EffAddrType::DirectY };
            (DIR_IND) => { EffAddrType::Indirect };
            (DIR_IND_LONG) => { EffAddrType::IndirectLong };
            (DIR_X_IND) => { EffAddrType::IndirectX };
            (DIR_IND_Y) => { EffAddrType::IndirectY };
            (DIR_IND_LONG_Y) => { EffAddrType::IndirectYLong };
            (LONG) => { EffAddrType::Long };
            (LONG_X) => { EffAddrType::LongX };
            (STK_S) => { EffAddrType::StackS };
            (STK_S_Y) => { EffAddrType::StackSY };
        }

        macro_rules! fetch_effaddr {
            ($op: ident, $mode: ident) => {{
                let bank = get_bank!($op);
                let tp = fetch_effaddr_aux!($mode);
                self.effaddr(tp, bank, rsrc)
            }};
        }

        macro_rules! read_value {
            ($addr: expr, $ty: ident) => { <$ty>::as_computation(<$ty>::read_from_addr(&$addr, rsrc).into()) };
        }

        macro_rules! eat_opcode {
            (RTL, IMPLIED) => {{
                self.reg.pc = self.pull16(rsrc).wrapping_add(1);
                self.reg.pb = self.pull8(rsrc);
                rsrc.tick(2 * CPU_CYCLES);
            }};
            (RTS, IMPLIED) => {{
                self.reg.pc = self.pull16(rsrc).wrapping_add(1);
                rsrc.tick(3 * CPU_CYCLES);
            }};

            (BRK, IMPLIED) => {{
                // rsrc.tick(CPU_CYCLES);
                self.fetch8(rsrc);
                self.interrupt(rsrc, Interrupt::BRK);
            }};
            (COP, IMM) => {{
                self.fetch8(rsrc);
                self.interrupt(rsrc, Interrupt::COP);
            }};

            (RTI, IMPLIED) => {{
                let v = self.pull8(rsrc);
                self.reg.set_p(v);
                self.reg.pc = self.pull16(rsrc);
                if !self.reg.e {
                    self.reg.pb = self.pull8(rsrc);
                }
                rsrc.tick(2 * CPU_CYCLES);
            }};

            (CLC, IMPLIED) => {{
                self.reg.p.set_c(false);
                rsrc.tick(CPU_CYCLES)
            }};
            (CLD, IMPLIED) => {{
                self.reg.p.set_d(false);
                rsrc.tick(CPU_CYCLES)
            }};
            (CLI, IMPLIED) => {{
                self.reg.p.set_i(false);
                rsrc.tick(CPU_CYCLES)
            }};
            (CLV, IMPLIED) => {{
                self.reg.p.set_v(false);
                rsrc.tick(CPU_CYCLES)
            }};
            (SEC, IMPLIED) => {{
                self.reg.p.set_c(true);
                rsrc.tick(CPU_CYCLES)
            }};
            (SED, IMPLIED) => {{
                self.reg.p.set_d(true);
                rsrc.tick(CPU_CYCLES)
            }};
            (SEI, IMPLIED) => {{
                self.reg.p.set_i(true);
                rsrc.tick(CPU_CYCLES)
            }};

            (REP, IMM) => {{
                let v = self.fetch8(rsrc);
                let cur = u8::from(self.reg.p.value());
                self.reg.set_p(cur & !v);
                rsrc.tick(CPU_CYCLES);
            }};
            (SEP, IMM) => {{
                let v = self.fetch8(rsrc);
                let cur = u8::from(self.reg.p.value());
                self.reg.set_p(cur | v);
                rsrc.tick(CPU_CYCLES);
            }};

            (NOP, IMPLIED) => {
                rsrc.tick(CPU_CYCLES)
            };
            (WDM, IMM) => {{
                self.fetch8(rsrc);
                ()
            }};

            (PHA, IMPLIED) => { self.instr_push(self.reg.p.m(), self.reg.get_a(), rsrc) };
            (PHX, IMPLIED) => { self.instr_push(self.reg.p.x(), self.reg.get_x(), rsrc) };
            (PHY, IMPLIED) => { self.instr_push(self.reg.p.x(), self.reg.get_y(), rsrc) };

            (PLA, IMPLIED) => { self.instr_pull(self.reg.p.m(), RegType::A, rsrc) };
            (PLX, IMPLIED) => { self.instr_pull(self.reg.p.x(), RegType::X, rsrc) };
            (PLY, IMPLIED) => { self.instr_pull(self.reg.p.x(), RegType::Y, rsrc) };

            (PHB, IMPLIED) => {{
                self.push8(self.reg.db, rsrc);
                rsrc.tick(CPU_CYCLES)
            }};
            (PHD, IMPLIED) => {{
                self.push16(self.reg.d, rsrc);
                rsrc.tick(CPU_CYCLES)
            }};
            (PHK, IMPLIED) => {{
                self.push8(self.reg.pb, rsrc);
                rsrc.tick(CPU_CYCLES)
            }};
            (PHP, IMPLIED) => {{
                self.push8(u8::from(self.reg.p), rsrc);
                rsrc.tick(CPU_CYCLES)
            }};
            (PLB, IMPLIED) => {{
                self.reg.db = self.pull8(rsrc);
                let cv = ComputationValue::Value8(self.reg.db.into());
                self.reg.set_p_n(cv);
                self.reg.set_p_z(cv);
                rsrc.tick(CPU_CYCLES);
                rsrc.tick(CPU_CYCLES);
            }};
            (PLD, IMPLIED) => {{
                self.reg.d = self.pull16(rsrc);
                let cv = ComputationValue::Value16(self.reg.d.into());
                self.reg.set_p_n(cv);
                self.reg.set_p_z(cv);
                rsrc.tick(2 * CPU_CYCLES);
            }};
            (PLP, IMPLIED) => {{
                self.reg.p = StatusRegister::from(self.pull8(rsrc));
                self.reg.norm_by_e();
                rsrc.tick(2 * CPU_CYCLES);
            }};

            (STP, IMPLIED) => {{
                rsrc.tick(2 * CPU_CYCLES);
                self.shutdown = true
            }};
            (WAI, IMPLIED) => {{
                rsrc.tick(2 * CPU_CYCLES);
                self.wait = true
            }};

            (TAX, IMPLIED) => {{
                let src = if self.reg.p.x() {
                    ComputationValue::Value8(self.reg.get_c().into())
                } else {
                    ComputationValue::Value16(self.reg.get_c().into())
                };
                self.instr_transfer(TransferRegType::Normal(RegType::X), src, rsrc);
            }};
            (TAY, IMPLIED) => {{
                let src = if self.reg.p.x() {
                    ComputationValue::Value8(self.reg.get_c().into())
                } else {
                    ComputationValue::Value16(self.reg.get_c().into())
                };
                self.instr_transfer(TransferRegType::Normal(RegType::Y), src, rsrc);
            }};
            (TSX, IMPLIED) => {
                self.instr_transfer(TransferRegType::Normal(RegType::X), self.reg.get_s(), rsrc)
            };
            (TXA, IMPLIED) => {
                self.instr_transfer(TransferRegType::Normal(RegType::A), self.reg.get_x(), rsrc)
            };
            (TXS, IMPLIED) => {{
                self.instr_transfer(TransferRegType::S, self.reg.get_x(), rsrc)
            }};
            (TXY, IMPLIED) => {
                self.instr_transfer(TransferRegType::Normal(RegType::Y), self.reg.get_x(), rsrc)
            };
            (TYA, IMPLIED) => {
                self.instr_transfer(TransferRegType::Normal(RegType::A), self.reg.get_y(), rsrc)
            };
            (TYX, IMPLIED) => {
                self.instr_transfer(TransferRegType::Normal(RegType::X), self.reg.get_y(), rsrc)
            };

            (TCD, IMPLIED) => {{
                let src = ComputationValue::Value16(self.reg.get_c().into());
                self.instr_transfer(TransferRegType::D, src, rsrc)
            }};
            (TCS, IMPLIED) => {{
                let src = ComputationValue::Value16(self.reg.get_c().into());
                self.instr_transfer(TransferRegType::S, src, rsrc)
            }};
            (TDC, IMPLIED) => {{
                let src = ComputationValue::Value16(self.reg.d.into());
                self.instr_transfer(TransferRegType::Normal(RegType::C), src, rsrc);
            }};
            (TSC, IMPLIED) => {{
                let src = ComputationValue::Value16(self.reg.get_s16().into());
                self.instr_transfer(TransferRegType::Normal(RegType::C), src, rsrc);
            }};

            (XBA, IMPLIED) => {{
                let v = self.reg.get_c();
                let h = ((v >> 8) & 0xFF) as u8;
                let l = ((v >> 0) & 0xFF) as u8;
                let v = ComputationValue::Value8(h.into());
                self.reg.set_c(((l as u16) << 8) | (h as u16));
                self.reg.set_p_n(v);
                self.reg.set_p_z(v);
                rsrc.tick(2 * CPU_CYCLES);
            }};

            (XCE, IMPLIED) => {{
                let tmp = self.reg.e;
                self.reg.e = self.reg.p.c();
                self.reg.p.set_c(tmp);
                self.reg.norm_by_e();
                rsrc.tick(CPU_CYCLES);
            }};

            (BCC, REL8) => {{
                let offset = self.fetch8(rsrc);
                self.instr_branch8(!self.reg.p.c(), offset, rsrc);
            }};
            (BCS, REL8) => {{
                let offset = self.fetch8(rsrc);
                self.instr_branch8(self.reg.p.c(), offset, rsrc);
            }};
            (BEQ, REL8) => {{
                let offset = self.fetch8(rsrc);
                self.instr_branch8(self.reg.p.z(), offset, rsrc);
            }};
            (BMI, REL8) => {{
                let offset = self.fetch8(rsrc);
                self.instr_branch8(self.reg.p.n(), offset, rsrc);
            }};
            (BNE, REL8) => {{
                let offset = self.fetch8(rsrc);
                self.instr_branch8(!self.reg.p.z(), offset, rsrc);
            }};
            (BPL, REL8) => {{
                let offset = self.fetch8(rsrc);
                self.instr_branch8(!self.reg.p.n(), offset, rsrc);
            }};
            (BRA, REL8) => {{
                let offset = self.fetch8(rsrc);
                self.instr_branch8(true, offset, rsrc);
            }};
            (BVC, REL8) => {{
                let offset = self.fetch8(rsrc);
                self.instr_branch8(!self.reg.p.v(), offset, rsrc);
            }};
            (BVS, REL8) => {{
                let offset = self.fetch8(rsrc);
                self.instr_branch8(self.reg.p.v(), offset, rsrc);
            }};

            (BRL, REL16) => {{
                let offset = self.fetch16(rsrc);
                self.reg.pc = self.reg.pc.wrapping_add(offset);
                rsrc.tick(CPU_CYCLES);
            }};
            (PER, REL16) => {{
                let offset = self.fetch16(rsrc);
                self.push16(self.reg.pc.wrapping_add(offset), rsrc)
            }};

            // MVN/MVP
            ($op: ident, SRC_DST) => {{
                let dst = self.fetch8(rsrc);
                let src = self.fetch8(rsrc);
                move_block!($op, dst, src)
            }};

            (ADC, IMM, $ty: ident) => {{
                let imm = fetch_imm!($ty);
                let res = adc!(self.reg.get_a(), imm, $ty);
                self.reg.set_a(res);
            }};
            (ADC, $mode: ident, $ty: ident) => {{
                let val = fetch_effaddr!(ADC, $mode);
                let val = read_value!(val, $ty);
                let res = adc!(self.reg.get_a(), val, $ty);
                self.reg.set_a(res);
            }};

            (SBC, IMM, $ty: ident) => {{
                let imm = fetch_imm!($ty);
                let res = sbc!(self.reg.get_a(), imm, $ty);
                self.reg.set_a(res);
            }};
            (SBC, $mode: ident, $ty: ident) => {{
                let val = fetch_effaddr!(ADC, $mode);
                let val = read_value!(val, $ty);
                let res = sbc!(self.reg.get_a(), val, $ty);
                self.reg.set_a(res);
            }};

            (DEC, ACC, $ty: ident) => {{
                let res = self.instr_dec::<$ty>(self.reg.get_a());
                self.reg.set_a(res);
            }};
            (DEX, IMPLIED, $ty: ident) => {{
                let res = self.instr_dec::<$ty>(self.reg.get_x());
                self.reg.set_x(res);
            }};
            (DEY, IMPLIED, $ty: ident) => {{
                let res = self.instr_dec::<$ty>(self.reg.get_y());
                self.reg.set_y(res);
            }};
            (DEC, $mode: ident, $ty: ident) => {{
                let addr = fetch_effaddr!(DEC, $mode);
                let res = read_value!(addr, $ty);
                let res = self.instr_dec::<$ty>(res);
                res.write_mem(addr, rsrc);
            }};

            (INC, ACC, $ty: ident) => {{
                let res = self.instr_inc::<$ty>(self.reg.get_a());
                self.reg.set_a(res);
            }};
            (INX, IMPLIED, $ty: ident) => {{
                let res = self.instr_inc::<$ty>(self.reg.get_x());
                self.reg.set_x(res);
            }};
            (INY, IMPLIED, $ty: ident) => {{
                let res = self.instr_inc::<$ty>(self.reg.get_y());
                self.reg.set_y(res);
            }};
            (INC, $mode: ident, $ty: ident) => {{
                let addr = fetch_effaddr!(INC, $mode);
                let res = read_value!(addr, $ty);
                let res = self.instr_inc::<$ty>(res);
                res.write_mem(addr, rsrc);
            }};

            (AND, $mode: ident, $ty: ident) => { bitwise_aux!(AND, $mode, $ty) };

            (EOR, $mode: ident, $ty: ident) => { bitwise_aux!(EOR, $mode, $ty) };

            (ORA, $mode: ident, $ty: ident) => { bitwise_aux!(ORA, $mode, $ty) };

            (ASL, ACC, $ty: ident) => {{
                let res = self.reg.get_a();
                let res = self.instr_asl::<$ty>(res);
                self.reg.set_a(res);
            }};
            (ASL, $mode: ident, $ty: ident) => {{
                let addr = fetch_effaddr!(ASL, $mode);
                let res = read_value!(addr, $ty);
                let res = self.instr_asl::<$ty>(res);
                res.write_mem(addr, rsrc);
            }};

            (LSR, ACC, $ty: ident) => {{
                let res = self.reg.get_a();
                let res = self.instr_lsr::<$ty>(res);
                self.reg.set_a(res);
            }};
            (LSR, $mode: ident, $ty: ident) => {{
                let addr = fetch_effaddr!(LSR, $mode);
                let res = read_value!(addr, $ty);
                let res = self.instr_lsr::<$ty>(res);
                res.write_mem(addr, rsrc);
            }};

            (ROL, ACC, $ty: ident) => {{
                let res = self.reg.get_a();
                let res = self.instr_rol::<$ty>(res);
                self.reg.set_a(res);
            }};
            (ROL, $mode: ident, $ty: ident) => {{
                let addr = fetch_effaddr!(ROL, $mode);
                let res = read_value!(addr, $ty);
                let res = self.instr_rol::<$ty>(res);
                res.write_mem(addr, rsrc);
            }};

            (ROR, ACC, $ty: ident) => {{
                let res = self.reg.get_a();
                let res = self.instr_ror::<$ty>(res);
                self.reg.set_a(res);
            }};
            (ROR, $mode: ident, $ty: ident) => {{
                let addr = fetch_effaddr!(ROR, $mode);
                let res = read_value!(addr, $ty);
                let res = self.instr_ror::<$ty>(res);
                res.write_mem(addr, rsrc);
            }};

            (CMP, $mode: ident, $ty: ident) => { comp_aux!(CMP, $mode, $ty) };
            (CPX, $mode: ident, $ty: ident) => { comp_aux!(CPX, $mode, $ty) };
            (CPY, $mode: ident, $ty: ident) => { comp_aux!(CPY, $mode, $ty) };

            (LDA, IMM, $ty: ident) => {{
                let imm = fetch_imm!($ty);
                self.instr_load(RegType::A, imm);
            }};
            (LDA, $mode: ident, $ty: ident) => {{
                let val = fetch_effaddr!(LDA, $mode);
                let val = read_value!(val, $ty);
                self.instr_load(RegType::A, val);
            }};

            (LDX, IMM, $ty: ident) => {{
                let imm = fetch_imm!($ty);
                self.instr_load(RegType::X, imm);
            }};
            (LDX, $mode: ident, $ty: ident) => {{
                let val = fetch_effaddr!(LDX, $mode);
                let val = read_value!(val, $ty);
                self.instr_load(RegType::X, val);
            }};

            (LDY, IMM, $ty: ident) => {{
                let imm = fetch_imm!($ty);
                self.instr_load(RegType::Y, imm);
            }};
            (LDY, $mode: ident, $ty: ident) => {{
                let val = fetch_effaddr!(LDY, $mode);
                let val = read_value!(val, $ty);
                self.instr_load(RegType::Y, val);
            }};

            (STA, $mode: ident, $ty: ident) => {{
                let addr = fetch_effaddr!(STA, $mode);
                let val = self.reg.get_a();
                val.write_mem(addr, rsrc);
            }};

            (STX, $mode: ident, $ty: ident) => {{
                let addr = fetch_effaddr!(STX, $mode);
                let val = self.reg.get_x();
                val.write_mem(addr, rsrc);
            }};

            (STY, $mode: ident, $ty: ident) => {{
                let addr = fetch_effaddr!(STY, $mode);
                let val = self.reg.get_y();
                val.write_mem(addr, rsrc);
            }};

            (STZ, $mode: ident, $ty: ident) => {{
                let addr = fetch_effaddr!(STY, $mode);
                let val = <$ty>::as_computation(0);
                val.write_mem(addr, rsrc);
            }};

            (BIT, IMM, $ty: ident) => {{
                let v1 = fetch_imm!($ty);
                let v1 = v1.value() as $ty;
                let v2 = self.reg.get_a().value() as $ty;
                let res = <$ty>::as_computation((v1 & v2).into());
                self.reg.set_p_z(res);
            }};
            (BIT, $mode: ident, $ty: ident) => {{
                let v1 = fetch_effaddr!(BIT, $mode);
                let v1 = read_value!(v1, $ty);
                let v2 = self.reg.get_a().value() as $ty;
                let res = <$ty>::as_computation(((v1.value() as $ty) & v2).into());
                self.reg.p.set_n(v1.get_msb());
                self.reg.p.set_v(v1.get_second_high_bit());
                self.reg.set_p_z(res);
            }};

            (TRB, $mode: ident, $ty: ident) => {{
                let addr = fetch_effaddr!(TRB, $mode);
                let v1 = read_value!(addr, $ty).value() as $ty;
                let v2 = self.reg.get_a().value() as $ty;
                let res = <$ty>::as_computation((v1 & !v2).into());
                rsrc.tick(CPU_CYCLES);
                self.reg.p.set_z((v1 & v2) == 0);
                res.write_mem(addr, rsrc);
            }};
            (TSB, $mode: ident, $ty: ident) => {{
                let addr = fetch_effaddr!(TRB, $mode);
                let v1 = read_value!(addr, $ty).value() as $ty;
                let v2 = self.reg.get_a().value() as $ty;
                let res = <$ty>::as_computation((v1 | v2).into());
                rsrc.tick(CPU_CYCLES);
                self.reg.p.set_z((v1 & v2) == 0);
                res.write_mem(addr, rsrc);
            }};

            (JMP, $mode: ident, $ty: ident) => {{
                let addr = fetch_effaddr!(JMP, $mode);
                let v = addr.raw();
                self.reg.pb = ((v >> 16) & 0xFF) as u8;
                self.reg.pc = (v & 0xFFFF) as u16;
            }};
            (JSL, $mode: ident, $ty: ident) => {{
                let addr = fetch_effaddr!(JSL, $mode);
                self.push8(self.reg.pb, rsrc);
                self.push16(self.reg.pc.wrapping_sub(1), rsrc);
                let v = addr.raw();
                self.reg.pb = ((v >> 16) & 0xFF) as u8;
                self.reg.pc = (v & 0xFFFF) as u16;
                rsrc.tick(CPU_CYCLES);
            }};
            (JSR, $mode: ident, $ty: ident) => {{
                let addr = fetch_effaddr!(JSR, $mode);
                self.push16(self.reg.pc.wrapping_sub(1), rsrc);
                self.reg.pc = (addr.raw() & 0xFFFF) as u16;
                rsrc.tick(CPU_CYCLES);
            }};

            (PEA, IMM, $ty: ident) => {{
                let imm = self.fetch16(rsrc);
                self.push16(imm, rsrc)
            }};
            (PEI, DIR, $ty: ident) => {{
                self.additional_cycle_w(rsrc);
                let ll = self.fetch8(rsrc);
                let addr = MemAddr::Addr8_16(0, self.reg.d).add8(ll);
                self.push16(u16::read_from_addr(&addr, rsrc), rsrc);
            }};

            ($op: ident, $mode: ident) => {
                with_computation_type!($op, eat_opcode, $op, $mode)
            };
        }

        macro_rules! adc {
            ($a: expr, $b: expr, $ty: ident) => {{
                if self.reg.p.d() {
                    // TODO?: overflow flag
                    let a = <$ty>::to_bcd($a.value() as $ty);
                    let b = <$ty>::to_bcd($b.value() as $ty);
                    let zero = &<$ty as TypeOps>::BcdType::ZERO;
                    let one = &<$ty as TypeOps>::BcdType::ONE;
                    let c = if self.reg.p.c() { one } else { zero };
                    let (res, c1) = a.wrapping_add(&b);
                    let (res, c2) = res.wrapping_add(&c);
                    self.reg.p.set_c(c1 || c2);
                    self.reg.p.set_z(res == *zero);
                    // self.reg.p.set_v(false);
                    let res = res.to_binary() as u32;
                    self.reg.p.set_n((res & (((<$ty>::MAX as u32) + 1) / 2)) != 0);
                    <$ty>::as_computation(res)
                } else {
                    let a = $a.value() as u32;
                    let b = $b.value() as u32;
                    let res = a + b + (self.reg.p.c() as u32);
                    self.reg.p.set_v(((a ^ res) & (b ^ res) & (((<$ty>::MAX as u32) + 1) / 2)) != 0);
                    let res = <$ty>::as_computation(res);
                    self.reg.set_p_c(res);
                    self.reg.set_p_n(res);
                    self.reg.set_p_z(res);
                    res
                }
            }};
        }

        macro_rules! sbc {
            ($a: expr, $b: expr, $ty: ident) => {{
                if self.reg.p.d() {
                    // TODO?: overflow flag
                    let a = <$ty>::to_bcd($a.value() as $ty);
                    let b = <$ty>::to_bcd($b.value() as $ty);
                    let zero = &<$ty as TypeOps>::BcdType::ZERO;
                    let one = &<$ty as TypeOps>::BcdType::ONE;
                    let c = if self.reg.p.c() { zero } else { one };
                    let (res, c1) = a.wrapping_add(&b.neg());
                    let (res, c2) = res.wrapping_add(&c.neg());
                    self.reg.p.set_c(c1 || c2);
                    self.reg.p.set_z(res == *zero);
                    self.reg.p.set_v(false);
                    let res = res.to_binary() as u32;
                    self.reg.p.set_n((res & (((<$ty>::MAX as u32) + 1) / 2)) != 0);
                    <$ty>::as_computation(res)
                } else {
                    let a = $a.value() as u32;
                    let b = ($b.value() as u16) as u32;
                    let b = b.wrapping_add(1).wrapping_sub(self.reg.p.c().into());
                    let res = a.wrapping_sub(b);
                    self.reg.p.set_v(((a ^ res) & (!b ^ res) & (((<$ty>::MAX as u32) + 1) / 2)) != 0);
                    let res = <$ty>::as_computation(res);
                    self.reg.p.set_c(b <= a);
                    self.reg.set_p_n(res);
                    self.reg.set_p_z(res);
                    res
                }
            }};
        }

        macro_rules! bitwise_aux {
            (OP2, $op: ident, IMM, $ty: ident) => { fetch_imm!($ty) };
            (OP2, $op: ident, $mode: ident, $ty: ident) => {{
                let val = fetch_effaddr!($op, $mode);
                read_value!(val, $ty)
            }};

            (OP, AND, $v0: expr, $v1: expr, $ty: ident) => { self.instr_and::<$ty>($v0, $v1) };
            (OP, EOR, $v0: expr, $v1: expr, $ty: ident) => { self.instr_eor::<$ty>($v0, $v1) };
            (OP, ORA, $v0: expr, $v1: expr, $ty: ident) => { self.instr_ora::<$ty>($v0, $v1) };

            ($op: ident, $mode: ident, $ty: ident) => {{
                let v0 = self.reg.get_a();
                let v1 = bitwise_aux!(OP2, $op, $mode, $ty);
                let res = bitwise_aux!(OP, $op, v0, v1, $ty);
                self.reg.set_a(res);
            }};
        }

        macro_rules! comp_aux {
            // first operand
            (OP1, CMP) => { self.reg.get_a() };
            (OP1, CPX) => { self.reg.get_x() };
            (OP1, CPY) => { self.reg.get_y() };

            // second operand
            (OP2, $op: ident, IMM, $ty: ident) => { fetch_imm!($ty) };
            (OP2, $op: ident, $mode: ident, $ty: ident) => {{
                let addr = fetch_effaddr!($op, $mode);
                read_value!(addr, $ty)
            }};

            ($op: ident, $mode: ident, $ty: ident) => {{
                let op1 = comp_aux!(OP1, $op);
                let op2 = comp_aux!(OP2, $op, $mode, $ty);
                self.instr_comp::<$ty>(op1, op2);
            }};
        }

        macro_rules! with_computation_type {
            (_X, $macro: ident, $($i: ident),*) => { with_computation_type!(self.reg.p.x(), $macro, $($i),*) };
            (_A, $macro: ident, $($i: ident),*) => { with_computation_type!(self.reg.p.m(), $macro, $($i),*) };

            (CPX, $macro: ident, $($i: ident),*) => { with_computation_type!(_X, $macro, $($i),*) };
            (CPY, $macro: ident, $($i: ident),*) => { with_computation_type!(_X, $macro, $($i),*) };
            (DEX, $macro: ident, $($i: ident),*) => { with_computation_type!(_X, $macro, $($i),*) };
            (DEY, $macro: ident, $($i: ident),*) => { with_computation_type!(_X, $macro, $($i),*) };
            (INX, $macro: ident, $($i: ident),*) => { with_computation_type!(_X, $macro, $($i),*) };
            (INY, $macro: ident, $($i: ident),*) => { with_computation_type!(_X, $macro, $($i),*) };
            (LDX, $macro: ident, $($i: ident),*) => { with_computation_type!(_X, $macro, $($i),*) };
            (LDY, $macro: ident, $($i: ident),*) => { with_computation_type!(_X, $macro, $($i),*) };
            (STX, $macro: ident, $($i: ident),*) => { with_computation_type!(_X, $macro, $($i),*) };
            (STY, $macro: ident, $($i: ident),*) => { with_computation_type!(_X, $macro, $($i),*) };
            (PHX, $macro: ident, $($i: ident),*) => { with_computation_type!(_X, $macro, $($i),*) };
            (PHY, $macro: ident, $($i: ident),*) => { with_computation_type!(_X, $macro, $($i),*) };
            (PLX, $macro: ident, $($i: ident),*) => { with_computation_type!(_X, $macro, $($i),*) };
            (PLY, $macro: ident, $($i: ident),*) => { with_computation_type!(_X, $macro, $($i),*) };

            ($_: ident, $macro: ident, $($i: ident),*) => { with_computation_type!(_A, $macro, $($i),*) };

            ($b: expr, $macro: ident, $($i: ident),*) => {
                if $b {
                    $macro!($($i),*, u8)
                } else {
                    $macro!($($i),*, u16)
                }
            };
        }

        macro_rules! move_block {
            (MVN, $dst: expr, $src: expr) => { self.instr_move_block($dst, $src, 1, rsrc) };
            (MVP, $dst: expr, $src: expr) => { self.instr_move_block($dst, $src, -1, rsrc) };
        }

        let opcode = self.fetch8(rsrc);
        opcode_to_ident!(eat_opcode, opcode);
    }

    pub fn exec_instr_test(&mut self, rsrc: &mut impl Resources) {
        self.exec_instr(rsrc)
    }

    fn interrupt(&mut self, rsrc: &mut impl Resources, interrupt: Interrupt) {
        let vector = match interrupt {
            Interrupt::BRK => {
                if !self.reg.e {
                    0xFFE6
                } else {
                    0xFFFE
                }
            }
            Interrupt::COP => {
                if !self.reg.e {
                    0xFFE4
                } else {
                    0xFFF4
                }
            }
            // SNES doesn't use ABORT
            // Interrupt::ABORT => {
            //     if !self.reg.e {
            //         0x00FFE8
            //     } else {
            //         0x00FFF8
            //     }
            // }
            Interrupt::NMI => {
                if !self.reg.e {
                    0x00FFEA
                } else {
                    0x00FFFA
                }
            }
            Interrupt::IRQ => {
                if !self.reg.e {
                    0x00FFEE
                } else {
                    0x00FFFE
                }
            }
        };
        if !self.reg.e {
            self.push8(self.reg.pb, rsrc);
        }
        self.push16(self.reg.pc, rsrc);
        self.push8(self.reg.p.value(), rsrc);
        self.reg.p.set_d(false);
        self.reg.p.set_i(true);
        let l = rsrc.read(vector) as u16;
        let h = rsrc.read(vector + 1) as u16;
        self.reg.pb = 0x00;
        self.reg.pc = (h << 8) | l
    }
}

#[cfg(test)]
mod tests {
    use crate::cpu::{ComputationValue, Resources, StatusRegister, CPU};
    use crate::emulator::{PendingEvents, NMI};
    use crate::opcode::ident_to_opcode;

    #[derive(Default)]
    struct MockResources {
        history: Vec<(u32, u8)>,
        nmi: NMI,
    }

    impl Resources for MockResources {
        fn tick(&mut self, _v: u8) {
            // do nothing
        }

        fn read(&mut self, addr: u32) -> u8 {
            self.history.iter().rev().filter(|x| x.0 == addr).next().map(|x| x.1).unwrap_or(0)
        }

        fn pending_events_mut(&mut self) -> &mut PendingEvents {
            unimplemented!()
        }

        fn write(&mut self, addr: u32, data: u8) {
            self.history.push((addr, data))
        }

        fn nmi_mut(&mut self) -> &mut NMI {
            &mut self.nmi
        }

        fn debugger_mut(&mut self) -> &mut crate::debug::Debugger {
            todo!()
        }
    }

    fn load_instr(bytes: &Vec<u8>, cpu: &CPU, rsrc: &mut impl Resources) {
        for (i, v) in bytes.iter().enumerate() {
            let addr = ((cpu.reg.pb as u32) << 16) | ((cpu.reg.pc + i as u16) as u32);
            rsrc.write(addr, *v)
        }
    }

    fn program_addr(cpu: &CPU) -> u32 {
        ((cpu.reg.pb as u32) << 16) | (cpu.reg.pc as u32)
    }

    fn cpu16() -> CPU {
        let mut cpu = CPU::default();
        cpu.reg.p.set_m(false);
        cpu.reg.p.set_x(false);
        cpu
    }

    #[test]
    fn test_abs() {
        let rsrc = &mut MockResources::default();
        let cpu = &mut cpu16();
        cpu.reg.db = 0x12;

        // LDA 0xFFFF
        load_instr(&vec![ident_to_opcode!(LDA, ABS), 0xFF, 0xFF], cpu, rsrc);
        rsrc.write(0x12FFFF, 0xAD);
        rsrc.write(0x130000, 0xDE);

        cpu.exec_instr_test(rsrc);

        assert_eq!(cpu.reg.get_a().value(), 0xDEAD);
    }

    #[test]
    fn test_abs_jmp() {
        let rsrc = &mut MockResources::default();
        let cpu = &mut cpu16();
        cpu.reg.pb = 0x12;

        // JMP 0xFFFF
        load_instr(&vec![ident_to_opcode!(JMP, ABS), 0xFF, 0xFF], cpu, rsrc);

        cpu.exec_instr_test(rsrc);

        assert_eq!(program_addr(cpu), 0x12FFFF);
    }

    #[test]
    fn test_abs_x() {
        let rsrc = &mut MockResources::default();
        let cpu = &mut cpu16();
        cpu.reg.db = 0x12;

        // LDA $FFFE,X
        load_instr(&vec![ident_to_opcode!(LDA, ABS_X), 0xFE, 0xFF], cpu, rsrc);
        rsrc.write(0x130008, 0xAD);
        rsrc.write(0x130009, 0xDE);
        cpu.reg.set_x(ComputationValue::Value16(0x000A));

        cpu.exec_instr_test(rsrc);

        assert_eq!(cpu.reg.get_a().value(), 0xDEAD);
    }

    #[test]
    fn test_abs_ind() {
        let rsrc = &mut MockResources::default();
        let cpu = &mut cpu16();
        cpu.reg.pb = 0x12;

        // JMP ($FFFF)
        load_instr(&vec![ident_to_opcode!(JMP, ABS_IND), 0xFF, 0xFF], cpu, rsrc);
        rsrc.write(0x00FFFF, 0x56);
        rsrc.write(0x000000, 0x34);

        cpu.exec_instr_test(rsrc);

        assert_eq!(program_addr(cpu), 0x123456);
    }

    #[test]
    fn test_abs_ind_long() {
        let rsrc = &mut MockResources::default();
        let cpu = &mut cpu16();
        cpu.reg.pb = 0x12;

        // JMP ($FFFF)
        load_instr(&vec![ident_to_opcode!(JMP, ABS_IND_LONG), 0xFF, 0xFF], cpu, rsrc);
        rsrc.write(0x00FFFF, 0x78);
        rsrc.write(0x000000, 0x56);
        rsrc.write(0x000001, 0x34);

        cpu.exec_instr_test(rsrc);

        assert_eq!(program_addr(cpu), 0x345678);
    }

    #[test]
    fn test_abs_x_ind() {
        let rsrc = &mut MockResources::default();
        let cpu = &mut cpu16();
        cpu.reg.pb = 0x12;

        // JMP ($FFFE,X)
        load_instr(&vec![ident_to_opcode!(JMP, ABS_IND_X), 0xFE, 0xFF], cpu, rsrc);
        rsrc.write(0x120008, 0x56);
        rsrc.write(0x120009, 0x34);
        cpu.reg.set_x(ComputationValue::Value16(0x000A));

        cpu.exec_instr_test(rsrc);

        assert_eq!(program_addr(cpu), 0x123456);
    }

    #[test]
    fn test_dir_1() {
        let rsrc = &mut MockResources::default();
        let cpu = &mut cpu16();
        cpu.reg.pb = 0xFF;
        cpu.reg.db = 0x12;

        // LDA $FF
        load_instr(&vec![ident_to_opcode!(LDA, DIR), 0xFF], cpu, rsrc);
        rsrc.write(0x00FFFF, 0xAD);
        rsrc.write(0x000000, 0xDE);
        cpu.reg.e = true;
        cpu.reg.norm_by_e();
        cpu.reg.d = 0xFF00;

        cpu.exec_instr_test(rsrc);

        assert_eq!(cpu.reg.p.m(), true);
        assert_eq!(cpu.reg.get_a(), ComputationValue::Value8(0xAD));
    }

    #[test]
    fn test_dir_2() {
        let rsrc = &mut MockResources::default();
        let cpu = &mut cpu16();
        cpu.reg.pb = 0xFF;
        cpu.reg.db = 0x12;

        // LDA $FF
        load_instr(&vec![ident_to_opcode!(LDA, DIR), 0xFF], cpu, rsrc);
        rsrc.write(0x00FFFF, 0xAD);
        rsrc.write(0x000000, 0xDE);
        cpu.reg.d = 0xFF00;

        cpu.exec_instr_test(rsrc);

        assert_eq!(cpu.reg.p.m(), false);
        assert_eq!(cpu.reg.get_a(), ComputationValue::Value16(0xDEAD));
    }

    #[test]
    fn test_dir_ind_1() {
        let rsrc = &mut MockResources::default();
        let cpu = &mut cpu16();
        cpu.reg.pb = 0xFF;
        cpu.reg.db = 0x12;

        // LDA $(FF)
        load_instr(&vec![ident_to_opcode!(LDA, DIR_IND), 0xFF], cpu, rsrc);
        rsrc.write(0x00FF00, 0xFF);
        rsrc.write(0x00FFFF, 0xFF);
        rsrc.write(0x12FFFF, 0xAD);
        rsrc.write(0x130000, 0xDE);
        cpu.reg.e = true;
        cpu.reg.norm_by_e();
        cpu.reg.d = 0xFF00;

        cpu.exec_instr_test(rsrc);

        assert_eq!(cpu.reg.p.m(), true);
        assert_eq!(cpu.reg.get_a(), ComputationValue::Value8(0xAD));
    }

    #[test]
    fn test_dir_ind_2() {
        let rsrc = &mut MockResources::default();
        let cpu = &mut cpu16();
        cpu.reg.pb = 0xFF;
        cpu.reg.db = 0x12;

        // LDA $(FF)
        load_instr(&vec![ident_to_opcode!(LDA, DIR_IND), 0xFF], cpu, rsrc);
        rsrc.write(0x000000, 0xFF);
        rsrc.write(0x00FFFF, 0xFF);
        rsrc.write(0x12FFFF, 0xAD);
        rsrc.write(0x130000, 0xDE);
        cpu.reg.d = 0xFF00;

        cpu.exec_instr_test(rsrc);

        assert_eq!(cpu.reg.p.m(), false);
        assert_eq!(cpu.reg.get_a(), ComputationValue::Value16(0xDEAD));
    }

    #[test]
    fn test_dir_ind_long() {
        let rsrc = &mut MockResources::default();
        let cpu = &mut cpu16();
        cpu.reg.pb = 0xFF;

        // LDA $[FE]
        load_instr(&vec![ident_to_opcode!(LDA, DIR_IND_LONG), 0xFE], cpu, rsrc);
        rsrc.write(0x000000, 0x12);
        rsrc.write(0x00FFFE, 0xFF);
        rsrc.write(0x00FFFF, 0xFF);
        rsrc.write(0x12FFFF, 0xAD);
        rsrc.write(0x130000, 0xDE);
        cpu.reg.d = 0xFF00;

        cpu.exec_instr_test(rsrc);

        assert_eq!(cpu.reg.p.m(), false);
        assert_eq!(cpu.reg.get_a(), ComputationValue::Value16(0xDEAD));
    }

    #[test]
    fn test_dir_x_ind_1() {
        let rsrc = &mut MockResources::default();
        let cpu = &mut cpu16();
        cpu.reg.pb = 0xFF;
        cpu.reg.db = 0x12;

        // LDA $(FE,X)
        load_instr(&vec![ident_to_opcode!(LDA, DIR_X_IND), 0xFE], cpu, rsrc);
        rsrc.write(0x00FF08, 0xFF);
        rsrc.write(0x00FF09, 0xFF);
        rsrc.write(0x12FFFF, 0xAD);
        rsrc.write(0x130000, 0xDE);
        cpu.reg.set_x(ComputationValue::Value8(0x000A));
        cpu.reg.e = true;
        cpu.reg.norm_by_e();
        cpu.reg.d = 0xFF00;

        cpu.exec_instr_test(rsrc);

        assert_eq!(cpu.reg.p.m(), true);
        assert_eq!(cpu.reg.get_a(), ComputationValue::Value8(0xAD));
    }

    #[test]
    fn test_dir_x_ind_2() {
        let rsrc = &mut MockResources::default();
        let cpu = &mut cpu16();
        cpu.reg.pb = 0xFF;
        cpu.reg.db = 0x12;

        // LDA $(FE,X)
        load_instr(&vec![ident_to_opcode!(LDA, DIR_X_IND), 0xFE], cpu, rsrc);
        rsrc.write(0x000008, 0xFF);
        rsrc.write(0x000009, 0xFF);
        rsrc.write(0x12FFFF, 0xAD);
        rsrc.write(0x130000, 0xDE);
        cpu.reg.set_x(ComputationValue::Value16(0x000A));
        cpu.reg.d = 0xFF00;

        cpu.exec_instr_test(rsrc);

        assert_eq!(cpu.reg.p.m(), false);
        assert_eq!(cpu.reg.get_a(), ComputationValue::Value16(0xDEAD));
    }

    #[test]
    fn test_dir_ind_y_1() {
        let rsrc = &mut MockResources::default();
        let cpu = &mut cpu16();
        cpu.reg.pb = 0xFF;
        cpu.reg.db = 0x12;

        // LDA $(FE),Y
        load_instr(&vec![ident_to_opcode!(LDA, DIR_IND_Y), 0xFF], cpu, rsrc);
        rsrc.write(0x00FF00, 0xFF);
        rsrc.write(0x00FFFF, 0xFE);
        rsrc.write(0x130008, 0xAD);
        rsrc.write(0x130009, 0xDE);
        cpu.reg.set_y(ComputationValue::Value8(0x000A));
        cpu.reg.e = true;
        cpu.reg.norm_by_e();
        cpu.reg.d = 0xFF00;

        cpu.exec_instr_test(rsrc);

        assert_eq!(cpu.reg.p.m(), true);
        assert_eq!(cpu.reg.get_a(), ComputationValue::Value8(0xAD));
    }

    #[test]
    fn test_dir_ind_y_2() {
        let rsrc = &mut MockResources::default();
        let cpu = &mut cpu16();
        cpu.reg.pb = 0xFF;
        cpu.reg.db = 0x12;

        // LDA $(FE),Y
        load_instr(&vec![ident_to_opcode!(LDA, DIR_IND_Y), 0xFF], cpu, rsrc);
        rsrc.write(0x000000, 0xFF);
        rsrc.write(0x00FFFF, 0xFE);
        rsrc.write(0x130008, 0xAD);
        rsrc.write(0x130009, 0xDE);
        cpu.reg.set_y(ComputationValue::Value16(0x000A));
        cpu.reg.d = 0xFF00;

        cpu.exec_instr_test(rsrc);

        assert_eq!(cpu.reg.p.m(), false);
        assert_eq!(cpu.reg.get_a(), ComputationValue::Value16(0xDEAD));
    }

    #[test]
    fn test_dir_ind_long_y() {
        let rsrc = &mut MockResources::default();
        let cpu = &mut cpu16();
        cpu.reg.pb = 0xFF;

        // LDA $[FE],Y
        load_instr(&vec![ident_to_opcode!(LDA, DIR_IND_LONG_Y), 0xFE], cpu, rsrc);
        rsrc.write(0x000000, 0x12);
        rsrc.write(0x00FFFE, 0xFC);
        rsrc.write(0x00FFFF, 0xFF);
        rsrc.write(0x130006, 0xAD);
        rsrc.write(0x130007, 0xDE);
        cpu.reg.set_y(ComputationValue::Value16(0x000A));
        cpu.reg.d = 0xFF00;

        cpu.exec_instr_test(rsrc);

        assert_eq!(cpu.reg.p.m(), false);
        assert_eq!(cpu.reg.get_a(), ComputationValue::Value16(0xDEAD));
    }

    #[test]
    fn test_stk_s() {
        let rsrc = &mut MockResources::default();
        let cpu = &mut cpu16();
        cpu.reg.pb = 0xFF;

        // LDA $FA,S
        load_instr(&vec![ident_to_opcode!(LDA, STK_S), 0xFA], cpu, rsrc);
        rsrc.write(0x00000A, 0xAD);
        rsrc.write(0x00000B, 0xDE);
        cpu.reg.s = 0xFF10;

        cpu.exec_instr_test(rsrc);

        assert_eq!(cpu.reg.get_a(), ComputationValue::Value16(0xDEAD));
    }

    #[test]
    fn test_stk_s_y() {
        let rsrc = &mut MockResources::default();
        let cpu = &mut cpu16();
        cpu.reg.pb = 0xFF;
        cpu.reg.db = 0x12;

        // LDA $(FA,S),Y
        load_instr(&vec![ident_to_opcode!(LDA, STK_S_Y), 0xFA], cpu, rsrc);
        rsrc.write(0x00000A, 0xF0);
        rsrc.write(0x00000B, 0xFF);
        rsrc.write(0x130040, 0xAD);
        rsrc.write(0x130041, 0xDE);
        cpu.reg.s = 0xFF10;
        cpu.reg.set_y(ComputationValue::Value16(0x0050));

        cpu.exec_instr_test(rsrc);

        assert_eq!(cpu.reg.get_a(), ComputationValue::Value16(0xDEAD));
    }

    // 0x0001 + 0x7FFF
    #[test]
    fn test_adc_1() {
        let rsrc = &mut MockResources::default();
        let cpu = &mut cpu16();

        // ADC #$7FFF
        load_instr(&vec![ident_to_opcode!(ADC, IMM), 0xFF, 0x7F], cpu, rsrc);
        cpu.reg.set_a(ComputationValue::Value16(0x0001));

        cpu.exec_instr_test(rsrc);

        assert_eq!(cpu.reg.p.n(), true);
        assert_eq!(cpu.reg.p.v(), true);
        assert_eq!(cpu.reg.p.z(), false);
        assert_eq!(cpu.reg.p.c(), false);
        assert_eq!(cpu.reg.get_a(), ComputationValue::Value16(0x8000));
    }

    // 0x0000 + 0x7FFF + 0x0001
    #[test]
    fn test_adc_2() {
        let rsrc = &mut MockResources::default();
        let cpu = &mut cpu16();

        // ADC #$7FFF
        load_instr(&vec![ident_to_opcode!(ADC, IMM), 0xFF, 0x7F], cpu, rsrc);
        cpu.reg.set_a(ComputationValue::Value16(0x0000));
        cpu.reg.p.set_c(true);

        cpu.exec_instr_test(rsrc);

        assert_eq!(cpu.reg.p.n(), true);
        assert_eq!(cpu.reg.p.v(), true);
        assert_eq!(cpu.reg.p.z(), false);
        assert_eq!(cpu.reg.p.c(), false);
        assert_eq!(cpu.reg.get_a(), ComputationValue::Value16(0x8000));
    }

    // 0x0000 + 0x7FFF
    #[test]
    fn test_adc_3() {
        let rsrc = &mut MockResources::default();
        let cpu = &mut cpu16();

        // ADC #$7FFF
        load_instr(&vec![ident_to_opcode!(ADC, IMM), 0xFF, 0x7F], cpu, rsrc);
        cpu.reg.set_a(ComputationValue::Value16(0x0000));

        cpu.exec_instr_test(rsrc);

        assert_eq!(cpu.reg.p.n(), false);
        assert_eq!(cpu.reg.p.v(), false);
        assert_eq!(cpu.reg.p.z(), false);
        assert_eq!(cpu.reg.p.c(), false);
        assert_eq!(cpu.reg.get_a(), ComputationValue::Value16(0x7FFF));
    }

    // 0x8000 + 0x7FFF + 0x0001
    #[test]
    fn test_adc_4() {
        let rsrc = &mut MockResources::default();
        let cpu = &mut cpu16();

        // ADC #$7FFF
        load_instr(&vec![ident_to_opcode!(ADC, IMM), 0xFF, 0x7F], cpu, rsrc);
        cpu.reg.set_a(ComputationValue::Value16(0x8000));
        cpu.reg.p.set_c(true);

        cpu.exec_instr_test(rsrc);

        assert_eq!(cpu.reg.p.n(), false);
        assert_eq!(cpu.reg.p.v(), false);
        assert_eq!(cpu.reg.p.z(), true);
        assert_eq!(cpu.reg.p.c(), true);
        assert_eq!(cpu.reg.get_a(), ComputationValue::Value16(0x0000));
    }

    // 0x7FFF + 0x7FFF + 0x0001
    #[test]
    fn test_adc_5() {
        let rsrc = &mut MockResources::default();
        let cpu = &mut cpu16();

        // ADC #$7FFF
        load_instr(&vec![ident_to_opcode!(ADC, IMM), 0xFF, 0x7F], cpu, rsrc);
        cpu.reg.set_a(ComputationValue::Value16(0x7FFF));
        cpu.reg.p.set_c(true);

        cpu.exec_instr_test(rsrc);

        assert_eq!(cpu.reg.p.n(), true);
        assert_eq!(cpu.reg.p.v(), true);
        assert_eq!(cpu.reg.p.z(), false);
        assert_eq!(cpu.reg.p.c(), false);
        assert_eq!(cpu.reg.get_a(), ComputationValue::Value16(0xFFFF));
    }

    #[test]
    fn test_sbc_1() {
        let rsrc = &mut MockResources::default();
        let cpu = &mut cpu16();

        // SBC #$2003
        load_instr(&vec![ident_to_opcode!(SBC, IMM), 0x03, 0x20], cpu, rsrc);
        cpu.reg.p.set_c(true);
        cpu.reg.set_a(ComputationValue::Value16(0x0001));
        cpu.reg.p.set_z(true); // for test

        cpu.exec_instr_test(rsrc);

        assert_eq!(cpu.reg.p.n(), true);
        assert_eq!(cpu.reg.p.v(), false);
        assert_eq!(cpu.reg.p.z(), false);
        assert_eq!(cpu.reg.p.c(), false);
        assert_eq!(cpu.reg.get_a(), ComputationValue::Value16(0xDFFE));
    }

    #[test]
    fn test_sbc_2() {
        let rsrc = &mut MockResources::default();
        let cpu = &mut cpu16();

        // SBC #$2003
        load_instr(&vec![ident_to_opcode!(SBC, IMM), 0x03, 0x20], cpu, rsrc);
        cpu.reg.p.set_c(true);
        cpu.reg.p.set_d(true);
        cpu.reg.set_a(ComputationValue::Value16(0x0001));
        cpu.reg.p.set_z(true); // for test

        cpu.exec_instr_test(rsrc);

        assert_eq!(cpu.reg.p.n(), false);
        // assert_eq!(cpu.reg.p.v(), false);
        assert_eq!(cpu.reg.p.z(), false);
        assert_eq!(cpu.reg.p.c(), false);
        assert_eq!(cpu.reg.get_a(), ComputationValue::Value16(0x7998));
    }

    #[test]
    fn test_cmp1() {
        let rsrc = &mut MockResources::default();
        let cpu = &mut cpu16();

        // CMP #$1234
        load_instr(&vec![ident_to_opcode!(CMP, IMM), 0x34, 0x12], cpu, rsrc);
        cpu.reg.set_a(ComputationValue::Value16(0x1234));
        cpu.reg.p.set_c(true);

        cpu.exec_instr_test(rsrc);

        assert_eq!(cpu.reg.p.n(), false);
        assert_eq!(cpu.reg.p.v(), false);
        assert_eq!(cpu.reg.p.z(), true);
        assert_eq!(cpu.reg.p.c(), true);
        assert_eq!(cpu.reg.get_a(), ComputationValue::Value16(0x1234));
    }

    #[test]
    fn test_cmp2() {
        let rsrc = &mut MockResources::default();
        let cpu = &mut cpu16();

        // CMP #$35
        load_instr(&vec![ident_to_opcode!(CMP, IMM), 0x35, 0x12], cpu, rsrc);
        cpu.reg.set_a(ComputationValue::Value16(0x1234));
        cpu.reg.e = true;
        cpu.reg.norm_by_e();
        cpu.reg.p.set_v(true);
        cpu.reg.p.set_c(true);
        cpu.reg.p.set_z(true); // for test

        cpu.exec_instr_test(rsrc);

        assert_eq!(cpu.reg.p.n(), true);
        assert_eq!(cpu.reg.p.v(), true);
        assert_eq!(cpu.reg.p.z(), false);
        assert_eq!(cpu.reg.p.c(), false);
        assert_eq!(cpu.reg.get_a().value(), 0x34);
    }

    #[test]
    fn test_inx() {
        let rsrc = &mut MockResources::default();
        let cpu = &mut cpu16();

        // INX #$35
        load_instr(&vec![ident_to_opcode!(INX, IMPLIED)], cpu, rsrc);
        cpu.reg.set_x(ComputationValue::Value16(0x7FFF));
        cpu.reg.p.set_z(true); // for test

        cpu.exec_instr_test(rsrc);

        assert_eq!(cpu.reg.p.n(), true);
        assert_eq!(cpu.reg.p.z(), false);
        assert_eq!(cpu.reg.get_x(), ComputationValue::Value16(0x8000));
    }

    #[test]
    fn test_eor() {
        let rsrc = &mut MockResources::default();
        let cpu = &mut cpu16();

        // EOR #$F103
        load_instr(&vec![ident_to_opcode!(EOR, IMM), 0x03, 0xF1], cpu, rsrc);
        cpu.reg.set_a(ComputationValue::Value16(0x0F06));
        cpu.reg.p.set_z(true); // for test

        cpu.exec_instr_test(rsrc);

        assert_eq!(cpu.reg.p.n(), true);
        assert_eq!(cpu.reg.p.z(), false);
        assert_eq!(cpu.reg.get_a(), ComputationValue::Value16(0xFE05));
    }

    #[test]
    fn test_bit() {
        let rsrc = &mut MockResources::default();
        let cpu = &mut cpu16();
        cpu.reg.db = 0x12;

        // BIT $ABCD
        load_instr(&vec![ident_to_opcode!(BIT, ABS), 0xCD, 0xAB], cpu, rsrc);
        rsrc.write(0x12ABCD, 0x9C);
        cpu.reg.set_a(ComputationValue::Value8(0x43));
        cpu.reg.p.set_m(true);

        cpu.exec_instr_test(rsrc);

        assert_eq!(cpu.reg.p.n(), true);
        assert_eq!(cpu.reg.p.v(), false);
        assert_eq!(cpu.reg.p.z(), true);
        assert_eq!(cpu.reg.get_a(), ComputationValue::Value8(0x43));
    }

    #[test]
    fn test_tsb() {
        let rsrc = &mut MockResources::default();
        let cpu = &mut cpu16();
        cpu.reg.db = 0x12;

        // TSB $ABCD
        load_instr(&vec![ident_to_opcode!(TSB, ABS), 0xCD, 0xAB], cpu, rsrc);
        rsrc.write(0x12ABCD, 0x9C);
        cpu.reg.set_a(ComputationValue::Value8(0x43));
        cpu.reg.p.set_m(true);

        cpu.exec_instr_test(rsrc);

        assert_eq!(cpu.reg.p.z(), true);
        assert_eq!(cpu.reg.get_a(), ComputationValue::Value8(0x43));
        assert_eq!(rsrc.read(0x12ABCD), 0xDF);
    }

    #[test]
    fn test_asl() {
        let rsrc = &mut MockResources::default();
        let cpu = &mut cpu16();
        cpu.reg.db = 0x12;

        // ASL $ABCD
        load_instr(&vec![ident_to_opcode!(ASL, ABS), 0xCD, 0xAB], cpu, rsrc);
        rsrc.write(0x12ABCD, 0x8F);
        cpu.reg.p.set_m(true);
        cpu.reg.p.set_z(true);
        cpu.reg.p.set_c(false); // for test

        cpu.exec_instr_test(rsrc);

        assert_eq!(cpu.reg.p.n(), false);
        assert_eq!(cpu.reg.p.z(), false);
        assert_eq!(cpu.reg.p.c(), true);
        assert_eq!(rsrc.read(0x12ABCD), 0x1E);
    }

    #[test]
    fn test_bne() {
        let rsrc = &mut MockResources::default();
        let cpu = &mut cpu16();
        cpu.reg.pb = 0xAB;
        cpu.reg.pc = 0xC000;

        // BNE $C023
        // $ABC023 - ($ABC000 + 2) = $21
        load_instr(&vec![ident_to_opcode!(BNE, REL8), 0x21], cpu, rsrc);

        cpu.exec_instr_test(rsrc);

        assert_eq!(program_addr(cpu), 0xABC023);
    }

    #[test]
    fn test_brl() {
        let rsrc = &mut MockResources::default();
        let cpu = &mut cpu16();
        cpu.reg.pb = 0xAB;
        cpu.reg.pc = 0xC000;

        // BRL $C023
        // $ABC045 - ($ABC000 + 3) = $42
        load_instr(&vec![ident_to_opcode!(BRL, REL16), 0x42, 0x00], cpu, rsrc);

        cpu.exec_instr_test(rsrc);

        assert_eq!(program_addr(cpu), 0xABC045);
    }

    #[test]
    fn test_rts() {
        let rsrc = &mut MockResources::default();
        let cpu = &mut cpu16();
        cpu.reg.pb = 0x12;

        // RTS
        load_instr(&vec![ident_to_opcode!(RTS, IMPLIED)], cpu, rsrc);
        rsrc.write(0x0001FE, 0x56);
        rsrc.write(0x0001FF, 0x34);
        cpu.reg.s = 0x01FD;

        cpu.exec_instr_test(rsrc);

        assert_eq!(program_addr(cpu), 0x123457);
        assert_eq!(cpu.reg.s, 0x01FF);
    }

    #[test]
    fn test_brk() {
        let rsrc = &mut MockResources::default();
        let cpu = &mut cpu16();
        cpu.reg.pb = 0x12;
        cpu.reg.pc = 0x3456;

        // BRK
        load_instr(&vec![ident_to_opcode!(BRK, IMPLIED)], cpu, rsrc);
        rsrc.write(0x00FFE6, 0xAB);
        rsrc.write(0x00FFE7, 0xCD);
        cpu.reg.s = 0x01FF;
        cpu.reg.p = StatusRegister::from(0x08);
        assert_eq!(cpu.reg.p.d(), true);

        cpu.exec_instr_test(rsrc);

        assert_eq!(rsrc.read(0x0001FF), 0x12);
        assert_eq!(rsrc.read(0x0001FE), 0x34);
        assert_eq!(rsrc.read(0x0001FD), 0x58);
        assert_eq!(rsrc.read(0x0001FC), 0x08);
        assert_eq!(program_addr(cpu), 0x00CDAB);
        assert_eq!(cpu.reg.s, 0x01FB);
        assert_eq!(cpu.reg.p.d(), false);
        assert_eq!(cpu.reg.p.i(), true);
    }

    #[test]
    fn test_rti() {
        let rsrc = &mut MockResources::default();
        let cpu = &mut cpu16();

        // RTI
        load_instr(&vec![ident_to_opcode!(RTI, IMPLIED)], cpu, rsrc);
        rsrc.write(0x0001FC, 0x08);
        rsrc.write(0x0001FD, 0x12);
        rsrc.write(0x0001FE, 0x34);
        rsrc.write(0x0001FF, 0x56);
        cpu.reg.s = 0x01FB;

        cpu.exec_instr_test(rsrc);

        assert_eq!(program_addr(cpu), 0x563412);
        assert_eq!(cpu.reg.s, 0x01FF);
        assert_eq!(u8::from(cpu.reg.p), 0x08);
    }

    #[test]
    fn test_cls() {
        let rsrc = &mut MockResources::default();
        let cpu = &mut cpu16();

        // CLC
        load_instr(&vec![ident_to_opcode!(CLC, IMPLIED)], cpu, rsrc);
        cpu.reg.p.set_c(true);

        cpu.exec_instr_test(rsrc);

        assert_eq!(cpu.reg.p.c(), false);
    }

    #[test]
    fn test_sep() {
        let rsrc = &mut MockResources::default();
        let cpu = &mut cpu16();

        // SEP #$21
        load_instr(&vec![ident_to_opcode!(SEP, IMM), 0x21], cpu, rsrc);

        cpu.exec_instr_test(rsrc);

        let p = cpu.reg.p;
        assert_eq!(p.m(), true);
        assert_eq!(p.c(), true);
        assert_eq!(p.n(), false);
        assert_eq!(p.v(), false);
        assert_eq!(p.x(), false);
        assert_eq!(p.d(), false);
        assert_eq!(p.i(), true);
        assert_eq!(p.z(), false);
    }

    #[test]
    fn test_lda() {
        let rsrc = &mut MockResources::default();
        let cpu = &mut cpu16();

        // LDA $123456
        load_instr(&vec![ident_to_opcode!(LDA, LONG), 0x56, 0x34, 0x12], cpu, rsrc);
        rsrc.write(0x123456, 0xAB);
        rsrc.write(0x123457, 0xCD);

        cpu.exec_instr_test(rsrc);

        assert_eq!(cpu.reg.get_a(), ComputationValue::Value16(0xCDAB));
        assert_eq!(cpu.reg.p.n(), true);
        assert_eq!(cpu.reg.p.z(), false);
    }

    #[test]
    fn test_mvp() {
        let rsrc = &mut MockResources::default();
        let cpu = &mut cpu16();

        // MVP #0,#0
        load_instr(&vec![ident_to_opcode!(MVP, SRC_DST), 0x00, 0x00], cpu, rsrc);
        rsrc.write(0x000FFF, 0xAB);
        rsrc.write(0x001000, 0xCD);
        cpu.reg.set_a(ComputationValue::Value16(0x0001));
        cpu.reg.set_x(ComputationValue::Value16(0x1000));
        cpu.reg.set_y(ComputationValue::Value16(0x2000));

        cpu.exec_instr_test(rsrc);
        cpu.exec_instr_test(rsrc);

        assert_eq!(cpu.reg.get_a(), ComputationValue::Value16(0xFFFF));
        assert_eq!(cpu.reg.get_x(), ComputationValue::Value16(0x0FFE));
        assert_eq!(cpu.reg.get_y(), ComputationValue::Value16(0x1FFE));
        assert_eq!(cpu.reg.db, 0x00);
        assert_eq!(rsrc.read(0x001FFF), 0xAB);
        assert_eq!(rsrc.read(0x002000), 0xCD);
    }

    #[test]
    fn test_pla() {
        let rsrc = &mut MockResources::default();
        let cpu = &mut cpu16();

        // PLA
        load_instr(&vec![ident_to_opcode!(PLA, IMPLIED)], cpu, rsrc);
        rsrc.write(0x0001FE, 0xAB);
        rsrc.write(0x0001FF, 0xCD);
        cpu.reg.s = 0x01FD;
        cpu.reg.p.set_z(true); // for test

        cpu.exec_instr_test(rsrc);

        assert_eq!(cpu.reg.get_a(), ComputationValue::Value16(0xCDAB));
        assert_eq!(cpu.reg.p.n(), true);
        assert_eq!(cpu.reg.p.z(), false);
        assert_eq!(cpu.reg.s, 0x01FF);
    }

    #[test]
    fn test_pld() {
        let rsrc = &mut MockResources::default();
        let cpu = &mut cpu16();

        // PLD
        load_instr(&vec![ident_to_opcode!(PLD, IMPLIED)], cpu, rsrc);
        rsrc.write(0x0001FE, 0xAB);
        rsrc.write(0x0001FF, 0xCD);
        cpu.reg.s = 0x01FD;
        cpu.reg.p.set_z(true); // for test

        cpu.exec_instr_test(rsrc);

        assert_eq!(cpu.reg.d, 0xCDAB);
        assert_eq!(cpu.reg.p.n(), true);
        assert_eq!(cpu.reg.p.z(), false);
        assert_eq!(cpu.reg.s, 0x01FF);
    }

    #[test]
    fn test_tax() {
        let rsrc = &mut MockResources::default();
        let cpu = &mut cpu16();

        // TXA
        load_instr(&vec![ident_to_opcode!(TXA, IMPLIED)], cpu, rsrc);
        cpu.reg.set_a(ComputationValue::Value16(0x1234));
        cpu.reg.set_x(ComputationValue::Value16(0xABCD));
        cpu.reg.p.set_m(true);
        cpu.reg.p.set_z(true); // for test

        cpu.exec_instr_test(rsrc);

        assert_eq!(cpu.reg.get_c(), 0x12CD);
        assert_eq!(cpu.reg.p.n(), true);
        assert_eq!(cpu.reg.p.z(), false);
    }

    #[test]
    fn test_tdc() {
        let rsrc = &mut MockResources::default();
        let cpu = &mut cpu16();

        // TDC
        load_instr(&vec![ident_to_opcode!(TDC, IMPLIED)], cpu, rsrc);
        cpu.reg.d = 0x1234;
        cpu.reg.set_a(ComputationValue::Value16(0x0000)); // for test
        cpu.reg.p.set_n(true); // for test
        cpu.reg.p.set_z(true); // for test

        cpu.exec_instr_test(rsrc);

        assert_eq!(cpu.reg.get_a(), ComputationValue::Value16(0x1234));
        assert_eq!(cpu.reg.p.n(), false);
        assert_eq!(cpu.reg.p.z(), false);
    }

    #[test]
    fn test_xba() {
        let rsrc = &mut MockResources::default();
        let cpu = &mut cpu16();

        // XBA
        load_instr(&vec![ident_to_opcode!(XBA, IMPLIED)], cpu, rsrc);
        cpu.reg.set_a(ComputationValue::Value16(0x6789));
        cpu.reg.p.set_n(true); // for test
        cpu.reg.p.set_z(true); // for test

        cpu.exec_instr_test(rsrc);

        assert_eq!(cpu.reg.get_c(), 0x8967);
        assert_eq!(cpu.reg.p.n(), false);
        assert_eq!(cpu.reg.p.z(), false);
    }

    #[test]
    fn test_xce() {
        let rsrc = &mut MockResources::default();
        let cpu = &mut cpu16();

        // XCE
        load_instr(&vec![ident_to_opcode!(XCE, IMPLIED)], cpu, rsrc);
        cpu.reg.set_a(ComputationValue::Value16(0x6789));
        cpu.reg.p.set_c(false);
        cpu.reg.e = true;

        cpu.exec_instr_test(rsrc);

        assert_eq!(cpu.reg.p.c(), true);
        assert_eq!(cpu.reg.e, false);
    }
}
