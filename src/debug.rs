use crate::cpu;
use crate::opcode::opcode_to_ident;
use std::collections::HashSet;

use lazy_static::lazy_static;
use regex::Regex;

pub fn debug_dump(v: &Vec<u8>, span: usize) {
    let mut i = 0;
    let mut finished = false;
    while !finished {
        let mut vec = Vec::new();
        for j in 0..span {
            if i + j == v.len() {
                finished = true;
                break;
            }
            vec.push(format!("{:#04X}", v[i]));
            i += 1;
        }
        println!("[{}]", vec.join(", "));
    }
}

#[derive(Default)]
pub struct Debugger {
    pb: u8,
    pc: u16,
    fetched: [Option<u8>; 4],
    fetched_idx: usize,
    instr_len: isize,

    breaks: HashSet<u32>,
    interrupting: bool,
}

impl Debugger {
    pub fn instr_prolog(&mut self, pb: u8, pc: u16) {
        self.pc = pc;
        self.pb = pb;
        self.instr_len = 0;
        self.fetched_idx = 0;
        self.fetched.fill(None);
    }

    pub fn tell_fetched(&mut self, v: u8) {
        self.fetched[self.fetched_idx] = Some(v);
        self.fetched_idx += 1;
    }

    fn fetched8(&self) -> Option<u8> {
        self.fetched[1]
    }

    fn fetched16(&self) -> Option<u16> {
        let lo = (self.fetched[1]?) as u16;
        let hi = (self.fetched[2]?) as u16;
        Some((hi << 8) | lo)
    }

    fn fetched24(&self) -> Option<u32> {
        let lo = (self.fetched[1]?) as u32;
        let mi = (self.fetched[2]?) as u32;
        let hi = (self.fetched[3]?) as u32;
        Some((hi << 16) | (mi << 8) | lo)
    }

    pub fn get_instr(&self) -> Option<String> {
        let opcode = self.fetched[0]?;
        let v8 = self.fetched8();
        let v16 = self.fetched16();
        let v24 = self.fetched24();
        macro_rules! stringify_addrmode {
            (ABS) => { format!("{:#06X}", v16?) };
            (ABS_X) => { format!("{:#06X},X", v16?) };
            (ABS_Y) => { format!("{:#06X},Y", v16?) };
            (ABS_IND) => { format!("({:#06X})", v16?) };
            (ABS_IND_LONG) => { format!("[{:#06X}]", v16?) };
            (ABS_IND_X) => { format!("({:#06X},X)", v16?) };
            (ACC) => { String::from("") };
            (DIR) => { format!("{:#04X}", v8?) };
            (DIR_X) => { format!("{:#04X},X", v8?) };
            (DIR_Y) => { format!("{:#04X},Y", v8?) };
            (DIR_IND) => { format!("({:#04X})", v8?) };
            (DIR_IND_LONG) => { format!("[{:#04X}]", v8?) };
            (DIR_X_IND) => { format!("({:#04X},X)", v8?) };
            (DIR_IND_Y) => { format!("({:#04X}),Y", v8?) };
            (DIR_IND_LONG_Y) => { format!("[{:#04X}],Y", v8?) };
            (IMM) => {
                if self.fetched_idx == 2 {
                    format!("#{:#04X}", v8?)
                } else {
                    format!("#{:#06X}", v16?)
                }
            };
            (IMPLIED) => { String::from("") };
            (LONG) => { format!("{:#08X}", v24?) };
            (LONG_X) => { format!("{:#08X},X", v24?) };
            (REL8) => { format!("{:#04X}", v8?) };
            (REL16) => { format!("{:#06X}", v16?) };
            (SRC_DST) => { format!("{:#04X},{:#04X}", self.fetched[0]?, self.fetched[1]?) };
            (STK_S) => { format!("{:#04X},S", v8?) };
            (STK_S_Y) => { format!("({:#04X},S),Y", v16?) };
        }

        macro_rules! debug_instr {
            ($opcode: ident, $mode: ident) => {{
                let opcode = stringify!($opcode);
                let raw_mode = stringify!($mode);
                let mode = stringify_addrmode!($mode);
                format!("({})\t{}\t{}", raw_mode, opcode, mode)
            }};
        }

        Some(opcode_to_ident!(debug_instr, opcode))
    }

    pub fn get_instr_addr(&self) -> u32 {
        ((self.pb as u32) << 16) | (self.pc as u32)
    }
}

pub trait Emulator {
    fn cpu_regs(&self) -> cpu::Regfile;
    fn read_mem(&mut self, addr: u32) -> u8;
}

enum Printable {
    Regs,
    Mem(u32),
}

fn parse_hex(s: &str) -> Option<u32> {
    u32::from_str_radix(s.trim_start_matches("0x"), 16).ok()
}

impl Printable {
    fn parse(s: &str) -> Result<Self, String> {
        if s == "regs" {
            Ok(Printable::Regs)
        } else {
            Self::parse_mem(s).map(|e| Printable::Mem(e)).ok_or(s.to_owned())
        }
    }

    fn parse_mem(s: &str) -> Option<u32> {
        lazy_static! {
            static ref RE: Regex = Regex::new(r"mem\[(0x[0-9a-fA-F]+)\]").unwrap();
        }
        let addr = RE.captures(s)?;
        let addr = addr.get(1)?;
        let addr = parse_hex(addr.as_str())?;
        Some(addr)
    }
}

enum Command {
    Step,
    Continue,
    Break(u32),
    Print(Printable),
}

impl Command {
    fn parse(s: &str) -> Result<Self, String> {
        lazy_static! {
            static ref SPACES: Regex = Regex::new(r"[ \t]+").unwrap();
        }
        let tokens: Vec<&str> = SPACES.split(s).collect();
        match tokens[0] {
            "b" if tokens.len() == 2 => match parse_hex(tokens[1]) {
                Some(addr) => Ok(Command::Break(addr)),
                None => Err("invalid address".to_string()),
            },
            "c" => Ok(Command::Continue),
            "s" => Ok(Command::Step),
            "p" if tokens.len() == 2 => {
                let p = Printable::parse(tokens[1])?;
                Ok(Command::Print(p))
            }
            "q" => panic!("quite"),
            _ => Err("unknown options".to_string()),
        }
    }
}

pub enum Control {
    Print(String),
    Exit,
}

impl Debugger {
    pub fn enable(&mut self) {
        self.interrupting = true
    }

    pub fn is_interrupting(&mut self) -> bool {
        let npc = ((self.pb as u32) << 16) | (self.pc as u32);
        if self.breaks.contains(&npc) {
            self.interrupting = true;
        }
        self.interrupting
    }

    pub fn exec(&mut self, s: &str, emu: &mut impl Emulator) -> Control {
        match Command::parse(s) {
            Ok(cmd) => match cmd {
                Command::Break(addr) => {
                    self.breaks.insert(addr);
                    Control::Print(format!("break: {:#08X}", addr))
                }
                Command::Step => Control::Exit,
                Command::Continue => {
                    self.interrupting = false;
                    Control::Exit
                }
                Command::Print(p) => Control::Print(match p {
                    Printable::Regs => format!("{:?}", emu.cpu_regs()),
                    Printable::Mem(addr) => {
                        println!("addr={:#08X}", addr);
                        format!("{:#04X}", emu.read_mem(addr))
                    }
                }),
            },
            Err(s) => Control::Print(s),
        }
    }
}
