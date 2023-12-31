// Generated code
macro_rules! decode {
    ($opcode: expr, $cb: ident) => {
        match $opcode {
            0xE8 => { $cb!(MOV, A, imm) },
            0xE6 => { $cb!(MOV, A, X ind) },
            0xBF => { $cb!(MOV, A, X ind inc) },
            0xE4 => { $cb!(MOV, A, dp) },
            0xF4 => { $cb!(MOV, A, dp X p) },
            0xE5 => { $cb!(MOV, A, abs) },
            0xF5 => { $cb!(MOV, A, abs X p) },
            0xF6 => { $cb!(MOV, A, abs Y p) },
            0xE7 => { $cb!(MOV, A, dp X p ind) },
            0xF7 => { $cb!(MOV, A, dp ind Y p) },
            0xCD => { $cb!(MOV, X, imm) },
            0xF8 => { $cb!(MOV, X, dp) },
            0xF9 => { $cb!(MOV, X, dp Y p) },
            0xE9 => { $cb!(MOV, X, abs) },
            0x8D => { $cb!(MOV, Y, imm) },
            0xEB => { $cb!(MOV, Y, dp) },
            0xFB => { $cb!(MOV, Y, dp X p) },
            0xEC => { $cb!(MOV, Y, abs) },
            0xC6 => { $cb!(MOV, X ind, A) },
            0xAF => { $cb!(MOV, X ind inc, A) },
            0xC4 => { $cb!(MOV, dp, A) },
            0xD4 => { $cb!(MOV, dp X p, A) },
            0xC5 => { $cb!(MOV, abs, A) },
            0xD5 => { $cb!(MOV, abs X p, A) },
            0xD6 => { $cb!(MOV, abs Y p, A) },
            0xC7 => { $cb!(MOV, dp X p ind, A) },
            0xD7 => { $cb!(MOV, dp ind Y p, A) },
            0xD8 => { $cb!(MOV, dp, X) },
            0xD9 => { $cb!(MOV, dp Y p, X) },
            0xC9 => { $cb!(MOV, abs, X) },
            0xCB => { $cb!(MOV, dp, Y) },
            0xDB => { $cb!(MOV, dp X p, Y) },
            0xCC => { $cb!(MOV, abs, Y) },
            0x7D => { $cb!(MOV, A, X) },
            0xDD => { $cb!(MOV, A, Y) },
            0x5D => { $cb!(MOV, X, A) },
            0xFD => { $cb!(MOV, Y, A) },
            0x9D => { $cb!(MOV, X, SP) },
            0xBD => { $cb!(MOV, SP, X) },
            0xFA => { $cb!(MOV, dp, dp) },
            0x8F => { $cb!(MOV, dp, imm) },
            0x88 => { $cb!(ADC, A, imm) },
            0x86 => { $cb!(ADC, A, X ind) },
            0x84 => { $cb!(ADC, A, dp) },
            0x94 => { $cb!(ADC, A, dp X p) },
            0x85 => { $cb!(ADC, A, abs) },
            0x95 => { $cb!(ADC, A, abs X p) },
            0x96 => { $cb!(ADC, A, abs Y p) },
            0x87 => { $cb!(ADC, A, dp X p ind) },
            0x97 => { $cb!(ADC, A, dp ind Y p) },
            0x99 => { $cb!(ADC, X ind, Y ind) },
            0x89 => { $cb!(ADC, dp, dp) },
            0x98 => { $cb!(ADC, dp, imm) },
            0xA8 => { $cb!(SBC, A, imm) },
            0xA6 => { $cb!(SBC, A, X ind) },
            0xA4 => { $cb!(SBC, A, dp) },
            0xB4 => { $cb!(SBC, A, dp X p) },
            0xA5 => { $cb!(SBC, A, abs) },
            0xB5 => { $cb!(SBC, A, abs X p) },
            0xB6 => { $cb!(SBC, A, abs Y p) },
            0xA7 => { $cb!(SBC, A, dp X p ind) },
            0xB7 => { $cb!(SBC, A, dp ind Y p) },
            0xB9 => { $cb!(SBC, X ind, Y ind) },
            0xA9 => { $cb!(SBC, dp, dp) },
            0xB8 => { $cb!(SBC, dp, imm) },
            0x68 => { $cb!(CMP, A, imm) },
            0x66 => { $cb!(CMP, A, X ind) },
            0x64 => { $cb!(CMP, A, dp) },
            0x74 => { $cb!(CMP, A, dp X p) },
            0x65 => { $cb!(CMP, A, abs) },
            0x75 => { $cb!(CMP, A, abs X p) },
            0x76 => { $cb!(CMP, A, abs Y p) },
            0x67 => { $cb!(CMP, A, dp X p ind) },
            0x77 => { $cb!(CMP, A, dp ind Y p) },
            0x79 => { $cb!(CMP, X ind, Y ind) },
            0x69 => { $cb!(CMP, dp, dp) },
            0x78 => { $cb!(CMP, dp, imm) },
            0xC8 => { $cb!(CMP, X, imm) },
            0x3E => { $cb!(CMP, X, dp) },
            0x1E => { $cb!(CMP, X, abs) },
            0xAD => { $cb!(CMP, Y, imm) },
            0x7E => { $cb!(CMP, Y, dp) },
            0x5E => { $cb!(CMP, Y, abs) },
            0x28 => { $cb!(AND, A, imm) },
            0x26 => { $cb!(AND, A, X ind) },
            0x24 => { $cb!(AND, A, dp) },
            0x34 => { $cb!(AND, A, dp X p) },
            0x25 => { $cb!(AND, A, abs) },
            0x35 => { $cb!(AND, A, abs X p) },
            0x36 => { $cb!(AND, A, abs Y p) },
            0x27 => { $cb!(AND, A, dp X p ind) },
            0x37 => { $cb!(AND, A, dp ind Y p) },
            0x39 => { $cb!(AND, X ind, Y ind) },
            0x29 => { $cb!(AND, dp, dp) },
            0x38 => { $cb!(AND, dp, imm) },
            0x08 => { $cb!(OR, A, imm) },
            0x06 => { $cb!(OR, A, X ind) },
            0x04 => { $cb!(OR, A, dp) },
            0x14 => { $cb!(OR, A, dp X p) },
            0x05 => { $cb!(OR, A, abs) },
            0x15 => { $cb!(OR, A, abs X p) },
            0x16 => { $cb!(OR, A, abs Y p) },
            0x07 => { $cb!(OR, A, dp X p ind) },
            0x17 => { $cb!(OR, A, dp ind Y p) },
            0x19 => { $cb!(OR, X ind, Y ind) },
            0x09 => { $cb!(OR, dp, dp) },
            0x18 => { $cb!(OR, dp, imm) },
            0x48 => { $cb!(EOR, A, imm) },
            0x46 => { $cb!(EOR, A, X ind) },
            0x44 => { $cb!(EOR, A, dp) },
            0x54 => { $cb!(EOR, A, dp X p) },
            0x45 => { $cb!(EOR, A, abs) },
            0x55 => { $cb!(EOR, A, abs X p) },
            0x56 => { $cb!(EOR, A, abs Y p) },
            0x47 => { $cb!(EOR, A, dp X p ind) },
            0x57 => { $cb!(EOR, A, dp ind Y p) },
            0x59 => { $cb!(EOR, X ind, Y ind) },
            0x49 => { $cb!(EOR, dp, dp) },
            0x58 => { $cb!(EOR, dp, imm) },
            0xBC => { $cb!(INC, A) },
            0xAB => { $cb!(INC, dp) },
            0xBB => { $cb!(INC, dp X p) },
            0xAC => { $cb!(INC, abs) },
            0x3D => { $cb!(INC, X) },
            0xFC => { $cb!(INC, Y) },
            0x9C => { $cb!(DEC, A) },
            0x8B => { $cb!(DEC, dp) },
            0x9B => { $cb!(DEC, dp X p) },
            0x8C => { $cb!(DEC, abs) },
            0x1D => { $cb!(DEC, X) },
            0xDC => { $cb!(DEC, Y) },
            0x1C => { $cb!(ASL, A) },
            0x0B => { $cb!(ASL, dp) },
            0x1B => { $cb!(ASL, dp X p) },
            0x0C => { $cb!(ASL, abs) },
            0x5C => { $cb!(LSR, A) },
            0x4B => { $cb!(LSR, dp) },
            0x5B => { $cb!(LSR, dp X p) },
            0x4C => { $cb!(LSR, abs) },
            0x3C => { $cb!(ROL, A) },
            0x2B => { $cb!(ROL, dp) },
            0x3B => { $cb!(ROL, dp X p) },
            0x2C => { $cb!(ROL, abs) },
            0x7C => { $cb!(ROR, A) },
            0x6B => { $cb!(ROR, dp) },
            0x7B => { $cb!(ROR, dp X p) },
            0x6C => { $cb!(ROR, abs) },
            0x9F => { $cb!(XCN, A) },
            0xBA => { $cb!(MOVW, YA, dp) },
            0xDA => { $cb!(MOVW, dp, YA) },
            0x3A => { $cb!(INCW, dp) },
            0x1A => { $cb!(DECW, dp) },
            0x7A => { $cb!(ADDW, YA, dp) },
            0x9A => { $cb!(SUBW, YA, dp) },
            0x5A => { $cb!(CMPW, YA, dp) },
            0xCF => { $cb!(MUL, YA) },
            0x9E => { $cb!(DIV, YA, X) },
            0xDF => { $cb!(DAA, A) },
            0xBE => { $cb!(DAS, A) },
            0x2F => { $cb!(BRA, rel) },
            0xF0 => { $cb!(BEQ, rel) },
            0xD0 => { $cb!(BNE, rel) },
            0xB0 => { $cb!(BCS, rel) },
            0x90 => { $cb!(BCC, rel) },
            0x70 => { $cb!(BVS, rel) },
            0x50 => { $cb!(BVC, rel) },
            0x30 => { $cb!(BMI, rel) },
            0x10 => { $cb!(BPL, rel) },
            0x03 => { $cb!(BBS, dp, 0, rel) },
            0x23 => { $cb!(BBS, dp, 1, rel) },
            0x43 => { $cb!(BBS, dp, 2, rel) },
            0x63 => { $cb!(BBS, dp, 3, rel) },
            0x83 => { $cb!(BBS, dp, 4, rel) },
            0xA3 => { $cb!(BBS, dp, 5, rel) },
            0xC3 => { $cb!(BBS, dp, 6, rel) },
            0xE3 => { $cb!(BBS, dp, 7, rel) },
            0x13 => { $cb!(BBC, dp, 0, rel) },
            0x33 => { $cb!(BBC, dp, 1, rel) },
            0x53 => { $cb!(BBC, dp, 2, rel) },
            0x73 => { $cb!(BBC, dp, 3, rel) },
            0x93 => { $cb!(BBC, dp, 4, rel) },
            0xB3 => { $cb!(BBC, dp, 5, rel) },
            0xD3 => { $cb!(BBC, dp, 6, rel) },
            0xF3 => { $cb!(BBC, dp, 7, rel) },
            0x2E => { $cb!(CBNE, dp, rel) },
            0xDE => { $cb!(CBNE, dp X p, rel) },
            0x6E => { $cb!(DBNZ, dp, rel) },
            0xFE => { $cb!(DBNZ, Y, rel) },
            0x5F => { $cb!(JMP, abs) },
            0x1F => { $cb!(JMP, abs X p ind) },
            0x3F => { $cb!(CALL, abs) },
            0x4F => { $cb!(PCALL, up) },
            0x01 => { $cb!(TCALL, 0) },
            0x11 => { $cb!(TCALL, 1) },
            0x21 => { $cb!(TCALL, 2) },
            0x31 => { $cb!(TCALL, 3) },
            0x41 => { $cb!(TCALL, 4) },
            0x51 => { $cb!(TCALL, 5) },
            0x61 => { $cb!(TCALL, 6) },
            0x71 => { $cb!(TCALL, 7) },
            0x81 => { $cb!(TCALL, 8) },
            0x91 => { $cb!(TCALL, 9) },
            0xA1 => { $cb!(TCALL, 10) },
            0xB1 => { $cb!(TCALL, 11) },
            0xC1 => { $cb!(TCALL, 12) },
            0xD1 => { $cb!(TCALL, 13) },
            0xE1 => { $cb!(TCALL, 14) },
            0xF1 => { $cb!(TCALL, 15) },
            0x0F => { $cb!(BRK) },
            0x6F => { $cb!(RET) },
            0x7F => { $cb!(RETI) },
            0x2D => { $cb!(PUSH, A) },
            0x4D => { $cb!(PUSH, X) },
            0x6D => { $cb!(PUSH, Y) },
            0x0D => { $cb!(PUSH, PSW) },
            0xAE => { $cb!(POP, A) },
            0xCE => { $cb!(POP, X) },
            0xEE => { $cb!(POP, Y) },
            0x8E => { $cb!(POP, PSW) },
            0x02 => { $cb!(SET1, dp, 0) },
            0x22 => { $cb!(SET1, dp, 1) },
            0x42 => { $cb!(SET1, dp, 2) },
            0x62 => { $cb!(SET1, dp, 3) },
            0x82 => { $cb!(SET1, dp, 4) },
            0xA2 => { $cb!(SET1, dp, 5) },
            0xC2 => { $cb!(SET1, dp, 6) },
            0xE2 => { $cb!(SET1, dp, 7) },
            0x12 => { $cb!(CLR1, dp, 0) },
            0x32 => { $cb!(CLR1, dp, 1) },
            0x52 => { $cb!(CLR1, dp, 2) },
            0x72 => { $cb!(CLR1, dp, 3) },
            0x92 => { $cb!(CLR1, dp, 4) },
            0xB2 => { $cb!(CLR1, dp, 5) },
            0xD2 => { $cb!(CLR1, dp, 6) },
            0xF2 => { $cb!(CLR1, dp, 7) },
            0x0E => { $cb!(TSET1, abs) },
            0x4E => { $cb!(TCLR1, abs) },
            0x4A => { $cb!(AND1, C, aaab, bit) },
            0x6A => { $cb!(AND1, C, not aaab, bit) },
            0x0A => { $cb!(OR1, C, aaab, bit) },
            0x2A => { $cb!(OR1, C, not aaab, bit) },
            0x8A => { $cb!(EOR1, C, aaab, bit) },
            0xEA => { $cb!(NOT1, aaab, bit) },
            0xAA => { $cb!(MOV1, C, aaab, bit) },
            0xCA => { $cb!(MOV1, aaab, bit, C) },
            0x60 => { $cb!(CLRC) },
            0x80 => { $cb!(SETC) },
            0xED => { $cb!(NOTC) },
            0xE0 => { $cb!(CLRV) },
            0x20 => { $cb!(CLRP) },
            0x40 => { $cb!(SETP) },
            0xA0 => { $cb!(EI) },
            0xC0 => { $cb!(DI) },
            0x00 => { $cb!(NOP) },
            0xEF => { $cb!(SLEEP) },
            0xFF => { $cb!(STOP) }
        }
    };
}

pub(crate) use decode;