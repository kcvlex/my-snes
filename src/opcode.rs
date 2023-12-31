macro_rules! opcode_to_ident {
    ($macro: ident, $code: expr) => {
        match $code {
            0x00 => $macro!(BRK, IMPLIED),
            0x01 => $macro!(ORA, DIR_X_IND),
            0x02 => $macro!(COP, IMM),
            0x03 => $macro!(ORA, STK_S),
            0x04 => $macro!(TSB, DIR),
            0x05 => $macro!(ORA, DIR),
            0x06 => $macro!(ASL, DIR),
            0x07 => $macro!(ORA, DIR_IND_LONG),
            0x08 => $macro!(PHP, IMPLIED),
            0x09 => $macro!(ORA, IMM),
            0x0A => $macro!(ASL, ACC),
            0x0B => $macro!(PHD, IMPLIED),
            0x0C => $macro!(TSB, ABS),
            0x0D => $macro!(ORA, ABS),
            0x0E => $macro!(ASL, ABS),
            0x0F => $macro!(ORA, LONG),
            0x10 => $macro!(BPL, REL8),
            0x11 => $macro!(ORA, DIR_IND_Y),
            0x12 => $macro!(ORA, DIR_IND),
            0x13 => $macro!(ORA, STK_S_Y),
            0x14 => $macro!(TRB, DIR),
            0x15 => $macro!(ORA, DIR_X),
            0x16 => $macro!(ASL, DIR_X),
            0x17 => $macro!(ORA, DIR_IND_LONG_Y),
            0x18 => $macro!(CLC, IMPLIED),
            0x19 => $macro!(ORA, ABS_Y),
            0x1A => $macro!(INC, ACC),
            0x1B => $macro!(TCS, IMPLIED),
            0x1C => $macro!(TRB, ABS),
            0x1D => $macro!(ORA, ABS_X),
            0x1E => $macro!(ASL, ABS_X),
            0x1F => $macro!(ORA, LONG_X),
            0x20 => $macro!(JSR, ABS),
            0x21 => $macro!(AND, DIR_X_IND),
            0x22 => $macro!(JSL, LONG),
            0x23 => $macro!(AND, STK_S),
            0x24 => $macro!(BIT, DIR),
            0x25 => $macro!(AND, DIR),
            0x26 => $macro!(ROL, DIR),
            0x27 => $macro!(AND, DIR_IND_LONG),
            0x28 => $macro!(PLP, IMPLIED),
            0x29 => $macro!(AND, IMM),
            0x2A => $macro!(ROL, ACC),
            0x2B => $macro!(PLD, IMPLIED),
            0x2C => $macro!(BIT, ABS),
            0x2D => $macro!(AND, ABS),
            0x2E => $macro!(ROL, ABS),
            0x2F => $macro!(AND, LONG),
            0x30 => $macro!(BMI, REL8),
            0x31 => $macro!(AND, DIR_IND_Y),
            0x32 => $macro!(AND, DIR_IND),
            0x33 => $macro!(AND, STK_S_Y),
            0x34 => $macro!(BIT, DIR_X),
            0x35 => $macro!(AND, DIR_X),
            0x36 => $macro!(ROL, DIR_X),
            0x37 => $macro!(AND, DIR_IND_LONG_Y),
            0x38 => $macro!(SEC, IMPLIED),
            0x39 => $macro!(AND, ABS_Y),
            0x3A => $macro!(DEC, ACC),
            0x3B => $macro!(TSC, IMPLIED),
            0x3C => $macro!(BIT, ABS_X),
            0x3D => $macro!(AND, ABS_X),
            0x3E => $macro!(ROL, ABS_X),
            0x3F => $macro!(AND, LONG_X),
            0x40 => $macro!(RTI, IMPLIED),
            0x41 => $macro!(EOR, DIR_X_IND),
            0x42 => $macro!(WDM, IMM),
            0x43 => $macro!(EOR, STK_S),
            0x44 => $macro!(MVP, SRC_DST),
            0x45 => $macro!(EOR, DIR),
            0x46 => $macro!(LSR, DIR),
            0x47 => $macro!(EOR, DIR_IND_LONG),
            0x48 => $macro!(PHA, IMPLIED),
            0x49 => $macro!(EOR, IMM),
            0x4A => $macro!(LSR, ACC),
            0x4B => $macro!(PHK, IMPLIED),
            0x4C => $macro!(JMP, ABS),
            0x4D => $macro!(EOR, ABS),
            0x4E => $macro!(LSR, ABS),
            0x4F => $macro!(EOR, LONG),
            0x50 => $macro!(BVC, REL8),
            0x51 => $macro!(EOR, DIR_IND_Y),
            0x52 => $macro!(EOR, DIR_IND),
            0x53 => $macro!(EOR, STK_S_Y),
            0x54 => $macro!(MVN, SRC_DST),
            0x55 => $macro!(EOR, DIR_X),
            0x56 => $macro!(LSR, DIR_X),
            0x57 => $macro!(EOR, DIR_IND_LONG_Y),
            0x58 => $macro!(CLI, IMPLIED),
            0x59 => $macro!(EOR, ABS_Y),
            0x5A => $macro!(PHY, IMPLIED),
            0x5B => $macro!(TCD, IMPLIED),
            0x5C => $macro!(JMP, LONG),
            0x5D => $macro!(EOR, ABS_X),
            0x5E => $macro!(LSR, ABS_X),
            0x5F => $macro!(EOR, LONG_X),
            0x60 => $macro!(RTS, IMPLIED),
            0x61 => $macro!(ADC, DIR_X_IND),
            0x62 => $macro!(PER, REL16),
            0x63 => $macro!(ADC, STK_S),
            0x64 => $macro!(STZ, DIR),
            0x65 => $macro!(ADC, DIR),
            0x66 => $macro!(ROR, DIR),
            0x67 => $macro!(ADC, DIR_IND_LONG),
            0x68 => $macro!(PLA, IMPLIED),
            0x69 => $macro!(ADC, IMM),
            0x6A => $macro!(ROR, ACC),
            0x6B => $macro!(RTL, IMPLIED),
            0x6C => $macro!(JMP, ABS_IND),
            0x6D => $macro!(ADC, ABS),
            0x6E => $macro!(ROR, ABS),
            0x6F => $macro!(ADC, LONG),
            0x70 => $macro!(BVS, REL8),
            0x71 => $macro!(ADC, DIR_IND_Y),
            0x72 => $macro!(ADC, DIR_IND),
            0x73 => $macro!(ADC, STK_S_Y),
            0x74 => $macro!(STZ, DIR_X),
            0x75 => $macro!(ADC, DIR_X),
            0x76 => $macro!(ROR, DIR_X),
            0x77 => $macro!(ADC, DIR_IND_LONG_Y),
            0x78 => $macro!(SEI, IMPLIED),
            0x79 => $macro!(ADC, ABS_Y),
            0x7A => $macro!(PLY, IMPLIED),
            0x7B => $macro!(TDC, IMPLIED),
            0x7C => $macro!(JMP, ABS_IND_X),
            0x7D => $macro!(ADC, ABS_X),
            0x7E => $macro!(ROR, ABS_X),
            0x7F => $macro!(ADC, LONG_X),
            0x80 => $macro!(BRA, REL8),
            0x81 => $macro!(STA, DIR_X_IND),
            0x82 => $macro!(BRL, REL16),
            0x83 => $macro!(STA, STK_S),
            0x84 => $macro!(STY, DIR),
            0x85 => $macro!(STA, DIR),
            0x86 => $macro!(STX, DIR),
            0x87 => $macro!(STA, DIR_IND_LONG),
            0x88 => $macro!(DEY, IMPLIED),
            0x89 => $macro!(BIT, IMM),
            0x8A => $macro!(TXA, IMPLIED),
            0x8B => $macro!(PHB, IMPLIED),
            0x8C => $macro!(STY, ABS),
            0x8D => $macro!(STA, ABS),
            0x8E => $macro!(STX, ABS),
            0x8F => $macro!(STA, LONG),
            0x90 => $macro!(BCC, REL8),
            0x91 => $macro!(STA, DIR_IND_Y),
            0x92 => $macro!(STA, DIR_IND),
            0x93 => $macro!(STA, STK_S_Y),
            0x94 => $macro!(STY, DIR_X),
            0x95 => $macro!(STA, DIR_X),
            0x96 => $macro!(STX, DIR_Y),
            0x97 => $macro!(STA, DIR_IND_LONG_Y),
            0x98 => $macro!(TYA, IMPLIED),
            0x99 => $macro!(STA, ABS_Y),
            0x9A => $macro!(TXS, IMPLIED),
            0x9B => $macro!(TXY, IMPLIED),
            0x9C => $macro!(STZ, ABS),
            0x9D => $macro!(STA, ABS_X),
            0x9E => $macro!(STZ, ABS_X),
            0x9F => $macro!(STA, LONG_X),
            0xA0 => $macro!(LDY, IMM),
            0xA1 => $macro!(LDA, DIR_X_IND),
            0xA2 => $macro!(LDX, IMM),
            0xA3 => $macro!(LDA, STK_S),
            0xA4 => $macro!(LDY, DIR),
            0xA5 => $macro!(LDA, DIR),
            0xA6 => $macro!(LDX, DIR),
            0xA7 => $macro!(LDA, DIR_IND_LONG),
            0xA8 => $macro!(TAY, IMPLIED),
            0xA9 => $macro!(LDA, IMM),
            0xAA => $macro!(TAX, IMPLIED),
            0xAB => $macro!(PLB, IMPLIED),
            0xAC => $macro!(LDY, ABS),
            0xAD => $macro!(LDA, ABS),
            0xAE => $macro!(LDX, ABS),
            0xAF => $macro!(LDA, LONG),
            0xB0 => $macro!(BCS, REL8),
            0xB1 => $macro!(LDA, DIR_IND_Y),
            0xB2 => $macro!(LDA, DIR_IND),
            0xB3 => $macro!(LDA, STK_S_Y),
            0xB4 => $macro!(LDY, DIR_X),
            0xB5 => $macro!(LDA, DIR_X),
            0xB6 => $macro!(LDX, DIR_Y),
            0xB7 => $macro!(LDA, DIR_IND_LONG_Y),
            0xB8 => $macro!(CLV, IMPLIED),
            0xB9 => $macro!(LDA, ABS_Y),
            0xBA => $macro!(TSX, IMPLIED),
            0xBB => $macro!(TYX, IMPLIED),
            0xBC => $macro!(LDY, ABS_X),
            0xBD => $macro!(LDA, ABS_X),
            0xBE => $macro!(LDX, ABS_Y),
            0xBF => $macro!(LDA, LONG_X),
            0xC0 => $macro!(CPY, IMM),
            0xC1 => $macro!(CMP, DIR_X_IND),
            0xC2 => $macro!(REP, IMM),
            0xC3 => $macro!(CMP, STK_S),
            0xC4 => $macro!(CPY, DIR),
            0xC5 => $macro!(CMP, DIR),
            0xC6 => $macro!(DEC, DIR),
            0xC7 => $macro!(CMP, DIR_IND_LONG),
            0xC8 => $macro!(INY, IMPLIED),
            0xC9 => $macro!(CMP, IMM),
            0xCA => $macro!(DEX, IMPLIED),
            0xCB => $macro!(WAI, IMPLIED),
            0xCC => $macro!(CPY, ABS),
            0xCD => $macro!(CMP, ABS),
            0xCE => $macro!(DEC, ABS),
            0xCF => $macro!(CMP, LONG),
            0xD0 => $macro!(BNE, REL8),
            0xD1 => $macro!(CMP, DIR_IND_Y),
            0xD2 => $macro!(CMP, DIR_IND),
            0xD3 => $macro!(CMP, STK_S_Y),
            0xD4 => $macro!(PEI, DIR),
            0xD5 => $macro!(CMP, DIR_X),
            0xD6 => $macro!(DEC, DIR_X),
            0xD7 => $macro!(CMP, DIR_IND_LONG_Y),
            0xD8 => $macro!(CLD, IMPLIED),
            0xD9 => $macro!(CMP, ABS_Y),
            0xDA => $macro!(PHX, IMPLIED),
            0xDB => $macro!(STP, IMPLIED),
            0xDC => $macro!(JMP, ABS_IND_LONG),
            0xDD => $macro!(CMP, ABS_X),
            0xDE => $macro!(DEC, ABS_X),
            0xDF => $macro!(CMP, LONG_X),
            0xE0 => $macro!(CPX, IMM),
            0xE1 => $macro!(SBC, DIR_X_IND),
            0xE2 => $macro!(SEP, IMM),
            0xE3 => $macro!(SBC, STK_S),
            0xE4 => $macro!(CPX, DIR),
            0xE5 => $macro!(SBC, DIR),
            0xE6 => $macro!(INC, DIR),
            0xE7 => $macro!(SBC, DIR_IND_LONG),
            0xE8 => $macro!(INX, IMPLIED),
            0xE9 => $macro!(SBC, IMM),
            0xEA => $macro!(NOP, IMPLIED),
            0xEB => $macro!(XBA, IMPLIED),
            0xEC => $macro!(CPX, ABS),
            0xED => $macro!(SBC, ABS),
            0xEE => $macro!(INC, ABS),
            0xEF => $macro!(SBC, LONG),
            0xF0 => $macro!(BEQ, REL8),
            0xF1 => $macro!(SBC, DIR_IND_Y),
            0xF2 => $macro!(SBC, DIR_IND),
            0xF3 => $macro!(SBC, STK_S_Y),
            0xF4 => $macro!(PEA, IMM),
            0xF5 => $macro!(SBC, DIR_X),
            0xF6 => $macro!(INC, DIR_X),
            0xF7 => $macro!(SBC, DIR_IND_LONG_Y),
            0xF8 => $macro!(SED, IMPLIED),
            0xF9 => $macro!(SBC, ABS_Y),
            0xFA => $macro!(PLX, IMPLIED),
            0xFB => $macro!(XCE, IMPLIED),
            0xFC => $macro!(JSR, ABS_IND_X),
            0xFD => $macro!(SBC, ABS_X),
            0xFE => $macro!(INC, ABS_X),
            0xFF => $macro!(SBC, LONG_X),
        }
    };
}
pub(crate) use opcode_to_ident;
