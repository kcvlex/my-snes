#![allow(dead_code)]

use crate::apu;
use modular_bitfield::prelude::*;

// Audio RAM (ARAM)
pub struct Memory {
    raw: Vec<u8>,
    control: CONTROL,
    ports_in: [u8; 4],
    ports_out: [u8; 4],
}

#[bitfield]
#[repr(u8)]
#[derive(Debug)]
struct CONTROL {
    enable_timer0: bool,
    enable_timer1: bool,
    enable_timer2: bool,
    #[skip]
    __: bool,
    reset_ports01: bool,
    reset_ports23: bool,
    #[skip]
    __: bool,
    enable_ipl_rom: bool,
}

pub trait Resources {
    fn add_cycles(&mut self, v: u8);
    fn dsp_read(&self, addr: u8) -> u8;
    fn dsp_write(&mut self, addr: u8, v: u8);
    fn timer0_mut(&mut self) -> &mut apu::Timer0;
    fn timer1_mut(&mut self) -> &mut apu::Timer1;
    fn timer2_mut(&mut self) -> &mut apu::Timer2;
}

impl Default for Memory {
    fn default() -> Self {
        Self {
            raw: vec![0x00; 0x10000],
            ports_in: [0; 4],
            ports_out: [0; 4],
            control: 0x80.into(),
        }
    }
}

impl Memory {
    pub fn cpu_port_read(&self, port: u8) -> u8 {
        // TODO?: Return bitwise OR of current and old value when reading while the register is being written
        match port {
            0..=3 => self.ports_out[port as usize],
            _ => unreachable!(),
        }
    }

    pub fn cpu_port_write(&mut self, port: u8, v: u8) {
        match port {
            0..=3 => self.ports_in[port as usize] = v,
            _ => unreachable!(),
        }
    }

    pub fn read(&mut self, addr: u16, rsrc: &mut impl Resources) -> u8 {
        rsrc.add_cycles(1);
        match addr {
            // Write Only
            0x00F0 | 0x00F1 => 0x00,

            // DSPDATA
            0x00F3 => rsrc.dsp_read(self.raw[0x00F2]),

            // Ports
            0x00F4..=0x00F7 => self.ports_in[(addr - 0x00F4) as usize],

            // Write Only
            0x00FA..=0x00FC => 0x00,

            0x00FD => rsrc.timer0_mut().read(),
            0x00FE => rsrc.timer1_mut().read(),
            0x00FF => rsrc.timer2_mut().read(),

            0xFFC0..=0xFFFF => {
                if self.control.enable_ipl_rom() {
                    IPL[(addr - 0xFFC0) as usize]
                } else {
                    self.raw[addr as usize]
                }
            }

            _ => self.raw[addr as usize],
        }
    }

    pub fn write(&mut self, addr: u16, data: u8, rsrc: &mut impl Resources) {
        rsrc.add_cycles(1);
        match addr {
            // TODO: TEST
            0x00F0 => (),

            // CONTROL
            0x00F1 => {
                self.control = data.into();
                if self.control.reset_ports01() {
                    self.ports_in[0] = 0x00;
                    self.ports_in[1] = 0x00;
                    self.ports_out[0] = 0x00;
                    self.ports_out[1] = 0x00;
                }
                if self.control.reset_ports23() {
                    self.ports_in[2] = 0x00;
                    self.ports_in[3] = 0x00;
                    self.ports_out[2] = 0x00;
                    self.ports_out[3] = 0x00;
                }
                rsrc.timer0_mut().set_enabled(self.control.enable_timer0());
                rsrc.timer1_mut().set_enabled(self.control.enable_timer1());
                rsrc.timer2_mut().set_enabled(self.control.enable_timer2());
            }

            // DSPADDR
            0x00F2 => self.raw[addr as usize] = data,

            // DSPDATA
            0x00F3 => rsrc.dsp_write(self.raw[0x00F2], data),

            // Ports
            0x00F4..=0x00F7 => self.ports_out[(addr - 0x00F4) as usize] = data,

            // T0DIV
            0x00FA => rsrc.timer0_mut().write_div(data),

            // T1DIV
            0x00FB => rsrc.timer1_mut().write_div(data),

            // T2DIV
            0x00FC => rsrc.timer2_mut().write_div(data),

            // Read-Only
            0x00FD..=0x00FF => (),

            0xFFC0..=0xFFFF => {
                if self.control.enable_ipl_rom() {
                    // FIXME?: Ignoring is correct?
                    ()
                } else {
                    self.raw[addr as usize] = data;
                }
                // self.raw[addr as usize] = data;
            }

            _ => self.raw[addr as usize] = data,
        }
    }

    pub fn raw_mut(&mut self) -> &mut [u8] {
        &mut self.raw[..]
    }
}

/* https://problemkaputt.de/fullsnes.htm#snesapumaincpucommunicationport
 *
 *  FFC0 CD EF        mov  x,EF           ;\
 *  FFC2 BD           mov  sp,x           ; zerofill RAM at [0001h..00EFh]
 *  FFC3 E8 00        mov  a,00           ; (ie. excluding I/O Ports at F0h..FFh)
 *                   @@zerofill_lop:      ; (though [00h..01h] destroyed below)
 *  FFC5 C6           mov  [x],a          ; (also sets stacktop to 01EFh, kinda
 *  FFC6 1D           dec  x              ; messy, nicer would be stacktop 01FFh)
 *  FFC7 D0 FC        jnz  @@zerofill_lop ;/
 *  FFC9 8F AA F4     mov  [F4],AA        ;\notify Main CPU that APU is ready
 *  FFCC 8F BB F5     mov  [F5],BB        ;/for communication
 *                   @@wait_for_cc:       ;\
 *  FFCF 78 CC F4     cmp  [F4],CC        ; wait for initial "kick" value
 *  FFD2 D0 FB        jnz  @@wait_for_cc  ;/
 *  FFD4 2F 19        jr   main
 *                   ;---
 *                   @@transfer_data:
 *                   @@wait_for_00:                               ;\
 *  FFD6 EB F4        mov  y,[F4]     ;index (should become 0)    ;
 *  FFD8 D0 FC        jnz  @@wait_for_00                          ;/
 *                   @@transfer_lop:
 *  FFDA 7E F4        cmp  y,[F4]
 *  FFDC D0 0B        jnz  FFE9          ------->
 *  FFDE E4 F5        mov  a,[F5]     ;get data
 *  FFE0 CB F4        mov  [F4],y     ;ack data
 *  FFE2 D7 00        mov  [[00]+y],a ;store data
 *  FFE4 FC           inc  y          ;addr lsb
 *  FFE5 D0 F3        jnz  @@transfer_lop
 *  FFE7 AB 01        inc  [01]       ;addr msb
 *                   @@
 *  FFE9 10 EF        jns  @@transfer_lop     ;strange...
 *  FFEB 7E F4        cmp  y,[F4]
 *  FFED 10 EB        jns  @@transfer_lop
 *                   ;- - -
 *                   main:
 *  FFEF BA F6        movw ya,[F6]                ;\copy transfer (or entrypoint)
 *  FFF1 DA 00        movw [00],ya    ;addr       ;/address to RAM at [0000h]
 *  FFF3 BA F4        movw ya,[F4]    ;cmd:kick
 *  FFF5 C4 F4        mov  [F4],a     ;ack kick
 *  FFF7 DD           mov  a,y        ;cmd
 *  FFF8 5D           mov  x,a        ;cmd
 *  FFF9 D0 DB        jnz  @@transfer_data
 *  FFFB 1F 00 00     jmp  [0000+x]   ;in: A=0, X=0, Y=0, SP=EFh, PSW=02h
 *                   ;---
 *  FFFE C0 FF        dw   FFC0  ;reset vector
 */
const IPL: [u8; 64] = [
    0xCD, 0xEF, 0xBD, 0xE8, 0x00, 0xC6, 0x1D, 0xD0, 0xFC, 0x8F, 0xAA, 0xF4, 0x8F, 0xBB, 0xF5, 0x78, 0xCC, 0xF4, 0xD0, 0xFB, 0x2F, 0x19, 0xEB, 0xF4, 0xD0, 0xFC, 0x7E, 0xF4, 0xD0, 0x0B, 0xE4, 0xF5,
    0xCB, 0xF4, 0xD7, 0x00, 0xFC, 0xD0, 0xF3, 0xAB, 0x01, 0x10, 0xEF, 0x7E, 0xF4, 0x10, 0xEB, 0xBA, 0xF6, 0xDA, 0x00, 0xBA, 0xF4, 0xC4, 0xF4, 0xDD, 0x5D, 0xD0, 0xDB, 0x1F, 0x00, 0x00, 0xC0, 0xFF,
];
