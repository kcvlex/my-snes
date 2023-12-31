use strum::IntoEnumIterator;
use strum_macros::EnumIter;

#[allow(dead_code)]
#[derive(Debug, Default)]
pub struct Header {
    title: String,
    speed: Speed,
    map_mode: MapMode,
    chipset: Chipset,
    rom_size: usize,
    ram_size: usize,
    country: u8,
    developer_id_code: u8,
    rom_version: u8,
    checksum: u16,
}

#[derive(Clone, Copy, Debug, Default)]
pub enum Speed {
    #[default]
    Slow,
    Fast,
}

// TODO: S-DD1, SA-1, SPC7110
#[derive(Clone, Copy, Debug, EnumIter, Default)]
pub enum MapMode {
    #[default]
    LoROM,
    HiROM,
    ExHiROM,
}

impl MapMode {
    fn header_offset(&self) -> usize {
        match self {
            MapMode::LoROM => 0x007F00,
            MapMode::HiROM => 0x00FF00,
            MapMode::ExHiROM => 0x40FF00,
        }
    }
}

#[allow(dead_code)]
#[derive(Clone, Copy, Debug)]
pub struct Chipset {
    has_ram: bool,
    has_battery: bool,
    has_rtc: bool,
    coprocessor: Option<Coprocessor>,
}

impl Default for Chipset {
    fn default() -> Self {
        Self {
            has_ram: false,
            has_battery: false,
            has_rtc: false,
            coprocessor: None,
        }
    }
}

// TODO: Custom
#[allow(non_camel_case_types)]
#[derive(Clone, Copy, Debug)]
pub enum Coprocessor {
    DSP,
    GSU,
    OBC1,
    SA_1,
    S_DD1,
    S_RTC,
}

pub struct ROM {
    raw: Vec<u8>,
    header: Header,
}

pub struct Cartridge {
    pub rom: ROM,
    pub sram: Vec<u8>,
}

impl ROM {
    pub fn try_load(raw: &[u8]) -> Result<Self, String> {
        // TODO: If multiple modes are "acceptable"
        let mut err: Vec<(MapMode, Box<String>)> = Vec::new();
        for mode in MapMode::iter() {
            match Self::try_detect_header(&raw, mode.header_offset()) {
                Ok(header) => {
                    println!("{:?}", header);
                    let res: Result<Self, String> = Ok(Self { raw: raw.to_vec(), header });
                    return res;
                }
                Err(ex) => {
                    println!("{:?}", ex);
                    err.push((mode.clone(), Box::new(ex)));
                }
            }
        }
        Ok(Self {
            raw: raw.to_vec(),
            header: Header::default(),
        })
    }

    fn try_detect_header(raw: &[u8], header_offset: usize) -> Result<Header, String> {
        const HEADER_SIZE: usize = 0x100;

        if raw.len() < header_offset + HEADER_SIZE {
            Err(format!("too short size: {}", raw.len()))?;
        }

        let header = &raw[header_offset..(header_offset + HEADER_SIZE)];

        // 0xFFC0
        let title = std::str::from_utf8(&header[0xC0..0xD5]).map_err(|e| format!("invalid title: {:?}", e))?.to_string();

        // 0xFFD5
        // TODO: Check if map_mode is expected one (it's corresponds to header_offset)
        let (speed, map_mode) = {
            let value = header[0xD5];
            if ((value >> 5) & 0b111) != 0b001 {
                Err(format!("invalid value of FFD5: {:#04X}", value))?;
            }
            let speed = ((value >> 4) & 1) != 0;
            let speed = if !speed { Speed::Slow } else { Speed::Fast };
            let map_mode = match value & 0x0F {
                0 => Ok(MapMode::LoROM),
                1 => Ok(MapMode::HiROM),
                5 => Ok(MapMode::ExHiROM),
                v @ _ => Err(format!("unknown map mode: {:#04X}", v)),
            }?;
            let res: Result<(Speed, MapMode), String> = Ok((speed, map_mode));
            res
        }?;

        // 0xFFD6
        let chipset = {
            let value = header[0xD6];
            let lo = value & 0x0F;
            let hi = (value >> 4) & 0x0F;
            let unknown_chipset = |v: u8| format!("unknown chipset: {:#04X}", v);
            let _ = match lo {
                0x0..=0x6 | 0x9 | 0xA => Ok(()),
                v @ _ => Err(unknown_chipset(v)),
            }?;
            let _ = match hi {
                0x00..=0x05 => Ok(()),
                v @ _ => Err(unknown_chipset(v)),
            }?;
            let coprocessor = match hi {
                0 => Ok(Coprocessor::DSP),
                1 => Ok(Coprocessor::GSU),
                2 => Ok(Coprocessor::OBC1),
                3 => Ok(Coprocessor::SA_1),
                4 => Ok(Coprocessor::S_DD1),
                5 => Ok(Coprocessor::S_RTC),
                v @ _ => Err(format!("unknown coprecessor: {:#04X}", v)),
            }?;
            let res: Result<Chipset, String> = Ok(Chipset {
                has_ram: !(lo != 0x00 || lo != 0x03 || lo != 0x06),
                has_battery: lo == 0x02 || 0x05 <= lo,
                has_rtc: lo == 0x09,
                coprocessor: if lo < 0x03 { None } else { Some(coprocessor) },
            });
            res
        }?;

        // 0xFFD7
        let rom_size = 1usize << header[0xD7];

        // 0xFFD8
        let ram_size = 1usize << header[0xD8];

        // 0xFFD9
        let country = header[0xD9];

        // 0xFFDA
        let developer_id_code = header[0xDA];

        // 0xFFDB
        let rom_version = header[0xDB];
        if rom_version != 0x00 {
            Err(format!("unknown ROM version: {:#04X}", rom_version))?;
        }

        let little_endian_u16 = |pair: &[u8]| {
            assert!(pair.len() == 2);
            let lo = pair[0] as u16;
            let hi = pair[1] as u16;
            (hi << 8) | lo
        };

        // 0xFFDC, 0xFFDD
        let checksum_comp = little_endian_u16(&header[0xDC..0xDE]);

        // 0xFFDE, 0xFFDF
        let checksum = little_endian_u16(&header[0xDE..0xE0]);

        if (checksum ^ checksum_comp) != 0xFFFF {
            Err(format!("unexpected checksum: checksum={:#06X} checksum_comp={:#06X}", checksum, checksum_comp))?;
        }

        // TODO: A loader must duplicate a part of data to fill up to the condition
        if 2 < raw.len().count_ones() {
            Err(format!("unexpected file size: {}", raw.len()))?;
        }

        {
            let read = |i: usize| {
                if (header_offset + 0xDC) <= i && i < (header_offset + 0xDE) {
                    // checksum complement
                    0xFF
                } else if (header_offset + 0xDE) <= i && i < (header_offset + 0xE0) {
                    // checksum
                    0x00
                } else {
                    raw[i]
                }
            };
            let mut acc = 0 as u16;
            let mut i = 0usize;
            while i < raw.len() {
                let x = read(i);
                i += 1;
                acc = acc.wrapping_add(x as u16);
            }

            if acc != checksum {
                Err(format!("invalid checksum: given={:#06X} calculated={:#06X}", checksum, acc))?;
            }
        }

        Ok(Header {
            title,
            speed,
            map_mode,
            chipset,
            rom_size,
            ram_size,
            country,
            developer_id_code,
            rom_version,
            checksum,
        })
    }
}

impl Cartridge {
    // TODO: SRAM
    pub fn make(raw: &[u8]) -> Result<Self, String> {
        let rom = ROM::try_load(raw)?;
        Ok(Self { rom, sram: vec![0x00; 0x1000] })
    }

    // TODO: SRAM
    pub fn mapped(&self, bank: u8, offset: u16) -> Option<u8> {
        match self.rom.header.map_mode {
            MapMode::LoROM => {
                let read = |bank: u8, offset: u16| {
                    assert!(0x80 <= bank);
                    assert!(0x8000 <= offset);
                    let bank = (bank - 0x80) as usize;
                    let offset = (offset - 0x8000) as usize;
                    let addr = 0x8000 * bank + offset;
                    self.rom.raw.get(addr).cloned()
                };
                match offset {
                    0x0000..=0x7FFF => None,
                    0x8000..=0xFFFF => {
                        if 0x80 <= bank {
                            read(bank, offset)
                        } else if bank <= 0x7D {
                            read(bank + 0x80, offset)
                        } else {
                            None
                        }
                    }
                }
            }
            MapMode::HiROM => {
                let read = |bank: u8, offset: u16| {
                    assert!(0xC0 <= bank);
                    let bank = (bank - 0xC0) as usize;
                    let addr = 0x10000 * bank + (offset as usize);
                    self.rom.raw.get(addr).cloned()
                };
                match bank {
                    0x00..=0x3F | 0x80..=0xBF => {
                        let bank = if bank <= 0x3F { bank + 0xC0 } else { bank + 0x40 };
                        if 0x8000 <= offset {
                            read(bank, offset)
                        } else {
                            None
                        }
                    }
                    0x40..=0x7D => read(bank + 0x80, offset), // TODO: correct?
                    0xC0..=0xFF => read(bank, offset),
                    _ => None,
                }
            }
            MapMode::ExHiROM => todo!(),
        }
    }
}
