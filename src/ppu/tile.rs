use crate::ppu::mmio::ChrSize;
use modular_bitfield::prelude::*;

#[bitfield]
#[repr(u16)]
#[derive(Clone, Copy, Default, Debug)]
pub struct BGMapEntry {
    pub character_num: B10,
    pub palette_num: B3,
    pub priority: bool,
    pub x_flip: bool,
    pub y_flip: bool,
}

#[derive(Default)]
pub struct BGContext {
    pub x: usize,
    pub y: usize,
    pub chr_size: (ChrSize, ChrSize),
}

pub struct BGEntryCache {
    entry: (BGMapEntry, u16),
    pre_x: usize,
    pre_y: usize,
}

impl Default for BGEntryCache {
    fn default() -> Self {
        Self {
            entry: (0.into(), 0),
            pre_x: usize::MAX,
            pre_y: usize::MAX,
        }
    }
}

impl BGEntryCache {
    pub fn is_consistent(&self, ctx: BGContext) -> bool {
        let (chr_w, chr_h) = ctx.chr_size;
        chr_w.rdiv(ctx.x) == chr_w.rdiv(self.pre_x) && chr_h.rdiv(ctx.y) == chr_h.rdiv(self.pre_y)
    }

    pub fn burst(&mut self, entry: (BGMapEntry, u16), x: usize, y: usize) {
        self.entry = entry;
        self.pre_x = x;
        self.pre_y = y;
    }

    pub fn value(&self) -> (BGMapEntry, u16) {
        self.entry
    }
}

pub struct BGPixelCache {
    cgram_addr: [u8; 8],
    base_addr: usize,
    pre_entry_addr: u16,
    pre_x: usize,
    pre_y: usize,
}

impl Default for BGPixelCache {
    fn default() -> Self {
        Self {
            cgram_addr: [0; 8],
            base_addr: 0,
            pre_entry_addr: u16::MAX,
            pre_x: usize::MAX,
            pre_y: usize::MAX,
        }
    }
}

impl BGPixelCache {
    pub fn burst(&mut self, entry: (BGMapEntry, u16), ctx: BGContext, bpp: u8, base_addr: usize, vram: &[u8]) {
        let (entry, entry_addr) = entry;
        self.pre_x = ctx.x;
        self.pre_y = ctx.y;
        self.pre_entry_addr = entry_addr;
        self.base_addr = base_addr;

        let (chr_w, chr_h) = ctx.chr_size;
        let tile_index = entry.character_num() as usize;
        let x = chr_w.get_offset(ctx.x, entry.x_flip());
        let y = chr_h.get_offset(ctx.y, entry.y_flip());
        let tile_index = tile_index + (x / 8) + ((y / 8) * 16);
        let tile_index = tile_index & 0x3FF; // 32x32 tiles
        let y = y % 8;
        let tile_size = (bpp as usize) * 4; // words
        let addr = self.base_addr + tile_index * tile_size + y;
        let addr = addr * 2;

        for e in self.cgram_addr.iter_mut() {
            *e = 0;
        }
        for i in 0..(bpp / 2) {
            let addr = (addr + (16 * i) as usize) & 0xFFFE;
            let lo_v = vram[addr + 0];
            let hi_v = vram[addr + 1];
            for x in 0..8 {
                let lo = (lo_v >> (7 - x)) & 1;
                let hi = (hi_v >> (7 - x)) & 1;
                self.cgram_addr[x] |= ((hi << 1) | lo) << (i * 2);
            }
        }
    }

    pub fn is_consistent(&self, entry_addr: u16, x: usize, y: usize) -> bool {
        self.pre_entry_addr == entry_addr && (x >> 3) == (self.pre_x >> 3) && y == self.pre_y
    }

    pub fn get_cgram_addr(&self, v: usize) -> u8 {
        self.cgram_addr[v]
    }
}
