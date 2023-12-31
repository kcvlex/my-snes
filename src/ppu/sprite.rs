use crate::ppu::mmio;

// Select a tile from 16x16 tiles which starts from `base`
#[derive(Debug)]
pub struct SpriteTileIndex {
    base: usize,
    offset: u8,
}

impl SpriteTileIndex {
    pub fn add(&self, x: u8, y: u8) -> Self {
        let ox = ((self.offset >> 0) & 0xF) as u8;
        let oy = ((self.offset >> 4) & 0xF) as u8;
        let x = (ox + x) & 0xF;
        let y = (oy + y) & 0xF;
        Self {
            base: self.base,
            offset: (y << 4) | x,
        }
    }

    fn word_addr(&self, bpp: u8) -> usize {
        let offset = self.offset as usize;
        self.base + (4 * bpp) as usize * offset
    }
}

#[derive(Debug)]
pub struct SpriteTile {
    index: SpriteTileIndex,
    x0: i16,
    y0: u8,
    bpp: u8,
    palette: u8,
    flip_x: bool,
    flip_y: bool,
}

pub struct SpritePixel {
    pub x: i16,
    pub palette: u8,
    pub color: mmio::CGDATA,
}

impl SpriteTile {
    pub fn pixel(&self, ofx: u8, y: u8, vram: &[u8], cgram: &[mmio::CGDATA]) -> Option<SpritePixel> {
        let ofx = match ofx {
            val @ 0..=7 => val as u8,
            _ => panic!("SpriteTile::color -> ofx={}", ofx),
        };
        let ofy = match y - self.y0 {
            val @ 0..=7 => val,
            _ => panic!("SpriteTile::color -> y={} y0={}", y, self.y0),
        };

        let x = self.x0 + ofx as i16;
        if !(0 <= x && x < 256) {
            return None;
        }

        let ofx = if self.flip_x { 7 - ofx } else { ofx };
        let ofy = if self.flip_y { 7 - ofy } else { ofy };

        let mut c: u8 = 0;
        for i in 0..(self.bpp / 2) {
            let addr = self.index.word_addr(self.bpp);
            let addr = addr + (8 * i + ofy) as usize; // words
            let addr = addr & 0x7FFF;
            let addr = addr * 2; // bytes
            let lo = (vram[addr + 0] >> (7 - ofx)) & 1;
            let hi = (vram[addr + 1] >> (7 - ofx)) & 1;
            c |= ((hi << 1) | lo) << (i * 2);
        }

        if c == 0 {
            None
        } else {
            let addr = (self.palette as usize) << self.bpp;
            let addr = addr + c as usize;
            Some(SpritePixel {
                x,
                palette: self.palette,
                color: cgram[addr],
            })
        }
    }
}

#[derive(Debug)]
pub struct Sprite {
    x: i16,
    y: u8,
    tile: u8,
    name_select: bool,
    palette: u8,
    pub priority: u8,
    flip_horizontal: bool,
    flip_vertical: bool,
    size: (u8, u8),
}

impl Sprite {
    fn first_tile_index(&self, objsel: mmio::OBJSEL) -> SpriteTileIndex {
        let base = (objsel.name_base_addr()) as usize;
        let base = base << 13;
        let base = base +
            if self.name_select {
                let name = objsel.name_select() as usize;
                (name + 1) << 12
            } else {
                0
            };
        SpriteTileIndex { base, offset: self.tile }
    }

    pub fn occur(&self, y: i16) -> bool {
        let (sx, sy) = self.size;
        let res = !(self.x + (sx as i16) < 0 || 256 <= self.x);
        let res = res && {
            let sp_y = self.y as i16;
            let y = y as i16;
            let sy = sy as i16;
            sp_y <= y && y < sp_y + sy
        };
        res
    }

    pub fn calc_tiles(&self, y: u8, objsel: mmio::OBJSEL) -> Vec<SpriteTile> {
        let mut res = Vec::new();
        let flip_x = self.flip_horizontal;
        let flip_y = self.flip_vertical;
        let ty = (y - self.y) / 8;
        let ty = if !flip_y { ty } else { self.size.1 / 8 - 1 - ty };
        let tile_index = self.first_tile_index(objsel.clone()).add(0, ty);
        for tx in 0..(self.size.0 / 8) {
            let x = self.x + (tx * 8) as i16;
            if !(0 <= x + 8 || 256 <= x) {
                continue;
            }
            let tx = if !flip_x { tx } else { self.size.0 / 8 - 1 - tx };
            res.push(self::SpriteTile {
                index: tile_index.add(tx, 0),
                x0: x,
                y0: self.y + (y - self.y) / 8 * 8,
                bpp: 4,
                palette: self.palette,
                flip_x,
                flip_y,
            });
        }
        res
    }

    // https://snes.nesdev.org/wiki/OAM_layout
    //
    // --+------------------+------------------+------------------+------------------------------------------------+
    // L | Sprite #0        | Sprite #0        | Sprite #0        | Sprite #0                                      |
    // O | x pos (low bits) | y pos            | tile (low bits)  | flip v/h, priority, palette, high bit of tile  |
    // W |                  |                  |                  | (2 bits)  (2 bits)  (3 bits)                   |
    //   +------------------+------------------+------------------+------------------------------------------------+
    // T | Sprite #1        | Sprite #1        | Sprite #1        | Sprite #1                                      |
    // A | x pos (low bits) | y pos            | tile (low bits)  | flip v/h, priority, palette, high bit of tile  |
    // B +------------------+------------------+------------------+------------------------------------------------+
    // L | Sprite #2        | Sprite #2        | Sprite #2        | Sprite #2                                      |
    // E | x pos (low bits) | y pos            | tile (low bits)  | flip v/h, priority, palette, high bit of tile  |
    //   +------------------+------------------+------------------+------------------------------------------------+
    //   |                  |                  |                  |                                                |
    //   |       ....       |    ...           |      ...         |                      ...                       |
    //   |                  |                  |                  |                                                |
    //   +------------------+------------------+------------------+------------------------------------------------+
    //   | Sprite #127      | Sprite #127      | Sprite #127      | Sprite #127                                    |
    //   | x pos (low bits) | y pos            | tile (low bits)  | flip v/h, priority, palette, high bit of tile  |
    // --+------------------+------------------+------------------+------------------------------------------------+
    // H | Sprites #0-3     | Sprites #4-7     | Sprites #8-11    | Sprites #12-15                                 |
    // I |                  |                  |                  |                                                |
    // G |                  |                  |                  |                                                |
    // H | size select bits | size select bits | size select bits | size select bits                               |
    //   |  |  |  |  |      |  |  |  |  |      |  |  |  |  |      |  |  |  |  |                                    |
    //   |  v  v  v  v      |  v  v  v  v      |  v  v  v  v      |  v  v  v  v                                    |
    // T | +--+--+--+--+    | +--+--+--+--+    | +--+--+--+--+    | +--+--+--+--+                                  |
    // A | |s |s |s |s |    | |s |s |s |s |    | |s |s |s |s |    | |s |s |s |s |                                  |
    // B | |#4|#3|#2|#1|    | |#4|#3|#2|#1|    | |#4|#3|#2|#1|    | |#4|#3|#2|#1|                                  |
    // L | | x| x| x| x|    | | x| x| x| x|    | | x| x| x| x|    | | x| x| x| x|                                  |
    // E | +--+--+--+--+    | +--+--+--+--+    | +--+--+--+--+    | +--+--+--+--+                                  |
    //   |   ^  ^  ^  ^     |   ^  ^  ^  ^     |   ^  ^  ^  ^     |   ^  ^  ^  ^                                   |
    //   |   |  |  |  |     |   |  |  |  |     |   |  |  |  |     |   |  |  |  |                                   |
    //   |   hi x bits      |   hi x bits      |   hi x bits      |   hi x bits                                    |
    //   +------------------+------------------+------------------+------------------------------------------------+
    //   |                  |                  |                  |                                                |
    //   |       ....       |    ...           |      ...         |                      ...                       |
    //   |                  |                  |                  |                                                |
    //   +------------------+------------------+------------------+------------------------------------------------+
    //   | Sprites #112-115 | Sprites #116-119 | Sprites #120-123 | Sprites #124-127                               |
    //   |                  |                  |                  |                                                |
    //   |                  |                  |                  |                                                |
    //   | size select bits | size select bits | size select bits | size select bits                               |
    //   |  |  |  |  |      |  |  |  |  |      |  |  |  |  |      |  |  |  |  |                                    |
    //   |  v  v  v  v      |  v  v  v  v      |  v  v  v  v      |  v  v  v  v                                    |
    //   | +--+--+--+--+    | +--+--+--+--+    | +--+--+--+--+    | +--+--+--+--+                                  |
    //   | |s |s |s |s |    | |s |s |s |s |    | |s |s |s |s |    | |s |s |s |s |                                  |
    //   | |#4|#3|#2|#1|    | |#4|#3|#2|#1|    | |#4|#3|#2|#1|    | |#4|#3|#2|#1|                                  |
    //   | | x| x| x| x|    | | x| x| x| x|    | | x| x| x| x|    | | x| x| x| x|                                  |
    //   | +--+--+--+--+    | +--+--+--+--+    | +--+--+--+--+    | +--+--+--+--+                                  |
    //   |   ^  ^  ^  ^     |   ^  ^  ^  ^     |   ^  ^  ^  ^     |   ^  ^  ^  ^                                   |
    //   |   |  |  |  |     |   |  |  |  |     |   |  |  |  |     |   |  |  |  |                                   |
    //   |   hi x bits      |   hi x bits      |   hi x bits      |   hi x bits                                    |
    //   +------------------+------------------+------------------+------------------------------------------------+
    pub fn read(oam: &Vec<u8>, obj_index: u8, objsel: mmio::OBJSEL) -> Self {
        let obj_index = obj_index as usize;
        let mod4 = obj_index & 0b11;
        let d_ext = oam[512 + obj_index / 4];
        let base = obj_index * 4;
        let raw = oam[base + 3];
        let x = {
            let x = oam[base] as i16;
            let high_x = ((d_ext >> (mod4 * 2)) & 1) == 1;
            let sub = if high_x { 256 } else { 0 };
            x - sub
        };
        Self {
            x,
            y: oam[base + 1],
            tile: oam[base + 2],
            name_select: (raw & 1) != 0,
            palette: (raw >> 1) & 0b111,
            priority: (raw >> 4) & 0b11,
            flip_horizontal: ((raw >> 6) & 1) != 0,
            flip_vertical: ((raw >> 7) & 1) != 0,
            size: {
                let sel = ((d_ext >> (mod4 * 2 + 1)) & 1) == 1;
                let size = objsel.obj_size();
                if sel {
                    size.1
                } else {
                    size.0
                }
            },
        }
    }
}
