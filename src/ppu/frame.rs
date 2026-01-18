use super::registers::{NesPpuRegister, SpriteSize};
use crate::rom::Mirroring;

#[rustfmt::skip]
pub(crate) static SYSTEM_PALLETE: [(u8,u8,u8); 64] = [
   (0x80, 0x80, 0x80), (0x00, 0x3D, 0xA6), (0x00, 0x12, 0xB0), (0x44, 0x00, 0x96), (0xA1, 0x00, 0x5E),
   (0xC7, 0x00, 0x28), (0xBA, 0x06, 0x00), (0x8C, 0x17, 0x00), (0x5C, 0x2F, 0x00), (0x10, 0x45, 0x00),
   (0x05, 0x4A, 0x00), (0x00, 0x47, 0x2E), (0x00, 0x41, 0x66), (0x00, 0x00, 0x00), (0x05, 0x05, 0x05),
   (0x05, 0x05, 0x05), (0xC7, 0xC7, 0xC7), (0x00, 0x77, 0xFF), (0x21, 0x55, 0xFF), (0x82, 0x37, 0xFA),
   (0xEB, 0x2F, 0xB5), (0xFF, 0x29, 0x50), (0xFF, 0x22, 0x00), (0xD6, 0x32, 0x00), (0xC4, 0x62, 0x00),
   (0x35, 0x80, 0x00), (0x05, 0x8F, 0x00), (0x00, 0x8A, 0x55), (0x00, 0x99, 0xCC), (0x21, 0x21, 0x21),
   (0x09, 0x09, 0x09), (0x09, 0x09, 0x09), (0xFF, 0xFF, 0xFF), (0x0F, 0xD7, 0xFF), (0x69, 0xA2, 0xFF),
   (0xD4, 0x80, 0xFF), (0xFF, 0x45, 0xF3), (0xFF, 0x61, 0x8B), (0xFF, 0x88, 0x33), (0xFF, 0x9C, 0x12),
   (0xFA, 0xBC, 0x20), (0x9F, 0xE3, 0x0E), (0x2B, 0xF0, 0x35), (0x0C, 0xF0, 0xA4), (0x05, 0xFB, 0xFF),
   (0x5E, 0x5E, 0x5E), (0x0D, 0x0D, 0x0D), (0x0D, 0x0D, 0x0D), (0xFF, 0xFF, 0xFF), (0xA6, 0xFC, 0xFF),
   (0xB3, 0xEC, 0xFF), (0xDA, 0xAB, 0xEB), (0xFF, 0xA8, 0xF9), (0xFF, 0xAB, 0xB3), (0xFF, 0xD2, 0xB0),
   (0xFF, 0xEF, 0xA6), (0xFF, 0xF7, 0x9C), (0xD7, 0xE8, 0x95), (0xA6, 0xED, 0xAF), (0xA2, 0xF2, 0xDA),
   (0x99, 0xFF, 0xFC), (0xDD, 0xDD, 0xDD), (0x11, 0x11, 0x11), (0x11, 0x11, 0x11)
];

pub(crate) struct Frame {
    /// Whole Data representing a frame of WIDTH*HEIGHT dimensions, left to right and top to bottom
    /// where each pixel are represented through three u8 for respectively red green and blue
    /// components.
    pub data: Vec<u8>,
}

impl Frame {
    const WIDTH: usize = 256;
    const HEIGHT: usize = 240;

    // Create a new void (entirely black) frame.
    pub(super) fn new() -> Self {
        Frame {
            data: vec![0; Frame::WIDTH * Frame::HEIGHT * 3],
        }
    }

    /// Set a specific frame to a specific color.
    pub(super) fn set_pixel(&mut self, coord: PixelCoord, rgb: (u8, u8, u8)) {
        let base = coord.y * 3 * Frame::WIDTH + coord.x * 3;
        if base + 2 < self.data.len() {
            self.data[base] = rgb.0;
            self.data[base + 1] = rgb.1;
            self.data[base + 2] = rgb.2;
        }
    }
}

pub(super) struct FrameRenderer {
    frame: Frame,
    pub(super) mirroring: Mirroring,
    pub(super) palette: [u8; 32],
    pub(super) vram: [u8; 2048],

    /// OAM data port
    /// https://www.nesdev.org/wiki/PPU_registers#OAM_data_($2004)_%3C%3E_read/write
    pub(super) oam_data: [u8; 256],
}

impl FrameRenderer {
    pub(super) fn new(mirroring: Mirroring) -> Self {
        Self {
            frame: Frame::new(),
            mirroring,
            oam_data: [0; 64 * 4],
            palette: [0; 32],
            vram: [0; 2048],
        }
    }

    pub(super) fn frame(&self) -> &Frame {
        &self.frame
    }

    pub(super) fn render_scanline(
        &mut self,
        chr_rom: &[u8],
        registers: &NesPpuRegister,
        scanline: usize,
    ) {
        if scanline >= 240 {
            return;
        }
        for x in 0..256 {
            let pixel = PixelCoord::new(x, scanline);
            self.render_background_pixel(chr_rom, registers, pixel);
        }
        self.render_sprites_for_scanline(chr_rom, registers, scanline);
    }

    fn render_background_pixel(
        &mut self,
        chr_rom: &[u8],
        registers: &NesPpuRegister,
        screen_pixel: PixelCoord,
    ) {
        let scroll_x = registers.scroll.horizontal_offset() as usize;
        let scroll_y = registers.scroll.vertical_offset() as usize;

        let mut scrolled_x = screen_pixel.x + scroll_x;
        let mut scrolled_y = screen_pixel.y + scroll_y;

        let mut use_second_nametable = false;

        // Handle horizontal wrapping and nametable switching
        if scrolled_x >= 256 {
            scrolled_x -= 256;
            if self.mirroring == Mirroring::Vertical {
                use_second_nametable = true;
            }
        }

        // Handle vertical wrapping and nametable switching
        if scrolled_y >= 240 {
            scrolled_y -= 240;
            if self.mirroring == Mirroring::Horizontal {
                use_second_nametable = true;
            }
        }

        let nametable_pixel = PixelCoord::new(scrolled_x, scrolled_y);
        let tile_coord = TileCoord::from_pixel(nametable_pixel);

        let (nametable, attribute_table) = self.get_nametable_and_attributes(
            use_second_nametable,
            registers.ctrl.base_nametable_address(),
        );

        let bg_palette = background_palette(&self.palette, attribute_table, tile_coord);

        let tile_index = nametable[tile_coord.to_nametable_index()] as u16;
        let bank = registers.ctrl.background_pattern_table_address();
        let tile_data =
            &chr_rom[(bank + tile_index * 16) as usize..=(bank + tile_index * 16 + 15) as usize];

        let (pixel_x_in_tile, pixel_y_in_tile) = tile_coord.pixel_within_tile(nametable_pixel);

        let upper = tile_data[pixel_y_in_tile];
        let lower = tile_data[pixel_y_in_tile + 8];

        let shift = 7 - pixel_x_in_tile;
        let value = ((lower >> shift) & 1) << 1 | ((upper >> shift) & 1);

        let rgb = match value {
            0 => SYSTEM_PALLETE[self.palette[0] as usize],
            1 => SYSTEM_PALLETE[bg_palette[1] as usize],
            2 => SYSTEM_PALLETE[bg_palette[2] as usize],
            3 => SYSTEM_PALLETE[bg_palette[3] as usize],
            _ => unreachable!(),
        };

        self.frame.set_pixel(screen_pixel, rgb);
    }

    fn get_nametable_and_attributes(
        &self,
        use_second: bool,
        base_nametable_addr: u16,
    ) -> (&[u8], &[u8]) {
        let (main_nt, second_nt) = match (&self.mirroring, base_nametable_addr) {
            (Mirroring::Vertical, 0x2000) | (Mirroring::Vertical, 0x2800) => {
                (&self.vram[0..0x400], &self.vram[0x400..0x800])
            }
            (Mirroring::Vertical, 0x2400) | (Mirroring::Vertical, 0x2C00) => {
                (&self.vram[0x400..0x800], &self.vram[0..0x400])
            }
            (Mirroring::Horizontal, 0x2000) | (Mirroring::Horizontal, 0x2400) => {
                (&self.vram[0..0x400], &self.vram[0x400..0x800])
            }
            (Mirroring::Horizontal, 0x2800) | (Mirroring::Horizontal, 0x2C00) => {
                (&self.vram[0x400..0x800], &self.vram[0..0x400])
            }
            (_, 0x2000) | (_, 0x2400) => (&self.vram[0..0x400], &self.vram[0..0x400]),
            (_, 0x2800) | (_, 0x2C00) => (&self.vram[0x400..0x800], &self.vram[0x400..0x800]),
            _ => panic!("Impossible nametable address"),
        };

        let nametable = if use_second { second_nt } else { main_nt };
        let attribute_table = &nametable[0x3c0..0x400];
        (nametable, attribute_table)
    }

    fn render_sprites_for_scanline(
        &mut self,
        chr_rom: &[u8],
        registers: &NesPpuRegister,
        scanline: usize,
    ) {
        // Sprites are evaluated in reverse priority order
        for sprite_index in (0..64).rev() {
            let oam_offset = sprite_index * 4;
            let sprite_y = self.oam_data[oam_offset] as usize;
            let tile_idx = self.oam_data[oam_offset + 1] as u16;
            let attributes = self.oam_data[oam_offset + 2];
            let sprite_x = self.oam_data[oam_offset + 3] as usize;

            let sprite_size = registers.ctrl.sprite_size();
            let sprite_height = match sprite_size {
                SpriteSize::Size8x8 => 8,
                SpriteSize::Size8x16 => 16,
            };

            // Check if sprite is on this scanline
            if scanline < sprite_y || scanline >= sprite_y + sprite_height {
                continue;
            }

            let flip_vertical = (attributes >> 7) & 1 == 1;
            let flip_horizontal = (attributes >> 6) & 1 == 1;
            let palette_idx = attributes & 0b11;
            let _behind_background = attributes & 0b0010_0000 != 0; // TODO: implement priority

            let sprite_palette = sprite_palette(&self.palette, palette_idx);

            let tile_data = match sprite_size {
                SpriteSize::Size8x16 => {
                    let bank = if tile_idx % 2 == 0 { 0 } else { 0x1000 };
                    let start_byte = bank + ((tile_idx / 2) * 0x0020) as usize;
                    &chr_rom[start_byte..start_byte + 32]
                }
                SpriteSize::Size8x8 => {
                    let bank = registers.ctrl.sprite_pattern_table_address();
                    let start_byte = (bank + tile_idx * 16) as usize;
                    &chr_rom[start_byte..start_byte + 16]
                }
            };

            let y_in_sprite = scanline - sprite_y;
            let y_in_tile = if flip_vertical {
                7 - (y_in_sprite % 8)
            } else {
                y_in_sprite % 8
            };

            let tile_offset = if sprite_size == SpriteSize::Size8x16 && y_in_sprite >= 8 {
                16
            } else {
                0
            };

            let upper = tile_data[tile_offset + y_in_tile];
            let lower = tile_data[tile_offset + y_in_tile + 8];

            for x_in_sprite in 0..8 {
                let x_in_tile = if flip_horizontal {
                    x_in_sprite
                } else {
                    7 - x_in_sprite
                };

                let value = ((lower >> x_in_tile) & 1) << 1 | ((upper >> x_in_tile) & 1);

                if value == 0 {
                    continue; // Transparent pixel
                }

                let screen_x = sprite_x + x_in_sprite;
                if screen_x >= 256 {
                    continue;
                }

                let rgb = match value {
                    1 => SYSTEM_PALLETE[sprite_palette[1] as usize],
                    2 => SYSTEM_PALLETE[sprite_palette[2] as usize],
                    3 => SYSTEM_PALLETE[sprite_palette[3] as usize],
                    _ => unreachable!(),
                };

                let pixel = PixelCoord::new(screen_x, scanline);
                self.frame.set_pixel(pixel, rgb);
            }
        }
    }

    // TODO: Implement proper sprite 0 hit detection
    // Should check:
    // 1. Sprite 0 is enabled and visible on current scanline
    // 2. Background is enabled
    // 3. Both sprite 0 pixel and background pixel are opaque (non-zero)
    // 4. X coordinate is not 255
    // 5. Rendering is enabled in leftmost 8 pixels (if x < 8)
    pub(super) fn check_sprite_0_hit(&self, _scanline: usize, _cycle: usize) -> bool {
        // Placeholder - not yet implemented correctly
        false
    }
}

fn sprite_palette(palette: &[u8], palette_idx: u8) -> [u8; 4] {
    let start = 0x11 + (palette_idx * 4) as usize;
    [0, palette[start], palette[start + 1], palette[start + 2]]
}

fn background_palette(palette: &[u8], attribute_table: &[u8], tile_coord: TileCoord) -> [u8; 4] {
    let attr_table_idx = tile_coord.row / 4 * 8 + tile_coord.col / 4;
    let attr_byte = attribute_table[attr_table_idx];

    let palette_idx = match (tile_coord.col % 4 / 2, tile_coord.row % 4 / 2) {
        (0, 0) => attr_byte & 0b11,
        (1, 0) => (attr_byte >> 2) & 0b11,
        (0, 1) => (attr_byte >> 4) & 0b11,
        (1, 1) => (attr_byte >> 6) & 0b11,
        _ => unreachable!(),
    };

    let palette_start = 1 + (palette_idx as usize) * 4;
    [
        palette[0],
        palette[palette_start],
        palette[palette_start + 1],
        palette[palette_start + 2],
    ]
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) struct PixelCoord {
    pub x: usize,
    pub y: usize,
}

impl PixelCoord {
    pub fn new(x: usize, y: usize) -> Self {
        Self { x, y }
    }

    pub fn to_linear(self) -> usize {
        self.y * 256 + self.x
    }

    pub fn from_linear(pos: usize) -> Self {
        Self {
            x: pos % 256,
            y: pos / 256,
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub(crate) struct TileCoord {
    pub col: usize,
    pub row: usize,
}

impl TileCoord {
    pub fn new(col: usize, row: usize) -> Self {
        Self { col, row }
    }

    pub fn from_pixel(pixel: PixelCoord) -> Self {
        Self {
            col: pixel.x / 8,
            row: pixel.y / 8,
        }
    }

    pub fn to_nametable_index(self) -> usize {
        self.row * 32 + self.col
    }

    pub fn pixel_within_tile(&self, pixel: PixelCoord) -> (usize, usize) {
        (pixel.x % 8, pixel.y % 8)
    }
}
