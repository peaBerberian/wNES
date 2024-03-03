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
            data: vec![0; (Frame::WIDTH) * (Frame::HEIGHT) * 3],
        }
    }

    /// Set a specific frame to a specific color.
    pub(super) fn set_pixel(&mut self, x: usize, y: usize, rgb: (u8, u8, u8)) {
        let base = y * 3 * Frame::WIDTH + x * 3;
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

    last_pixel: usize,
}

impl FrameRenderer {
    pub(super) fn new(mirroring: Mirroring) -> Self {
        Self {
            frame: Frame::new(),
            mirroring,
            oam_data: [0; 64 * 4],
            palette: [0; 32],
            vram: [0; 2048],
            last_pixel: 0,
        }
    }

    pub(super) fn frame(&self) -> &Frame {
        &self.frame
    }

    pub(super) fn construct_frame(
        &mut self,
        chr_rom: &[u8],
        registers: &NesPpuRegister,
        until: usize,
    ) {
        self.render_background_on_frame(chr_rom, registers, until);

        for i in (0..self.oam_data.len()).step_by(4).rev() {
            let tile_idx = self.oam_data[i + 1] as u16;
            let tile_x = self.oam_data[i + 3] as usize;
            let tile_y = self.oam_data[i] as usize;

            let flip_vertical = if self.oam_data[i + 2] >> 7 & 1 == 1 {
                true
            } else {
                false
            };
            let flip_horizontal = if self.oam_data[i + 2] >> 6 & 1 == 1 {
                true
            } else {
                false
            };
            let pallette_idx = self.oam_data[i + 2] & 0b11;

            // TODO
            let _is_behind_background = self.oam_data[i + 2] & 0b0010_0000 > 0;
            let sprite_palette = sprite_palette(&self.palette, pallette_idx);
            let sprite_size = registers.ctrl.sprite_size();

            let tile = match sprite_size {
                SpriteSize::Size8x16 => {
                    let base = if tile_idx % 2 == 0 { 0 } else { 0x1000 };
                    let start_byte = base + ((tile_idx / 2) * 0x0020) as usize;
                    // TODO this has never been tested, to check
                    &chr_rom[start_byte..=start_byte + 31]
                }

                SpriteSize::Size8x8 => {
                    let bank: u16 = registers.ctrl.sprite_pattern_table_address();
                    let start_byte = (bank + tile_idx * 16) as usize;
                    &chr_rom[start_byte..=start_byte + 15]
                }
            };

            // TODO unsure of how to progress for 8x16 sprites
            for y in 0..=7 {
                let mut upper = tile[y];
                let mut lower = tile[y + 8];
                'x_loop: for x in (0..=7).rev() {
                    let value = (1 & lower) << 1 | (1 & upper);
                    upper = upper >> 1;
                    lower = lower >> 1;
                    let rgb = match value {
                        0 => continue 'x_loop, // skip coloring the pixel
                        1 => SYSTEM_PALLETE[sprite_palette[1] as usize],
                        2 => SYSTEM_PALLETE[sprite_palette[2] as usize],
                        3 => SYSTEM_PALLETE[sprite_palette[3] as usize],
                        _ => panic!("can't be"),
                    };
                    let (x, y) = match (flip_horizontal, flip_vertical) {
                        (false, false) => (tile_x + x, tile_y + y),
                        (true, false) => (tile_x + 7 - x, tile_y + y),
                        (false, true) => (tile_x + x, tile_y + 7 - y),
                        (true, true) => (tile_x + 7 - x, tile_y + 7 - y),
                    };
                    let curr_pixel = y * 256 + x;
                    if curr_pixel <= until && curr_pixel >= self.last_pixel {
                        self.frame.set_pixel(x, y, rgb);
                    }
                }
            }
        }
        if until < 61440 {
            self.last_pixel = until + 1;
        } else {
            self.last_pixel = 0;
        }
    }

    fn render_background_on_frame(
        &mut self,
        chr_rom: &[u8],
        registers: &NesPpuRegister,
        until: usize,
    ) {
        let bank = registers.ctrl.background_pattern_table_address();

        let scroll_x = (registers.scroll.horizontal_offset()) as usize;
        let scroll_y = (registers.scroll.vertical_offset()) as usize;

        let (main_nametable, second_nametable) =
            match (&self.mirroring, registers.ctrl.base_nametable_address()) {
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
                _ => panic!("Impossible"),
            };

        let second_nametable_config = match &self.mirroring {
            Mirroring::Vertical => (
                second_nametable,
                0,                       // crop_start_x
                0,                       // crop_start_y
                scroll_x,                // crop_end_x,
                240,                     // crop_end_y
                256 - scroll_x as isize, // pixel_offset_x
                0,                       // pixel_offset_y
            ),
            Mirroring::Horizontal => (
                second_nametable,
                0,                       // crop_start_x
                0,                       // crop_start_y
                256,                     // crop_end_x,
                scroll_y,                // crop_end_y
                0,                       // pixel_offset_x
                240 - scroll_y as isize, // pixel_offset_y
            ),
            _ => panic!("Not implemented mirroring yet: {:?}", &self.mirroring),
        };

        let name_table_configs = [
            (
                main_nametable,
                scroll_x,             // crop_start_x
                scroll_y,             // crop_start_y
                256,                  // crop_end_x,
                240,                  // crop_end_y
                -(scroll_x as isize), // pixel_offset_x
                -(scroll_y as isize), // pixel_offset_y
            ),
            second_nametable_config,
        ];

        name_table_configs.into_iter().for_each(
            |(
                name_table,
                crop_start_x,
                crop_start_y,
                crop_end_x,
                crop_end_y,
                pixel_offset_x,
                pixel_offset_y,
            )| {
                let base_nt_idx = (crop_start_y / 8) * 32;
                let end_nt_idx = ((crop_end_y) / 8) * 32;
                for i in base_nt_idx..end_nt_idx {
                    let tile_column = i % 32;
                    let tile_row = i / 32;
                    let attribute_table = &name_table[0x3c0..0x400];
                    let bg_pal =
                        background_palette(&self.palette, attribute_table, tile_column, tile_row);

                    let tile = name_table[i] as u16;
                    let tile =
                        &chr_rom[(bank + tile * 16) as usize..=(bank + tile * 16 + 15) as usize];
                    for y in 0..=7 {
                        let mut upper = tile[y];
                        let mut lower = tile[y + 8];
                        for x in (0..=7).rev() {
                            let pixel_x = tile_column * 8 + x;
                            let pixel_y = tile_row * 8 + y;
                            if pixel_x >= crop_start_x
                                && pixel_x < crop_end_x
                                && pixel_y >= crop_start_y
                                && pixel_y < crop_end_y
                            {
                                let x = (pixel_x as isize + pixel_offset_x) as usize;
                                let y = (pixel_y as isize + pixel_offset_y) as usize;
                                let curr_pixel = y * 256 + x;
                                if curr_pixel >= self.last_pixel && curr_pixel <= until {
                                    let value = (1 & lower) << 1 | (1 & upper);
                                    upper = upper >> 1;
                                    lower = lower >> 1;
                                    let rgb = match value {
                                        0 => SYSTEM_PALLETE[self.palette[0] as usize],
                                        1 => SYSTEM_PALLETE[bg_pal[1] as usize],
                                        2 => SYSTEM_PALLETE[bg_pal[2] as usize],
                                        3 => SYSTEM_PALLETE[bg_pal[3] as usize],
                                        _ => panic!("can't be"),
                                    };
                                    self.frame.set_pixel(x, y, rgb);
                                }
                            }
                        }
                    }
                }
            },
        );
    }
}

fn sprite_palette(palette: &[u8], pallete_idx: u8) -> [u8; 4] {
    let start = 0x11 + (pallete_idx * 4) as usize;
    [0, palette[start], palette[start + 1], palette[start + 2]]
}

fn background_palette(
    palette: &[u8],
    attribute_table: &[u8],
    tile_column: usize,
    tile_row: usize,
) -> [u8; 4] {
    let attr_table_idx = tile_row / 4 * 8 + tile_column / 4;
    let attr_byte = attribute_table[attr_table_idx]; // note: still using hardcoded first nametable

    let pallet_idx = match (tile_column % 4 / 2, tile_row % 4 / 2) {
        (0, 0) => attr_byte & 0b11,
        (1, 0) => (attr_byte >> 2) & 0b11,
        (0, 1) => (attr_byte >> 4) & 0b11,
        (1, 1) => (attr_byte >> 6) & 0b11,
        (_, _) => panic!("should not happen"),
    };

    let pallete_start: usize = 1 + (pallet_idx as usize) * 4;
    [
        palette[0],
        palette[pallete_start],
        palette[pallete_start + 1],
        palette[pallete_start + 2],
    ]
}
