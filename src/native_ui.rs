use sdl2::event::Event;
use sdl2::keyboard::Keycode;
// use sdl2::pixels::Color;
use sdl2::pixels::PixelFormatEnum;
// use sdl2::EventPump;

use crate::controller::NesController;
use crate::ppu::{Frame, NesPpu, SYSTEM_PALLETE};

#[derive(Clone, Debug)]
pub(crate) enum NativeUiError {
    UnknownSdl2Error(String),
}

pub(crate) struct NativeUi {
    event_pump: sdl2::EventPump,
    canvas: sdl2::render::WindowCanvas,
    texture_creator: sdl2::render::TextureCreator<sdl2::video::WindowContext>,
}

impl NativeUi {
    pub(crate) fn try_new() -> Result<Self, NativeUiError> {
        // init sdl2
        let sdl_context = sdl2::init()?;
        let window = sdl_context
            .video()?
            .window("wNES", (256.0 * 3.0) as u32, (240.0 * 3.0) as u32)
            .position_centered()
            .build()?;

        let event_pump = sdl_context.event_pump()?;

        let mut canvas = window.into_canvas().present_vsync().build()?;
        canvas.set_scale(3.0, 3.0)?;

        let creator = canvas.texture_creator();
        // let texture = creator
        //     .create_texture_target(PixelFormatEnum::RGB24, 256, 240)?;

        // // let mut rng = rand::thread_rng();

        Ok(Self {
            event_pump,
            canvas,
            texture_creator: creator,
        })
    }

    pub fn render_frame(
        &mut self,
        frame: &mut Frame,
        ppu: &NesPpu,
        (controller1, _): (&mut NesController, &mut NesController),
    ) {
        render(ppu, frame);
        let mut texture = self
            .texture_creator
            .create_texture_target(PixelFormatEnum::RGB24, 256, 240)
            .unwrap();
        texture.update(None, &frame.data, 256 * 3).unwrap();
        self.canvas.copy(&texture, None, None).unwrap();
        self.canvas.present();
        for event in self.event_pump.poll_iter() {
            match event {
                Event::Quit { .. }
                | Event::KeyDown {
                    keycode: Some(Keycode::Escape),
                    ..
                } => std::process::exit(0),
                Event::KeyDown {
                    keycode: Some(kc), ..
                } => match kc {
                    Keycode::Up => controller1.set_up(true),
                    Keycode::Down => controller1.set_down(true),
                    Keycode::Left => controller1.set_left(true),
                    Keycode::Right => controller1.set_right(true),
                    Keycode::A => controller1.set_a(true),
                    Keycode::B => controller1.set_b(true),
                    Keycode::Return => controller1.set_start(true),
                    Keycode::Backspace => controller1.set_select(true),
                    _ => {}
                },
                Event::KeyUp {
                    keycode: Some(kc), ..
                } => match kc {
                    Keycode::Up => controller1.set_up(false),
                    Keycode::Down => controller1.set_down(false),
                    Keycode::Left => controller1.set_left(false),
                    Keycode::Right => controller1.set_right(false),
                    Keycode::A => controller1.set_a(false),
                    Keycode::B => controller1.set_b(false),
                    Keycode::Return => controller1.set_start(false),
                    Keycode::Backspace => controller1.set_select(false),
                    _ => {}
                },
                _ => { /* do nothing */ }
            }
        }
    }
}

impl From<String> for NativeUiError {
    fn from(err: String) -> Self {
        NativeUiError::UnknownSdl2Error(err)
    }
}

impl From<sdl2::video::WindowBuildError> for NativeUiError {
    fn from(err: sdl2::video::WindowBuildError) -> Self {
        NativeUiError::UnknownSdl2Error(err.to_string())
    }
}

impl From<sdl2::IntegerOrSdlError> for NativeUiError {
    fn from(err: sdl2::IntegerOrSdlError) -> Self {
        NativeUiError::UnknownSdl2Error(err.to_string())
    }
}

impl From<sdl2::render::TextureValueError> for NativeUiError {
    fn from(err: sdl2::render::TextureValueError) -> Self {
        NativeUiError::UnknownSdl2Error(err.to_string())
    }
}

fn background_palette(
    ppu: &NesPpu,
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
        ppu.palette[0],
        ppu.palette[pallete_start],
        ppu.palette[pallete_start + 1],
        ppu.palette[pallete_start + 2],
    ]
}

fn sprite_palette(ppu: &NesPpu, pallete_idx: u8) -> [u8; 4] {
    let start = 0x11 + (pallete_idx * 4) as usize;
    [
        0,
        ppu.palette[start],
        ppu.palette[start + 1],
        ppu.palette[start + 2],
    ]
}

fn render_background(ppu: &NesPpu, frame: &mut Frame) {
    let bank = ppu.reg_ctrl.background_pattern_table_address();

    let scroll_x = (ppu.reg_scroll.horizontal_offset()) as usize;
    let scroll_y = (ppu.reg_scroll.vertical_offset()) as usize;

    if scroll_x > 0 || scroll_y > 0 {
        println!("scroll: {scroll_x}, {scroll_y}");
    }

    // TODO handle vertical scrolling and/or mirroring
    use crate::rom::Mirroring;
    let (main_nametable, second_nametable) =
        match (&ppu.mirroring, ppu.reg_ctrl.base_nametable_address()) {
            (Mirroring::Vertical, 0x2000) | (Mirroring::Vertical, 0x2800) => {
                (&ppu.vram[0..0x400], &ppu.vram[0x400..0x800])
            }
            (Mirroring::Vertical, 0x2400) | (Mirroring::Vertical, 0x2C00) => {
                (&ppu.vram[0x400..0x800], &ppu.vram[0..0x400])
            }
            (_, _) => {
                panic!("Not supported mirroring type {:?}", ppu.mirroring);
            }
        };

    let name_table_configs = [
        (
            main_nametable,
            scroll_x, // crop_start_x
            scroll_y, // crop_start_y
            256,      // crop_end_x,
            240,      // crop_end_y
            -(scroll_x as isize), // pixel_offset_x
            -(scroll_y as isize), // pixel_offset_y
        ),
        (
            second_nametable,
            0,              // crop_start_x
            0,              // crop_start_y
            scroll_x,       // crop_end_x,
            240,            // crop_end_y
            256 - scroll_x as isize, // pixel_offset_x
            0,              // pixel_offset_y
        ),
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
            for i in 0..0x3c0 {
                let tile_column = i % 32;
                let tile_row = i / 32;
                let attribute_table = &name_table[0x3c0..0x400];
                let bg_pal = background_palette(ppu, attribute_table, tile_column, tile_row);

                let tile = ppu.vram[i] as u16;
                let tile =
                    &ppu.chr_rom[(bank + tile * 16) as usize..=(bank + tile * 16 + 15) as usize];
                for y in 0..=7 {
                    let mut upper = tile[y];
                    let mut lower = tile[y + 8];

                    for x in (0..=7).rev() {
                        let value = (1 & lower) << 1 | (1 & upper);
                        upper = upper >> 1;
                        lower = lower >> 1;
                        let rgb = match value {
                            0 => SYSTEM_PALLETE[ppu.palette[0] as usize],
                            1 => SYSTEM_PALLETE[bg_pal[1] as usize],
                            2 => SYSTEM_PALLETE[bg_pal[2] as usize],
                            3 => SYSTEM_PALLETE[bg_pal[3] as usize],
                            _ => panic!("can't be"),
                        };
                        let pixel_x = tile_column * 8 + x;
                        let pixel_y = tile_row * 8 + y;

                        if pixel_x >= crop_start_x
                            && pixel_x < crop_end_x
                            && pixel_y >= crop_start_y
                            && y < crop_end_y
                        {
                            frame.set_pixel((pixel_x as isize + pixel_offset_x) as usize, (pixel_y as isize + pixel_offset_y) as usize, rgb)
                        }
                    }
                }
            }
        },
    );
}

pub(crate) fn render(ppu: &NesPpu, frame: &mut Frame) {
    render_background(ppu, frame);
    for i in (0..ppu.oam_data.len()).step_by(4).rev() {
        let tile_idx = ppu.oam_data[i + 1] as u16;
        let tile_x = ppu.oam_data[i + 3] as usize;
        let tile_y = ppu.oam_data[i] as usize;

        let flip_vertical = if ppu.oam_data[i + 2] >> 7 & 1 == 1 {
            true
        } else {
            false
        };
        let flip_horizontal = if ppu.oam_data[i + 2] >> 6 & 1 == 1 {
            true
        } else {
            false
        };
        let pallette_idx = ppu.oam_data[i + 2] & 0b11;

        // TODO
        let _is_behind_background = ppu.oam_data[i + 2] & 0b0010_0000 > 0;
        let sprite_palette = sprite_palette(ppu, pallette_idx);
        let sprite_size = ppu.reg_ctrl.sprite_size();

        let tile = match sprite_size {
            crate::ppu::SpriteSize::Size8x16 => {
                let base = if tile_idx % 2 == 0 { 0 } else { 0x1000 };
                let start_byte = base + ((tile_idx / 2) * 0x0020) as usize;
                // TODO this has never been tested, to check
                &ppu.chr_rom[start_byte..=start_byte + 31]
            }

            crate::ppu::SpriteSize::Size8x8 => {
                let bank: u16 = ppu.reg_ctrl.sprite_pattern_table_address();
                let start_byte = (bank + tile_idx * 16) as usize;
                &ppu.chr_rom[start_byte..=start_byte + 15]
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
                match (flip_horizontal, flip_vertical) {
                    (false, false) => frame.set_pixel(tile_x + x, tile_y + y, rgb),
                    (true, false) => frame.set_pixel(tile_x + 7 - x, tile_y + y, rgb),
                    (false, true) => frame.set_pixel(tile_x + x, tile_y + 7 - y, rgb),
                    (true, true) => frame.set_pixel(tile_x + 7 - x, tile_y + 7 - y, rgb),
                }
            }
        }
    }
}
