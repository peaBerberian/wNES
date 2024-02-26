mod bus;
mod controller;
mod cpu;
mod native_ui;
mod ppu;
mod rom;

use sdl2::event::Event;
use sdl2::keyboard::Keycode;
// use sdl2::pixels::Color;
use sdl2::pixels::PixelFormatEnum;
// use sdl2::EventPump;

// use cpu::{CpuComputationResult, NesCpu};
use ppu::{Frame, NesPpu, SYSTEM_PALLETE};

fn main() {
    let rom = match std::fs::read("./pacman_eu.nes") {
        Ok(rom) => rom,
        Err(e) => {
            eprintln!("Failed to read ROM.nes file: {}", e);
            return;
        }
    };

    let parsed = match rom::Rom::from_ines_file(rom) {
        Err(e) => {
            eprintln!("Could not read loaded ROM {:?}", e);
            return;
        }
        Ok(parsed) => parsed,
    };

    // init sdl2
    let sdl_context = sdl2::init().unwrap();
    let window = sdl_context
        .video()
        .unwrap()
        .window("wNES", (256.0 * 3.0) as u32, (240.0 * 3.0) as u32)
        .position_centered()
        .build()
        .unwrap();

    let mut event_pump = sdl_context.event_pump().unwrap();

    let mut canvas = window.into_canvas().present_vsync().build().unwrap();
    canvas.set_scale(3.0, 3.0).unwrap();

    let creator = canvas.texture_creator();
    let mut texture = creator
        .create_texture_target(PixelFormatEnum::RGB24, 256, 240)
        .unwrap();

    // let mut rng = rand::thread_rng();

    let mut frame = Frame::new();

    let bus = bus::NesBus::new(&parsed, move |ppu: &NesPpu, (controller1, _)| {
        render(ppu, &mut frame);
        texture.update(None, &frame.data, 256 * 3).unwrap();
        canvas.copy(&texture, None, None).unwrap();
        canvas.present();
        for event in event_pump.poll_iter() {
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
    });
    let mut cpu = cpu::NesCpu::new(bus);
    loop {
        match cpu.next_op() {
            _ => {}
        }
    }
}

fn background_palette(ppu: &NesPpu, tile_column: usize, tile_row: usize) -> [u8; 4] {
    let attr_table_idx = tile_row / 4 * 8 + tile_column / 4;
    let attr_byte = ppu.vram[0x3c0 + attr_table_idx]; // note: still using hardcoded first nametable

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

pub(crate) fn render(ppu: &NesPpu, frame: &mut Frame) {
    let bank = ppu.reg_ctrl.background_pattern_table_address();

    for i in 0..0x3c0 {
        let tile_column = i % 32;
        let tile_row = i / 32;
        let bg_pal = background_palette(ppu, tile_column, tile_row);

        let tile = ppu.vram[i] as u16;
        let tile = &ppu.chr_rom[(bank + tile * 16) as usize..=(bank + tile * 16 + 15) as usize];
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
                frame.set_pixel(tile_column * 8 + x, tile_row * 8 + y, rgb)
            }
        }
    }

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
        let sprite_palette = sprite_palette(ppu, pallette_idx);
        let bank: u16 = ppu.reg_ctrl.sprite_pattern_table_address();

        let tile =
            &ppu.chr_rom[(bank + tile_idx * 16) as usize..=(bank + tile_idx * 16 + 15) as usize];

        for y in 0..=7 {
            let mut upper = tile[y];
            let mut lower = tile[y + 8];
            'ololo: for x in (0..=7).rev() {
                let value = (1 & lower) << 1 | (1 & upper);
                upper = upper >> 1;
                lower = lower >> 1;
                let rgb = match value {
                    0 => continue 'ololo, // skip coloring the pixel
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
