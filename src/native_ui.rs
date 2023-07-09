/// # Native UI
///
/// Code implementing the UI logic and event loop when running the emulator under SDL12.

use sdl2::event::Event;
use sdl2::EventPump;
use sdl2::keyboard::Keycode;
use sdl2::pixels::Color;
use sdl2::pixels::PixelFormatEnum;

use super::cpu::{NesCpu, CpuComputationResult};

/// This implementation rely on a frame_buffer mapped to specific address starting from
/// `FRAME_BUFFER_START` (top left) to `FRAME_BUFFER_END` (bottom right). Each location
/// contains the Red value, Green value and Blue value of each pixel on a single byte.
///
/// `u16` because 16-bit addressing is typically used to point to addresses here.
const FRAME_BUFFER_START: u16 = 0x200;
const FRAME_BUFFER_END: u16 = 0x600;

/// This implementation relies on a Random Number Generator at the following memory location.
/// Its value will be randomly changed at between CPU instruction.
const RNG_ADDR: u16 = 0xFE;

/// Memory address for the last key pressed, as an ASCII code
const KEYPRESS_ADDR: u16 = 0xFF;

pub(super) fn run(
    cpu: &mut NesCpu
) {
    // init sdl2
    let sdl_context = sdl2::init().unwrap();
    let video_subsystem = sdl_context.video().unwrap();
    let window = video_subsystem
        .window("Easy6502 Simulator", (32.0 * 10.0) as u32, (32.0 * 10.0) as u32)
        .position_centered()
        .build().unwrap();

    let mut event_pump = sdl_context.event_pump().unwrap();

    let mut canvas = window.into_canvas().present_vsync().build().unwrap();
    canvas.set_scale(10.0, 10.0).unwrap();
    let creator = canvas.texture_creator();
    let mut texture = creator
        .create_texture_target(PixelFormatEnum::RGB24, 32, 32).unwrap();

    let mut frame_buff = [0u8; 32 * 3 * 32];
    use rand::Rng;
    let mut rng = rand::thread_rng();

    loop {
        read_user_input(cpu, &mut event_pump);
        cpu.write_u8_at(RNG_ADDR, rng.gen_range(1, 16));
        if update_frame_buffer(cpu, &mut frame_buff) {
            texture.update(None, &frame_buff, 32 * 3).unwrap();
            canvas.copy(&texture, None, None).unwrap();
            canvas.present();
        }
        match cpu.next_op() {
            CpuComputationResult { brk: true, .. } => {
                return;
            },
            _ => {},
        }
        std::thread::sleep(std::time::Duration::new(0, 70_000));
    }
}

fn read_user_input(cpu: &mut NesCpu, event_pump: &mut EventPump) {
    for event in event_pump.poll_iter() {
        match event {
            Event::Quit { .. } | Event::KeyDown { keycode: Some(Keycode::Escape), .. } => {
                std::process::exit(0)
            },
            Event::KeyDown { keycode: Some(Keycode::W), .. } => {
                cpu.write_u8_at(KEYPRESS_ADDR, 0x77);
            },
            Event::KeyDown { keycode: Some(Keycode::S), .. } => {
                cpu.write_u8_at(KEYPRESS_ADDR, 0x73);
            },
            Event::KeyDown { keycode: Some(Keycode::A), .. } => {
                cpu.write_u8_at(KEYPRESS_ADDR, 0x61);
            },
            Event::KeyDown { keycode: Some(Keycode::D), .. } => {
                cpu.write_u8_at(KEYPRESS_ADDR, 0x64);
            }
            _ => {}
        }
    }
}

fn byte_to_rgba(byte: u8) -> Color {
   match byte {
       0 => sdl2::pixels::Color::BLACK,
       1 => sdl2::pixels::Color::WHITE,
       2 | 9 => sdl2::pixels::Color::GREY,
       3 | 10 => sdl2::pixels::Color::RED,
       4 | 11 => sdl2::pixels::Color::GREEN,
       5 | 12 => sdl2::pixels::Color::BLUE,
       6 | 13 => sdl2::pixels::Color::MAGENTA,
       7 | 14 => sdl2::pixels::Color::YELLOW,
       _ => sdl2::pixels::Color::CYAN,
   }
}


fn update_frame_buffer(cpu: &mut NesCpu, frame_buff: &mut [u8; 32 * 3 * 32]) -> bool {
   let mut frame_idx = 0;
   let mut update = false;
   for i in FRAME_BUFFER_START..FRAME_BUFFER_END {
       let color_idx = cpu.read_u8_at(i);
       let (b1, b2, b3) = byte_to_rgba(color_idx).rgb();
       if frame_buff[frame_idx] != b1 || frame_buff[frame_idx + 1] != b2 || frame_buff[frame_idx + 2] != b3 {
           frame_buff[frame_idx] = b1;
           frame_buff[frame_idx + 1] = b2;
           frame_buff[frame_idx + 2] = b3;
           update = true;
       }
       frame_idx += 3;
   }
   update
}
