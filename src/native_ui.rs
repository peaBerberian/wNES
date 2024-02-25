///// # Native UI
/////
///// Code implementing the UI logic and event loop when running the emulator under SDL12.
//use sdl2::event::Event;
//use sdl2::keyboard::Keycode;
//use sdl2::pixels::Color;
//use sdl2::pixels::PixelFormatEnum;
//use sdl2::EventPump;

//use super::cpu::{CpuComputationResult, NesCpu};
//use super::ppu::{Frame, NesPpu, SYSTEM_PALLETE};

///// This implementation rely on a frame_buffer mapped to specific address starting from
///// `FRAME_BUFFER_START` (top left) to `FRAME_BUFFER_END` (bottom right). Each location
///// contains the Red value, Green value and Blue value of each pixel on a single byte.
/////
///// `u16` because 16-bit addressing is typically used to point to addresses here.
//const FRAME_BUFFER_START: u16 = 0x200;
//const FRAME_BUFFER_END: u16 = 0x600;

///// This implementation relies on a Random Number Generator at the following memory location.
///// Its value will be randomly changed at between CPU instruction.
//const RNG_ADDR: u16 = 0xFE;

///// Memory address for the last key pressed, as an ASCII code
//const KEYPRESS_ADDR: u16 = 0xFF;

//pub(super) fn run(cpu: &mut NesCpu) {
//    // init sdl2
//    let sdl_context = sdl2::init().unwrap();
//    let window = sdl_context
//        .video()
//        .unwrap()
//        .window("wNES", (256.0 * 3.0) as u32, (240.0 * 3.0) as u32)
//        .position_centered()
//        .build()
//        .unwrap();

//    let mut event_pump = sdl_context.event_pump().unwrap();

//    let mut canvas = window.into_canvas().present_vsync().build().unwrap();
//    canvas.set_scale(3.0, 3.0).unwrap();

//    let creator = canvas.texture_creator();
//    let mut texture = creator
//        .create_texture_target(PixelFormatEnum::RGB24, 256, 240)
//        .unwrap();

//    use rand::Rng;
//    let mut rng = rand::thread_rng();

//    // {
//    //     let bank = cpu.bus.ppu.reg_ctrl.background_pattern_table_address();
//    //     let vram = &cpu.bus.ppu.vram;
//    //     let chr_rom = &cpu.bus.ppu.chr_rom;
//    //     render(bank, vram, chr_rom, &mut cpu.bus.ppu.frame);
//    //     texture
//    //         .update(None, &cpu.bus.ppu.frame.data, 256 * 3)
//    //         .unwrap();
//    //     canvas.copy(&texture, None, None).unwrap();
//    //     canvas.present();
//    // }

//    loop {
//        read_user_input(cpu, &mut event_pump);
//        cpu.write_u8_at(RNG_ADDR, rng.gen_range(1, 16));

//        // if update_frame_buffer(cpu, &mut frame_buff) {
//        //     let bank = cpu.bus.ppu.reg_ctrl.background_pattern_table_address();
//        //     let vram = &cpu.bus.ppu.vram;
//        //     let chr_rom = &cpu.bus.ppu.chr_rom;
//        //     render(bank, vram, chr_rom, &mut cpu.bus.ppu.frame);
//        //     texture
//        //         .update(None, &cpu.bus.ppu.frame.data, 256 * 3)
//        //         .unwrap();
//        // }
//        match cpu.next_op() {
//            CpuComputationResult { brk: true, .. } => {
//                return;
//            }
//            _ => {}
//        }
//        std::thread::sleep(std::time::Duration::new(0, 70_000));
//    }
//}

//pub fn render(background_chr_bank: u16, vram: &[u8; 2048], chr_rom: &[u8], frame: &mut Frame) {
//    for i in 0..0x03c0 {
//        // just for now, lets use the first nametable
//        let tile = vram[i] as u16;
//        let tile_x = i % 32;
//        let tile_y = i / 32;
//        let tile = &chr_rom[(background_chr_bank + tile * 16) as usize
//            ..=(background_chr_bank + tile * 16 + 15) as usize];

//        for y in 0..=7 {
//            let mut upper = tile[y];
//            let mut lower = tile[y + 8];

//            for x in (0..=7).rev() {
//                let value = (1 & upper) << 1 | (1 & lower);
//                upper = upper >> 1;
//                lower = lower >> 1;
//                let rgb = match value {
//                    0 => SYSTEM_PALLETE[0x01],
//                    1 => SYSTEM_PALLETE[0x23],
//                    2 => SYSTEM_PALLETE[0x27],
//                    3 => SYSTEM_PALLETE[0x30],
//                    _ => panic!("Impossible value"),
//                };
//                frame.set_pixel(tile_x * 8 + x, tile_y * 8 + y, rgb)
//            }
//        }
//    }
//}

//fn read_user_input(cpu: &mut NesCpu, event_pump: &mut EventPump) {
//    for event in event_pump.poll_iter() {
//        match event {
//            Event::Quit { .. }
//            | Event::KeyDown {
//                keycode: Some(Keycode::Escape),
//                ..
//            } => std::process::exit(0),
//            Event::KeyDown {
//                keycode: Some(Keycode::W),
//                ..
//            } => {
//                cpu.write_u8_at(KEYPRESS_ADDR, 0x77);
//            }
//            Event::KeyDown {
//                keycode: Some(Keycode::S),
//                ..
//            } => {
//                cpu.write_u8_at(KEYPRESS_ADDR, 0x73);
//            }
//            Event::KeyDown {
//                keycode: Some(Keycode::A),
//                ..
//            } => {
//                cpu.write_u8_at(KEYPRESS_ADDR, 0x61);
//            }
//            Event::KeyDown {
//                keycode: Some(Keycode::D),
//                ..
//            } => {
//                cpu.write_u8_at(KEYPRESS_ADDR, 0x64);
//            }
//            _ => {}
//        }
//    }
//}

//fn byte_to_rgba(byte: u8) -> Color {
//    match byte {
//        0 => sdl2::pixels::Color::BLACK,
//        1 => sdl2::pixels::Color::WHITE,
//        2 | 9 => sdl2::pixels::Color::GREY,
//        3 | 10 => sdl2::pixels::Color::RED,
//        4 | 11 => sdl2::pixels::Color::GREEN,
//        5 | 12 => sdl2::pixels::Color::BLUE,
//        6 | 13 => sdl2::pixels::Color::MAGENTA,
//        7 | 14 => sdl2::pixels::Color::YELLOW,
//        _ => sdl2::pixels::Color::CYAN,
//    }
//}

//fn update_frame_buffer(cpu: &mut NesCpu, frame_buff: &mut [u8; 32 * 3 * 32]) -> bool {
//    let mut frame_idx = 0;
//    let mut update = false;
//    for i in FRAME_BUFFER_START..FRAME_BUFFER_END {
//        let color_idx = cpu.read_u8_at(i);
//        let (b1, b2, b3) = byte_to_rgba(color_idx).rgb();
//        if frame_buff[frame_idx] != b1
//            || frame_buff[frame_idx + 1] != b2
//            || frame_buff[frame_idx + 2] != b3
//        {
//            frame_buff[frame_idx] = b1;
//            frame_buff[frame_idx + 1] = b2;
//            frame_buff[frame_idx + 2] = b3;
//            update = true;
//        }
//        frame_idx += 3;
//    }
//    update
//}
