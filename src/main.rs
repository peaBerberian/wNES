mod bus;
mod controller;
mod cpu;
mod native_ui;
mod ppu;
mod rom;

use ppu::Frame;
use std::env;

fn main() {
    let mut args = env::args().skip(1);
    let Some(rom_filename) = args.next() else {
        eprintln!("Needs a ROM in argument");
        return;
    };

    if args.peekable().peek().is_some() {
        eprintln!("Too many arguments. wNes just needs the ROM you want to check.");
        return;
    }

    let rom_file = match std::fs::read(rom_filename) {
        Ok(rom_file) => rom_file,
        Err(e) => {
            eprintln!("Failed to read ROM.nes file: {}", e);
            return;
        }
    };
    run(rom_file);
}

fn run(rom_file: Vec<u8>) {
    let parsed = match rom::Rom::from_ines_file(rom_file) {
        Err(e) => {
            eprintln!("Could not read loaded ROM: {:?}", e);
            return;
        }
        Ok(parsed) => parsed,
    };

    use native_ui::WNesUi;
    let mut ui = match native_ui::NativeUi::try_new() {
        Ok(ui) => ui,
        Err(e) => {
            eprintln!("Could not initialize UI: {:?}", e);
            return;
        }
    };

    // let mut rng = rand::thread_rng();

    let bus = bus::NesBus::new(&parsed, move |frame: Frame, ctrls| {
        ui.render_frame(frame, ctrls);
    });
    let mut cpu = cpu::NesCpu::new(bus);
    loop {
        cpu.next_op();
    }
}
