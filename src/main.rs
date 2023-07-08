mod bus;
mod cpu;
mod ppu;
mod native_ui;
mod rom;

fn main() {
    let rom = match std::fs::read("./ROM.nes") {
        Ok(rom) => rom,
        Err(e) => {
            eprintln!("Failed to read ROM.nes file: {}", e);
            return;
        },
    };
    let parsed = match rom::Rom::from_ines_file(rom) {
        Err(e) => {
            eprintln!("Could not read loaded ROM {:?}", e);
            return;
        },
        Ok(parsed) => parsed,
    };
    let mut bus = bus::NesBus::new(&parsed);
    let mut cpu = cpu::NesCpu::new(&mut bus);
    native_ui::run(&mut cpu);
}
