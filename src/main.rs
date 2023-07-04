mod bus;
mod cpu;
mod native_ui;

fn main() {
    let mut bus = bus::NesBus::new();
    let rom = match std::fs::read("./ROM.nes") {
        Ok(rom) => rom,
        Err(e) => {
            eprintln!("Failed to read ROM.nes file: {}", e);
            return;
        },
    };
    if let Err(e) = bus.load_rom(rom) {
        eprintln!("Could not read loaded ROM {:?}", e);
        return;
    }
    let mut cpu = cpu::NesCpu::new(&mut bus);
    native_ui::run(&mut cpu);
}
