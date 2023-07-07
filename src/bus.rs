//  _______________ $10000  _______________
// | PRG-ROM       |       |               |
// | Upper Bank    |       |               |
// |_ _ _ _ _ _ _ _| $C000 | PRG-ROM       |
// | PRG-ROM       |       |               |
// | Lower Bank    |       |               |
// |_______________| $8000 |_______________|
// | SRAM          |       | SRAM          |
// |_______________| $6000 |_______________|
// | Expansion ROM |       | Expansion ROM |
// |_______________| $4020 |_______________|
// | I/O Registers |       |               |
// |_ _ _ _ _ _ _ _| $4000 |               |
// | Mirrors       |       | I/O Registers |
// | $2000-$2007   |       |               |
// |_ _ _ _ _ _ _ _| $2008 |               |
// | I/O Registers |       |               |
// |_______________| $2000 |_______________|
// | Mirrors       |       |               |
// | $0000-$07FF   |       |               |
// |_ _ _ _ _ _ _ _| $0800 |               |
// | RAM           |       | RAM           |
// |_ _ _ _ _ _ _ _| $0200 |               |
// | Stack         |       |               |
// |_ _ _ _ _ _ _ _| $0100 |               |
// | Zero Page     |       |               |
// |_______________| $0000 |_______________|

const MEMORY_START_ADDR: u16 = 0;
const MEMORY_END_ADDR: u16 = 0x3FFF;

const PRG_ROM_START_ADDR: u16 = 0x8000;
const PRG_ROM_END_ADDR: u16 = 0xFFFF;

pub(crate) struct NesBus {
    prg_rom: Vec<u8>,
    memory: [u8; 2048],
}

impl NesBus {
    pub(crate) fn new() -> Self {
        Self {
            prg_rom: vec![],
            memory: [0; 2048],
        }
    }

    pub(crate) fn load_rom(
        &mut self,
        program: Vec<u8>
    ) -> Result<(), RomParsingError> {
        self.prg_rom = parse_rom(program)?;
        Ok(())
    }

    pub(crate) fn read(&self, mut addr: u16) -> u8 {
        match addr {
            MEMORY_START_ADDR ..= MEMORY_END_ADDR => {
                // Only the 11 least significant bits of the 16 bits address
                // is actually read on NES hardware.
                let actual_mem_addr = addr & 0b0000_0111_1111_1111;
                self.memory[actual_mem_addr as usize]
            },

            PRG_ROM_START_ADDR ..= PRG_ROM_END_ADDR => {
                addr -= 0x8000;
                if self.prg_rom.len() <= 0x4000 && addr >= 0x4000 {
                    // PRG ROM is either 16KiB or 32kiB. If 16kiB, mirror the
                    // second half (into upper bank) as expected on hardware.
                    addr %= 0x4000;
                }
                let addr = addr as usize;
                if addr >= self.prg_rom.len() {
                    0
                } else {
                    self.prg_rom[addr]
                }
            },

            // Not implemented yet
            _ => 0,
        }
    }

    pub(crate) fn write(&mut self, addr: u16, val: u8) {
        match addr {
            MEMORY_START_ADDR ..= MEMORY_END_ADDR => {
                let actual_mem_addr = addr & 0b0000_0111_1111_1111;
                self.memory[actual_mem_addr as usize] = val;
            },
            _ => { },
        }
    }
}

#[derive(Clone, Debug)]
pub(crate) enum RomParsingError {
    NoNESHeader,
    PlayChoice10Game,
    VsUnisystemGame,
}

enum Mirroring {
    FourScreen,
    Vertical,
    Horizontal,
}

enum TvSystem {
    Ntsc,
    Pal,
}

fn parse_rom(rom: Vec<u8>) -> Result<Vec<u8>, RomParsingError> {
    if &rom[0..4] != b"NES\x1A" {
        return Err(RomParsingError::NoNESHeader)
    }

    // Size of PRG ROM in 16 KiB units
    let len_prg_rom = rom[4] as usize * 16384 /* 16 kiB */;

    // Size of CHR ROM in 8 KiB units (Value 0 means the board uses CHR RAM)
    let len_chr_rom = rom[5] as usize * 8192 /* 8 kiB */;

    let f6 = rom[6];
    let lower_mapper = f6 >> 4;

    // Ignore mirroring control or above mirroring bit; instead provide four-screen VRAM
    let has_four_screen_bit = f6 & 0b0000_1000 != 0;

    // 512-byte trainer at $7000-$71FF (stored before PRG data)
    let has_trainer_bit = f6 & 0b0000_0100 != 0;

    // If on the cartridge contains battery-backed PRG RAM ($6000-7FFF) or other persistent memory
    let has_battery_ram = f6 & 0b0000_0010 != 0;
    let mirroring = match (has_four_screen_bit, f6 & 0b0001) {
        (true, _) => Mirroring::FourScreen,
        (false, 0) => Mirroring::Horizontal,
        (false, _) => Mirroring::Vertical,
    };

    let f7 = rom[7];
    // Upper nibble of mapper number
    let upper_mapper = f7 & 0b1111_0000;

    // Construct whole mapper now
    let mapper = upper_mapper | lower_mapper;

    // If equal to 2, flags 8-15 are in NES 2.0 format
    let format = f7 & 0b0000_1100 >> 2;

    // Determines if it is made for a Nintendo PlayChoice-10 or not
    let has_playchoice10_bit = f7 & 0b0000_0010 != 0;

    if has_playchoice10_bit {
        return Err(RomParsingError::PlayChoice10Game);
    }

    // Determines if it is made for a Nintendo VS Unisystem or not
    let has_vs_unisystem_bit = f7 & 0b0000_0001 != 0;

    if has_vs_unisystem_bit {
        return Err(RomParsingError::VsUnisystemGame);
    }


    // Size of PRG RAM in 8 KiB units (Value 0 infers 8 KiB for compatibility; see PRG RAM circuit on nesdev.com)
    let mut len_prg_ram = rom[8] as usize * 8192;
    len_prg_ram = if len_prg_ram == 0 { 8192 } else { 0 };

    let f9 = rom[8];
    let tv_system = match f9 & 0b0000_0001 {
        1 => TvSystem::Pal,
        _ => TvSystem::Ntsc,
    };

    // let mapper = (rom[7] & 0b1111_0000) | (rom[6] >> 4);

    // let four_screen = rom[6] & 0b1000 != 0;
    // let vertical_mirroring = rom[6] & 0b1 != 0;

    let skip_trainer = rom[6] & 0b100 != 0;

    let prg_rom_start = 16 + if skip_trainer { 512 } else { 0 };
    let chr_rom_start = prg_rom_start + len_prg_rom;

    Ok(rom[prg_rom_start..(prg_rom_start + len_prg_rom)].to_vec())
        //        Ok(Rom {
        //            prg_rom: rom[prg_rom_start..(prg_rom_start + len_prg_rom)].to_vec(),
        //            chr_rom: rom[chr_rom_start..(chr_rom_start + len_chr_rom)].to_vec(),
        //            mapper: mapper,
        //            screen_mirroring: screen_mirroring,
        //        })
}
