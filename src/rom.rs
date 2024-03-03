/// # ROM
///
/// Code parsing the NES ROM files initially written for the iNES emulator.

#[derive(Clone, Debug)]
pub(crate) enum RomParsingError {
    NoNESHeader,
    PlayChoice10Game,
    VsUnisystemGame,
}

pub(crate) struct Rom {
    prg_rom: Vec<u8>,
    chr_rom: Vec<u8>,
    mirroring: Mirroring,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) enum Mirroring {
    FourScreen,
    Vertical,
    Horizontal,
}

#[derive(Clone, Copy, Debug)]
pub(crate) enum TvSystem {
    Ntsc,
    Pal,
}

impl Rom {
    pub(super) fn from_ines_file(rom: Vec<u8>) -> Result<Self, RomParsingError> {
        if &rom[0..4] != b"NES\x1A" {
            return Err(RomParsingError::NoNESHeader);
        }

        // Size of PRG ROM in 16 KiB units
        let len_prg_rom = rom[4] as usize * 16384 /* 16 kiB */;

        // Size of CHR ROM in 8 KiB units (Value 0 means the board uses CHR RAM)
        let len_chr_rom = rom[5] as usize * 8192 /* 8 kiB */;
        // println!("{len_chr_rom}");

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

        Ok(Self {
            prg_rom: rom[prg_rom_start..(prg_rom_start + len_prg_rom)].to_vec(),
            chr_rom: rom[chr_rom_start..(chr_rom_start + len_chr_rom)].to_vec(),
            mirroring,
        })
    }

    pub(crate) fn chr_rom(&self) -> &[u8] {
        &self.chr_rom
    }

    pub(crate) fn prg_rom(&self) -> &[u8] {
        &self.prg_rom
    }

    pub(crate) fn mirroring(&self) -> Mirroring {
        self.mirroring
    }
}
