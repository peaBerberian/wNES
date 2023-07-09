/// # Bus emulation
///
/// Emulates a "bus" concept, that a NES CPU emulation will use to read and write to specific
/// address. This bus emulation actually owns the PPU emulation to facilitate communication.
///
/// ## Addressable memory representation
///
///  _______________ $10000  _______________
/// | PRG-ROM       |       |               |
/// | Upper Bank    |       |               |
/// |_ _ _ _ _ _ _ _| $C000 | PRG-ROM       |
/// | PRG-ROM       |       |               |
/// | Lower Bank    |       |               |
/// |_______________| $8000 |_______________|
/// | SRAM          |       | SRAM          |
/// |_______________| $6000 |_______________|
/// | Expansion ROM |       | Expansion ROM |
/// |_______________| $4020 |_______________|
/// | I/O Registers |       |               |
/// |_ _ _ _ _ _ _ _| $4000 |               |
/// | Mirrors       |       | I/O Registers |
/// | $2000-$2007   |       |               |
/// |_ _ _ _ _ _ _ _| $2008 |               |
/// | I/O Registers |       |               |
/// |_______________| $2000 |_______________|
/// | Mirrors       |       |               |
/// | $0000-$07FF   |       |               |
/// |_ _ _ _ _ _ _ _| $0800 |               |
/// | RAM           |       | RAM           |
/// |_ _ _ _ _ _ _ _| $0200 |               |
/// | Stack         |       |               |
/// |_ _ _ _ _ _ _ _| $0100 |               |
/// | Zero Page     |       |               |
/// |_______________| $0000 |_______________|

use crate::{rom::Rom, ppu::NesPpu};

const MEMORY_START_ADDR: u16 = 0;
const MEMORY_END_ADDR: u16 = 0x1FFF;

const PRG_ROM_START_ADDR: u16 = 0x8000;
const PRG_ROM_END_ADDR: u16 = 0xFFFF;

const PPU_REGISTERS_MIRRORS_START: u16 = 0x2008;
const PPU_REGISTERS_MIRRORS_END: u16 = 0x3FFF;

pub(crate) struct NesBus {
    prg_rom: Vec<u8>,
    memory: [u8; 2048],
    ppu: NesPpu,
}

impl NesBus {
    pub(crate) fn new(
        rom: &Rom
    ) -> Self {
        Self {
            prg_rom: rom.prg_rom().to_owned(),
            memory: [0; 2048],
            ppu: NesPpu::new(rom.chr_rom().to_owned(), rom.mirroring()),
        }
    }

    pub(crate) fn read(&mut self, mut addr: u16) -> u8 {
        match addr {
            MEMORY_START_ADDR ..= MEMORY_END_ADDR => {
                // Only the 11 least significant bits of the 16 bits address
                // is actually read on NES hardware.
                let actual_mem_addr = addr & 0b0000_0111_1111_1111;
                self.memory[actual_mem_addr as usize]
            },

            0x2002 => self.ppu.read_status(),
            0x2004 => self.ppu.read_oam_data(),
            0x2007 => self.ppu.read_data(),

            PPU_REGISTERS_MIRRORS_START..=PPU_REGISTERS_MIRRORS_END => {
                let mirror_down_addr = addr & 0b00100000_00000111;
                self.read(mirror_down_addr)
            }

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

            0x2000 => self.ppu.write_ctrl(val),
            0x2001 => self.ppu.write_mask(val),
            0x2003 => self.ppu.write_oam_address(val),
            0x2004 => self.ppu.write_oam_data(val),
            0x2005 => self.ppu.write_scroll(val),
            0x2006 => self.ppu.write_addr(val),
            0x2007 => self.ppu.write_data(val),
            0x4014 => {
                let start_buff = (val as u16) << 8;
                let mut buff = [0u8; 256];
                for i in 0..256u16 {
                    buff[i as usize] = self.read(start_buff + i);
                }
                self.ppu.write_oam_dma(buff);
            },
            _ => { },
        }
    }

    pub(crate) fn tick(&mut self, cycles: u8) {
       self.ppu.tick(cycles as u32 * 3);
   }

    pub(crate) fn should_handle_nmi_interrupt(&mut self) -> bool {
        self.ppu.should_handle_nmi_interrupt()
    }
}
