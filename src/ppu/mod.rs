use crate::rom::Mirroring;

mod registers;

use registers::{
    PpuAddrRegister, PpuCtrlRegister, PpuMaskRegister, PpuScrollRegister, PpuStatusRegister,
};

/// $0000-$0FFF 	$1000 	Pattern table 0
/// $1000-$1FFF 	$1000 	Pattern table 1
/// $2000-$23FF 	$0400 	Nametable 0
/// $2400-$27FF 	$0400 	Nametable 1
/// $2800-$2BFF 	$0400 	Nametable 2
/// $2C00-$2FFF 	$0400 	Nametable 3
/// $3000-$3EFF 	$0F00 	Mirrors of $2000-$2EFF
/// $3F00-$3F1F 	$0020 	Palette RAM indexes
/// $3F20-$3FFF 	$00E0 	Mirrors of $3F00-$3F1F
const CHR_ROM_START: u16 = 0x0000;
const CHR_ROM_END: u16 = 0x1FFF;
const PALETTE_RAM_START: u16 = 0x03F00;
const PALETTE_RAM_END: u16 = 0x03FFF;
const NAME_TABLE_START: u16 = 0x2000;
const NAME_TABLE_END: u16 = 0x2FFF;

#[rustfmt::skip]
pub static SYSTEM_PALLETE: [(u8,u8,u8); 64] = [
   (0x80, 0x80, 0x80), (0x00, 0x3D, 0xA6), (0x00, 0x12, 0xB0), (0x44, 0x00, 0x96), (0xA1, 0x00, 0x5E),
   (0xC7, 0x00, 0x28), (0xBA, 0x06, 0x00), (0x8C, 0x17, 0x00), (0x5C, 0x2F, 0x00), (0x10, 0x45, 0x00),
   (0x05, 0x4A, 0x00), (0x00, 0x47, 0x2E), (0x00, 0x41, 0x66), (0x00, 0x00, 0x00), (0x05, 0x05, 0x05),
   (0x05, 0x05, 0x05), (0xC7, 0xC7, 0xC7), (0x00, 0x77, 0xFF), (0x21, 0x55, 0xFF), (0x82, 0x37, 0xFA),
   (0xEB, 0x2F, 0xB5), (0xFF, 0x29, 0x50), (0xFF, 0x22, 0x00), (0xD6, 0x32, 0x00), (0xC4, 0x62, 0x00),
   (0x35, 0x80, 0x00), (0x05, 0x8F, 0x00), (0x00, 0x8A, 0x55), (0x00, 0x99, 0xCC), (0x21, 0x21, 0x21),
   (0x09, 0x09, 0x09), (0x09, 0x09, 0x09), (0xFF, 0xFF, 0xFF), (0x0F, 0xD7, 0xFF), (0x69, 0xA2, 0xFF),
   (0xD4, 0x80, 0xFF), (0xFF, 0x45, 0xF3), (0xFF, 0x61, 0x8B), (0xFF, 0x88, 0x33), (0xFF, 0x9C, 0x12),
   (0xFA, 0xBC, 0x20), (0x9F, 0xE3, 0x0E), (0x2B, 0xF0, 0x35), (0x0C, 0xF0, 0xA4), (0x05, 0xFB, 0xFF),
   (0x5E, 0x5E, 0x5E), (0x0D, 0x0D, 0x0D), (0x0D, 0x0D, 0x0D), (0xFF, 0xFF, 0xFF), (0xA6, 0xFC, 0xFF),
   (0xB3, 0xEC, 0xFF), (0xDA, 0xAB, 0xEB), (0xFF, 0xA8, 0xF9), (0xFF, 0xAB, 0xB3), (0xFF, 0xD2, 0xB0),
   (0xFF, 0xEF, 0xA6), (0xFF, 0xF7, 0x9C), (0xD7, 0xE8, 0x95), (0xA6, 0xED, 0xAF), (0xA2, 0xF2, 0xDA),
   (0x99, 0xFF, 0xFC), (0xDD, 0xDD, 0xDD), (0x11, 0x11, 0x11), (0x11, 0x11, 0x11)
];

pub(crate) struct Frame {
   pub data: Vec<u8>,
}

impl Frame {
   const WIDTH: usize = 256;
   const HIGHT: usize = 240;

   pub fn new() -> Self {
       Frame {
           data: vec![0; (Frame::WIDTH) * (Frame::HIGHT) * 3],
       }
   }

   pub fn set_pixel(&mut self, x: usize, y: usize, rgb: (u8, u8, u8)) {
       let base = y * 3 * Frame::WIDTH + x * 3;
       if base + 2 < self.data.len() {
           self.data[base] = rgb.0;
           self.data[base + 1] = rgb.1;
           self.data[base + 2] = rgb.2;
       }
   }
}

fn show_tile(chr_rom: &Vec<u8>, bank: usize, tile_n: usize) ->Frame {
   assert!(bank <= 1);

   let mut frame = Frame::new();
   let bank = (bank * 0x1000) as usize;

   let tile = &chr_rom[(bank + tile_n * 16)..=(bank + tile_n * 16 + 15)];

   for y in 0..=7 {
       let mut upper = tile[y];
       let mut lower = tile[y + 8];

       for x in (0..=7).rev() {
           let value = (1 & upper) << 1 | (1 & lower);
           upper = upper >> 1;
           lower = lower >> 1;
           let rgb = match value {
               0 => SYSTEM_PALLETE[0x01],
               1 => SYSTEM_PALLETE[0x23],
               2 => SYSTEM_PALLETE[0x27],
               3 => SYSTEM_PALLETE[0x30],
               _ => panic!("can't be"),
           };
           frame.set_pixel(x, y, rgb)
       }
   }

   frame
}

pub(crate) struct NesPpu {
    chr_rom: Vec<u8>,
    mirroring: Mirroring,
    cycles: u32,
    curr_scanline: usize,

    palette: [u8; 32],

    // TODO
    prev_read_value: u8,

    vram: [u8; 2048],
    /// OAM address port
    /// https://www.nesdev.org/wiki/PPU_registers#OAM_address_($2003)_%3E_write
    oam_address: u8,

    /// OAM data port
    /// https://www.nesdev.org/wiki/PPU_registers#OAM_data_($2004)_%3C%3E_read/write
    oam_data: [u8; 256],

    /// PPU control register
    /// https://www.nesdev.org/wiki/PPU_registers#Controller_($2000)_%3E_write
    reg_ctrl: PpuCtrlRegister,

    /// PPU mask register
    /// https://www.nesdev.org/wiki/PPU_registers#Mask_($2001)_%3E_write
    reg_mask: PpuMaskRegister,

    /// PPU status register
    /// https://www.nesdev.org/wiki/PPU_registers#Status_($2002)_%3C_read
    reg_status: PpuStatusRegister,

    /// PPU scrolling position register
    /// https://www.nesdev.org/wiki/PPU_registers#Scroll_($2005)_%3E%3E_write_x2
    reg_scroll: PpuScrollRegister,

    /// PPU address register
    /// https://www.nesdev.org/wiki/PPU_registers#Address_($2006)_%3E%3E_write_x2
    reg_addr: PpuAddrRegister,

    unhandled_nmi_interrupt: bool,

    pub(crate) frame: Frame,
}

impl NesPpu {
    pub(crate) fn new(chr_rom: Vec<u8>, mirroring: Mirroring) -> Self {
        let tile_frame = show_tile(&chr_rom, 1,12);
        Self {
            chr_rom,
            curr_scanline: 0,
            cycles: 0,
            mirroring,
            oam_address: 0,
            oam_data: [0; 64 * 4],
            palette: [0; 32],
            prev_read_value: 0,
            vram: [0; 2048],
            reg_addr: PpuAddrRegister::new(),
            reg_ctrl: PpuCtrlRegister::new(),
            reg_mask: PpuMaskRegister::new(),
            reg_scroll: PpuScrollRegister::new(),
            reg_status: PpuStatusRegister::new(),
            unhandled_nmi_interrupt: true,
            frame: tile_frame,
        }
    }

    pub(crate) fn write_ctrl(&mut self, value: u8) {
        self.reg_ctrl.write(value);
    }

    pub(crate) fn write_mask(&mut self, value: u8) {
        self.reg_mask.write(value);
    }

    pub(crate) fn read_status(&mut self) -> u8 {
        let val = self.reg_status.read();
        self.reg_status.set_in_vblank(false);
        self.reg_addr.reset_address_latch();
        self.reg_scroll.reset_address_latch();
        val
    }

    pub(crate) fn write_oam_address(&mut self, value: u8) {
        self.oam_address = value;
    }

    pub(crate) fn read_oam_data(&mut self) -> u8 {
        self.oam_data[self.oam_address as usize]
    }

    pub(crate) fn write_oam_data(&mut self, val: u8) {
        self.oam_data[self.oam_address as usize] = val;
        self.oam_address = self.oam_address.wrapping_add(1);
    }

    pub(crate) fn write_scroll(&mut self, value: u8) {
        self.reg_scroll.write(value);
    }

    pub(crate) fn write_addr(&mut self, value: u8) {
        self.reg_addr.write(value);
    }

    pub(crate) fn write_data(&mut self, value: u8) {
        let vram_increment = self.reg_ctrl.vram_address_increment();
        let addr = self.reg_addr.read_address(vram_increment);
        match addr {
            NAME_TABLE_START..=NAME_TABLE_END => {
                self.vram[self.normalize_vram_address(addr) as usize] = value;
            }

            // $3F10/$3F14/$3F18/$3F1C are mirrors of $3F00/$3F04/$3F08/$3F0C
            0x3F10 | 0x3F14 | 0x3F18 | 0x3F1C => {
                self.palette[(addr - 0x10 - PALETTE_RAM_START) as usize] = value;
            }

            PALETTE_RAM_START..=PALETTE_RAM_END => {
                self.palette[(addr - PALETTE_RAM_START) as usize] = value;
            }

            _ => {}
        }
    }

    pub(crate) fn read_data(&mut self) -> u8 {
        let vram_increment = self.reg_ctrl.vram_address_increment();
        let addr = self.reg_addr.read_address(vram_increment);
        match addr {
            CHR_ROM_START..=CHR_ROM_END => {
                let result = self.prev_read_value;
                self.prev_read_value = self.chr_rom[addr as usize];
                result
            }

            // TODO include mirroring or something
            NAME_TABLE_START..=NAME_TABLE_END => {
                let result = self.prev_read_value;
                self.prev_read_value = self.vram[self.normalize_vram_address(addr) as usize];
                result
            }

            // TODO palette mirrors
            PALETTE_RAM_START..=PALETTE_RAM_END => {
                self.palette[(addr - PALETTE_RAM_START) as usize]
            }
            _ => panic!("PPU unexpected {:04X} address read", addr),
        }
    }

    pub(crate) fn write_oam_dma(&mut self, data: [u8; 256]) {
        for x in data.into_iter() {
            self.oam_data[self.oam_address as usize] = x;
            self.oam_address = self.oam_address.wrapping_add(1);
        }
    }

    /// When there's vertical or horizontal mirroring, we can simplify vram through 2*1 KiB VRAM
    /// address pages:
    ///   - the `0x2000-0x2400` equivalent (in reality the 0x400 first bytes of VRAM): first screen
    ///   - the `0x2400-0x2800` equivalent (in reality the 0x400-0x800 byte range of VRAM): second
    ///     screen
    ///
    /// This then greatly simplify vram handling.
    ///
    /// TODO four screen mirroring should handle 4 kiB
    fn normalize_vram_address(&self, addr: u16) -> u16 {
        // 0x3000-0x3EFF is just a mirror of 0x2000-0x2EFF
        let mirrored_vram = addr & 0b10111111111111;
        let global_vram_offset = mirrored_vram - 0x2000;
        let name_table_nb = global_vram_offset / 0x400;

        match (&self.mirroring, name_table_nb) {
            (Mirroring::Vertical, 2) | (Mirroring::Vertical, 3) => global_vram_offset - 0x800,
            (Mirroring::Horizontal, 1) => global_vram_offset - 0x400,
            (Mirroring::Horizontal, 2) => global_vram_offset - 0x400,
            (Mirroring::Horizontal, 3) => global_vram_offset - 0x800,
            _ => global_vram_offset,
        }
    }

    pub(crate) fn tick(&mut self, cycles: u32) -> bool {
       self.cycles += cycles;
       if self.cycles >= 341 {
           self.cycles = self.cycles - 341;
           self.curr_scanline += 1;

           if self.curr_scanline == 241 {
               if self.reg_ctrl.generate_vblank_nmi() {
                   self.reg_status.set_in_vblank(true);
                   self.unhandled_nmi_interrupt = true;
               }
           }

           if self.curr_scanline >= 262 {
               self.curr_scanline = 0;
               self.unhandled_nmi_interrupt = false;
               self.reg_status.set_in_vblank(false);
               return true;
           }
       }
       return false;
   }

    pub(crate) fn should_handle_nmi_interrupt(&mut self) -> bool {
        if self.unhandled_nmi_interrupt {
            self.unhandled_nmi_interrupt = false;
            true
        } else {
            false
        }
    }
}
