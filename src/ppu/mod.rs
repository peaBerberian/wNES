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
}

impl NesPpu {
    pub(crate) fn new(chr_rom: Vec<u8>, mirroring: Mirroring) -> Self {
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
