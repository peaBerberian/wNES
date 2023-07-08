use crate::rom::Mirroring;

mod registers;

use registers::{
    PpuAddrRegister, PpuCtrlRegister, PpuMaskRegister, PpuScrollRegister, PpuStatusRegister,
};

pub(super) struct NesPpu {
    chr_rom: Vec<u8>,
    mirroring: Mirroring,
    cycles: usize,
    curr_scanline: usize,

    // TODO
    last_read: u8,

    vram: [u8; 2048],
    oam_addr: u8,
    oam: [u8; 256],
    palette: [u8; 32],

    reg_ctrl: PpuCtrlRegister,
    reg_mask: PpuMaskRegister,
    reg_status: PpuStatusRegister,
    reg_scroll: PpuScrollRegister,
    reg_addr: PpuAddrRegister,
}

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

impl NesPpu {
    pub(super) fn new(chr_rom: Vec<u8>, mirroring: Mirroring) -> Self {
        Self {
            chr_rom,
            mirroring,
            oam_addr: 0,
            oam: [0; 64 * 4],
            palette: [0; 32],
            vram: [0; 2048],
            cycles: 0,
            curr_scanline: 0,
            reg_addr: PpuAddrRegister::new(),
            last_read: 0,
            reg_ctrl: PpuCtrlRegister::new(),
            reg_mask: PpuMaskRegister::new(),
            reg_status: PpuStatusRegister::new(),
            reg_scroll: PpuScrollRegister::new(),
        }
    }

    pub(super) fn write_ctrl(&mut self, value: u8) {
        self.reg_ctrl.write(value);
    }

    pub(super) fn write_mask(&mut self, value: u8) {
        self.reg_mask.write(value);
    }

    pub(super) fn read_status(&mut self) -> u8 {
        let val = self.reg_status.read();
        self.reg_status.set_in_vblank(false);
        self.reg_addr.reset_address_latch();
        self.reg_scroll.reset_address_latch();
        val
    }

    pub(super) fn write_oam_address(&mut self, value: u8) {
        self.oam_addr = value;
    }

    pub(super) fn read_oam_data(&mut self) -> u8 {
        self.oam[self.oam_addr as usize]
    }

    pub(super) fn write_oam_data(&mut self, val: u8) {
        self.oam[self.oam_addr as usize] = val;
        self.oam_addr.wrapping_add(1);
    }

    pub(super) fn write_scroll(&mut self, value: u8) {
        self.reg_scroll.write(value);
    }

    pub(super) fn write_addr(&mut self, value: u8) {
        self.reg_addr.write(value);
    }

    pub(super) fn write_data(&mut self, value: u8) {
        let vram_increment = self.reg_ctrl.vram_address_increment();
        let addr = self.reg_addr.read_address(vram_increment);
        match addr {
            NAME_TABLE_START..=NAME_TABLE_END => {
                self.vram[self.normalize_vram_address(addr) as usize] = value;
            }

            PALETTE_RAM_START..=PALETTE_RAM_END => {
                self.palette[(addr - PALETTE_RAM_START) as usize] = value;
            }

            // $3F10/$3F14/$3F18/$3F1C are mirrors of $3F00/$3F04/$3F08/$3F0C
            0x3F10 | 0x3F14 | 0x3F18 | 0x3F1C => {
                self.palette[(addr - 0x10 - PALETTE_RAM_START) as usize] = value;
            }
            _ => {}
        }
    }

    pub(super) fn read_data(&mut self) -> u8 {
        let vram_increment = self.reg_ctrl.vram_address_increment();
        let addr = self.reg_addr.read_address(vram_increment);
        match addr {
            CHR_ROM_START..=CHR_ROM_END => {
                let result = self.last_read;
                self.last_read = self.chr_rom[addr as usize];
                result
            }

            // TODO include mirroring or something
            NAME_TABLE_START..=NAME_TABLE_END => {
                let result = self.last_read;
                self.last_read = self.vram[self.normalize_vram_address(addr) as usize];
                result
            }

            // TODO palette mirrors
            PALETTE_RAM_START..=PALETTE_RAM_END => {
                self.palette[(addr - PALETTE_RAM_START) as usize]
            }
            _ => panic!("PPU unexpected {:04X} address read", addr),
        }
    }

    pub(super) fn write_oam_dma(&mut self, data: [u8; 256]) {
        for x in data.into_iter() {
            self.oam[self.oam_addr as usize] = x;
            self.oam_addr = self.oam_addr.wrapping_add(1);
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
}
