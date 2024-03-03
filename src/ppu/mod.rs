use crate::rom::Mirroring;

mod frame;
mod registers;

pub(crate) use frame::Frame;
use frame::FrameRenderer;

use registers::NesPpuRegister;

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
    registers: NesPpuRegister,
    prev_read_value: u8,

    /// OAM address port
    /// https://www.nesdev.org/wiki/PPU_registers#OAM_address_($2003)_%3E_write
    oam_address: u8,

    unhandled_nmi_interrupt: bool,
    frame_renderer: FrameRenderer,
}

impl NesPpu {
    pub(crate) fn new(chr_rom: Vec<u8>, mirroring: Mirroring) -> Self {
        Self {
            chr_rom,
            curr_scanline: 0,
            cycles: 0,
            mirroring,
            oam_address: 0,
            prev_read_value: 0,
            registers: NesPpuRegister::new(),
            unhandled_nmi_interrupt: false,
            frame_renderer: FrameRenderer::new(mirroring),
        }
    }

    pub(crate) fn write_ctrl(&mut self, value: u8) {
        let before_vblank_nmi = self.registers.ctrl.generate_vblank_nmi();
        self.registers.ctrl.write(value);
        if !before_vblank_nmi
            && self.registers.ctrl.generate_vblank_nmi()
            && self.registers.status.in_vblank()
        {
            // TODO check why this breaks
            // self.unhandled_nmi_interrupt = true;
        }
    }

    pub(crate) fn write_mask(&mut self, value: u8) {
        self.registers.mask.write(value);
    }

    pub(crate) fn read_status(&mut self) -> u8 {
        let val = self.registers.status.read();
        self.registers.status.set_in_vblank(false);
        self.registers.addr.reset_address_latch();
        self.registers.scroll.reset_address_latch();
        val
    }

    pub(crate) fn write_oam_address(&mut self, value: u8) {
        self.oam_address = value;
    }

    pub(crate) fn read_oam_data(&mut self) -> u8 {
        self.frame_renderer.oam_data[self.oam_address as usize]
    }

    pub(crate) fn write_oam_data(&mut self, val: u8) {
        self.frame_renderer.oam_data[self.oam_address as usize] = val;
        self.oam_address = self.oam_address.wrapping_add(1);
    }

    pub(crate) fn write_scroll(&mut self, value: u8) {
        self.registers.scroll.write(value);
    }

    pub(crate) fn write_addr(&mut self, value: u8) {
        self.registers.addr.write(value);
    }

    pub(crate) fn write_data(&mut self, value: u8) {
        let vram_increment = self.registers.ctrl.vram_address_increment();
        let addr = self.registers.addr.read_address(vram_increment);
        match addr {
            NAME_TABLE_START..=NAME_TABLE_END => {
                self.frame_renderer.vram[self.normalize_vram_address(addr) as usize] = value;
            }

            // $3F10/$3F14/$3F18/$3F1C are mirrors of $3F00/$3F04/$3F08/$3F0C
            0x3F10 | 0x3F14 | 0x3F18 | 0x3F1C => {
                self.frame_renderer.palette[(addr - 0x10 - PALETTE_RAM_START) as usize] = value;
            }

            PALETTE_RAM_START..=PALETTE_RAM_END => {
                self.frame_renderer.palette[(addr - PALETTE_RAM_START) as usize] = value;
            }

            _ => {}
        }
    }

    pub(crate) fn read_data(&mut self) -> u8 {
        let vram_increment = self.registers.ctrl.vram_address_increment();
        let addr = self.registers.addr.read_address(vram_increment);
        match addr {
            CHR_ROM_START..=CHR_ROM_END => {
                let result = self.prev_read_value;
                self.prev_read_value = self.chr_rom[addr as usize];
                result
            }

            // TODO include mirroring or something
            NAME_TABLE_START..=NAME_TABLE_END => {
                let result = self.prev_read_value;
                self.prev_read_value =
                    self.frame_renderer.vram[self.normalize_vram_address(addr) as usize];
                result
            }

            // TODO palette mirrors
            PALETTE_RAM_START..=PALETTE_RAM_END => {
                self.frame_renderer.palette[(addr - PALETTE_RAM_START) as usize]
            }
            _ => panic!("PPU unexpected {:04X} address read", addr),
        }
    }

    pub(crate) fn write_oam_dma(&mut self, data: [u8; 256]) {
        for x in data.into_iter() {
            self.frame_renderer.oam_data[self.oam_address as usize] = x;
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

    pub(crate) fn tick(&mut self, cycles: u32) -> Option<&Frame> {
        self.cycles += cycles;
        if self.cycles >= 341 {
            let y = self.frame_renderer.oam_data[0] as usize;
            let x = self.frame_renderer.oam_data[3] as u32;

            // TODO remaining conditions?
            // https://www.nesdev.org/wiki/PPU_OAM#Sprite_zero_hits
            if y == self.curr_scanline
                && x <= self.cycles
                && self.registers.mask.show_bg()
                && self.registers.mask.show_sprites()
            {
                self.registers.status.set_sprite_0_hit(true);
                self.frame_renderer.construct_frame(
                    &self.chr_rom,
                    &self.registers,
                    y * 256 + usize::min(x as usize, 255),
                );
            }
            self.cycles = self.cycles - 341;
            self.curr_scanline += 1;

            if self.curr_scanline == 241 {
                self.registers.status.set_in_vblank(true);
                self.registers.status.set_sprite_0_hit(false);
                if self.registers.ctrl.generate_vblank_nmi() {
                    self.unhandled_nmi_interrupt = true;
                    self.frame_renderer
                        .construct_frame(&self.chr_rom, &self.registers, 61440);
                    return Some(self.frame_renderer.frame());
                }
            } else if self.curr_scanline >= 262 {
                self.curr_scanline = 0;
                self.unhandled_nmi_interrupt = false;
                self.registers.status.set_in_vblank(false);
                self.registers.status.set_sprite_0_hit(false);
            }
        }
        None
    }

    pub(crate) fn handle_nmi_interrupt(&mut self) -> bool {
        if self.unhandled_nmi_interrupt {
            self.unhandled_nmi_interrupt = false;
            true
        } else {
            false
        }
    }
}
