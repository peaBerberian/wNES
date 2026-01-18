pub(super) struct NesPpuRegister {
    /// PPU control register
    /// https://www.nesdev.org/wiki/PPU_registers#Controller_($2000)_%3E_write
    pub(super) ctrl: PpuCtrlRegister,

    /// PPU mask register
    /// https://www.nesdev.org/wiki/PPU_registers#Mask_($2001)_%3E_write
    pub(super) mask: PpuMaskRegister,

    /// PPU status register
    /// https://www.nesdev.org/wiki/PPU_registers#Status_($2002)_%3C_read
    pub(super) status: PpuStatusRegister,

    /// PPU scrolling position register
    /// https://www.nesdev.org/wiki/PPU_registers#Scroll_($2005)_%3E%3E_write_x2
    pub(super) scroll: PpuScrollRegister,

    /// PPU address register
    /// https://www.nesdev.org/wiki/PPU_registers#Address_($2006)_%3E%3E_write_x2
    pub(super) addr: PpuAddrRegister,
}

impl NesPpuRegister {
    pub(super) fn new() -> Self {
        Self {
            ctrl: PpuCtrlRegister::new(),
            mask: PpuMaskRegister::new(),
            status: PpuStatusRegister::new(),
            scroll: PpuScrollRegister::new(),
            addr: PpuAddrRegister::new(),
        }
    }
}

/// Implement PPU Control Registevertical_offset
///
/// This struct is mainly dumb storage and doesn't have much logic own its own.
///
/// VPHB SINN
/// |||| ||||
/// |||| ||++- Base nametable address
/// |||| ||    (0 = $2000; 1 = $2400; 2 = $2800; 3 = $2C00)
/// |||| |+--- VRAM address increment per CPU read/write of PPUDATA
/// |||| |     (0: add 1, going across; 1: add 32, going down)
/// |||| +---- Sprite pattern table address for 8x8 sprites
/// ||||       (0: $0000; 1: $1000; ignored in 8x16 mode)
/// |||+------ Background pattern table address (0: $0000; 1: $1000)
/// ||+------- Sprite size (0: 8x8 pixels; 1: 8x16 pixels – see PPU OAM#Byte 1)
/// |+-------- PPU master/slave select
/// |          (0: read backdrop from EXT pins; 1: output color on EXT pins)
/// +--------- Generate an NMI at the start of the
///            vertical blanking interval (0: off; 1: on)
pub(super) struct PpuCtrlRegister {
    val: u8,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) enum SpriteSize {
    Size8x8,
    Size8x16,
}

impl PpuCtrlRegister {
    fn new() -> Self {
        Self { val: 0 }
    }

    pub(super) fn base_nametable_address(&self) -> u16 {
        // TODO enum type?
        match self.val & 0b0000_0011 {
            0 => 0x2000,
            1 => 0x2400,
            2 => 0x2800,
            _ => 0x2C00,
        }
    }

    pub(super) fn vram_address_increment(&self) -> u8 {
        if self.val & 0b0000_0100 > 0 {
            32
        } else {
            1
        }
    }

    pub(super) fn sprite_pattern_table_address(&self) -> u16 {
        if self.val & 0b0000_1000 > 0 {
            0x1000
        } else {
            0x0000
        }
    }

    pub(super) fn background_pattern_table_address(&self) -> u16 {
        if self.val & 0b0001_0000 > 0 {
            0x1000
        } else {
            0x0000
        }
    }

    pub(super) fn sprite_size(&self) -> SpriteSize {
        if self.val & 0b0010_0000 > 0 {
            SpriteSize::Size8x16
        } else {
            SpriteSize::Size8x8
        }
    }

    pub(super) fn generate_vblank_nmi(&self) -> bool {
        self.val & 0b1000_0000 > 0
    }

    pub(super) fn write(&mut self, data: u8) {
        self.val = data;
    }
}

/// Implement PPU Mask Register
///
/// This struct is mainly dumb storage and doesn't have much logic own its own.
///
/// 7  bit  0
/// ---- ----
/// BGRs bMmG
/// |||| ||||
/// |||| |||+- Greyscale (0: normal color, 1: produce a greyscale display)
/// |||| ||+-- 1: Show background in leftmost 8 pixels of screen, 0: Hide
/// |||| |+--- 1: Show sprites in leftmost 8 pixels of screen, 0: Hide
/// |||| +---- 1: Show background
/// |||+------ 1: Show sprites
/// ||+------- Emphasize red (green on PAL/Dendy)
/// |+-------- Emphasize green (red on PAL/Dendy)
/// +--------- Emphasize blue
pub(super) struct PpuMaskRegister {
    val: u8,
}

impl PpuMaskRegister {
    fn new() -> Self {
        Self { val: 0 }
    }

    pub(super) fn greyscale(&self) -> bool {
        self.val & 0b0000_0001 > 0
    }

    pub(super) fn set_greyscale(&mut self, val: bool) {
        if val {
            self.val |= 0b0000_0001;
        } else {
            self.val &= 0b1111_1110;
        }
    }

    pub(super) fn show_bg_leftmost_8_px(&self) -> bool {
        self.val & 0b0000_0010 > 0
    }

    pub(super) fn set_bg_leftmost_8_px(&mut self, val: bool) {
        if val {
            self.val |= 0b0000_0010;
        } else {
            self.val &= 0b1111_1101;
        }
    }

    pub(super) fn show_sprites_leftmost_8_px(&self) -> bool {
        self.val & 0b0000_0100 > 0
    }

    pub(super) fn set_sprites_leftmost_8_px(&mut self, val: bool) {
        if val {
            self.val |= 0b0000_0100;
        } else {
            self.val &= 0b1111_1011;
        }
    }

    pub(super) fn show_bg(&self) -> bool {
        self.val & 0b0000_1000 > 0
    }

    pub(super) fn set_show_bg(&mut self, val: bool) {
        if val {
            self.val |= 0b0000_1000;
        } else {
            self.val &= 0b1111_0111;
        }
    }

    pub(super) fn show_sprites(&self) -> bool {
        self.val & 0b0001_0000 > 0
    }

    pub(super) fn set_show_sprites(&mut self, val: bool) {
        if val {
            self.val |= 0b0001_0000;
        } else {
            self.val &= 0b1110_1111;
        }
    }

    pub(super) fn emphasize_red(&self) -> bool {
        self.val & 0b0010_0000 > 0
    }

    pub(super) fn emphasize_green(&self) -> bool {
        self.val & 0b0100_0000 > 0
    }

    pub(super) fn emphasize_blue(&self) -> bool {
        self.val & 0b1000_0000 > 0
    }

    pub(super) fn write(&mut self, data: u8) {
        self.val = data;
    }
}

/// Implement PPU Status Register
///
/// This struct is mainly dumb storage and doesn't have much logic own its own.
///
/// 7  bit  0
/// ---- ----
/// VSO. ....
/// |||| ||||
/// |||+-++++- PPU open bus. Returns stale PPU bus contents.
/// ||+------- Sprite overflow. The intent was for this flag to be set
/// ||         whenever more than eight sprites appear on a scanline, but a
/// ||         hardware bug causes the actual behavior to be more complicated
/// ||         and generate false positives as well as false negatives; see
/// ||         PPU sprite evaluation. This flag is set during sprite
/// ||         evaluation and cleared at dot 1 (the second dot) of the
/// ||         pre-render line.
/// |+-------- Sprite 0 Hit.  Set when a nonzero pixel of sprite 0 overlaps
/// |          a nonzero background pixel; cleared at dot 1 of the pre-render
/// |          line.  Used for raster timing.
/// +--------- Vertical blank has started (0: not in vblank; 1: in vblank).
///            Set at dot 1 of line 241 (the line *after* the post-render
///            line); cleared after reading $2002 and at dot 1 of the
///            pre-render line.
pub(super) struct PpuStatusRegister {
    val: u8,
}

impl PpuStatusRegister {
    fn new() -> Self {
        Self { val: 0 }
    }

    pub(super) fn read(&self) -> u8 {
        self.val
    }

    pub(super) fn sprite_overflow(&self) -> bool {
        self.val & 0b0010_0000 > 0
    }

    pub(super) fn set_sprite_overflow(&mut self, val: bool) {
        if val {
            self.val |= 0b0010_0000;
        } else {
            self.val &= 0b1101_1111;
        }
    }

    pub(super) fn sprite_0_hit(&self) -> bool {
        self.val & 0b0100_0000 > 0
    }

    pub(super) fn set_sprite_0_hit(&mut self, val: bool) {
        if val {
            self.val |= 0b0100_0000;
        } else {
            self.val &= 0b1011_1111;
        }
    }

    pub(super) fn in_vblank(&self) -> bool {
        self.val & 0b1000_0000 > 0
    }

    pub(super) fn set_in_vblank(&mut self, val: bool) {
        if val {
            self.val |= 0b1000_0000;
        } else {
            self.val &= 0b0111_1111;
        }
    }
}

/// Represent the way the PpuScrollRegister will be addressed
enum PpuScrollRegisterAddressLatch {
    /// Wrote value will affect the horizontal offset.
    Horizontal,
    /// Wrote value will affect the vertical offset.
    Vertical,
}

/// Implement PPU Scroll Register
///
/// This struct is mainly dumb storage and doesn't have much logic own its own.
pub(super) struct PpuScrollRegister {
    horizontal_offset: u8,
    vertical_offset: u8,
    address_latch: PpuScrollRegisterAddressLatch,
}

impl PpuScrollRegister {
    fn new() -> Self {
        Self {
            horizontal_offset: 0,
            vertical_offset: 0,
            address_latch: PpuScrollRegisterAddressLatch::Horizontal,
        }
    }

    pub(super) fn write(&mut self, val: u8) {
        match self.address_latch {
            PpuScrollRegisterAddressLatch::Horizontal => {
                self.horizontal_offset = val;
                self.address_latch = PpuScrollRegisterAddressLatch::Vertical;
            }
            PpuScrollRegisterAddressLatch::Vertical => {
                self.vertical_offset = val;
                self.address_latch = PpuScrollRegisterAddressLatch::Horizontal;
            }
        }
    }

    pub(super) fn reset_address_latch(&mut self) {
        self.address_latch = PpuScrollRegisterAddressLatch::Horizontal;
    }

    pub(super) fn horizontal_offset(&self) -> u8 {
        self.horizontal_offset
    }

    pub(super) fn vertical_offset(&self) -> u8 {
        self.vertical_offset
    }
}

/// Implement PPU Address Register
///
/// This struct is mainly dumb storage and doesn't have much logic own its own.
pub(super) struct PpuAddrRegister {
    first: u8,
    second: u8,
    next_address_is_first: bool,
}

impl PpuAddrRegister {
    fn new() -> Self {
        Self {
            first: 0,
            second: 0,
            next_address_is_first: true,
        }
    }

    pub(super) fn write(&mut self, data: u8) {
        if self.next_address_is_first {
            self.first = 0b011_1111 & data;
        } else {
            self.second = data;
        }
        self.next_address_is_first = !self.next_address_is_first;
    }

    pub(super) fn reset_address_latch(&mut self) {
        self.next_address_is_first = true;
    }

    pub(super) fn read_address(&mut self, increment: u8) -> u16 {
        let res = ((self.first as u16) << 8) | (self.second as u16);
        self.increment_stored_address(increment);
        res
    }

    fn increment_stored_address(&mut self, to_add: u8) {
        let prev_snd = self.second;
        self.second = self.second.wrapping_add(to_add);
        if prev_snd > self.second {
            self.first = 0b011_1111 & self.first.wrapping_add(1);
        }
    }
}
