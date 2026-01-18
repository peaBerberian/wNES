//! # CPU Status Register
//!
//! Simple implementation of the status register of the NES' CPU.

pub(super) struct CpuStatusRegister {
    /// From lsb to msb:
    ///   0 - Carry Flag
    ///   1 - Zero Flag
    ///   2 - Interrupt disable
    ///   3 - Decimal Mode Flag
    ///   4 - B flag
    ///   5 - Always 1
    ///   6 - Overflow Flag
    ///   7 - Negative Flag
    status: u8,
}

impl CpuStatusRegister {
    pub(super) fn new() -> Self {
        Self {
            status: 0b0010_0100,
        }
    }

    pub(super) fn force(&mut self, byt: u8) {
        self.status = byt & 0b1110_1111;
    }

    pub(super) fn carry(&self) -> bool {
        self.status & 0b0000_0001 > 0
    }

    pub(super) fn zero(&self) -> bool {
        self.status & 0b0000_0010 > 0
    }

    // pub(super) fn interrupt_disable(&self) -> bool {
    //     self.status & 0b0000_0100 > 0
    // }

    // pub(super) fn decimal(&self) -> bool {
    //     self.status & 0b0000_1000 > 0
    // }

    pub(super) fn overflow(&self) -> bool {
        self.status & 0b0100_0000 > 0
    }

    pub(super) fn negative(&self) -> bool {
        self.status & 0b1000_0000 > 0
    }

    pub(super) fn set_carry(&mut self, val: bool) {
        if val {
            self.status |= 0b0000_0001;
        } else {
            self.status &= 0b1111_1110;
        }
    }

    pub(super) fn set_zero(&mut self, val: bool) {
        if val {
            self.status |= 0b0000_0010;
        } else {
            self.status &= 0b1111_1101;
        }
    }

    pub(super) fn set_interrupt_disable(&mut self, val: bool) {
        if val {
            self.status |= 0b0000_0100;
        } else {
            self.status &= 0b1111_1011;
        }
    }

    pub(super) fn set_decimal(&mut self, val: bool) {
        if val {
            self.status |= 0b0000_1000;
        } else {
            self.status &= 0b1111_0111;
        }
    }

    pub(super) fn set_negative(&mut self, val: bool) {
        if val {
            self.status |= 0b1000_0000;
        } else {
            self.status &= 0b0111_1111;
        }
    }

    pub(super) fn set_overflow(&mut self, val: bool) {
        if val {
            self.status |= 0b0100_0000;
        } else {
            self.status &= 0b1011_1111;
        }
    }

    pub(super) fn as_byte(&self, with_b_flag: bool) -> u8 {
        if with_b_flag {
            self.status | 0b0011_0000
        } else {
            self.status | 0b010_0000
        }
    }
}
