/// Abstract over a NES Controller allowing both to indicate a button press and to allowing
/// the right behavior on read and write access as architectured by the original hardware.
///
/// It is intended that you create one `NesController` per different physical controller.
///
/// A key difference with original hardware is that though originally only the first NES Controller
/// should have a set behavior on write (which then impacts the second controller), here all
/// `NesController` created have to be written.
pub(crate) struct NesController {
    /// While `strobe_enabled` is `true`, the shift registers in the controllers are continuously
    /// reloaded from the button states, and reading any controller value will keep returning the
    /// current state of the first button (A).
    /// Once at `false`, this reloading will stop.
    ///
    /// It is intented to be used to restart cycling through buttons, as read will keep incrementing
    /// the index of the button read.
    strobe_enabled: bool,
    /// The NesController's button statuses can only be read one button at a time, each consecutive
    /// read giving the value for the next button until all (8) buttons have been read in which
    /// case all consecutive reads will return as if they are pressed.
    ///
    /// The buttons in order correspond to:
    /// 0 - A
    /// 1 - B
    /// 2 - Select
    /// 3 - Start
    /// 4 - Up
    /// 5 - Down
    /// 6 - Left
    /// 7 - Right
    ///
    /// `next_button_index` correspond to the index of the button that should be polled in the next
    /// read, or `8` if buttons have all been read (in which case you probably want to "strobe").
    next_button_index: u8,
    /// Regroups the pressed status (`1` being pressed, `0` being un-pressed) of each of the 8
    /// buttons on the NES Controller, each bit corresponding to a particular button. With `0`
    /// corresponding to the least significant bit to `7` being the most significant one:
    /// 0 - A
    /// 1 - B
    /// 2 - Select
    /// 3 - Start
    /// 4 - Up
    /// 5 - Down
    /// 6 - Left
    /// 7 - Right
    curr_button_state: u8,
}

impl NesController {
    /// Create a new `NesController` in a reseted status (strobe disabled, no button pressed).
    pub(crate) fn new() -> Self {
        Self {
            strobe_enabled: false,
            next_button_index: 0,
            curr_button_state: 0,
        }
    }

    /// Write `data` on the `NesController` address. This should be only useful for `strobe`
    /// enabling, where only the least significant bit is read.
    ///
    /// Note that unlike on original hardware, all created `NesController` have to be written on
    /// when a write has to be performed (on original hardware, only the first `NesController` can
    /// be written to, and it has the same impact on the other `NesController`).
    pub(crate) fn write(&mut self, data: u8) {
        self.strobe_enabled = data & 1 == 1;
        if self.strobe_enabled {
            self.next_button_index = 0;
        }
    }

    /// Read `status` of the next button, of the `A` button if `strobe` has been enabled through a
    /// preceding `write` call, or just `1` if all buttons have already been read.
    ///
    /// `1` indicates that the button is currently pressed, `0` indicates that it is un-pressed.
    pub(crate) fn read_next(&mut self) -> u8 {
        if self.next_button_index > 7 {
            return 1;
        }
        let button_bit = 1 << self.next_button_index;
        let response = (self.curr_button_state & button_bit) >> self.next_button_index;
        if !self.strobe_enabled && self.next_button_index <= 7 {
            self.next_button_index += 1;
        }
        response
    }

    /// Update the "pressed" status for the `A` button of this controller.
    ///
    /// # Arguments
    ///
    /// * `is_pressed` - `true` if the button just began to be pressed.
    /// `false` if it was just un-pressed.
    pub(crate) fn set_a(&mut self, is_pressed: bool) {
        if is_pressed {
            self.curr_button_state |= 0b00000001;
        } else {
            self.curr_button_state &= 0b11111110;
        }
    }

    /// Update the "pressed" status for the `B` button of this controller.
    ///
    /// # Arguments
    ///
    /// * `is_pressed` - `true` if the button just began to be pressed.
    /// `false` if it was just un-pressed.
    pub(crate) fn set_b(&mut self, is_pressed: bool) {
        if is_pressed {
            self.curr_button_state |= 0b00000010;
        } else {
            self.curr_button_state &= 0b11111101;
        }
    }

    /// Update the "pressed" status for the `Select` button of this controller.
    ///
    /// # Arguments
    ///
    /// * `is_pressed` - `true` if the button just began to be pressed.
    /// `false` if it was just un-pressed.
    pub(crate) fn set_select(&mut self, is_pressed: bool) {
        if is_pressed {
            self.curr_button_state |= 0b00000100;
        } else {
            self.curr_button_state &= 0b11111011;
        }
    }

    /// Update the "pressed" status for the `Start` button of this controller.
    ///
    /// # Arguments
    ///
    /// * `is_pressed` - `true` if the button just began to be pressed.
    /// `false` if it was just un-pressed.
    pub(crate) fn set_start(&mut self, is_pressed: bool) {
        if is_pressed {
            self.curr_button_state |= 0b00001000;
        } else {
            self.curr_button_state &= 0b11110111;
        }
    }

    /// Update the "pressed" status for the `Up` button of this controller.
    ///
    /// # Arguments
    ///
    /// * `is_pressed` - `true` if the button just began to be pressed.
    /// `false` if it was just un-pressed.
    pub(crate) fn set_up(&mut self, is_pressed: bool) {
        if is_pressed {
            self.curr_button_state |= 0b00010000;
        } else {
            self.curr_button_state &= 0b11101111;
        }
    }

    /// Update the "pressed" status for the `Down` button of this controller.
    ///
    /// # Arguments
    ///
    /// * `is_pressed` - `true` if the button just began to be pressed.
    /// `false` if it was just un-pressed.
    pub(crate) fn set_down(&mut self, is_pressed: bool) {
        if is_pressed {
            self.curr_button_state |= 0b00100000;
        } else {
            self.curr_button_state &= 0b11011111;
        }
    }

    /// Update the "pressed" status for the `Left` button of this controller.
    ///
    /// # Arguments
    ///
    /// * `is_pressed` - `true` if the button just began to be pressed.
    /// `false` if it was just un-pressed.
    pub(crate) fn set_left(&mut self, is_pressed: bool) {
        if is_pressed {
            self.curr_button_state |= 0b01000000;
        } else {
            self.curr_button_state &= 0b10111111;
        }
    }

    /// Update the "pressed" status for the `Right` button of this controller.
    ///
    /// # Arguments
    ///
    /// * `is_pressed` - `true` if the button just began to be pressed.
    /// `false` if it was just un-pressed.
    pub(crate) fn set_right(&mut self, is_pressed: bool) {
        if is_pressed {
            self.curr_button_state |= 0b10000000;
        } else {
            self.curr_button_state &= 0b01111111;
        }
    }
}
