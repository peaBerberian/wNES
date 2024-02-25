pub(crate) struct NesController {
    strobe_enabled: bool,
    curr_btn_index: u8,
    button_state: u8,
}

impl NesController {
    pub(crate) fn new() -> Self {
        Self {
            strobe_enabled: false,
            curr_btn_index: 0,
            button_state: 0,
        }
    }

    pub(crate) fn write(&mut self, data: u8) {
        self.strobe_enabled = data & 1 == 1;
        if self.strobe_enabled {
            self.curr_btn_index = 0;
        }
    }

    pub(crate) fn read(&mut self) -> u8 {
        if self.curr_btn_index > 7 {
            return 1;
        }
        let button_bit = 1 << self.curr_btn_index;
        let response = (self.button_state & button_bit) >> self.curr_btn_index;
        if !self.strobe_enabled && self.curr_btn_index <= 7 {
            self.curr_btn_index += 1;
        }
        response
    }

    pub(crate) fn set_a(&mut self, is_pressed: bool) {
        if is_pressed {
            self.button_state |= 0b00000001;
        } else {
            self.button_state &= 0b11111110;
        }
    }
    pub(crate) fn set_b(&mut self, is_pressed: bool) {
        if is_pressed {
            self.button_state |= 0b00000010;
        } else {
            self.button_state &= 0b11111101;
        }
    }
    pub(crate) fn set_select(&mut self, is_pressed: bool) {
        if is_pressed {
            self.button_state |= 0b00000100;
        } else {
            self.button_state &= 0b11111011;
        }
    }
    pub(crate) fn set_start(&mut self, is_pressed: bool) {
        if is_pressed {
            self.button_state |= 0b00001000;
        } else {
            self.button_state &= 0b11110111;
        }
    }
    pub(crate) fn set_up(&mut self, is_pressed: bool) {
        if is_pressed {
            self.button_state |= 0b00010000;
        } else {
            self.button_state &= 0b11101111;
        }
    }
    pub(crate) fn set_down(&mut self, is_pressed: bool) {
        if is_pressed {
            self.button_state |= 0b00100000;
        } else {
            self.button_state &= 0b11011111;
        }
    }
    pub(crate) fn set_left(&mut self, is_pressed: bool) {
        if is_pressed {
            self.button_state |= 0b01000000;
        } else {
            self.button_state &= 0b10111111;
        }
    }
    pub(crate) fn set_right(&mut self, is_pressed: bool) {
        if is_pressed {
            self.button_state |= 0b10000000;
        } else {
            self.button_state &= 0b01111111;
        }
    }
}
