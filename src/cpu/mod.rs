mod flags;
mod op_code;

use op_code::{Instruction, AddressMode};

/// The lowest byte in Nes' memory dedicated to the stack.
const STACK_LO: u16 = 0x0100;

/// The lowest byte at which the program is accessible
const PROGRAM_START_OFFSET: u16 = 0x0600;

pub(super) struct NesCpu {
    /// Value of the `A` register
    reg_a: u8,

    /// Value of the `X` register
    reg_x: u8,

    /// Value of the `Y` register
    reg_y: u8,

    /// flags register.
    flags: flags::CpuFlags,

    /// The Nes' stack is descending (it grows downward in terms of memory address).
    ///
    /// `stack_pointer` is the offset relative to `STACK_LO` where the first
    /// unset byte of the stack is found.
    stack_pointer: u8,

    memory: [u8; 0xFFFF + 1],

    /// Value of the Program Counter Register. Storing the address of the next
    /// instruction to read.
    program_counter: u16,

    /// Value of the Maximum address at which the current loaded ROM is
    /// accessing.
    /// TODO really needed?
    program_max_ptr: u16,
}

impl NesCpu {
    pub(super) fn new() -> Self {
        NesCpu {
            reg_a: 0,
            reg_x: 0,
            reg_y: 0,
            flags: flags::CpuFlags::new(),
            stack_pointer: 0xFF,
            program_counter: 0,
            memory: [0; 0xFFFF + 1],
            program_max_ptr: 0,
        }
    }

    pub(super) fn load(&mut self, program: &[u8]) {
        let usize_start = PROGRAM_START_OFFSET as usize;
        self.memory[usize_start .. (usize_start + program.len())].copy_from_slice(program);
        self.program_max_ptr = program.len() as u16 + PROGRAM_START_OFFSET;
        self.program_counter = PROGRAM_START_OFFSET;

        // TODO Remove after test
        self.write_u16_at(0xFFFC, PROGRAM_START_OFFSET);
    }

    pub(super) fn read_u8_at(&self, addr: u16) -> u8 {
        self.memory[usize::from(addr)]
    }

    pub(super) fn write_u8_at(&mut self, addr: u16, val: u8) {
        self.memory[usize::from(addr)] = val;
    }

    fn read_u16_at(&self, addr: u16) -> u16 {
        if addr == 0xFFFF {
            // TODO check what should be done
            u16::from(self.read_u8_at(0xFFFF)) << 8
        } else {
            u16::from(self.read_u8_at(addr)) |
                u16::from(self.read_u8_at(addr + 1)) << 8
        }
    }

    // TODO as bidule to bidule::from? check
    fn write_u16_at(&mut self, addr: u16, val: u16) {
        if addr == 0xFFFF {
            // TODO check what should be done
            self.write_u8_at(0xFFFF, (val & 0x00FF) as u8)
        } else {
            self.write_u8_at(addr, (val & 0x00FF) as u8);
            self.write_u8_at(addr + 1, (val >> 8) as u8);
        }
    }

    pub(super) fn reg_a(&self) -> u8 {
        self.reg_a
    }

    pub(super) fn reg_x(&self) -> u8 {
        self.reg_x
    }

    fn push_stack_u8(&mut self, val: u8) {
        self.write_u8_at(STACK_LO + (self.stack_pointer as u16), val);
        self.stack_pointer = self.stack_pointer.wrapping_sub(1);
    }

    fn pop_stack_u8(&mut self) -> u8 {
        self.stack_pointer = self.stack_pointer.wrapping_add(1);
        self.read_u8_at(STACK_LO + (self.stack_pointer as u16))
    }

    fn push_stack_u16(&mut self, val: u16) {
        // There's a risk of wrapping around and all that so we cannot always
        // rely on already created methods.
        // Just do it two times.
        let hi = (val >> 8) as u8;
        let lo = (val & 0xff) as u8;
        self.push_stack_u8(hi);
        self.push_stack_u8(lo);
    }

    fn pop_stack_u16(&mut self) -> u16 {
        // There's a risk of wrapping around and all that so we cannot always
        // rely on already created methods.
        // Just do it two times.
        let lo = self.pop_stack_u8() as u16;
        let hi = self.pop_stack_u8() as u16;
        hi << 8 | lo
    }

    fn compute_addr(&mut self, mode: AddressMode) -> Result<u16, AddressComputationError> {
        match mode {
            AddressMode::Immediate => {
                if self.program_max_ptr <= self.program_counter {
                    return Err(AddressComputationError::MemoryOverflow);
                }
                let pc = self.program_counter;
                self.program_counter += 1;

                // Give directly address of ROM value
                Ok(pc)
            },
            AddressMode::Absolute => {
                if self.program_max_ptr <= self.program_counter + 1 {
                    return Err(AddressComputationError::MemoryOverflow);
                }
                let val = self.read_u16_at(self.program_counter);
                self.program_counter += 2;
                Ok(val)
            },
            AddressMode::Unknown => Err(AddressComputationError::NoAddress),
            AddressMode::Implied => Err(AddressComputationError::NoAddress),
            AddressMode::ZeroPage => {
                if self.program_max_ptr <= self.program_counter {
                    return Err(AddressComputationError::MemoryOverflow);
                }
                let current_counter = self.program_counter;
                let val = self.read_u8_at(current_counter);
                self.program_counter += 1;
                Ok(val as u16)
            },
            AddressMode::AbsoluteX => {
                if self.program_max_ptr <= self.program_counter + 1 {
                    return Err(AddressComputationError::MemoryOverflow);
                }
                let current_counter = self.program_counter;
                let val = self.read_u16_at(current_counter);
                self.program_counter += 2;
                Ok(val.wrapping_add(self.reg_x as u16))
            },
            AddressMode::AbsoluteY => {
                if self.program_max_ptr <= self.program_counter + 1 {
                    return Err(AddressComputationError::MemoryOverflow);
                }
                let current_counter = self.program_counter;
                let val = self.read_u16_at(current_counter);
                self.program_counter += 2;
                Ok(val.wrapping_add(self.reg_y as u16))
            },
            AddressMode::ZeroPageX => {
                if self.program_max_ptr <= self.program_counter {
                    return Err(AddressComputationError::MemoryOverflow);
                }
                let current_counter = self.program_counter;
                let val = self.read_u8_at(current_counter);
                self.program_counter += 1;
                Ok(val.wrapping_add(self.reg_x) as u16)
            },
            AddressMode::ZeroPageY => {
                if self.program_max_ptr <= self.program_counter {
                    return Err(AddressComputationError::MemoryOverflow);
                }
                let current_counter = self.program_counter;
                let val = self.read_u8_at(current_counter);
                self.program_counter += 1;
                Ok(val.wrapping_add(self.reg_y) as u16)
            },
            AddressMode::IndirectZeroPageX => {
                if self.program_max_ptr <= self.program_counter {
                    return Err(AddressComputationError::MemoryOverflow);
                }
                let current_counter = self.program_counter;
                let val = self.read_u8_at(current_counter);
                self.program_counter += 1;
                let base_addr = val.wrapping_add(self.reg_x) as u16;
                Ok(self.read_u16_at(base_addr))
            },
            AddressMode::IndirectZeroPageY => {
                if self.program_max_ptr <= self.program_counter {
                    return Err(AddressComputationError::MemoryOverflow);
                }
                let current_counter = self.program_counter;
                let val = self.read_u8_at(current_counter);
                self.program_counter += 1;
                let base_addr = self.read_u16_at(val as u16);
                Ok(base_addr.wrapping_add(self.reg_y as u16))
            },
            AddressMode::Relative => {
                if self.program_max_ptr <= self.program_counter {
                    return Err(AddressComputationError::MemoryOverflow);
                }
                let current_counter = self.program_counter;
                let val = self.read_u8_at(current_counter);
                self.program_counter += 1;
                if val <= 127 {
                    Ok(self.program_counter + val as u16)
                } else {
                    Ok(self.program_counter - ((val ^ 0xFF) + 1) as u16)
                }
            },

            AddressMode::Indirect => {
                if self.program_max_ptr <= self.program_counter + 1 {
                    return Err(AddressComputationError::MemoryOverflow);
                }
                let current_counter = self.program_counter;
                let val = self.read_u16_at(current_counter);
                self.program_counter += 2;

                // Funnily enough, the 6502 had a well-known bug which lead to wrapping around a
                // memory page when using Indirect addressing instead of crossing page boundaries.
                //
                // It's safer to replicate that bug here.
                if val & 0x00FF == 0x00FF {
                    let lo = self.read_u8_at(val);
                    let hi = self.read_u8_at(val & 0xFF00);
                    Ok((hi as u16) << 8 | (lo as u16))
                } else {
                    Ok(val)
                }
            },
        }
    }

    fn operand_value(&mut self, mode: AddressMode) -> Result<u8, AddressComputationError> {
        let addr_ope = self.compute_addr(mode)?;
        Ok(self.read_u8_at(addr_ope))
    }

    fn set_zero_and_negative_flags(&mut self, val: u8) {
        self.flags.set_zero(val == 0);
        self.flags.set_negative(val & 0b1000_0000 != 0);
    }

    pub(super) fn reset(&mut self) {
        self.reg_a = 0;
        self.reg_x = 0;
        self.reg_y = 0;
        self.flags.reset();
        self.stack_pointer = 0xFF;
        self.program_counter = self.read_u16_at(0xFFFC);
    }

    pub(super) fn next(&mut self) -> Result<bool, AddressComputationError> {
        let Some(op_code) = self.memory.get(self.program_counter as usize) else {
            return Err(AddressComputationError::MemoryOverflow);
        };
        let parsed = op_code::parse(*op_code);

        if cfg!(feature = "debug_cpu") {
            println!("CPU: a: {}, x: {}, y: {}, pc: {}, sp: {}, flags: {},",
                self.reg_a, self.reg_x, self.reg_y, self.program_counter,
                self.stack_pointer, self.flags.as_byte(false));
            println!("CPU: {} New instruction: {:?}, {:?}", self.program_counter, parsed.0, parsed.1);
        }

        self.program_counter += 1;
        match parsed {
            (Instruction::ADC, mode) => {
                let val = self.operand_value(mode)?;
                self.exec_adc(val);
            },
            (Instruction::AND, mode) => {
                let val = self.operand_value(mode)?;
                self.exec_and(val);
            },
            (Instruction::ASL, AddressMode::Implied) => self.exec_asl_acc(),
            (Instruction::ASL, mode) => {
                let addr = self.compute_addr(mode)?;
                self.exec_asl_mem(addr);
            },
            (Instruction::BCC, AddressMode::Relative) => {
                let addr = self.compute_addr(AddressMode::Relative)?;
                self.exec_bcc(addr);
            },
            (Instruction::BCS, AddressMode::Relative) => {
                let addr = self.compute_addr(AddressMode::Relative)?;
                self.exec_bcs(addr);
            },
            (Instruction::BEQ, AddressMode::Relative) => {
                let addr = self.compute_addr(AddressMode::Relative)?;
                self.exec_beq(addr);
            },
            (Instruction::BIT, mode) => {
                let val = self.operand_value(mode)?;
                self.exec_bit(val);
            },
            (Instruction::BMI, AddressMode::Relative) => {
                let addr = self.compute_addr(AddressMode::Relative)?;
                self.exec_bmi(addr);
            },
            (Instruction::BNE, AddressMode::Relative) => {
                let addr = self.compute_addr(AddressMode::Relative)?;
                self.exec_bne(addr);
            },
            (Instruction::BPL, AddressMode::Relative) => {
                let addr = self.compute_addr(AddressMode::Relative)?;
                self.exec_bpl(addr);
            },
            (Instruction::BRK, AddressMode::Implied) => {
                self.exec_brk();
                // TODO normally we should just continue execution from 0xFFFE
                return Ok(true);
            },
            (Instruction::BVC, AddressMode::Relative) => {
                let addr = self.compute_addr(AddressMode::Relative)?;
                self.exec_bvc(addr);
            },
            (Instruction::BVS, AddressMode::Relative) => {
                let addr = self.compute_addr(AddressMode::Relative)?;
                self.exec_bvs(addr);
            },
            (Instruction::CLC, AddressMode::Implied) => self.exec_clc(),
            (Instruction::CLD, AddressMode::Implied) => self.exec_cld(),
            (Instruction::CLI, AddressMode::Implied) => self.exec_cli(),
            (Instruction::CLV, AddressMode::Implied) => self.exec_clv(),
            (Instruction::CMP, mode) => {
                let val = self.operand_value(mode)?;
                self.exec_cmp(val);
            },
            (Instruction::CPX, mode) => {
                let val = self.operand_value(mode)?;
                self.exec_cpx(val);
            },
            (Instruction::CPY, mode) => {
                let val = self.operand_value(mode)?;
                self.exec_cpy(val);
            },
            (Instruction::DEC, mode) => {
                let addr = self.compute_addr(mode)?;
                self.exec_dec(addr);
            },
            (Instruction::DEX, AddressMode::Implied) => self.exec_dex(),
            (Instruction::DEY, AddressMode::Implied) => self.exec_dey(),
            (Instruction::EOR, mode) => {
                let val = self.operand_value(mode)?;
                self.exec_eor(val);
            },
            (Instruction::INC, mode) => {
                let addr = self.compute_addr(mode)?;
                self.exec_inc(addr);
            },
            (Instruction::INX, AddressMode::Implied) => self.exec_inx(),
            (Instruction::INY, AddressMode::Implied) => self.exec_iny(),
            (Instruction::JMP, mode) => {
                let addr = self.compute_addr(mode)?;
                self.exec_jmp(addr);
            },
            (Instruction::JSR, mode) => {
                let addr = self.compute_addr(mode)?;
                self.exec_jsr(addr);
            },
            (Instruction::LDA, mode) => {
                let val = self.operand_value(mode)?;
                self.exec_lda(val);
            },
            (Instruction::LDX, mode) => {
                let val = self.operand_value(mode)?;
                self.exec_ldx(val);
            },
            (Instruction::LDY, mode) => {
                let val = self.operand_value(mode)?;
                self.exec_ldy(val);
            },
            (Instruction::LSR, AddressMode::Implied) => self.exec_lsr_acc(),
            (Instruction::LSR, mode) => {
                let addr = self.compute_addr(mode)?;
                self.exec_lsr_mem(addr);
            },
            (Instruction::NOP, AddressMode::Implied) => { },
            (Instruction::ORA, mode) => {
                let val = self.operand_value(mode)?;
                self.exec_ora(val);
            },
            (Instruction::PHA, AddressMode::Implied) => self.exec_pha(),
            (Instruction::PHP, AddressMode::Implied) => self.exec_php(),
            (Instruction::PLA, AddressMode::Implied) => self.exec_pla(),
            (Instruction::PLP, AddressMode::Implied) => self.exec_plp(),
            (Instruction::ROL, AddressMode::Implied) => self.exec_rol_acc(),
            (Instruction::ROL, mode) => {
                let addr = self.compute_addr(mode)?;
                self.exec_rol_mem(addr);
            },
            (Instruction::ROR, AddressMode::Implied) => self.exec_ror_acc(),
            (Instruction::ROR, mode) => {
                let addr = self.compute_addr(mode)?;
                self.exec_ror_mem(addr);
            },
            (Instruction::RTI, AddressMode::Implied) => self.exec_rti(),
            (Instruction::RTS, AddressMode::Implied) => self.exec_rts(),
            (Instruction::SBC, mode) => {
                let val = self.operand_value(mode)?;
                self.exec_sbc(val);
            },
            (Instruction::SEC, AddressMode::Implied) => self.exec_sec(),
            (Instruction::SED, AddressMode::Implied) => self.exec_sed(),
            (Instruction::SEI, AddressMode::Implied) => self.exec_sei(),
            (Instruction::STA, mode) => {
                let addr = self.compute_addr(mode)?;
                self.exec_sta(addr);
            },
            (Instruction::STX, mode) => {
                let addr = self.compute_addr(mode)?;
                self.exec_stx(addr);
            },
            (Instruction::STY, mode) => {
                let addr = self.compute_addr(mode)?;
                self.exec_sty(addr);
            },
            (Instruction::TAX, AddressMode::Implied) => self.exec_tax(),
            (Instruction::TAY, AddressMode::Implied) => self.exec_tay(),
            (Instruction::TSX, AddressMode::Implied) => self.exec_tsx(),
            (Instruction::TXA, AddressMode::Implied) => self.exec_txa(),
            (Instruction::TXS, AddressMode::Implied) => self.exec_txs(),
            (Instruction::TYA, AddressMode::Implied) => self.exec_tya(),
            _ => {
                if cfg!(feature = "debug_cpu") {
                    println!("CPU: Unrecognized instruction: {}", *op_code);
                }
            },
        };
        Ok(false)
    }

    // Instructions

    fn exec_adc(&mut self, val: u8) {
        // println!("CPU: Add with carry (a: {}, val: {})", self.reg_a, val);
        let res = u16::from(self.reg_a) + u16::from(val) + if self.flags.carry() {
            1
        } else {
            0
        };
        self.flags.set_carry(res > 0b1111_1111);

        let res = res as u8;
        self.reg_a = res as u8;

        // TODO this may be wrong I had no brain cells left for that one
        self.flags.set_overflow((val ^ res) & (res ^ self.reg_a) & 0x80 != 0);
        self.set_zero_and_negative_flags(res);
    }

    fn exec_and(&mut self, val: u8) {
        // println!("CPU: Logical AND (a: {}, val: {}, res: {})", self.reg_a, val, self.reg_a & val);
        self.reg_a &= val;
        self.set_zero_and_negative_flags(self.reg_a);
    }

    fn exec_asl_acc(&mut self) {
        let orig = self.reg_a;
        self.reg_a = orig << 1;
        self.flags.set_carry(orig & 0b1000_0000 > 0);
        self.set_zero_and_negative_flags(self.reg_a);
    }

    fn exec_asl_mem(&mut self, addr: u16) {
        let val = self.read_u8_at(addr);
        let res = val << 1;
        self.write_u8_at(addr, res);
        self.flags.set_carry(val & 0b1000_0000 > 0);
        self.set_zero_and_negative_flags(val);
    }

    fn exec_bcc(&mut self, addr: u16) {
        if !self.flags.carry() {
            self.program_counter = addr;
        }
    }

    fn exec_bcs(&mut self, addr: u16) {
        if self.flags.carry() {
            self.program_counter = addr;
        }
    }

    fn exec_beq(&mut self, addr: u16) {
        if self.flags.zero() {
            self.program_counter = addr;
        }
    }

    fn exec_bit(&mut self, val: u8) {
        let res = val & self.reg_a;
        self.flags.set_zero(res == 0);
        self.flags.set_overflow(val & 0b0100_0000 != 0);
        self.flags.set_negative(val & 0b1000_0000 != 0);
    }

    fn exec_bmi(&mut self, addr: u16) {
        if self.flags.negative() {
            self.program_counter = addr;
        }
    }

    fn exec_bne(&mut self, addr: u16) {
        if !self.flags.zero() {
            self.program_counter = addr;
        }
    }

    fn exec_bpl(&mut self, addr: u16) {
        if !self.flags.negative() {
            self.program_counter = addr;
        }
    }

    fn exec_brk(&mut self) {
        self.push_stack_u16(self.program_counter);
        self.push_stack_u8(self.flags.as_byte(true));
        self.program_counter = self.read_u16_at(0xFFFE);
    }

    fn exec_bvc(&mut self, addr: u16) {
        if !self.flags.overflow() {
            self.program_counter = addr;
        }
    }

    fn exec_bvs(&mut self, addr: u16) {
        if self.flags.overflow() {
            self.program_counter = addr;
        }
    }

    fn exec_clc(&mut self) {
        // println!("CPU: Clear Carry Flag");
        self.flags.set_carry(false);
    }

    fn exec_cld(&mut self) {
        // println!("CPU: Clear Decimal Mode");
        self.flags.set_decimal(false);
    }

    fn exec_cli(&mut self) {
        // println!("CPU: Clear Interrupt Disable");
        self.flags.set_interrupt_disable(false);
    }

    fn exec_clv(&mut self) {
        // println!("CPU: Clear Overflow Flag");
        self.flags.set_overflow(false);
    }

    fn exec_cmp(&mut self, val: u8) {
        // println!("CPU: Compare (a: {}, val: {})", self.reg_a, val);
        self.flags.set_carry(self.reg_a >= val);
        self.flags.set_zero(self.reg_a == val);
        self.flags.set_negative((self.reg_a.wrapping_sub(val)) & 0b1000_0000 > 0);
    }

    fn exec_cpx(&mut self, val: u8) {
        // println!("CPU: Compare X Register (x: {}, val: {})", self.reg_x, val);
        self.flags.set_carry(self.reg_x >= val);
        self.flags.set_zero(self.reg_x == val);
        self.flags.set_negative((self.reg_x.wrapping_sub(val)) & 0b1000_0000 > 0);
    }

    fn exec_cpy(&mut self, val: u8) {
        // println!("CPU: Compare Y Register (y: {}, val: {})", self.reg_y, val);
        self.flags.set_carry(self.reg_y >= val);
        self.flags.set_zero(self.reg_y == val);
        self.flags.set_negative((self.reg_y.wrapping_sub(val)) & 0b1000_0000 > 0);
    }

    fn exec_dec(&mut self, addr: u16) {
        let val = self.read_u8_at(addr);
        let res = val.wrapping_sub(1);
        self.write_u8_at(addr, res);
        self.set_zero_and_negative_flags(res);
    }

    fn exec_dex(&mut self) {
        // println!("CPU: Decrement X Register (x: {}, res: {})", self.reg_x, self.reg_x.wrapping_sub(1));
        self.reg_x = self.reg_x.wrapping_sub(1);
        self.set_zero_and_negative_flags(self.reg_x);
    }

    fn exec_dey(&mut self) {
        // println!("CPU: Decrement Y Register (y: {}, res: {})", self.reg_y, self.reg_y.wrapping_sub(1));
        self.reg_y = self.reg_y.wrapping_sub(1);
        self.set_zero_and_negative_flags(self.reg_y);
    }

    fn exec_eor(&mut self, val: u8) {
        self.reg_a ^= val;
        self.set_zero_and_negative_flags(self.reg_a);
    }

    fn exec_inc(&mut self, addr: u16) {
        let val = self.read_u8_at(addr);
        let res = val.wrapping_add(1);
        self.write_u8_at(addr, res);
        self.set_zero_and_negative_flags(res);
    }

    fn exec_inx(&mut self) {
        // println!("CPU: Increment X Register (x: {}, res: {})", self.reg_x, self.reg_x.wrapping_add(1));
        self.reg_x = self.reg_x.wrapping_add(1);
        self.set_zero_and_negative_flags(self.reg_x);
    }

    fn exec_iny(&mut self) {
        // println!("CPU: Increment Y Register (x: {}, res: {})", self.reg_y, self.reg_y.wrapping_add(1));
        self.reg_y = self.reg_y.wrapping_add(1);
        self.set_zero_and_negative_flags(self.reg_y);
    }

    fn exec_jmp(&mut self, addr: u16) {
        // println!("CPU: Jump (addr: {addr}");
        self.program_counter = addr;
    }

    fn exec_jsr(&mut self, addr: u16) {
        // println!(
        //     "CPU: Jump to Subroutine (saved: {}, new: {})",
        //     self.program_counter - 1,
        //     addr
        // );

        // JSR pushes the address-1 of the next operation on to the stack
        self.push_stack_u16(self.program_counter - 1);
        self.program_counter = addr;
    }

    fn exec_lda(&mut self, val: u8) {
        // println!("CPU: Load Accumulator (val: {val})");
        self.reg_a = val;
        self.set_zero_and_negative_flags(val);
    }

    fn exec_ldx(&mut self, val: u8) {
        // println!("CPU: Load X Register (val: {val})");
        self.reg_x = val;
        self.set_zero_and_negative_flags(val);
    }

    fn exec_ldy(&mut self, val: u8) {
        // println!("CPU: Load Y Register (val: {val})");
        self.reg_y = val;
        self.set_zero_and_negative_flags(val);
    }

    fn exec_lsr_acc(&mut self) {
        let orig = self.reg_a;
        self.reg_a = orig >> 1;
        self.flags.set_carry(orig & 0b0000_0001 > 0);
        self.set_zero_and_negative_flags(self.reg_a);
    }

    fn exec_lsr_mem(&mut self, addr: u16) {
        let val = self.read_u8_at(addr);
        let res = val >> 1;
        self.write_u8_at(addr, res);
        self.flags.set_carry(val & 0b0000_0001 > 0);
        self.set_zero_and_negative_flags(val);
    }

    fn exec_ora(&mut self, val: u8) {
        self.reg_a = val | self.reg_a;
        self.set_zero_and_negative_flags(self.reg_a);
    }

    fn exec_pha(&mut self) {
        self.push_stack_u8(self.reg_a);
    }

    fn exec_php(&mut self) {
        self.push_stack_u8(self.flags.as_byte(true));
    }

    fn exec_pla(&mut self) {
        let popped = self.pop_stack_u8();
        self.reg_a = popped;
        self.set_zero_and_negative_flags(self.reg_a);
    }

    fn exec_plp(&mut self) {
        let popped = self.pop_stack_u8();
        self.flags.force(popped);
    }

    fn exec_rol_acc(&mut self) {
        let orig = self.reg_a;
        self.reg_a = orig << 1;
        if self.flags.carry() {
            self.reg_a += 1;
        }
        self.flags.set_carry(orig & 0b1000_0000 > 0);
        self.set_zero_and_negative_flags(self.reg_a);
    }

    fn exec_rol_mem(&mut self, addr: u16) {
        let val = self.read_u8_at(addr);
        let mut new_val = val << 1;
        if self.flags.carry() {
            new_val += 1;
        }
        self.write_u8_at(addr, new_val);
        self.flags.set_carry(val & 0b1000_0000 > 0);
        self.set_zero_and_negative_flags(new_val);
    }

    fn exec_ror_acc(&mut self) {
        let orig = self.reg_a;
        self.reg_a = orig >> 1;
        if self.flags.carry() {
            self.reg_a = self.reg_a | 0b1000_0000
        }
        self.flags.set_carry(orig & 0b0000_0001 > 0);
        self.set_zero_and_negative_flags(self.reg_a);
    }

    fn exec_ror_mem(&mut self, addr: u16) {
        let val = self.read_u8_at(addr);
        let mut new_val = val >> 1;
        if self.flags.carry() {
            new_val = new_val | 0b1000_0000
        }
        self.write_u8_at(addr, new_val);
        self.flags.set_carry(val & 0b0000_0001 > 0);
        self.set_zero_and_negative_flags(new_val);
    }

    fn exec_rti(&mut self) {
        let popped = self.pop_stack_u8();
        self.flags.force(popped);
        let popped = self.pop_stack_u16();
        self.program_counter = popped;
    }

    fn exec_rts(&mut self) {
        let popped = self.pop_stack_u16();
        // println!(
        //     "CPU: Return to Subroutine (addr: {})",
        //     popped + 1
        // );
        self.program_counter = popped + 1;
    }

    fn exec_sbc(&mut self, val: u8) {
        // Note: The NES 6502 doesn't support decimal mode, so at least there's that.

        // That link and a LOT of thinking made me more or less understand what I was doing, at
        // some point:
        // http://www.righto.com/2012/12/the-6502-overflow-flag-explained.html
        let res = u16::from(self.reg_a) + u16::from(!val) + if self.flags.carry() {
            1
        } else {
            0
        };
        self.flags.set_carry(res > 0b1111_1111);

        let res = res as u8;
        self.reg_a = res as u8;

        // TODO this may be wrong I had no brain cells left for that one
        self.flags.set_overflow((val ^ res) & (res ^ self.reg_a) & 0x80 != 0);
        self.set_zero_and_negative_flags(res);
    }

    fn exec_sec(&mut self) {
        // println!("CPU: Set Carry Flag");
        self.flags.set_carry(true);
    }

    fn exec_sed(&mut self) {
        // println!("CPU: Set Decimal Flag");
        self.flags.set_decimal(true);
    }

    fn exec_sei(&mut self) {
        // println!("CPU: Set Interrupt Disable");
        self.flags.set_interrupt_disable(true);
    }

    fn exec_sta(&mut self, addr: u16) {
        // println!("CPU: Store Accumulator (addr: {addr})");
        self.write_u8_at(addr, self.reg_a);
    }

    fn exec_stx(&mut self, addr: u16) {
        // println!("CPU: Store X Register (x: {}, addr: {addr})", self.reg_x);
        self.write_u8_at(addr, self.reg_x);
    }

    fn exec_sty(&mut self, addr: u16) {
        // println!("CPU: Store Y Register (x: {}, addr: {addr})", self.reg_x);
        self.write_u8_at(addr, self.reg_x);
    }

    fn exec_tax(&mut self) {
        // println!("CPU: Transfer Accumulator to X (a: {})", self.reg_a);
        self.reg_x = self.reg_a;
        self.set_zero_and_negative_flags(self.reg_a)
    }

    fn exec_tay(&mut self) {
        // println!("CPU: Transfer Accumulator to Y (a: {})", self.reg_a);
        self.reg_y = self.reg_a;
        self.set_zero_and_negative_flags(self.reg_a)
    }

    fn exec_tsx(&mut self) {
        // println!("CPU: Transfer Stack Pointer to X (s: {})", self.stack_pointer);
        self.reg_x = self.stack_pointer;
        self.set_zero_and_negative_flags(self.reg_x)
    }

    fn exec_txa(&mut self) {
        // println!("CPU: Transfer X to Accumulator (x: {})", self.reg_x);
        self.reg_a = self.reg_x;
        self.set_zero_and_negative_flags(self.reg_a)
    }

    fn exec_txs(&mut self) {
        // println!("CPU: Transfer X to Stack Pointer (x: {})", self.reg_x);
        self.stack_pointer = self.reg_x;
    }

    fn exec_tya(&mut self) {
        // println!("CPU: Transfer Y to Accumulator (y: {})", self.reg_x);
        self.reg_a = self.reg_y;
        self.set_zero_and_negative_flags(self.reg_a)
    }
}

#[cfg(test)]
mod test {
    use super::NesCpu;

    #[test]
    fn test_0xa9_lda_immediate_load_data() {
        let mut cpu = NesCpu::new();
        cpu.load(&[0xa9, 0x05, 0x00]);
        while cpu.next().unwrap() {}
        assert_eq!(cpu.reg_a, 0x05);
        assert!(cpu.flags.as_byte(false) & 0b0000_0010 == 0b00);
        assert!(cpu.flags.as_byte(false) & 0b1000_0000 == 0);
    }

    #[test]
    fn test_0xa9_lda_zero_flag() {
        let mut cpu = NesCpu::new();
        cpu.load(&[0xa9, 0x00, 0x00]);
        while cpu.next().unwrap() {}
        assert!(cpu.flags.as_byte(false) & 0b0000_0010 == 0b10);
    }

    #[test]
    fn test_0xaa_tax_move_a_to_x() {
        let mut cpu = NesCpu::new();
        cpu.reg_a = 10;
        cpu.load(&[0xaa, 0x00]);
        while cpu.next().unwrap() {}
        assert_eq!(cpu.reg_x, 10)
    }

    #[test]
    fn test_5_ops_working_together() {
        let mut cpu = NesCpu::new();
        cpu.load(&[0xa9, 0xc0, 0xaa, 0xe8, 0x00]);
        while cpu.next().unwrap() {}
        assert_eq!(cpu.reg_x, 0xc1)
    }

    #[test]
    fn test_inx_overflow() {
        let mut cpu = NesCpu::new();
        cpu.reg_x = 0xff;
        cpu.load(&[0xe8, 0xe8, 0x00]);
        while cpu.next().unwrap() {}
        assert_eq!(cpu.reg_x, 1)
    }

    #[test]
    fn test_dex_overflow() {
        let mut cpu = NesCpu::new();
        cpu.reg_x = 0x00;
        cpu.load(&[0xCA, 0xCA, 0x00]);
        while cpu.next().unwrap() {}
        assert_eq!(cpu.reg_x, 254)
    }
}

#[derive(Debug, Clone)]
pub(super) enum AddressComputationError {
    // TODO is there a default behavior like setting lo to `0` or something?
    MemoryOverflow,
    NoAddress,
}

