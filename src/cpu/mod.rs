/// # CPU emulation
///
/// Emulates the NES 6502-family microprocessor.

mod debug;
mod flags;
mod op_code;

use op_code::{AddressMode, Instruction, OpCode};
use crate::bus::NesBus;

pub(crate) struct CpuComputationResult {
    pub(crate) cycles: u8,
    pub(crate) brk: bool,
}

/// The lowest byte in Nes' memory dedicated to the stack.
const STACK_LO: u16 = 0x0100;

pub(super) struct NesCpu<'a> {
    /// Value in the `A` register
    reg_a: u8,

    /// Value in the `X` register
    reg_x: u8,

    /// Value in the `Y` register
    reg_y: u8,

    /// flags register.
    flags: flags::CpuFlags,

    bus: &'a mut NesBus,

    /// The Nes' stack is descending (it grows downward in terms of memory address).
    ///
    /// `stack_pointer` is the offset relative to `STACK_LO` where the first
    /// unset byte of the stack is found.
    stack_pointer: u8,

    /// Value of the Program Counter Register. Storing the address of the next
    /// instruction to read.
    program_counter: u16,
}

impl<'a> NesCpu<'a> {
    pub(super) fn new(bus: &'a mut NesBus) -> Self {
        NesCpu {
            reg_a: 0,
            reg_x: 0,
            reg_y: 0,
            flags: flags::CpuFlags::new(),
            stack_pointer: 0xFD,
            program_counter: 0xC000,
            bus,
        }
    }

    pub(crate) fn read_u8_at(&mut self, addr: u16) -> u8 {
        self.bus.read(addr)
    }

    pub(crate) fn write_u8_at(&mut self, addr: u16, val: u8) {
        self.bus.write(addr, val);
    }

    pub(super) fn next_op(&mut self) -> Result<CpuComputationResult, AddressComputationError> {
        if cfg!(feature = "debug_cpu") {
            use super::cpu::debug::*;
            let format = format_instr(self.bus, self.program_counter, self.reg_x, self.reg_y);
            let reg_status = format!(
                "A:{:02X} X:{:02X} Y:{:02X} P:{:02X} SP:{:02X}",
                self.reg_a,
                self.reg_x,
                self.reg_y,
                self.flags.as_byte(false),
                self.stack_pointer
            );
            println!(
                "{:04X}  {:8} {:32} {}",
                self.program_counter, format.hex, format.fmt, reg_status
            );
        }
        let op_code = self.bus.read(self.program_counter);
        let parsed_op = OpCode::new(op_code);
        self.program_counter += 1;
        match (parsed_op.instr(), parsed_op.mode()) {
            (Instruction::ADC, mode) => {
                let val = self.operand_value(mode)?;
                self.exec_adc(val);
            }
            (Instruction::AND, mode) => {
                let val = self.operand_value(mode)?;
                self.exec_and(val);
            }
            (Instruction::ASL, AddressMode::Accumulator) => self.exec_asl_acc(),
            (Instruction::ASL, mode) => {
                let addr = self.compute_addr(mode)?;
                self.exec_asl_mem(addr);
            }
            (Instruction::BCC, AddressMode::Relative) => {
                let addr = self.compute_addr(AddressMode::Relative)?;
                self.exec_bcc(addr);
            }
            (Instruction::BCS, AddressMode::Relative) => {
                let addr = self.compute_addr(AddressMode::Relative)?;
                self.exec_bcs(addr);
            }
            (Instruction::BEQ, AddressMode::Relative) => {
                let addr = self.compute_addr(AddressMode::Relative)?;
                self.exec_beq(addr);
            }
            (Instruction::BIT, mode) => {
                let val = self.operand_value(mode)?;
                self.exec_bit(val);
            }
            (Instruction::BMI, AddressMode::Relative) => {
                let addr = self.compute_addr(AddressMode::Relative)?;
                self.exec_bmi(addr);
            }
            (Instruction::BNE, AddressMode::Relative) => {
                let addr = self.compute_addr(AddressMode::Relative)?;
                self.exec_bne(addr);
            }
            (Instruction::BPL, AddressMode::Relative) => {
                let addr = self.compute_addr(AddressMode::Relative)?;
                self.exec_bpl(addr);
            }
            (Instruction::BRK, AddressMode::Implied) => {
                self.exec_brk();
                // TODO normally we should just continue execution from 0xFFFE
                return Ok(CpuComputationResult {
                    cycles: parsed_op.nb_cycles(),
                    brk: true,
                });
            }
            (Instruction::BVC, AddressMode::Relative) => {
                let addr = self.compute_addr(AddressMode::Relative)?;
                self.exec_bvc(addr);
            }
            (Instruction::BVS, AddressMode::Relative) => {
                let addr = self.compute_addr(AddressMode::Relative)?;
                self.exec_bvs(addr);
            }
            (Instruction::CLC, AddressMode::Implied) => self.exec_clc(),
            (Instruction::CLD, AddressMode::Implied) => self.exec_cld(),
            (Instruction::CLI, AddressMode::Implied) => self.exec_cli(),
            (Instruction::CLV, AddressMode::Implied) => self.exec_clv(),
            (Instruction::CMP, mode) => {
                let val = self.operand_value(mode)?;
                self.exec_cmp(val);
            }
            (Instruction::CPX, mode) => {
                let val = self.operand_value(mode)?;
                self.exec_cpx(val);
            }
            (Instruction::CPY, mode) => {
                let val = self.operand_value(mode)?;
                self.exec_cpy(val);
            }
            (Instruction::DEC, mode) => {
                let addr = self.compute_addr(mode)?;
                self.exec_dec(addr);
            }
            (Instruction::DEX, AddressMode::Implied) => self.exec_dex(),
            (Instruction::DEY, AddressMode::Implied) => self.exec_dey(),
            (Instruction::EOR, mode) => {
                let val = self.operand_value(mode)?;
                self.exec_eor(val);
            }
            (Instruction::INC, mode) => {
                let addr = self.compute_addr(mode)?;
                self.exec_inc(addr);
            }
            (Instruction::INX, AddressMode::Implied) => self.exec_inx(),
            (Instruction::INY, AddressMode::Implied) => self.exec_iny(),
            (Instruction::JMP, mode) => {
                let addr = self.compute_addr(mode)?;
                self.exec_jmp(addr);
            }
            (Instruction::JSR, mode) => {
                let addr = self.compute_addr(mode)?;
                self.exec_jsr(addr);
            }
            (Instruction::LDA, mode) => {
                let val = self.operand_value(mode)?;
                self.exec_lda(val);
            }
            (Instruction::LDX, mode) => {
                let val = self.operand_value(mode)?;
                self.exec_ldx(val);
            }
            (Instruction::LDY, mode) => {
                let val = self.operand_value(mode)?;
                self.exec_ldy(val);
            }
            (Instruction::LSR, AddressMode::Accumulator) => self.exec_lsr_acc(),
            (Instruction::LSR, mode) => {
                let addr = self.compute_addr(mode)?;
                self.exec_lsr_mem(addr);
            }
            (Instruction::NOP, AddressMode::Implied) => {}
            (Instruction::ORA, mode) => {
                let val = self.operand_value(mode)?;
                self.exec_ora(val);
            }
            (Instruction::PHA, AddressMode::Implied) => self.exec_pha(),
            (Instruction::PHP, AddressMode::Implied) => self.exec_php(),
            (Instruction::PLA, AddressMode::Implied) => self.exec_pla(),
            (Instruction::PLP, AddressMode::Implied) => self.exec_plp(),
            (Instruction::ROL, AddressMode::Accumulator) => self.exec_rol_acc(),
            (Instruction::ROL, mode) => {
                let addr = self.compute_addr(mode)?;
                self.exec_rol_mem(addr);
            }
            (Instruction::ROR, AddressMode::Accumulator) => self.exec_ror_acc(),
            (Instruction::ROR, mode) => {
                let addr = self.compute_addr(mode)?;
                self.exec_ror_mem(addr);
            }
            (Instruction::RTI, AddressMode::Implied) => self.exec_rti(),
            (Instruction::RTS, AddressMode::Implied) => self.exec_rts(),
            (Instruction::SBC, mode) => {
                let val = self.operand_value(mode)?;
                self.exec_sbc(val);
            }
            (Instruction::SEC, AddressMode::Implied) => self.exec_sec(),
            (Instruction::SED, AddressMode::Implied) => self.exec_sed(),
            (Instruction::SEI, AddressMode::Implied) => self.exec_sei(),
            (Instruction::STA, mode) => {
                let addr = self.compute_addr(mode)?;
                self.exec_sta(addr);
            }
            (Instruction::STX, mode) => {
                let addr = self.compute_addr(mode)?;
                self.exec_stx(addr);
            }
            (Instruction::STY, mode) => {
                let addr = self.compute_addr(mode)?;
                self.exec_sty(addr);
            }
            (Instruction::TAX, AddressMode::Implied) => self.exec_tax(),
            (Instruction::TAY, AddressMode::Implied) => self.exec_tay(),
            (Instruction::TSX, AddressMode::Implied) => self.exec_tsx(),
            (Instruction::TXA, AddressMode::Implied) => self.exec_txa(),
            (Instruction::TXS, AddressMode::Implied) => self.exec_txs(),
            (Instruction::TYA, AddressMode::Implied) => self.exec_tya(),

            (Instruction::UDCP, mode) => {
                let addr = self.compute_addr(mode)?;
                self.exec_unofficial_dcp(addr);
            }
            (Instruction::ULAX, mode) => {
                let val = self.operand_value(mode)?;
                self.exec_unofficial_lax(val);
            }
            (Instruction::UNOP, AddressMode::Implied) => {}
            (Instruction::UNOP, mode) => {
                let _ = self.compute_addr(mode)?;
            }
            (Instruction::USAX, mode) => {
                let addr = self.compute_addr(mode)?;
                self.exec_unofficial_sax(addr);
            }
            (Instruction::USBC, mode) => {
                let val = self.operand_value(mode)?;
                self.exec_sbc(val);
            }
            _ => {
                if cfg!(feature = "debug_cpu") {
                    eprintln!("Unrecognized CPU instruction: {:02X}", op_code);
                }
            }
        };
        self.bus.tick(parsed_op.nb_cycles());
        Ok(CpuComputationResult {
            cycles: parsed_op.nb_cycles(),
            brk: false,
        })
    }

    fn read_u16_at(&mut self, addr: u16) -> u16 {
        if addr == 0xFFFF {
            // TODO check what should be done
            u16::from(self.read_u8_at(0xFFFF)) << 8
        } else {
            u16::from(self.read_u8_at(addr)) | u16::from(self.read_u8_at(addr + 1)) << 8
        }
    }

    fn push_stack_u8(&mut self, val: u8) {
        self.bus.write(STACK_LO + (self.stack_pointer as u16), val);
        self.stack_pointer = self.stack_pointer.wrapping_sub(1);
    }

    fn pop_stack_u8(&mut self) -> u8 {
        self.stack_pointer = self.stack_pointer.wrapping_add(1);
        self.bus.read(STACK_LO + (self.stack_pointer as u16))
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
                let pc = self.program_counter;
                self.program_counter += 1;

                // Give directly address of ROM value
                Ok(pc)
            }
            AddressMode::Absolute => {
                let val = self.read_u16_at(self.program_counter);
                self.program_counter += 2;
                Ok(val)
            }
            AddressMode::Unknown => Err(AddressComputationError::NoAddress),
            AddressMode::Accumulator => Err(AddressComputationError::NoAddress),
            AddressMode::Implied => Err(AddressComputationError::NoAddress),
            AddressMode::ZeroPage => {
                let current_counter = self.program_counter;
                let val = self.read_u8_at(current_counter);
                self.program_counter += 1;
                Ok(val as u16)
            }
            AddressMode::AbsoluteX => {
                let current_counter = self.program_counter;
                let val = self.read_u16_at(current_counter);
                self.program_counter += 2;
                Ok(val.wrapping_add(self.reg_x as u16))
            }
            AddressMode::AbsoluteY => {
                let current_counter = self.program_counter;
                let val = self.read_u16_at(current_counter);
                self.program_counter += 2;
                Ok(val.wrapping_add(self.reg_y as u16))
            }
            AddressMode::ZeroPageX => {
                let current_counter = self.program_counter;
                let val = self.read_u8_at(current_counter);
                self.program_counter += 1;
                Ok(val.wrapping_add(self.reg_x) as u16)
            }
            AddressMode::ZeroPageY => {
                let current_counter = self.program_counter;
                let val = self.read_u8_at(current_counter);
                self.program_counter += 1;
                Ok(val.wrapping_add(self.reg_y) as u16)
            }
            AddressMode::IndirectZeroPageX => {
                let current_counter = self.program_counter;
                let val = self.read_u8_at(current_counter);
                self.program_counter += 1;
                let base_addr = val.wrapping_add(self.reg_x);
                let mem = u16::from(self.read_u8_at(base_addr as u16))
                    | u16::from(self.read_u8_at(base_addr.wrapping_add(1) as u16)) << 8;
                Ok(mem)
            }
            AddressMode::IndirectZeroPageY => {
                let current_counter = self.program_counter;
                let val = self.read_u8_at(current_counter);
                self.program_counter += 1;
                let base_addr = u16::from(self.read_u8_at(val as u16))
                    | u16::from(self.read_u8_at(val.wrapping_add(1) as u16)) << 8;
                Ok(base_addr.wrapping_add(self.reg_y as u16))
            }
            AddressMode::Relative => {
                let current_counter = self.program_counter;
                let val = self.read_u8_at(current_counter);
                self.program_counter += 1;
                if val <= 127 {
                    Ok(self.program_counter + val as u16)
                } else {
                    Ok(self.program_counter - ((val ^ 0xFF) + 1) as u16)
                }
            }

            AddressMode::Indirect => {
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
                    let addr = self.read_u16_at(val);
                    Ok(addr)
                }
            }
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

    // Instructions

    fn exec_adc(&mut self, val: u8) {
        // println!("CPU: Add with carry (a: {}, val: {})", self.reg_a, val);
        let res = u16::from(self.reg_a) + u16::from(val) + if self.flags.carry() { 1 } else { 0 };
        self.flags.set_carry(res > 0b1111_1111);

        let res = res as u8;

        // TODO this may be wrong I had no brain cells left for that one
        self.flags
            .set_overflow((val ^ res) & (res ^ self.reg_a) & 0x80 != 0);
        self.set_zero_and_negative_flags(res);
        self.reg_a = res;
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
        self.set_zero_and_negative_flags(res);
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
        self.flags
            .set_negative((self.reg_a.wrapping_sub(val)) & 0b1000_0000 > 0);
    }

    fn exec_cpx(&mut self, val: u8) {
        // println!("CPU: Compare X Register (x: {}, val: {})", self.reg_x, val);
        self.flags.set_carry(self.reg_x >= val);
        self.flags.set_zero(self.reg_x == val);
        self.flags
            .set_negative((self.reg_x.wrapping_sub(val)) & 0b1000_0000 > 0);
    }

    fn exec_cpy(&mut self, val: u8) {
        // println!("CPU: Compare Y Register (y: {}, val: {})", self.reg_y, val);
        self.flags.set_carry(self.reg_y >= val);
        self.flags.set_zero(self.reg_y == val);
        self.flags
            .set_negative((self.reg_y.wrapping_sub(val)) & 0b1000_0000 > 0);
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
        self.set_zero_and_negative_flags(res);
    }

    fn exec_ora(&mut self, val: u8) {
        self.reg_a |= val;
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
            self.reg_a = self.reg_a.wrapping_add(1);
        }
        self.flags.set_carry(orig & 0b1000_0000 > 0);
        self.set_zero_and_negative_flags(self.reg_a);
    }

    fn exec_rol_mem(&mut self, addr: u16) {
        let val = self.read_u8_at(addr);
        let mut new_val = val << 1;
        if self.flags.carry() {
            new_val = new_val.wrapping_add(1);
        }
        self.write_u8_at(addr, new_val);
        self.flags.set_carry(val & 0b1000_0000 > 0);
        self.set_zero_and_negative_flags(new_val);
    }

    fn exec_ror_acc(&mut self) {
        let orig = self.reg_a;
        self.reg_a = orig >> 1;
        if self.flags.carry() {
            self.reg_a |= 0b1000_0000
        }
        self.flags.set_carry(orig & 0b0000_0001 > 0);
        self.set_zero_and_negative_flags(self.reg_a);
    }

    fn exec_ror_mem(&mut self, addr: u16) {
        let val = self.read_u8_at(addr);
        let mut new_val = val >> 1;
        if self.flags.carry() {
            new_val |= 0b1000_0000
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
        let res = u16::from(self.reg_a) + u16::from(!val) + if self.flags.carry() { 1 } else { 0 };
        self.flags.set_carry(res > 0b1111_1111);

        let res = res as u8;

        // TODO this may be wrong I had no brain cells left for that one
        self.flags
            .set_overflow(((val ^ 0xFF) ^ res) & (res ^ self.reg_a) & 0x80 != 0);
        self.set_zero_and_negative_flags(res);

        self.reg_a = res;
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
        self.write_u8_at(addr, self.reg_y);
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

    fn exec_unofficial_dcp(&mut self, addr: u16) {
        let mut val = self.read_u8_at(addr);
        val = val.wrapping_sub(1);
        self.write_u8_at(addr, val);
        self.flags.set_carry(self.reg_a >= val);
        self.flags.set_zero(self.reg_a == val);
        self.flags
            .set_negative((self.reg_a.wrapping_sub(val)) & 0b1000_0000 > 0);
    }

    fn exec_unofficial_lax(&mut self, val: u8) {
        self.reg_a = val;
        self.reg_x = val;
        self.set_zero_and_negative_flags(val);
    }

    fn exec_unofficial_sax(&mut self, addr: u16) {
        let res = self.reg_a & self.reg_x;
        self.write_u8_at(addr, res);
    }
}

#[derive(Debug, Clone)]
pub(super) enum AddressComputationError {
    // TODO is there a default behavior like setting lo to `0` or something?
    // MemoryOverflow,
    NoAddress,
}
