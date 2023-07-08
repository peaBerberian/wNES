pub(super) struct OpCode {
    instr: Instruction,
    mode: AddressMode,
    nb_cycles: u8,
    nb_bytes: u8,
    page_crossing_cycle: bool,
}

impl OpCode {
    fn new(instr: Instruction, mode: AddressMode) -> Self {
        let mut page_crossing_cycle = false;
        let nb_cycles = match instr {
            Instruction::BRK => 7,
            Instruction::JMP => {
                if mode == AddressMode::Indirect {
                    5
                } else {
                    3
                }
            }
            Instruction::RTI | Instruction::RTS => 6,
            Instruction::PHA | Instruction::PHP => 3,
            Instruction::PLA | Instruction::PLP => 4,
            Instruction::ASL
            | Instruction::LSR
            | Instruction::ROL
            | Instruction::ROR
            | Instruction::INC
            | Instruction::DEC
            | Instruction::UDCP
            | Instruction::URLA
            | Instruction::USLO
            | Instruction::USRE
            | Instruction::URRA
            | Instruction::UISB => match mode {
                AddressMode::Accumulator => 2,
                AddressMode::ZeroPage => 5,
                AddressMode::ZeroPageX => 6,
                AddressMode::Absolute => 6,
                AddressMode::AbsoluteX => 7,
                AddressMode::AbsoluteY => 7,
                AddressMode::IndirectZeroPageX => 8,
                AddressMode::IndirectZeroPageY => 8,
                _ => 0,
            },
            _ => match mode {
                AddressMode::Immediate => 2,
                AddressMode::ZeroPage => 3,
                AddressMode::ZeroPageX | AddressMode::ZeroPageY => 4,
                AddressMode::Absolute => 4,
                AddressMode::AbsoluteX | AddressMode::AbsoluteY => {
                    if instr == Instruction::STA {
                        5
                    } else {
                        page_crossing_cycle = true;
                        4
                    }
                }
                AddressMode::IndirectZeroPageX => 6,
                AddressMode::IndirectZeroPageY => {
                    if instr == Instruction::STA {
                        6
                    } else {
                        page_crossing_cycle = true;
                        5
                    }
                }
                AddressMode::Accumulator => 2,
                AddressMode::Relative => 2,
                AddressMode::Implied => 2,
                AddressMode::Indirect => 4,
                AddressMode::Unknown => 0,
            },
        };

        let nb_bytes = match mode {
            AddressMode::Immediate
            | AddressMode::ZeroPage
            | AddressMode::ZeroPageX
            | AddressMode::ZeroPageY
            | AddressMode::IndirectZeroPageX
            | AddressMode::IndirectZeroPageY => 2,
            AddressMode::Absolute
            | AddressMode::AbsoluteX
            | AddressMode::AbsoluteY
            | AddressMode::Indirect => 3,
            AddressMode::Implied => 1,
            AddressMode::Relative => 2,
            AddressMode::Accumulator => 1,
            AddressMode::Unknown => 1,
        };

        Self {
            instr,
            mode,
            nb_cycles,
            nb_bytes,
            page_crossing_cycle,
        }
    }

    pub(super) fn instr(&self) -> Instruction {
        self.instr
    }

    pub(super) fn mode(&self) -> AddressMode {
        self.mode
    }

    pub(super) fn nb_cycles(&self) -> u8 {
        self.nb_cycles
    }

    pub(super) fn nb_bytes(&self) -> u8 {
        self.nb_bytes
    }

    pub(super) fn page_crossing_cycle(&self) -> bool {
        self.page_crossing_cycle
    }
}

pub(super) fn parse(instr: u8) -> OpCode {
    // TODO as an array/map insteady?
    match instr {
        // ADC
        0x69 => OpCode::new(Instruction::ADC, AddressMode::Immediate),
        0x65 => OpCode::new(Instruction::ADC, AddressMode::ZeroPage),
        0x75 => OpCode::new(Instruction::ADC, AddressMode::ZeroPageX),
        0x6D => OpCode::new(Instruction::ADC, AddressMode::Absolute),
        0x7D => OpCode::new(Instruction::ADC, AddressMode::AbsoluteX),
        0x79 => OpCode::new(Instruction::ADC, AddressMode::AbsoluteY),
        0x61 => OpCode::new(Instruction::ADC, AddressMode::IndirectZeroPageX),
        0x71 => OpCode::new(Instruction::ADC, AddressMode::IndirectZeroPageY),

        // AND
        0x29 => OpCode::new(Instruction::AND, AddressMode::Immediate),
        0x25 => OpCode::new(Instruction::AND, AddressMode::ZeroPage),
        0x35 => OpCode::new(Instruction::AND, AddressMode::ZeroPageX),
        0x2D => OpCode::new(Instruction::AND, AddressMode::Absolute),
        0x3D => OpCode::new(Instruction::AND, AddressMode::AbsoluteX),
        0x39 => OpCode::new(Instruction::AND, AddressMode::AbsoluteY),
        0x21 => OpCode::new(Instruction::AND, AddressMode::IndirectZeroPageX),
        0x31 => OpCode::new(Instruction::AND, AddressMode::IndirectZeroPageY),

        // ASL
        0x0A => OpCode::new(Instruction::ASL, AddressMode::Accumulator),
        0x06 => OpCode::new(Instruction::ASL, AddressMode::ZeroPage),
        0x16 => OpCode::new(Instruction::ASL, AddressMode::ZeroPageX),
        0x0E => OpCode::new(Instruction::ASL, AddressMode::Absolute),
        0x1E => OpCode::new(Instruction::ASL, AddressMode::AbsoluteX),

        0x90 => OpCode::new(Instruction::BCC, AddressMode::Relative),

        0xB0 => OpCode::new(Instruction::BCS, AddressMode::Relative),

        0xF0 => OpCode::new(Instruction::BEQ, AddressMode::Relative),

        // BIT
        0x24 => OpCode::new(Instruction::BIT, AddressMode::ZeroPage),
        0x2C => OpCode::new(Instruction::BIT, AddressMode::Absolute),

        0x30 => OpCode::new(Instruction::BMI, AddressMode::Relative),

        0xD0 => OpCode::new(Instruction::BNE, AddressMode::Relative),

        0x10 => OpCode::new(Instruction::BPL, AddressMode::Relative),

        0x00 => OpCode::new(Instruction::BRK, AddressMode::Implied),

        0x50 => OpCode::new(Instruction::BVC, AddressMode::Relative),

        0x70 => OpCode::new(Instruction::BVS, AddressMode::Relative),

        0x18 => OpCode::new(Instruction::CLC, AddressMode::Implied),

        0xD8 => OpCode::new(Instruction::CLD, AddressMode::Implied),

        0x58 => OpCode::new(Instruction::CLI, AddressMode::Implied),

        0xB8 => OpCode::new(Instruction::CLV, AddressMode::Implied),

        // CMP
        0xC9 => OpCode::new(Instruction::CMP, AddressMode::Immediate),
        0xC5 => OpCode::new(Instruction::CMP, AddressMode::ZeroPage),
        0xD5 => OpCode::new(Instruction::CMP, AddressMode::ZeroPageX),
        0xCD => OpCode::new(Instruction::CMP, AddressMode::Absolute),
        0xDD => OpCode::new(Instruction::CMP, AddressMode::AbsoluteX),
        0xD9 => OpCode::new(Instruction::CMP, AddressMode::AbsoluteY),
        0xC1 => OpCode::new(Instruction::CMP, AddressMode::IndirectZeroPageX),
        0xD1 => OpCode::new(Instruction::CMP, AddressMode::IndirectZeroPageY),

        // CPX
        0xE0 => OpCode::new(Instruction::CPX, AddressMode::Immediate),
        0xE4 => OpCode::new(Instruction::CPX, AddressMode::ZeroPage),
        0xEC => OpCode::new(Instruction::CPX, AddressMode::Absolute),

        // CPY
        0xC0 => OpCode::new(Instruction::CPY, AddressMode::Immediate),
        0xC4 => OpCode::new(Instruction::CPY, AddressMode::ZeroPage),
        0xCC => OpCode::new(Instruction::CPY, AddressMode::Absolute),

        // DEC
        0xC6 => OpCode::new(Instruction::DEC, AddressMode::ZeroPage),
        0xD6 => OpCode::new(Instruction::DEC, AddressMode::ZeroPageX),
        0xCE => OpCode::new(Instruction::DEC, AddressMode::Absolute),
        0xDE => OpCode::new(Instruction::DEC, AddressMode::AbsoluteX),

        0xCA => OpCode::new(Instruction::DEX, AddressMode::Implied),

        0x88 => OpCode::new(Instruction::DEY, AddressMode::Implied),

        // EOR
        0x49 => OpCode::new(Instruction::EOR, AddressMode::Immediate),
        0x45 => OpCode::new(Instruction::EOR, AddressMode::ZeroPage),
        0x55 => OpCode::new(Instruction::EOR, AddressMode::ZeroPageX),
        0x4D => OpCode::new(Instruction::EOR, AddressMode::Absolute),
        0x5D => OpCode::new(Instruction::EOR, AddressMode::AbsoluteX),
        0x59 => OpCode::new(Instruction::EOR, AddressMode::AbsoluteY),
        0x41 => OpCode::new(Instruction::EOR, AddressMode::IndirectZeroPageX),
        0x51 => OpCode::new(Instruction::EOR, AddressMode::IndirectZeroPageY),

        // INC
        0xE6 => OpCode::new(Instruction::INC, AddressMode::ZeroPage),
        0xF6 => OpCode::new(Instruction::INC, AddressMode::ZeroPageX),
        0xEE => OpCode::new(Instruction::INC, AddressMode::Absolute),
        0xFE => OpCode::new(Instruction::INC, AddressMode::AbsoluteX),

        0xE8 => OpCode::new(Instruction::INX, AddressMode::Implied),

        0xC8 => OpCode::new(Instruction::INY, AddressMode::Implied),

        // JMP
        0x4C => OpCode::new(Instruction::JMP, AddressMode::Absolute),
        0x6C => OpCode::new(Instruction::JMP, AddressMode::Indirect),

        0x20 => OpCode::new(Instruction::JSR, AddressMode::Absolute),

        // LDA
        0xA9 => OpCode::new(Instruction::LDA, AddressMode::Immediate),
        0xA5 => OpCode::new(Instruction::LDA, AddressMode::ZeroPage),
        0xB5 => OpCode::new(Instruction::LDA, AddressMode::ZeroPageX),
        0xAD => OpCode::new(Instruction::LDA, AddressMode::Absolute),
        0xBD => OpCode::new(Instruction::LDA, AddressMode::AbsoluteX),
        0xB9 => OpCode::new(Instruction::LDA, AddressMode::AbsoluteY),
        0xA1 => OpCode::new(Instruction::LDA, AddressMode::IndirectZeroPageX),
        0xB1 => OpCode::new(Instruction::LDA, AddressMode::IndirectZeroPageY),

        // LDX
        0xA2 => OpCode::new(Instruction::LDX, AddressMode::Immediate),
        0xA6 => OpCode::new(Instruction::LDX, AddressMode::ZeroPage),
        0xB6 => OpCode::new(Instruction::LDX, AddressMode::ZeroPageY),
        0xAE => OpCode::new(Instruction::LDX, AddressMode::Absolute),
        0xBE => OpCode::new(Instruction::LDX, AddressMode::AbsoluteY),

        // LDY
        0xA0 => OpCode::new(Instruction::LDY, AddressMode::Immediate),
        0xA4 => OpCode::new(Instruction::LDY, AddressMode::ZeroPage),
        0xB4 => OpCode::new(Instruction::LDY, AddressMode::ZeroPageX),
        0xAC => OpCode::new(Instruction::LDY, AddressMode::Absolute),
        0xBC => OpCode::new(Instruction::LDY, AddressMode::AbsoluteX),

        // LSR
        0x4A => OpCode::new(Instruction::LSR, AddressMode::Accumulator),
        0x46 => OpCode::new(Instruction::LSR, AddressMode::ZeroPage),
        0x56 => OpCode::new(Instruction::LSR, AddressMode::ZeroPageX),
        0x4E => OpCode::new(Instruction::LSR, AddressMode::Absolute),
        0x5E => OpCode::new(Instruction::LSR, AddressMode::AbsoluteX),

        0xEA => OpCode::new(Instruction::NOP, AddressMode::Implied),

        // ORA
        0x09 => OpCode::new(Instruction::ORA, AddressMode::Immediate),
        0x05 => OpCode::new(Instruction::ORA, AddressMode::ZeroPage),
        0x15 => OpCode::new(Instruction::ORA, AddressMode::ZeroPageX),
        0x0D => OpCode::new(Instruction::ORA, AddressMode::Absolute),
        0x1D => OpCode::new(Instruction::ORA, AddressMode::AbsoluteX),
        0x19 => OpCode::new(Instruction::ORA, AddressMode::AbsoluteY),
        0x01 => OpCode::new(Instruction::ORA, AddressMode::IndirectZeroPageX),
        0x11 => OpCode::new(Instruction::ORA, AddressMode::IndirectZeroPageY),

        0x48 => OpCode::new(Instruction::PHA, AddressMode::Implied),

        0x08 => OpCode::new(Instruction::PHP, AddressMode::Implied),

        0x68 => OpCode::new(Instruction::PLA, AddressMode::Implied),

        0x28 => OpCode::new(Instruction::PLP, AddressMode::Implied),

        // ROL
        0x2A => OpCode::new(Instruction::ROL, AddressMode::Accumulator),
        0x26 => OpCode::new(Instruction::ROL, AddressMode::ZeroPage),
        0x36 => OpCode::new(Instruction::ROL, AddressMode::ZeroPageX),
        0x2E => OpCode::new(Instruction::ROL, AddressMode::Absolute),
        0x3E => OpCode::new(Instruction::ROL, AddressMode::AbsoluteX),

        // ROR
        0x6A => OpCode::new(Instruction::ROR, AddressMode::Accumulator),
        0x66 => OpCode::new(Instruction::ROR, AddressMode::ZeroPage),
        0x76 => OpCode::new(Instruction::ROR, AddressMode::ZeroPageX),
        0x6E => OpCode::new(Instruction::ROR, AddressMode::Absolute),
        0x7E => OpCode::new(Instruction::ROR, AddressMode::AbsoluteX),

        0x40 => OpCode::new(Instruction::RTI, AddressMode::Implied),

        0x60 => OpCode::new(Instruction::RTS, AddressMode::Implied),

        // SBC
        0xE9 => OpCode::new(Instruction::SBC, AddressMode::Immediate),
        0xE5 => OpCode::new(Instruction::SBC, AddressMode::ZeroPage),
        0xF5 => OpCode::new(Instruction::SBC, AddressMode::ZeroPageX),
        0xED => OpCode::new(Instruction::SBC, AddressMode::Absolute),
        0xFD => OpCode::new(Instruction::SBC, AddressMode::AbsoluteX),
        0xF9 => OpCode::new(Instruction::SBC, AddressMode::AbsoluteY),
        0xE1 => OpCode::new(Instruction::SBC, AddressMode::IndirectZeroPageX),
        0xF1 => OpCode::new(Instruction::SBC, AddressMode::IndirectZeroPageY),

        0x38 => OpCode::new(Instruction::SEC, AddressMode::Implied),

        0xF8 => OpCode::new(Instruction::SED, AddressMode::Implied),

        0x78 => OpCode::new(Instruction::SEI, AddressMode::Implied),

        // STA
        0x85 => OpCode::new(Instruction::STA, AddressMode::ZeroPage),
        0x95 => OpCode::new(Instruction::STA, AddressMode::ZeroPageX),
        0x8D => OpCode::new(Instruction::STA, AddressMode::Absolute),
        0x9D => OpCode::new(Instruction::STA, AddressMode::AbsoluteX),
        0x99 => OpCode::new(Instruction::STA, AddressMode::AbsoluteY),
        0x81 => OpCode::new(Instruction::STA, AddressMode::IndirectZeroPageX),
        0x91 => OpCode::new(Instruction::STA, AddressMode::IndirectZeroPageY),

        // STX
        0x86 => OpCode::new(Instruction::STX, AddressMode::ZeroPage),
        0x96 => OpCode::new(Instruction::STX, AddressMode::ZeroPageY),
        0x8E => OpCode::new(Instruction::STX, AddressMode::Absolute),

        // STY
        0x84 => OpCode::new(Instruction::STY, AddressMode::ZeroPage),
        0x94 => OpCode::new(Instruction::STY, AddressMode::ZeroPageX),
        0x8C => OpCode::new(Instruction::STY, AddressMode::Absolute),

        0xAA => OpCode::new(Instruction::TAX, AddressMode::Implied),

        0xA8 => OpCode::new(Instruction::TAY, AddressMode::Implied),

        0xBA => OpCode::new(Instruction::TSX, AddressMode::Implied),

        0x8A => OpCode::new(Instruction::TXA, AddressMode::Implied),

        0x9A => OpCode::new(Instruction::TXS, AddressMode::Implied),

        0x98 => OpCode::new(Instruction::TYA, AddressMode::Implied),

        // Unofficial ones
        0xC7 => OpCode::new(Instruction::UDCP, AddressMode::ZeroPage),
        0xD7 => OpCode::new(Instruction::UDCP, AddressMode::ZeroPageX),
        0xCF => OpCode::new(Instruction::UDCP, AddressMode::Absolute),
        0xDF => OpCode::new(Instruction::UDCP, AddressMode::AbsoluteX),
        0xDB => OpCode::new(Instruction::UDCP, AddressMode::AbsoluteY),
        0xC3 => OpCode::new(Instruction::UDCP, AddressMode::IndirectZeroPageX),
        0xD3 => OpCode::new(Instruction::UDCP, AddressMode::IndirectZeroPageY),

        0x27 => OpCode::new(Instruction::URLA, AddressMode::ZeroPage),
        0x37 => OpCode::new(Instruction::URLA, AddressMode::ZeroPageX),
        0x2F => OpCode::new(Instruction::URLA, AddressMode::Absolute),
        0x3F => OpCode::new(Instruction::URLA, AddressMode::AbsoluteX),
        0x3B => OpCode::new(Instruction::URLA, AddressMode::AbsoluteY),
        0x23 => OpCode::new(Instruction::URLA, AddressMode::IndirectZeroPageX),
        0x33 => OpCode::new(Instruction::URLA, AddressMode::IndirectZeroPageY),

        0x07 => OpCode::new(Instruction::USLO, AddressMode::ZeroPage),
        0x17 => OpCode::new(Instruction::USLO, AddressMode::ZeroPageX),
        0x0F => OpCode::new(Instruction::USLO, AddressMode::Absolute),
        0x1F => OpCode::new(Instruction::USLO, AddressMode::AbsoluteX),
        0x1B => OpCode::new(Instruction::USLO, AddressMode::AbsoluteY),
        0x03 => OpCode::new(Instruction::USLO, AddressMode::IndirectZeroPageX),
        0x13 => OpCode::new(Instruction::USLO, AddressMode::IndirectZeroPageY),

        0x47 => OpCode::new(Instruction::USRE, AddressMode::ZeroPage),
        0x57 => OpCode::new(Instruction::USRE, AddressMode::ZeroPageX),
        0x4F => OpCode::new(Instruction::USRE, AddressMode::Absolute),
        0x5F => OpCode::new(Instruction::USRE, AddressMode::AbsoluteX),
        0x5B => OpCode::new(Instruction::USRE, AddressMode::AbsoluteY),
        0x43 => OpCode::new(Instruction::USRE, AddressMode::IndirectZeroPageX),
        0x53 => OpCode::new(Instruction::USRE, AddressMode::IndirectZeroPageY),

        // UNOP
        0x1A => OpCode::new(Instruction::UNOP, AddressMode::Implied),
        0x3A => OpCode::new(Instruction::UNOP, AddressMode::Implied),
        0x5A => OpCode::new(Instruction::UNOP, AddressMode::Implied),
        0x7A => OpCode::new(Instruction::UNOP, AddressMode::Implied),
        0xDA => OpCode::new(Instruction::UNOP, AddressMode::Implied),
        0xFA => OpCode::new(Instruction::UNOP, AddressMode::Implied),
        0x80 => OpCode::new(Instruction::UNOP, AddressMode::Immediate),
        0x82 => OpCode::new(Instruction::UNOP, AddressMode::Immediate),
        0x89 => OpCode::new(Instruction::UNOP, AddressMode::Immediate),
        0xC2 => OpCode::new(Instruction::UNOP, AddressMode::Immediate),
        0xE2 => OpCode::new(Instruction::UNOP, AddressMode::Immediate),
        0x04 => OpCode::new(Instruction::UNOP, AddressMode::ZeroPage),
        0x44 => OpCode::new(Instruction::UNOP, AddressMode::ZeroPage),
        0x64 => OpCode::new(Instruction::UNOP, AddressMode::ZeroPage),
        0x14 => OpCode::new(Instruction::UNOP, AddressMode::ZeroPageX),
        0x34 => OpCode::new(Instruction::UNOP, AddressMode::ZeroPageX),
        0x54 => OpCode::new(Instruction::UNOP, AddressMode::ZeroPageX),
        0x74 => OpCode::new(Instruction::UNOP, AddressMode::ZeroPageX),
        0xD4 => OpCode::new(Instruction::UNOP, AddressMode::ZeroPageX),
        0xF4 => OpCode::new(Instruction::UNOP, AddressMode::ZeroPageX),
        0x0C => OpCode::new(Instruction::UNOP, AddressMode::Absolute),
        0x1C => OpCode::new(Instruction::UNOP, AddressMode::AbsoluteX),
        0x3C => OpCode::new(Instruction::UNOP, AddressMode::AbsoluteX),
        0x5C => OpCode::new(Instruction::UNOP, AddressMode::AbsoluteX),
        0x7C => OpCode::new(Instruction::UNOP, AddressMode::AbsoluteX),
        0xDC => OpCode::new(Instruction::UNOP, AddressMode::AbsoluteX),
        0xFC => OpCode::new(Instruction::UNOP, AddressMode::AbsoluteX),

        0xA7 => OpCode::new(Instruction::ULAX, AddressMode::ZeroPage),
        0xB7 => OpCode::new(Instruction::ULAX, AddressMode::ZeroPageY),
        0xAF => OpCode::new(Instruction::ULAX, AddressMode::Absolute),
        0xBF => OpCode::new(Instruction::ULAX, AddressMode::AbsoluteY),
        0xA3 => OpCode::new(Instruction::ULAX, AddressMode::IndirectZeroPageX),
        0xB3 => OpCode::new(Instruction::ULAX, AddressMode::IndirectZeroPageY),

        0x87 => OpCode::new(Instruction::USAX, AddressMode::ZeroPage),
        0x97 => OpCode::new(Instruction::USAX, AddressMode::ZeroPageY),
        0x8f => OpCode::new(Instruction::USAX, AddressMode::Absolute),
        0x83 => OpCode::new(Instruction::USAX, AddressMode::IndirectZeroPageX),

        0xEB => OpCode::new(Instruction::USBC, AddressMode::Immediate),

        _ => OpCode::new(Instruction::Unknown, AddressMode::Unknown),
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub(super) enum AddressMode {
    /// For Unrecognized instructions
    Unknown,
    /// These instructions apply on the Accumulator register.
    Accumulator,
    /// These instructions act directly on one or more registers or flags internal to the CPU.
    /// Therefore, these instructions are principally single-byte instructions, lacking an explicit
    /// operand. The operand is implied, as it is already provided by the very instruction.

    /// Instructions targeting exclusively the contents of the accumulator may or may not be denoted
    /// by using an explicit "A" as the operand, depending on the flavor of syntax.
    /// (This may be regarded as a special address mode of its own, but it is really a special case
    /// of an implied instruction. It is still a single-byte instruction and no operand is provided
    /// in machine language.)
    Implied,
    /// Here, a literal operand is given immediately after the instruction. The operand is always
    /// an 8-bit value and the total instruction length is always 2 bytes. In memory, the operand
    /// is a single byte following immediately after the instruction code.
    Immediate,
    /// Absolute addressing modes provides the 16-bit address of a memory location, the contents of
    /// which used as the operand to the instruction. In machine language, the address is provided
    /// in two bytes immediately after the instruction (making these 3-byte instructions) in
    /// low-byte, high-byte order (LLHH) or little-endian.
    ///
    /// Absolute addresses are also used for the jump instructions JMP and JSR to provide the
    /// address for the next instruction to continue with in the control flow.
    Absolute,
    /// The 16-bit address space available to the 6502 is thought to consist of 256 "pages" of 256
    /// memory locations each ($00...$FF). In this model the high-byte of an address gives the page
    /// number and the low-byte a location inside this page. The very first of these pages, where
    /// the high-byte is zero (addresses $0000...$00FF), is somewhat special.
    ///
    /// The zero-page address mode is similar to absolute address mode, but these instructions use
    /// only a single byte for the operand, the low-byte, while the high-byte is assumed to be zero
    /// by definition. Therefore, these instructions have a total length of just two bytes (one less
    /// than absolute mode) and take one CPU cycle less to execute, as there is one byte less to
    /// fetch.
    ZeroPage,
    /// Adds the contents of the X-register to the provided 16 bit address to give the effective
    /// address, which provides the operand.
    ///
    /// As the base address is a 16-bit value, these are generally 3-byte instructions. Since there
    /// is an additional operation to perform to determine the effective address, these instructions
    /// are one cycle slower than those using absolute addressing mode.
    ///
    /// If the addition of the contents of the index register effects in a change of the high-byte
    /// given by the base address so that the effective address is on the next memory page, the
    /// additional operation to increment the high-byte takes another CPU cycle. This is also known
    /// as a crossing of page boundaries.
    AbsoluteX,
    /// Like AbsoluteX, except with the Y register instead of X.
    AbsoluteY,
    /// Adds the contents of the X-register to the provided ZeroPage bit address to give the
    /// effective address, which provides the operand.
    ///
    /// Never affects the high-byte of the effective address, which will simply wrap around in the
    /// zero-page, and there is no penalty for crossing any page boundaries.
    ZeroPageX,
    /// Like ZeroPageX, except with the Y register instead of X.
    ZeroPageY,
    /// Likes `ZeroPageX`, but after the X-register has been added to the base address, instead of
    /// directly accessing this, an additional lookup is performed, reading the contents of
    /// resulting address and the next one (in LLHH little-endian order), in order to determine the
    /// effective address.
    ///
    /// Like with `ZeroPageX` mode, the total instruction length is 2 bytes, but there are two
    /// additional CPU cycles in order to fetch the effective 16-bit address. As `ZeroPageX` mode, a
    /// lookup address will never overflow into the next page, but will simply wrap around in the
    /// zero-page.
    ///
    /// These instructions are useful, whenever we want to loop over a table of pointers to disperse
    /// addresses, or where we want to apply the same operation to various addresses, which we have
    /// stored as a table in the ZeroPage.
    IndirectZeroPageX,
    /// A pointer is first read (from the given ZeroPage address) and resolved and only then the
    /// contents of the Y-register is added to this to give the effective address.
    ///
    /// Like with ZeroPageY mode, the total instruction length is 2 bytes, but there it takes an
    /// additional CPU cycles to resolve and index the 16-bit pointer.
    /// As with AbsoluteX mode, the effective address may overflow into the next page, in the case
    /// of which the execution uses an extra CPU cycle.
    ///
    /// These instructions are useful, wherever we want to perform lookups on varying bases
    /// addresses or whenever we want to loop over tables, the base address of which we have stored
    /// in the ZeroPage.
    IndirectZeroPageY,
    /// Exclusive to conditional branch instructions, which branch in the execution path depending
    /// on the state of a given CPU flag.
    /// Here, the instruction provides only a relative offset, which is added to the contents of the
    /// program counter as it points to the immediate next instruction.
    /// The relative offset is a signed single byte value in two's complement encoding (giving a
    /// range of −128...127), which allows for branching up to half a page forwards and backwards.
    ///
    /// These instructions are always of 2 bytes length and perform in 2 CPU cycles, if the branch
    /// is not taken (the condition resolving to 'false'), and 3 cycles, if the branch is taken
    /// (when the condition is true). If a branch is taken and the target is on a different page,
    /// this adds another CPU cycle (4 in total).
    Relative,
    Indirect,
}

#[allow(clippy::upper_case_acronyms)]
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(super) enum Instruction {
    /// Unrecognized instruction
    Unknown,
    /// add with carry
    ADC,
    /// and (with accumulator)
    AND,
    /// arithmetic shift left
    ASL,
    /// branch on carry clear
    BCC,
    /// branch on carry set
    BCS,
    /// branch on equal (zero set)
    BEQ,
    /// bit test
    BIT,
    /// branch on minus (negative set)
    BMI,
    /// branch on not equal (zero clear)
    BNE,
    /// branch on plus (negative clear)
    BPL,
    /// break / interrupt
    BRK,
    /// branch on overflow clear
    BVC,
    /// branch on overflow set
    BVS,
    /// clear carry
    CLC,
    /// clear decimal
    CLD,
    /// clear interrupt disable
    CLI,
    /// clear overflow
    CLV,
    /// compare (with accumulator)
    CMP,
    /// compare with X
    CPX,
    /// compare with Y
    CPY,
    /// decrement
    DEC,
    /// decrement X
    DEX,
    /// decrement Y
    DEY,
    /// exclusive or (with accumulator)
    EOR,
    /// increment
    INC,
    /// increment X
    INX,
    /// increment Y
    INY,
    /// jump
    JMP,
    /// jump subroutine
    JSR,
    /// load accumulator
    LDA,
    /// load X
    LDX,
    /// load Y
    LDY,
    /// logical shift right
    LSR,
    /// no operation
    NOP,
    /// or with accumulator
    ORA,
    /// push accumulator
    PHA,
    /// push processor status (SR)
    PHP,
    /// pull accumulator
    PLA,
    /// pull processor status (SR)
    PLP,
    /// rotate left
    ROL,
    /// rotate right
    ROR,
    /// return from interrupt
    RTI,
    /// return from subroutine
    RTS,
    /// subtract with carry
    SBC,
    /// set carry
    SEC,
    /// set decimal
    SED,
    /// set interrupt disable
    SEI,
    /// store accumulator
    STA,
    /// store X
    STX,
    /// store Y
    STY,
    /// transfer accumulator to X
    TAX,
    /// transfer accumulator to Y
    TAY,
    /// transfer stack pointer to X
    TSX,
    /// transfer X to accumulator
    TXA,
    /// transfer X to stack pointer
    TXS,
    /// transfer Y to accumulator
    TYA,

    /// Subtract 1 from memory (without borrow).
    UDCP,

    /// Rotate one bit left in memory, then AND accumulator with memory
    URLA,

    /// Shift left one bit in memory, then OR accumulator with memory.
    USLO,

    /// Shift right one bit in memory, then EOR accumulator with memory.
    USRE,

    /// Unofficial NOPs which might fetch
    UNOP,

    /// Unofficial LDA + TAX
    ULAX,

    // TODO
    URRA,

    UISB,

    /// Unofficial AND X register with accumulator and store result in memory.
    USAX,

    /// Same as SBC
    USBC,
}
