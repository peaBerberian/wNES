pub(super) fn parse(instr: u8) -> (Instruction, AddressMode) {
    match instr {
        // ADC
        0x69 => (Instruction::ADC, AddressMode::Immediate),
        0x65 => (Instruction::ADC, AddressMode::ZeroPage),
        0x75 => (Instruction::ADC, AddressMode::ZeroPageX),
        0x6D => (Instruction::ADC, AddressMode::Absolute),
        0x7D => (Instruction::ADC, AddressMode::AbsoluteX),
        0x79 => (Instruction::ADC, AddressMode::AbsoluteY),
        0x61 => (Instruction::ADC, AddressMode::IndirectZeroPageX),
        0x71 => (Instruction::ADC, AddressMode::IndirectZeroPageY),

        // AND
        0x29 => (Instruction::AND, AddressMode::Immediate),
        0x25 => (Instruction::AND, AddressMode::ZeroPage),
        0x35 => (Instruction::AND, AddressMode::ZeroPageX),
        0x2D => (Instruction::AND, AddressMode::Absolute),
        0x3D => (Instruction::AND, AddressMode::AbsoluteX),
        0x39 => (Instruction::AND, AddressMode::AbsoluteY),
        0x21 => (Instruction::AND, AddressMode::IndirectZeroPageX),
        0x31 => (Instruction::AND, AddressMode::IndirectZeroPageY),

        // ASL
        0x0A => (Instruction::ASL, AddressMode::Implied),
        0x06 => (Instruction::ASL, AddressMode::ZeroPage),
        0x16 => (Instruction::ASL, AddressMode::ZeroPageX),
        0x0E => (Instruction::ASL, AddressMode::Absolute),
        0x1E => (Instruction::ASL, AddressMode::AbsoluteX),

        0x90 => (Instruction::BCC, AddressMode::Relative),

        0xB0 => (Instruction::BCS, AddressMode::Relative),

        0xF0 => (Instruction::BEQ, AddressMode::Relative),

        // BIT
        0x24 => (Instruction::BIT, AddressMode::ZeroPage),
        0x2C => (Instruction::BIT, AddressMode::Absolute),

        0x30 => (Instruction::BMI, AddressMode::Relative),

        0xD0 => (Instruction::BNE, AddressMode::Relative),

        0x10 => (Instruction::BPL, AddressMode::Relative),

        0x00 => (Instruction::BRK, AddressMode::Implied),

        0x50 => (Instruction::BVC, AddressMode::Relative),

        0x70 => (Instruction::BVS, AddressMode::Relative),

        0x18 => (Instruction::CLC, AddressMode::Implied),

        0xD8 => (Instruction::CLD, AddressMode::Implied),

        0x58 => (Instruction::CLI, AddressMode::Implied),

        0xB8 => (Instruction::CLV, AddressMode::Implied),

        // CMP
        0xC9 => (Instruction::CMP, AddressMode::Immediate),
        0xC5 => (Instruction::CMP, AddressMode::ZeroPage),
        0xD5 => (Instruction::CMP, AddressMode::ZeroPageX),
        0xCD => (Instruction::CMP, AddressMode::Absolute),
        0xDD => (Instruction::CMP, AddressMode::AbsoluteX),
        0xD9 => (Instruction::CMP, AddressMode::AbsoluteY),
        0xC1 => (Instruction::CMP, AddressMode::IndirectZeroPageX),
        0xD1 => (Instruction::CMP, AddressMode::IndirectZeroPageY),

        // CPX
        0xE0 => (Instruction::CPX, AddressMode::Immediate),
        0xE4 => (Instruction::CPX, AddressMode::ZeroPage),
        0xEC => (Instruction::CPX, AddressMode::Absolute),

        // CPY
        0xC0 => (Instruction::CPY, AddressMode::Immediate),
        0xC4 => (Instruction::CPY, AddressMode::ZeroPage),
        0xCC => (Instruction::CPY, AddressMode::Absolute),

        // DEC
        0xC6 => (Instruction::DEC, AddressMode::ZeroPage),
        0xD6 => (Instruction::DEC, AddressMode::ZeroPageX),
        0xCE => (Instruction::DEC, AddressMode::Absolute),
        0xDE => (Instruction::DEC, AddressMode::AbsoluteX),

        0xCA => (Instruction::DEX, AddressMode::Implied),

        0x88 => (Instruction::DEY, AddressMode::Implied),

        // EOR
        0x49 => (Instruction::EOR, AddressMode::Immediate),
        0x45 => (Instruction::EOR, AddressMode::ZeroPage),
        0x55 => (Instruction::EOR, AddressMode::ZeroPageX),
        0x4D => (Instruction::EOR, AddressMode::Absolute),
        0x5D => (Instruction::EOR, AddressMode::AbsoluteX),
        0x59 => (Instruction::EOR, AddressMode::AbsoluteY),
        0x41 => (Instruction::EOR, AddressMode::IndirectZeroPageX),
        0x51 => (Instruction::EOR, AddressMode::IndirectZeroPageY),

        // INC
        0xE6 => (Instruction::INC, AddressMode::ZeroPage),
        0xF6 => (Instruction::INC, AddressMode::ZeroPageX),
        0xEE => (Instruction::INC, AddressMode::Absolute),
        0xFE => (Instruction::INC, AddressMode::AbsoluteX),

        0xE8 => (Instruction::INX, AddressMode::Implied),

        0xC8 => (Instruction::INY, AddressMode::Implied),

        // JMP
        0x4C => (Instruction::JMP, AddressMode::Absolute),
        0x6C => (Instruction::JMP, AddressMode::Indirect),

        0x20 => (Instruction::JSR, AddressMode::Absolute),

        // LDA
        0xA9 => (Instruction::LDA, AddressMode::Immediate),
        0xA5 => (Instruction::LDA, AddressMode::ZeroPage),
        0xB5 => (Instruction::LDA, AddressMode::ZeroPageX),
        0xAD => (Instruction::LDA, AddressMode::Absolute),
        0xBD => (Instruction::LDA, AddressMode::AbsoluteX),
        0xB9 => (Instruction::LDA, AddressMode::AbsoluteY),
        0xA1 => (Instruction::LDA, AddressMode::IndirectZeroPageX),
        0xB1 => (Instruction::LDA, AddressMode::IndirectZeroPageY),

        // LDX
        0xA2 => (Instruction::LDX, AddressMode::Immediate),
        0xA6 => (Instruction::LDX, AddressMode::ZeroPage),
        0xB6 => (Instruction::LDX, AddressMode::ZeroPageX),
        0xAE => (Instruction::LDX, AddressMode::Absolute),
        0xBE => (Instruction::LDX, AddressMode::AbsoluteX),

        // LDY
        0xA0 => (Instruction::LDY, AddressMode::Immediate),
        0xA4 => (Instruction::LDY, AddressMode::ZeroPage),
        0xB4 => (Instruction::LDY, AddressMode::ZeroPageY),
        0xAC => (Instruction::LDY, AddressMode::Absolute),
        0xBC => (Instruction::LDY, AddressMode::AbsoluteY),

        // LSR
        0x4A => (Instruction::LSR, AddressMode::Implied),
        0x46 => (Instruction::LSR, AddressMode::ZeroPage),
        0x56 => (Instruction::LSR, AddressMode::ZeroPageX),
        0x4E => (Instruction::LSR, AddressMode::Absolute),
        0x5E => (Instruction::LSR, AddressMode::AbsoluteX),

        0xEA => (Instruction::NOP, AddressMode::Implied),

        // ORA
        0x09 => (Instruction::ORA, AddressMode::Immediate),
        0x05 => (Instruction::ORA, AddressMode::ZeroPage),
        0x15 => (Instruction::ORA, AddressMode::ZeroPageX),
        0x0D => (Instruction::ORA, AddressMode::Absolute),
        0x1D => (Instruction::ORA, AddressMode::AbsoluteX),
        0x19 => (Instruction::ORA, AddressMode::AbsoluteY),
        0x01 => (Instruction::ORA, AddressMode::IndirectZeroPageX),
        0x11 => (Instruction::ORA, AddressMode::IndirectZeroPageY),

        0x48 => (Instruction::PHA, AddressMode::Implied),

        0x08 => (Instruction::PHP, AddressMode::Implied),

        0x68 => (Instruction::PLA, AddressMode::Implied),

        0x28 => (Instruction::PLP, AddressMode::Implied),

        // ROL
        0x2A => (Instruction::ROL, AddressMode::Implied),
        0x26 => (Instruction::ROL, AddressMode::ZeroPage),
        0x36 => (Instruction::ROL, AddressMode::ZeroPageX),
        0x2E => (Instruction::ROL, AddressMode::Absolute),
        0x3E => (Instruction::ROL, AddressMode::AbsoluteX),

        // ROR
        0x6A => (Instruction::ROR, AddressMode::Implied),
        0x66 => (Instruction::ROR, AddressMode::ZeroPage),
        0x76 => (Instruction::ROR, AddressMode::ZeroPageX),
        0x6E => (Instruction::ROR, AddressMode::Absolute),
        0x7E => (Instruction::ROR, AddressMode::AbsoluteX),

        0x40 => (Instruction::RTI, AddressMode::Implied),

        0x60 => (Instruction::RTS, AddressMode::Implied),

        // SBC
        0xE9 => (Instruction::SBC, AddressMode::Immediate),
        0xE5 => (Instruction::SBC, AddressMode::ZeroPage),
        0xF5 => (Instruction::SBC, AddressMode::ZeroPageX),
        0xED => (Instruction::SBC, AddressMode::Absolute),
        0xFD => (Instruction::SBC, AddressMode::AbsoluteX),
        0xF9 => (Instruction::SBC, AddressMode::AbsoluteY),
        0xE1 => (Instruction::SBC, AddressMode::IndirectZeroPageX),
        0xF1 => (Instruction::SBC, AddressMode::IndirectZeroPageY),

        0x38 => (Instruction::SEC, AddressMode::Implied),

        0xF8 => (Instruction::SED, AddressMode::Implied),

        0x78 => (Instruction::SEI, AddressMode::Implied),

        // STA
        0x85 => (Instruction::STA, AddressMode::ZeroPage),
        0x95 => (Instruction::STA, AddressMode::ZeroPageX),
        0x8D => (Instruction::STA, AddressMode::Absolute),
        0x9D => (Instruction::STA, AddressMode::AbsoluteX),
        0x99 => (Instruction::STA, AddressMode::AbsoluteY),
        0x81 => (Instruction::STA, AddressMode::IndirectZeroPageX),
        0x91 => (Instruction::STA, AddressMode::IndirectZeroPageY),

        // STX
        0x86 => (Instruction::STX, AddressMode::ZeroPage),
        0x96 => (Instruction::STX, AddressMode::ZeroPageY),
        0x8E => (Instruction::STX, AddressMode::Absolute),

        // STY
        0x84 => (Instruction::STY, AddressMode::ZeroPage),
        0x94 => (Instruction::STY, AddressMode::ZeroPageX),
        0x8C => (Instruction::STY, AddressMode::Absolute),

        0xAA => (Instruction::TAX, AddressMode::Implied),

        0xA8 => (Instruction::TAY, AddressMode::Implied),

        0xBA => (Instruction::TSX, AddressMode::Implied),

        0x8A => (Instruction::TXA, AddressMode::Implied),

        0x9A => (Instruction::TXS, AddressMode::Implied),

        0x98 => (Instruction::TYA, AddressMode::Implied),

        _ => (Instruction::Unknown, AddressMode::Unknown),
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub(super) enum AddressMode {
    /// For Unrecognized instructions
    Unknown,
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

#[derive(Clone, Debug)]
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
}
