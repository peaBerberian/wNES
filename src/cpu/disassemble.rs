use super::op_code::AddressMode;

pub(super) enum OperandSyntaxError {
    Overflow,
}

pub(super) fn get_operand_syntax(
    mode: AddressMode,
    bus: &super::NesBus,
    offset: u16,
) -> String {
    match mode {
        AddressMode::Immediate => format!("#${:X?}", bus.read(offset)),
        AddressMode::Absolute => format!("${:X?}{:X?}", bus.read(offset), bus.read(offset + 1)),
        AddressMode::AbsoluteX => format!("${:X?}{:X?},X", bus.read(offset), bus.read(offset + 1)),
        AddressMode::AbsoluteY => format!("${:X?}{:X?},Y", bus.read(offset), bus.read(offset + 1)),
        AddressMode::ZeroPage => format!("${:X?}", bus.read(offset)),
        AddressMode::ZeroPageX => format!("${:X?},X", bus.read(offset)),
        AddressMode::ZeroPageY => format!("${:X?},Y", bus.read(offset)),
        AddressMode::Relative => if bus.read(offset) <= 127 {
            format!("*+{}", bus.read(offset))
        } else {
            format!("*-{}", (bus.read(offset) ^ 0xFF) + 1)
        },
        AddressMode::Indirect => format!("(${:X?}{:X?})", bus.read(offset), bus.read(offset + 1)),
        AddressMode::IndirectZeroPageX => format!("(${:X?}{:X?},X)", bus.read(offset), bus.read(offset + 1)),
        AddressMode::IndirectZeroPageY => format!("(${:X?}{:X?}),Y", bus.read(offset), bus.read(offset + 1)),
        AddressMode::Accumulator => "A".to_owned(),
        AddressMode::Implied => "".to_owned(),
        AddressMode::Unknown => "".to_owned(),
    }
}
// pub(super) fn get_operand_syntax(
//     mode: AddressMode,
//     prg: &super::NesBus,
//     offset: u16,
// ) -> Result<String, OperandSyntaxError> {
//     let offset_usize = offset as usize;
//     match mode {
//         AddressMode::Immediate => match prg.get(offset_usize) {
//             Some(val) => Ok(format!("#${:X?}", val)),
//             _ => Err(OperandSyntaxError::Overflow),
//         },
//         AddressMode::Absolute => match (prg.get(offset_usize), prg.get(offset_usize + 1)) {
//             (Some(first), Some(scnd)) => Ok(format!("${:X?}{:X?}", first, scnd)),
//             _ => Err(OperandSyntaxError::Overflow),
//         },
//         AddressMode::AbsoluteX => match (prg.get(offset_usize), prg.get(offset_usize + 1)) {
//             (Some(first), Some(scnd)) => Ok(format!("${:X?}{:X?},X", first, scnd)),
//             _ => Err(OperandSyntaxError::Overflow),
//         },
//         AddressMode::AbsoluteY => match (prg.get(offset_usize), prg.get(offset_usize + 1)) {
//             (Some(first), Some(scnd)) => Ok(format!("${:X?}{:X?},Y", first, scnd)),
//             _ => Err(OperandSyntaxError::Overflow),
//         },
//         AddressMode::ZeroPage => match prg.get(offset_usize) {
//             Some(val) => Ok(format!("${:X?}", val)),
//             _ => Err(OperandSyntaxError::Overflow),
//         },
//         AddressMode::ZeroPageX => match prg.get(offset_usize) {
//             Some(val) => Ok(format!("${:X?},X", val)),
//             _ => Err(OperandSyntaxError::Overflow),
//         },
//         AddressMode::ZeroPageY => match prg.get(offset_usize) {
//             Some(val) => Ok(format!("${:X?},Y", val)),
//             _ => Err(OperandSyntaxError::Overflow),
//         },
//         AddressMode::Relative => match prg.get(offset_usize) {
//             Some(val) if val <= &127 => Ok(format!("*+{}", val)),
//             Some(val) => Ok(format!("*-{}", (val ^ 0xFF) + 1)),
//             _ => Err(OperandSyntaxError::Overflow),
//         },
//         AddressMode::Indirect => match (prg.get(offset_usize), prg.get(offset_usize + 1)) {
//             (Some(first), Some(scnd)) => Ok(format!("(${:X?}{:X?})", first, scnd)),
//             _ => Err(OperandSyntaxError::Overflow),
//         },
//         AddressMode::IndirectZeroPageX => {
//             match (prg.get(offset_usize), prg.get(offset_usize + 1)) {
//                 (Some(first), Some(scnd)) => Ok(format!("(${:X?}{:X?},X)", first, scnd)),
//                 _ => Err(OperandSyntaxError::Overflow),
//             }
//         }
//         AddressMode::IndirectZeroPageY => {
//             match (prg.get(offset_usize), prg.get(offset_usize + 1)) {
//                 (Some(first), Some(scnd)) => Ok(format!("(${:X?}{:X?}),Y", first, scnd)),
//                 _ => Err(OperandSyntaxError::Overflow),
//             }
//         }
//         AddressMode::Accumulator => Ok("A".to_owned()),
//         AddressMode::Implied => Ok("".to_owned()),
//         AddressMode::Unknown => Ok("".to_owned()),
//     }
// }

/// For the given opcode, returns the three letter 6502 Assembly mnemonic, or `None` if unknown.
///
/// This method is mainly useful for logging, debugging and disassembly purposes.
pub(super) fn get_mnemonic(instr: u8) -> Option<&'static str> {
    match instr {
        0x69 | 0x65 | 0x75 | 0x6D | 0x7D | 0x79 | 0x61 | 0x71 => Some("ADC"),
        0x29 | 0x25 | 0x35 | 0x2D | 0x3D | 0x39 | 0x21 | 0x31 => Some("AND"),
        0x0A | 0x06 | 0x16 | 0x0E | 0x1E => Some("ASL"),
        0x90 => Some("BCC"),
        0xB0 => Some("BCS"),
        0xF0 => Some("BEQ"),
        0x24 | 0x2C => Some("BIT"),
        0x30 => Some("BMI"),
        0xD0 => Some("BNE"),
        0x10 => Some("BPL"),
        0x00 => Some("BRK"),
        0x50 => Some("BVC"),
        0x70 => Some("BVS"),
        0x18 => Some("CLC"),
        0xD8 => Some("CLD"),
        0x58 => Some("CLI"),
        0xB8 => Some("CLV"),
        0xC9 | 0xC5 | 0xD5 | 0xCD | 0xDD | 0xD9 | 0xC1 | 0xD1 => Some("CMP"),
        0xE0 | 0xE4 | 0xEC => Some("CPX"),
        0xC0 | 0xC4 | 0xCC => Some("CPY"),
        0xC6 | 0xD6 | 0xCE | 0xDE => Some("DEC"),
        0xCA => Some("DEX"),
        0x88 => Some("DEY"),
        0x49 | 0x45 | 0x55 | 0x4D | 0x5D | 0x59 | 0x41 | 0x51 => Some("EOR"),
        0xE6 | 0xF6 | 0xEE | 0xFE => Some("INC"),
        0xE8 => Some("INX"),
        0xC8 => Some("INY"),
        0x4C | 0x6C => Some("JMP"),
        0x20 => Some("JSR"),
        0xA9 | 0xA5 | 0xB5 | 0xAD | 0xBD | 0xB9 | 0xA1 | 0xB1 => Some("LDA"),
        0xA2 | 0xA6 | 0xB6 | 0xAE | 0xBE => Some("LDX"),
        0xA0 | 0xA4 | 0xB4 | 0xAC | 0xBC => Some("LDY"),
        0x4A | 0x46 | 0x56 | 0x4E | 0x5E => Some("LSR"),
        0xEA => Some("NOP"),
        0x09 | 0x05 | 0x15 | 0x0D | 0x1D | 0x19 | 0x01 | 0x11 => Some("ORA"),
        0x48 => Some("PHA"),
        0x08 => Some("PHP"),
        0x68 => Some("PLA"),
        0x28 => Some("PLP"),
        0x2A | 0x26 | 0x36 | 0x2E | 0x3E => Some("ROL"),
        0x6A | 0x66 | 0x76 | 0x6E | 0x7E => Some("ROR"),
        0x40 => Some("RTI"),
        0x60 => Some("RTS"),
        0xE9 | 0xE5 | 0xF5 | 0xED | 0xFD | 0xF9 | 0xE1 | 0xF1 => Some("SBC"),
        0x38 => Some("SEC"),
        0xF8 => Some("SED"),
        0x78 => Some("SEI"),
        0x85 | 0x95 | 0x8D | 0x9D | 0x99 | 0x81 | 0x91 => Some("STA"),
        0x86 | 0x96 | 0x8E => Some("STX"),
        0x84 | 0x94 | 0x8C => Some("STY"),
        0xAA => Some("TAX"),
        0xA8 => Some("TAY"),
        0xBA => Some("TSX"),
        0x8A => Some("TXA"),
        0x9A => Some("TXS"),
        0x98 => Some("TYA"),
        _ => None,
    }
}

// fn byte_to_ascii_hex(
//     byt: u8
// ) -> [u8; 2] {
//     let divided = byt / 16;
//     let modulo = byt % 16;
//     let first_digit = if divided > 9 {
//         divided + b'A'
//     } else {
//         divided + b'0'
//     };
//     let second_digit = if modulo > 9 {
//         modulo + b'A'
//     } else {
//         modulo + b'0'
//     };
//     [first_digit, second_digit]
// }
