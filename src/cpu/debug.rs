use super::op_code::{self, AddressMode};

pub(super) fn format_instr(
    bus: &mut super::NesBus,
    offset: u16,
    reg_x: u8,
    reg_y: u8,
) -> (String, String) {
    let op_code = bus.read(offset);
    let parsed_op = op_code::parse(op_code);
    let mnemon = get_mnemonic(op_code).unwrap_or(" ???");
    let (operand_hex, operand_value) = match parsed_op.mode() {
        AddressMode::Immediate => {
            let val = bus.read(offset + 1);
            (format!("{:02X}", val), format!("#${:02X?}", val))
        }
        AddressMode::Absolute => {
            let first = bus.read(offset + 1);
            let second = bus.read(offset + 2);
            (
                format!("{:02X} {:02X}", first, second),
                format!("${:02X?}{:02X?}", second, first),
            )
        }
        AddressMode::AbsoluteX => {
            let first = bus.read(offset + 1);
            let second = bus.read(offset + 2);
            (
                format!("{:02X} {:02X}", first, second),
                format!("${:02X}{:02X},X", second, first),
            )
        }
        AddressMode::AbsoluteY => {
            let first = bus.read(offset + 1);
            let second = bus.read(offset + 2);
            (
                format!("{:02X} {:02X}", first, second),
                format!("${:02X}{:02X},Y", second, first),
            )
        }
        AddressMode::ZeroPage => {
            let val = bus.read(offset + 1);
            (format!("{:02X}", val), format!("${:02X?}", val))
        }
        AddressMode::ZeroPageX => {
            let val = bus.read(offset + 1);
            (format!("{:02X}", val), format!("${:02X?},X", val))
        }
        AddressMode::ZeroPageY => {
            let val = bus.read(offset + 1);
            (format!("{:02X}", val), format!("${:02X?},Y", val))
        }
        AddressMode::Relative => {
            let val = bus.read(offset + 1);
            if val <= 127 {
                (
                    format!("{:02X}", val),
                    format!("${:02X}", offset + 2 + val as u16),
                )
            } else {
                (
                    format!("{:02X}", val),
                    format!("${:02X}", offset + 2 - ((val ^ 0xFF) + 1) as u16),
                )
            }
        }
        AddressMode::Indirect => {
            let first = bus.read(offset + 1);
            let second = bus.read(offset + 2);
            (
                format!("{:02X} {:02X}", first, second),
                format!("(${:02X?}{:02X?})", second, first),
            )
        }
        AddressMode::IndirectZeroPageX => {
            let first = bus.read(offset + 1);
            (format!("{:02X}", first), format!("(${:02X?},X)", first))
        }
        AddressMode::IndirectZeroPageY => {
            let first = bus.read(offset + 1);
            (format!("{:02X}", first), format!("(${:02X?}),Y", first))
        }
        AddressMode::Accumulator => ("".to_owned(), "A".to_owned()),
        AddressMode::Implied => ("".to_owned(), "".to_owned()),
        AddressMode::Unknown => ("".to_owned(), "".to_owned()),
    };

    let resolved = if op_code == 0x20 || op_code == 0x4C {
        // JMP and JSR are not resolved as the operand is just the address jumped to
        None
    } else {
        fmt_resolved_operand(parsed_op.mode(), bus, offset + 1, reg_x, reg_y)
    };

    let instruction_hex = format!("{:02X} {:05}", op_code, operand_hex);
    let readable_format = format!("{} {}", mnemon, operand_value);
    if let Some(resolved) = resolved {
        let new_val = format!("{}{}", readable_format, resolved);
        (instruction_hex, new_val)
    } else {
        (instruction_hex, readable_format)
    }
}

/// For the given opcode, returns the three letter 6502 Assembly mnemonic, or `None` if unknown.
///
/// This method is mainly useful for logging, debugging and disassembly purposes.
fn get_mnemonic(instr: u8) -> Option<&'static str> {
    match instr {
        0x69 | 0x65 | 0x75 | 0x6D | 0x7D | 0x79 | 0x61 | 0x71 => Some(" ADC"),
        0x29 | 0x25 | 0x35 | 0x2D | 0x3D | 0x39 | 0x21 | 0x31 => Some(" AND"),
        0x0A | 0x06 | 0x16 | 0x0E | 0x1E => Some(" ASL"),
        0x90 => Some(" BCC"),
        0xB0 => Some(" BCS"),
        0xF0 => Some(" BEQ"),
        0x24 | 0x2C => Some(" BIT"),
        0x30 => Some(" BMI"),
        0xD0 => Some(" BNE"),
        0x10 => Some(" BPL"),
        0x00 => Some(" BRK"),
        0x50 => Some(" BVC"),
        0x70 => Some(" BVS"),
        0x18 => Some(" CLC"),
        0xD8 => Some(" CLD"),
        0x58 => Some(" CLI"),
        0xB8 => Some(" CLV"),
        0xC9 | 0xC5 | 0xD5 | 0xCD | 0xDD | 0xD9 | 0xC1 | 0xD1 => Some(" CMP"),
        0xE0 | 0xE4 | 0xEC => Some(" CPX"),
        0xC0 | 0xC4 | 0xCC => Some(" CPY"),
        0xC6 | 0xD6 | 0xCE | 0xDE => Some(" DEC"),
        0xCA => Some(" DEX"),
        0x88 => Some(" DEY"),
        0x49 | 0x45 | 0x55 | 0x4D | 0x5D | 0x59 | 0x41 | 0x51 => Some(" EOR"),
        0xE6 | 0xF6 | 0xEE | 0xFE => Some(" INC"),
        0xE8 => Some(" INX"),
        0xC8 => Some(" INY"),
        0x4C | 0x6C => Some(" JMP"),
        0x20 => Some(" JSR"),
        0xA9 | 0xA5 | 0xB5 | 0xAD | 0xBD | 0xB9 | 0xA1 | 0xB1 => Some(" LDA"),
        0xA2 | 0xA6 | 0xB6 | 0xAE | 0xBE => Some(" LDX"),
        0xA0 | 0xA4 | 0xB4 | 0xAC | 0xBC => Some(" LDY"),
        0x4A | 0x46 | 0x56 | 0x4E | 0x5E => Some(" LSR"),
        0xEA => Some(" NOP"),
        0x09 | 0x05 | 0x15 | 0x0D | 0x1D | 0x19 | 0x01 | 0x11 => Some(" ORA"),
        0x48 => Some(" PHA"),
        0x08 => Some(" PHP"),
        0x68 => Some(" PLA"),
        0x28 => Some(" PLP"),
        0x2A | 0x26 | 0x36 | 0x2E | 0x3E => Some(" ROL"),
        0x6A | 0x66 | 0x76 | 0x6E | 0x7E => Some(" ROR"),
        0x40 => Some(" RTI"),
        0x60 => Some(" RTS"),
        0xE9 | 0xE5 | 0xF5 | 0xED | 0xFD | 0xF9 | 0xE1 | 0xF1 => Some(" SBC"),
        0x38 => Some(" SEC"),
        0xF8 => Some(" SED"),
        0x78 => Some(" SEI"),
        0x85 | 0x95 | 0x8D | 0x9D | 0x99 | 0x81 | 0x91 => Some(" STA"),
        0x86 | 0x96 | 0x8E => Some(" STX"),
        0x84 | 0x94 | 0x8C => Some(" STY"),
        0xAA => Some(" TAX"),
        0xA8 => Some(" TAY"),
        0xBA => Some(" TSX"),
        0x8A => Some(" TXA"),
        0x9A => Some(" TXS"),
        0x98 => Some(" TYA"),

        0x80 | 0x82 | 0x89 | 0xC2 | 0xE2 | 0x04 | 0x44 | 0x64 | 0x14 | 0x34 | 0x54 | 0x74
        | 0xD4 | 0xF4 | 0x0C | 0x1C | 0x3C | 0x5C | 0x7C | 0xDC | 0xFC | 0x1A | 0x3A | 0x5A
        | 0x7A | 0xDA | 0xFA => Some("*NOP"),
        0xA7 | 0xB7 | 0xAF | 0xBF | 0xA3 | 0xB3 => Some("*LAX"),
        0x87 | 0x97 | 0x8f | 0x83 => Some("*SAX"),
        0xEB => Some("*SBC"),
        0xC7 |
        0xD7 |
        0xCF |
        0xDF |
        0xDB |
        0xC3 |
        0xD3 => Some("*DCP"),

        _ => None,
    }
}

fn fmt_resolved_operand(
    mode: AddressMode,
    bus: &mut super::NesBus,
    offset: u16,
    reg_x: u8,
    reg_y: u8,
) -> Option<String> {
    match mode {
        AddressMode::Immediate => None,
        AddressMode::Unknown => None,
        // AddressMode::Absolute => None,
        AddressMode::Accumulator => None,
        AddressMode::Implied => None,
        AddressMode::Relative => None,
        AddressMode::ZeroPage => {
            let val = bus.read(offset);
            Some(format!(" = {:02X}", bus.read(val as u16)))
        }
        AddressMode::AbsoluteX => {
            let val = read_u16_at(bus, offset);
            let addr = val.wrapping_add(reg_x as u16);
            let res = bus.read(addr);
            Some(format!(" @ {:04X} = {:02X}", addr, res))
        }
        AddressMode::AbsoluteY => {
            let val = read_u16_at(bus, offset);
            let addr = val.wrapping_add(reg_y as u16);
            let res = bus.read(addr);
            Some(format!(" @ {:04X} = {:02X}", addr, res))
        }
        AddressMode::ZeroPageX => {
            let val = bus.read(offset);
            let addr = val.wrapping_add(reg_x) as u16;
            let res = bus.read(addr);
            Some(format!(" @ {:02X} = {:02X}", addr, res))
        }
        AddressMode::ZeroPageY => {
            let val = bus.read(offset);
            let addr = val.wrapping_add(reg_y) as u16;
            let res = bus.read(addr);
            Some(format!(" @ {:02X} = {:02X}", addr, res))
        }
        AddressMode::IndirectZeroPageX => {
            let val = bus.read(offset);
            let base_addr = val.wrapping_add(reg_x);
            let mem = u16::from(bus.read(base_addr as u16))
                | u16::from(bus.read(base_addr.wrapping_add(1) as u16)) << 8;
            Some(format!(
                " @ {:02X} = {:04X} = {:02X}",
                base_addr,
                mem,
                bus.read(mem)
            ))
        }
        AddressMode::IndirectZeroPageY => {
            let val = bus.read(offset);
            let base_addr = u16::from(bus.read(val as u16))
                | u16::from(bus.read(val.wrapping_add(1) as u16)) << 8;
            let addr = base_addr.wrapping_add(reg_y as u16);
            let mem = bus.read(addr) as u16;
            Some(format!(" = {:04X} @ {:04X} = {:02X}", base_addr, addr, mem))
        }
        AddressMode::Absolute => {
            let val = read_u16_at(bus, offset);
            Some(format!(" = {:02X}", bus.read(val)))
        }
        // AddressMode::Relative => {
        //     let val = bus.read(offset);
        //     if val <= 127 {
        //         Some(format!(" = {:02X}", offset + 1 + val as u16))
        //     } else {
        //         Some(format!(" = {:02X}", offset + 1 - ((val ^ 0xFF) + 1) as u16))
        //     }
        // }
        AddressMode::Indirect => {
            let val = read_u16_at(bus, offset);

            // Funnily enough, the 6502 had a well-known bug which lead to wrapping around a
            // memory page when using Indirect addressing instead of crossing page boundaries.
            //
            // It's safer to replicate that bug here.
            if val & 0x00FF == 0x00FF {
                let lo = bus.read(val);
                let hi = bus.read(val & 0xFF00);
                Some(format!(" = {:04X}", (hi as u16) << 8 | (lo as u16)))
            } else {
                let addr = read_u16_at(bus, val);
                Some(format!(" = {:04X}", addr))
            }
        }
    }
}

fn read_u16_at(bus: &mut super::NesBus, addr: u16) -> u16 {
    if addr == 0xFFFF {
        // TODO check what should be done
        u16::from(bus.read(0xFFFF)) << 8
    } else {
        u16::from(bus.read(addr)) | u16::from(bus.read(addr + 1)) << 8
    }
}

// enum OperandSyntaxError {
//     Overflow,
// }

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

// fn format_instr(
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
