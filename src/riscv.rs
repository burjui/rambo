#![allow(clippy::unreadable_literal)]

use std::error::Error;
use std::fmt;
use std::ops::Range;

pub(crate) fn lb(rd: u8, rs1: u8, offset: u16) -> Result<u32, OutOfRange> {
    i_instruction(0b0000011, rd, 0b000, rs1, offset)
}

pub(crate) fn add(rd: u8, rs1: u8, rs2: u8) -> Result<u32, OutOfRange> {
    r_instruction(0b0110011, rd, 0b000, rs1, rs2, 0x000_0000)
}

pub(crate) fn ebreak() -> Result<u32, OutOfRange> {
    i_instruction(0b1110011, 0b00000, 0b000, 0b00000, 0b0000_0000_0001)
}

fn r_instruction(opcode: u8, rd: u8, funct3: u8, rs1: u8, rs2: u8, funct7: u8) -> Result<u32, OutOfRange> {
    check_opcode(opcode)?;
    check_register(rd)?;
    check_funct3(funct3)?;
    check_register(rs1)?;
    check_register(rs2)?;
    check_funct7(funct7)?;
    Ok((u32::from(opcode)) |
        (u32::from(rd) << 7) |
        (u32::from(funct3) << 12) |
        (u32::from(rs1) << 15) |
        (u32::from(rs2) << 20) |
        (u32::from(funct7) << 25))
}

fn i_instruction(opcode: u8, rd: u8, funct3: u8, rs1: u8, imm: u16) -> Result<u32, OutOfRange> {
    check_opcode(opcode)?;
    check_register(rd)?;
    check_funct3(funct3)?;
    check_range(imm, 0 .. 1 << 12, "imm")?;
    Ok((u32::from(opcode)) |
        (u32::from(rd) << 7) |
        (u32::from(funct3) << 12) |
        (u32::from(rs1) << 15) |
        (u32::from(imm) << 20))
}

fn check_opcode(opcode: u8) -> Result<(), OutOfRange> {
    check_range(opcode, 0b11 .. 1 << 7, "opcode")
}

fn check_register(register: u8) -> Result<(), OutOfRange> {
    check_range(register, 0 .. 31, "register")
}

fn check_funct3(funct3: u8) -> Result<(), OutOfRange> {
    check_range(funct3, 0 .. 1 << 3, "funct3")
}

fn check_funct7(funct7: u8) -> Result<(), OutOfRange> {
    check_range(funct7, 0 .. 1 << 7, "funct7")
}

fn check_range<T: PartialOrd>(value: T, range: Range<T>, name: &'static str) -> Result<(), OutOfRange>
    where u32: std::convert::From<T>
{
    if value >= range.start && value < range.end {
        Ok(())
    } else {
        Err(OutOfRange::new(value, range, name))
    }
}


#[derive(Debug)]
pub(crate) struct OutOfRange {
    value: u32,
    range: Range<u32>,
    name: &'static str
}

impl OutOfRange {
    fn new<T>(value: T, range: Range<T>, name: &'static str) -> Self
        where u32: std::convert::From<T>
    {
        Self {
            value: u32::from(value),
            range: u32::from(range.start) .. u32::from(range.end),
            name,
        }
    }
}

impl Error for OutOfRange {}

impl fmt::Display for OutOfRange {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} ({}) is out of range: {} .. {}", self.name, self.value, self.range.start, self.range.end)
    }
}
