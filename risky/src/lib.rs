#![allow(clippy::unreadable_literal)]

use std::collections::HashSet;
use std::error::Error;
use std::fmt;
use std::ops::BitAnd;
use std::ops::Range;

pub mod instructions {
    use crate::csr_instruction;
    use crate::fence_instruction;
    use crate::i_instruction;
    use crate::u_instruction;
    use crate::s_instruction;
    use crate::b_instruction;
    use crate::j_instruction;
    use crate::r_instruction;
    use crate::opcode;
    use crate::parse_fence_mask;
    use crate::shift_instruction;
    use crate::Result;

    macro_rules! instruction {
        (R: $name: ident, $opcode: path, funct3 $funct3: literal, funct7 $funct7: literal) => {
            pub fn $name(rd: u8, rs1: u8, rs2: u8) -> Result<u32> {
                r_instruction($opcode, rd, $funct3, rs1, rs2, $funct7)
            }
        };

        (I: $name: ident, $opcode: path, funct3 $funct3: literal) => {
            pub fn $name(rd: u8, rs1: u8, imm: u16) -> Result<u32> {
                i_instruction($opcode, rd, $funct3, rs1, imm)
            }
        };

        (S: $name: ident, $opcode: path, funct3 $funct3: literal) => {
            pub fn $name(imm: u16, rs1: u8, rs2: u8) -> Result<u32> {
                s_instruction($opcode, imm, $funct3, rs1, rs2)
            }
        };

        (B: $name: ident, $opcode: path, funct3 $funct3: literal) => {
            pub fn $name(imm: u16, rs1: u8, rs2: u8) -> Result<u32> {
                b_instruction($opcode, imm, $funct3, rs1, rs2)
            }
        };

        (U: $name: ident, $opcode: path) => {
            pub fn $name(rd: u8, imm: u32) -> Result<u32> {
                u_instruction($opcode, rd, imm)
            }
        };

        (J: $name: ident, $opcode: path) => {
            pub fn $name(rd: u8, imm: u32) -> Result<u32> {
                j_instruction($opcode, rd, imm)
            }
        };
    }

    // RV32I Base Instruction Set

    instruction!(U: lui, opcode::LUI);
    instruction!(U: auipc, opcode::AUIPC);
    instruction!(J: jal, opcode::JAL);
    instruction!(I: jalr, opcode::JALR, funct3 0b000);
    instruction!(B: beq, opcode::BRANCH, funct3 0b000);
    instruction!(B: bne, opcode::BRANCH, funct3 0b001);
    instruction!(B: blt, opcode::BRANCH, funct3 0b100);
    instruction!(B: bge, opcode::BRANCH, funct3 0b101);
    instruction!(B: bltu, opcode::BRANCH, funct3 0b110);
    instruction!(B: bgeu, opcode::BRANCH, funct3 0b111);
    instruction!(I: lb, opcode::LOAD, funct3 0b000);
    instruction!(I: lh, opcode::LOAD, funct3 0b001);
    instruction!(I: lw, opcode::LOAD, funct3 0b010);
    instruction!(I: lbu, opcode::LOAD, funct3 0b100);
    instruction!(I: lhu, opcode::LOAD, funct3 0b101);
    instruction!(S: sb, opcode::STORE, funct3 0b000);
    instruction!(S: sh, opcode::STORE, funct3 0b001);
    instruction!(S: sw, opcode::STORE, funct3 0b010);
    instruction!(I: addi, opcode::OP_IMM, funct3 0b000);
    instruction!(I: slti, opcode::OP_IMM, funct3 0b010);
    instruction!(I: sltiu, opcode::OP_IMM, funct3 0b011);
    instruction!(I: xori, opcode::OP_IMM, funct3 0b100);
    instruction!(I: ori, opcode::OP_IMM, funct3 0b110);
    instruction!(I: andi, opcode::OP_IMM, funct3 0b111);
    instruction!(R: add, opcode::OP, funct3 0b000, funct7 0b0000000);
    instruction!(R: sub, opcode::OP, funct3 0b000, funct7 0b0100000);
    instruction!(R: sll, opcode::OP, funct3 0b001, funct7 0b0000000);
    instruction!(R: slt, opcode::OP, funct3 0b010, funct7 0b0000000);
    instruction!(R: sltu, opcode::OP, funct3 0b011, funct7 0b0000000);
    instruction!(R: xor, opcode::OP, funct3 0b100, funct7 0b0000000);
    instruction!(R: srl, opcode::OP, funct3 0b101, funct7 0b0000000);
    instruction!(R: sra, opcode::OP, funct3 0b101, funct7 0b0100000);
    instruction!(R: or, opcode::OP, funct3 0b110, funct7 0b0000000);
    instruction!(R: and, opcode::OP, funct3 0b111, funct7 0b0000000);
    instruction!(I: csrrw, opcode::SYSTEM, funct3 0b001);
    instruction!(I: csrrs, opcode::SYSTEM, funct3 0b010);
    instruction!(I: csrrc, opcode::SYSTEM, funct3 0b011);

    pub fn slli(rd: u8, rs1: u8, shamt: u8) -> Result<u32> {
        shift_instruction(opcode::OP_IMM, rd, 0b001, rs1, shamt, 0b0000000)
    }

    pub fn srli(rd: u8, rs1: u8, shamt: u8) -> Result<u32> {
        shift_instruction(opcode::OP_IMM, rd, 0b101, rs1, shamt, 0b0000000)
    }

    pub fn srai(rd: u8, rs1: u8, shamt: u8) -> Result<u32> {
        shift_instruction(opcode::OP_IMM, rd, 0b101, rs1, shamt, 0b0100000)
    }

    pub fn fence(pred: &'static str, succ: &'static str) -> Result<u32> {
        let pred = parse_fence_mask(pred)?;
        let succ = parse_fence_mask(succ)?;
        fence_instruction(0b000, pred, succ)
    }

    pub fn fence_i() -> Result<u32> {
        fence_instruction(0b001, 0b0000, 0b0000)
    }

    pub fn ecall() -> Result<u32> {
        i_instruction(opcode::SYSTEM, 0b00000, 0b000, 0b00000, 0b0000_0000_0000)
    }

    pub fn ebreak() -> Result<u32> {
        i_instruction(opcode::SYSTEM, 0b00000, 0b000, 0b00000, 0b0000_0000_0001)
    }

    pub fn csrrwi(rd: u8, rs1: u8, csr: u16) -> Result<u32> {
        csr_instruction(rd, 0b101, rs1, csr)
    }

    pub fn csrrsi(rd: u8, rs1: u8, csr: u16) -> Result<u32> {
        csr_instruction(rd, 0b110, rs1, csr)
    }

    pub fn csrrci(rd: u8, rs1: u8, csr: u16) -> Result<u32> {
        csr_instruction(rd, 0b111, rs1, csr)
    }

    // RV32M Standard Extension

    instruction!(R: mul, opcode::OP, funct3 0b000, funct7 0b0000001);
    instruction!(R: mulh, opcode::OP, funct3 0b001, funct7 0b0000001);
    instruction!(R: mulhsu, opcode::OP, funct3 0b010, funct7 0b0000001);
    instruction!(R: mulhu, opcode::OP, funct3 0b011, funct7 0b0000001);
    instruction!(R: div, opcode::OP, funct3 0b100, funct7 0b0000001);
    instruction!(R: divu, opcode::OP, funct3 0b101, funct7 0b0000001);
    instruction!(R: rem, opcode::OP, funct3 0b110, funct7 0b0000001);
    instruction!(R: remu, opcode::OP, funct3 0b111, funct7 0b0000001);
}

// Implementation

mod opcode {
    macro_rules! opcodes {
        ($($name: ident $bits6to2: literal,)+) => ($(pub(crate) const $name: u8 = $bits6to2 << 2 | 0b11;)+)
    }

    opcodes!(
        LOAD       0b00000,
        LOAD_FP    0b00001,
        MISC_MEM   0b00011,
        OP_IMM     0b00100,
        AUIPC      0b00101,
        OP_IMM_32  0b00110,

        STORE      0b01000,
        STORE_FP   0b01001,
        AMO        0b01011,
        OP         0b01100,
        LUI        0b01101,
        OP_32      0b01110,

        MADD       0b10000,
        MSUB       0b10001,
        NMSUB      0b10010,
        NMADD      0b10011,
        OP_FP      0b10100,

        BRANCH     0b11000,
        JALR       0b11001,
        JAL        0b11011,
        SYSTEM     0b11100,
    );
}

pub type Result<T> = std::result::Result<T, RISCVError>;

#[derive(Debug)]
pub enum RISCVError {
    OutOfRange {
        value: u32,
        range: Range<u32>,
        name: &'static str,
    },
    NotMultipleOfTwo {
        value: u32,
        name: &'static str,
    },
    FenceFlag {
        value: &'static str,
        name: char,
        kind: &'static str,
    },
}

impl fmt::Display for RISCVError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            RISCVError::OutOfRange { value, range, name } =>
                write!(f, "{} = {} (0x{:08x}) is out of range: {} .. {} (0x{:08x} .. (0x{:08x}))",
                       name, value, value, range.start, range.end, range.start, range.end),
            RISCVError::NotMultipleOfTwo { value, name } =>
                write!(f, "{} = {} (0x{:08x}) is not a multiple of 2", name, value, value),
            RISCVError::FenceFlag { value, name, kind } =>
                write!(f, "{} fence flag `{}' in fence mask `{}'", kind, name, value),
        }
    }
}

impl Error for RISCVError {}

fn shift_instruction(opcode: u8, rd: u8, funct3: u8, rs1: u8, shamt: u8, funct7: u8) -> Result<u32> {
    check_range(shamt, 0 .. 1 << 5, "shamt")?;
    check_funct7(funct7)?;
    let imm = u16::from(shamt) | u16::from(funct7) << 5;
    i_instruction(opcode, rd, funct3, rs1, imm)
}

fn fence_instruction(funct3: u8, pred: u8, succ: u8) -> Result<u32> {
    check_funct3(funct3)?;
    check_range(pred, 0 .. 1 << 4, "pred")?;
    check_range(succ, 0 .. 1 << 4, "pred")?;
    let imm = u16::from(succ) | u16::from(pred) << 4;
    i_instruction(opcode::MISC_MEM, 0x00000, 0b00, 0x00000, imm)
}

fn parse_fence_mask(s: &'static str) -> Result<u8> {
    let mut chars_processed = HashSet::new();
    let mut mask = 0;
    for c in s.chars() {
        if chars_processed.contains(&c) {
            return Err(RISCVError::FenceFlag {
                value: s,
                name: c,
                kind: "duplicate",
            });
        }
        chars_processed.insert(c);
        mask |= match c {
            'i' => 1 << 3,
            'o' => 1 << 2,
            'r' => 1 << 1,
            'w' => 1,
            _ => return Err(RISCVError::FenceFlag {
                value: s,
                name: c,
                kind: "invalid",
            }),
        };
    }
    Ok(mask)
}

fn csr_instruction(rd: u8, funct3: u8, rs1: u8, csr: u16) -> Result<u32> {
    i_instruction(opcode::SYSTEM, rd, funct3, rs1, csr)
}

fn r_instruction(opcode: u8, rd: u8, funct3: u8, rs1: u8, rs2: u8, funct7: u8) -> Result<u32> {
    check_opcode(opcode)?;
    check_register(rd)?;
    check_funct3(funct3)?;
    check_register(rs1)?;
    check_register(rs2)?;
    check_funct7(funct7)?;
    Ok(u32::from(opcode) |
        (u32::from(rd) << 7) |
        (u32::from(funct3) << 12) |
        (u32::from(rs1) << 15) |
        (u32::from(rs2) << 20) |
        (u32::from(funct7) << 25))
}

fn i_instruction(opcode: u8, rd: u8, funct3: u8, rs1: u8, imm: u16) -> Result<u32> {
    check_opcode(opcode)?;
    check_register(rd)?;
    check_funct3(funct3)?;
    check_imm_i_s(imm)?;
    Ok(u32::from(opcode) |
        (u32::from(rd) << 7) |
        (u32::from(funct3) << 12) |
        (u32::from(rs1) << 15) |
        (u32::from(imm) << 20))
}

fn s_instruction(opcode: u8, imm: u16, funct3: u8, rs1: u8, rs2: u8) -> Result<u32> {
    check_opcode(opcode)?;
    check_imm_i_s(imm)?;
    check_funct3(funct3)?;
    check_register(rs1)?;
    check_register(rs2)?;
    Ok(u32::from(opcode) |
        (u32::from(imm & 0b11111) << 7) |
        (u32::from(funct3) << 12) |
        (u32::from(rs1) << 15) |
        (u32::from(rs2) << 20) |
        (u32::from((imm >> 5) & 0b111_1111) << 25))
}

fn b_instruction(opcode: u8, imm: u16, funct3: u8, rs1: u8, rs2: u8) -> Result<u32> {
    check_opcode(opcode)?;
    check_imm_b(imm)?;
    check_funct3(funct3)?;
    check_register(rs1)?;
    check_register(rs2)?;
    Ok(u32::from(opcode) |
        (u32::from((imm >> 11) & 0b1) << 7) |
        (u32::from((imm >> 1) & 0b1111) << 8) |
        (u32::from(funct3) << 12) |
        (u32::from(rs1) << 15) |
        (u32::from(rs2) << 20) |
        (u32::from((imm >> 5) & 0b11_1111) << 25) |
        (u32::from((imm >> 12) & 0b1) << 31))
}

fn u_instruction(opcode: u8, rd: u8, imm: u32) -> Result<u32> {
    check_opcode(opcode)?;
    check_register(rd)?;
    check_imm_u(imm)?;
    Ok(u32::from(opcode) |
        (u32::from(rd) << 7) |
        (imm << 12))
}

fn j_instruction(opcode: u8, rd: u8, imm: u32) -> Result<u32> {
    check_opcode(opcode)?;
    check_register(rd)?;
    let imm =
        (imm >> 12) & 0b1111_1111 |
            ((imm >> 11) & 0b1) << 8 |
            ((imm >> 1) & 0b11_1111_1111) << 9 |
            ((imm >> 20) & 0b1) << 19;
    check_imm_u(imm)?;
    Ok(u32::from(opcode) |
        (u32::from(rd) << 7) |
        (imm << 12))
}

fn check_opcode(opcode: u8) -> Result<()> {
    check_range(opcode, 0b11 .. 1 << 7, "opcode")
}

fn check_register(register: u8) -> Result<()> {
    check_range(register, 0 .. 1 << 5, "register")
}

fn check_imm_i_s(imm: u16) -> Result<()> {
    check_range(imm, 0 .. 1 << 12, "imm")
}

fn check_imm_b(imm: u16) -> Result<()> {
    check_range(imm, 0 .. 1 << 13, "imm")?;
    check_multiple_of_two(imm, "i")
}

fn check_imm_u(imm: u32) -> Result<()> {
    check_range(imm, 0 .. 1 << 20, "imm")
}

fn check_funct3(funct3: u8) -> Result<()> {
    check_range(funct3, 0 .. 1 << 3, "funct3")
}

fn check_funct7(funct7: u8) -> Result<()> {
    check_range(funct7, 0 .. 1 << 7, "funct7")
}

fn check_range<T: PartialOrd>(value: T, range: Range<T>, name: &'static str) -> Result<()>
    where u32: std::convert::From<T>
{
    if value >= range.start && value < range.end {
        Ok(())
    } else {
        Err(RISCVError::OutOfRange {
            value: u32::from(value),
            range: u32::from(range.start) .. u32::from(range.end),
            name,
        })
    }
}

fn check_multiple_of_two<T>(value: T, name: &'static str) -> Result<()>
    where T: BitAnd + PartialEq,
          u32: std::convert::From<T>,
          u32: std::convert::From<T>,
{
    let value = u32::from(value);
    if value & 0b1 == 0 {
        Ok(())
    } else {
        Err(RISCVError::NotMultipleOfTwo { value, name })
    }
}
