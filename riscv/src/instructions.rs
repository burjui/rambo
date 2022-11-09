use core::fmt;
use std::{collections::HashSet, fmt::Display, ops::Range};

macro_rules! instruction {
    (R: $name: ident, $opcode: path, funct3 $funct3: literal, funct7 $funct7: literal) => {
        // TODO #[track_caller]
        pub fn $name(rd: u8, rs1: u8, rs2: u8) -> u32 {
            r_instruction(stringify!($name), $opcode, rd, $funct3, rs1, rs2, $funct7)
        }
    };

    (I: $name: ident, $opcode: path, funct3 $funct3: literal) => {
        // TODO #[track_caller]
        pub fn $name(rd: u8, rs1: u8, imm: i16) -> u32 {
            i_instruction(stringify!($name), $opcode, rd, $funct3, rs1, imm)
        }
    };

    (S: $name: ident, $opcode: path, funct3 $funct3: literal) => {
        // TODO #[track_caller]
        pub fn $name(rs1: u8, imm: i16, rs2: u8) -> u32 {
            s_instruction(stringify!($name), $opcode, imm, $funct3, rs1, rs2)
        }
    };

    (B: $name: ident, $opcode: path, funct3 $funct3: literal) => {
        // TODO #[track_caller]
        pub fn $name(imm: i16, rs1: u8, rs2: u8) -> u32 {
            b_instruction(stringify!($name), $opcode, imm, $funct3, rs1, rs2)
        }
    };

    (U: $name: ident, $opcode: path) => {
        // TODO #[track_caller]
        pub fn $name(rd: u8, imm: i32) -> u32 {
            u_instruction(stringify!($name), $opcode, rd, imm)
        }
    };

    (J: $name: ident, $opcode: path) => {
        // TODO #[track_caller]
        pub fn $name(rd: u8, imm: i32) -> u32 {
            j_instruction(stringify!($name), $opcode, rd, imm)
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

pub fn slli(rd: u8, rs1: u8, shamt: u8) -> u32 {
    r_instruction("slli", opcode::OP_IMM, rd, 0b001, rs1, shamt, 0b0000000)
}

pub fn srli(rd: u8, rs1: u8, shamt: u8) -> u32 {
    r_instruction("srli", opcode::OP_IMM, rd, 0b101, rs1, shamt, 0b0000000)
}

pub fn srai(rd: u8, rs1: u8, shamt: u8) -> u32 {
    r_instruction("srai", opcode::OP_IMM, rd, 0b101, rs1, shamt, 0b0100000)
}

pub fn fence(pred: &'static str, succ: &'static str) -> u32 {
    let pred = parse_fence_mask(pred).unwrap();
    let succ = parse_fence_mask(succ).unwrap();
    fence_instruction("fence", 0b000, pred, succ)
}

pub fn fence_i() -> u32 {
    fence_instruction("fence_i", 0b001, 0b0000, 0b0000)
}

pub fn ecall() -> u32 {
    i_instruction(
        "ecall",
        opcode::SYSTEM,
        0b00000,
        0b000,
        0b00000,
        0b0000_0000_0000,
    )
}

pub fn ebreak() -> u32 {
    i_instruction(
        "ebreak",
        opcode::SYSTEM,
        0b00000,
        0b000,
        0b00000,
        0b0000_0000_0001,
    )
}

pub fn csrrwi(rd: u8, rs1: u8, csr: u16) -> u32 {
    csr_instruction("csrrwi", rd, 0b101, rs1, csr)
}

pub fn csrrsi(rd: u8, rs1: u8, csr: u16) -> u32 {
    csr_instruction("csrrsi", rd, 0b110, rs1, csr)
}

pub fn csrrci(rd: u8, rs1: u8, csr: u16) -> u32 {
    csr_instruction("csrrci", rd, 0b111, rs1, csr)
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

// Implementation

mod opcode {
    macro_rules! opcodes {
        ($($name: ident $bits6to2: literal,)+) => ($(pub(crate) const $name: u8 = $bits6to2 << 2 | 0b11;)+)
    }

    opcodes!(
        LOAD       0b00000,
        // LOAD_FP    0b00001,
        MISC_MEM   0b00011,
        OP_IMM     0b00100,
        AUIPC      0b00101,
        // OP_IMM_32  0b00110,

        STORE      0b01000,
        // STORE_FP   0b01001,
        // AMO        0b01011,
        OP         0b01100,
        LUI        0b01101,
        // OP_32      0b01110,

        // MADD       0b10000,
        // MSUB       0b10001,
        // NMSUB      0b10010,
        // NMADD      0b10011,
        // OP_FP      0b10100,

        BRANCH     0b11000,
        JALR       0b11001,
        JAL        0b11011,
        SYSTEM     0b11100,
    );
}

type Result<T> = core::result::Result<T, String>;

fn fence_instruction(function_name: &'static str, funct3: u8, pred: u8, succ: u8) -> u32 {
    check_funct3(function_name, funct3).unwrap();
    check_range(function_name, "pred", pred, 0..1 << 4).unwrap();
    check_range(function_name, "succ", succ, 0..1 << 4).unwrap();
    let imm = i16::from(succ | pred << 4);
    i_instruction(function_name, opcode::MISC_MEM, 0x00000, 0b00, 0x00000, imm)
}

fn parse_fence_mask(mask_str: &'static str) -> Result<u8> {
    let mut chars_processed = HashSet::new();
    let mut mask = 0;
    for flag_name in mask_str.chars() {
        if chars_processed.contains(&flag_name) {
            return Err(fence_flag_error("unknown", flag_name, mask_str));
        }
        chars_processed.insert(flag_name);
        mask |= match flag_name {
            'i' => 1 << 3,
            'o' => 1 << 2,
            'r' => 1 << 1,
            'w' => 1,
            _ => return Err(fence_flag_error("invalid", flag_name, mask_str)),
        };
    }
    Ok(mask)
}

fn fence_flag_error(kind: &'static str, flag_name: char, mask_str: &str) -> String {
    format!(
        "{} fence flag name `{}' in fence mask `{}'",
        kind, flag_name, mask_str
    )
}

fn csr_instruction(function_name: &'static str, rd: u8, funct3: u8, rs1: u8, csr: u16) -> u32 {
    i_instruction(function_name, opcode::SYSTEM, rd, funct3, rs1, csr as i16)
}

fn r_instruction(
    function_name: &'static str,
    opcode: u8,
    rd: u8,
    funct3: u8,
    rs1: u8,
    rs2: u8,
    funct7: u8,
) -> u32 {
    check_opcode(function_name, opcode).unwrap();
    check_register(function_name, rd).unwrap();
    check_funct3(function_name, funct3).unwrap();
    check_register(function_name, rs1).unwrap();
    check_register(function_name, rs2).unwrap();
    check_funct7(function_name, funct7).unwrap();
    u32::from(opcode)
        | (u32::from(rd) << 7)
        | (u32::from(funct3) << 12)
        | (u32::from(rs1) << 15)
        | (u32::from(rs2) << 20)
        | (u32::from(funct7) << 25)
}

fn i_instruction(
    function_name: &'static str,
    opcode: u8,
    rd: u8,
    funct3: u8,
    rs1: u8,
    imm: i16,
) -> u32 {
    check_opcode(function_name, opcode).unwrap();
    check_register(function_name, rd).unwrap();
    check_funct3(function_name, funct3).unwrap();
    check_imm_i_s(function_name, imm).unwrap();
    u32::from(opcode)
        | (u32::from(rd) << 7)
        | (u32::from(funct3) << 12)
        | (u32::from(rs1) << 15)
        | ((imm as u32) << 20)
}

fn s_instruction(
    function_name: &'static str,
    opcode: u8,
    imm: i16,
    funct3: u8,
    rs1: u8,
    rs2: u8,
) -> u32 {
    check_opcode(function_name, opcode).unwrap();
    check_imm_i_s(function_name, imm).unwrap();
    check_funct3(function_name, funct3).unwrap();
    check_register(function_name, rs1).unwrap();
    check_register(function_name, rs2).unwrap();
    let imm_u32 = imm as u32;
    u32::from(opcode)
        | ((imm_u32 & 0b11111) << 7)
        | (u32::from(funct3) << 12)
        | (u32::from(rs1) << 15)
        | (u32::from(rs2) << 20)
        | ((imm_u32 >> 5) & 0b111_1111) << 25
}

fn b_instruction(
    function_name: &'static str,
    opcode: u8,
    imm: i16,
    funct3: u8,
    rs1: u8,
    rs2: u8,
) -> u32 {
    check_opcode(function_name, opcode).unwrap();
    check_imm_b(function_name, imm).unwrap();
    check_funct3(function_name, funct3).unwrap();
    check_register(function_name, rs1).unwrap();
    check_register(function_name, rs2).unwrap();
    let imm_u32 = imm as u32;
    u32::from(opcode)
        | ((imm_u32 >> 11) & 0b1 << 7)
        | ((imm_u32 >> 1) & 0b1111 << 8)
        | (u32::from(funct3) << 12)
        | (u32::from(rs1) << 15)
        | (u32::from(rs2) << 20)
        | ((imm_u32 >> 5) & 0b11_1111 << 25)
        | ((imm_u32 >> 12) & 0b1 << 31)
}

fn u_instruction(function_name: &'static str, opcode: u8, rd: u8, imm: i32) -> u32 {
    check_opcode(function_name, opcode).unwrap();
    check_register(function_name, rd).unwrap();
    u32::from(opcode) | (u32::from(rd) << 7) | (imm as u32 & 0xFFFF_F000)
}

fn j_instruction(function_name: &'static str, opcode: u8, rd: u8, imm: i32) -> u32 {
    check_opcode(function_name, opcode).unwrap();
    check_register(function_name, rd).unwrap();
    check_imm_j(function_name, imm).unwrap();
    let imm_u32 = imm as u32;
    let imm_field = (imm_u32 >> 12 & 0b1111_1111) << 12
        | (imm_u32 >> 11 & 0b1) << 20
        | (imm_u32 >> 1 & 0b11_1111_1111) << 21
        | (imm_u32 >> 20 & 0b1) << 31;
    u32::from(opcode) | (u32::from(rd) << 7) | imm_field
}

fn check_opcode(function_name: &'static str, opcode: u8) -> Result<()> {
    check_range(function_name, "opcode", opcode, 0b11..1 << 7)
}

fn check_register(function_name: &'static str, register: u8) -> Result<()> {
    check_range(function_name, "register", register, 0..1 << 5)
}

fn check_imm_i_s(function_name: &'static str, imm: i16) -> Result<()> {
    check_range(function_name, "imm", imm, -(1 << 11)..1 << 11)
}

fn check_imm_b(function_name: &'static str, imm: i16) -> Result<()> {
    check_range(function_name, "imm", imm, -(1 << 12)..1 << 12).and_then(|()| match imm & 1 {
        0 => Ok(()),
        _ => Err(format!(
            "imm = {} (0x{:08x}) is not a multiple of 2",
            imm, imm
        )),
    })
}

fn check_imm_j(function_name: &'static str, imm: i32) -> Result<()> {
    check_range(function_name, "imm", imm, -(1 << 20)..1 << 20).and_then(|()| match imm & 1 {
        0 => Ok(()),
        _ => Err(format!(
            "imm = {} (0x{:08x}) is not a multiple of 2",
            imm, imm
        )),
    })
}

fn check_funct3(function_name: &'static str, funct3: u8) -> Result<()> {
    check_range(function_name, "funct3", funct3, 0..1 << 3)
}

fn check_funct7(function_name: &'static str, funct7: u8) -> Result<()> {
    check_range(function_name, "funct7", funct7, 0..1 << 7)
}

fn check_range<T>(
    function_name: &'static str,
    value_name: &'static str,
    value: T,
    range: Range<T>,
) -> Result<()>
where
    T: PartialOrd + Display + fmt::LowerHex,
{
    if value >= range.start && value < range.end {
        Ok(())
    } else {
        Err(format!(
            "{}: {} = {} (0x{:08x}) is out of range: {} .. {} (0x{:x} .. 0x{:x})",
            function_name, value_name, value, value, range.start, range.end, range.start, range.end
        ))
    }
}
