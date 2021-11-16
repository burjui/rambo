use byteorder::ReadBytesExt;
use std::fmt;
use std::io::Cursor;
use std::ops::Deref;

pub struct Decoder<'a>(Cursor<&'a [u8]>);

impl Iterator for Decoder<'_> {
    type Item = (Op, u64);

    fn next(&mut self) -> Option<Self::Item> {
        let offset = self.0.position();
        self.0
            .read_u32::<byteorder::LittleEndian>()
            .map(|instruction| {
                let op = Op::parse(instruction).unwrap_or_else(|| {
                    panic!(
                        "failed to parse instruction 0x{:08x} at 0x{:08x}: 0x{:08x}",
                        instruction, offset, instruction
                    )
                });
                (op, offset)
            })
            .ok()
    }
}

pub fn decode(code: &[u8]) -> Decoder<'_> {
    Decoder(Cursor::new(code))
}

pub struct Op(pub rvsim::Op);

impl Deref for Op {
    type Target = rvsim::Op;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl Op {
    pub fn parse(instruction: u32) -> Option<Self> {
        rvsim::Op::parse(instruction).map(Self)
    }
}

impl fmt::Debug for Op {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.0 {
            rvsim::Op::Lui { rd, u_imm } => write!(f, "lui x{}, 0x{:x}", rd, u_imm),
            rvsim::Op::Auipc { rd, u_imm } => write!(f, "auipc x{}, 0x{:x}", rd, u_imm),
            rvsim::Op::Jal { rd, j_imm } => write!(f, "jal x{}, 0x{:x}", rd, j_imm),
            rvsim::Op::Jalr { rd, rs1, i_imm } => {
                write!(f, "jalr x{}, x{}, 0x{:x}", rd, rs1, i_imm)
            }
            rvsim::Op::Beq { rs1, rs2, b_imm } => {
                write!(f, "beq x{}, x{}, 0x{:x}", rs1, rs2, b_imm)
            }
            rvsim::Op::Bne { rs1, rs2, b_imm } => {
                write!(f, "bne x{}, x{}, 0x{:x}", rs1, rs2, b_imm)
            }
            rvsim::Op::Blt { rs1, rs2, b_imm } => {
                write!(f, "blt x{}, x{}, 0x{:x}", rs1, rs2, b_imm)
            }
            rvsim::Op::Bge { rs1, rs2, b_imm } => {
                write!(f, "bge x{}, x{}, 0x{:x}", rs1, rs2, b_imm)
            }
            rvsim::Op::Bltu { rs1, rs2, b_imm } => {
                write!(f, "bltu x{}, x{}, 0x{:x}", rs1, rs2, b_imm)
            }
            rvsim::Op::Bgeu { rs1, rs2, b_imm } => {
                write!(f, "bgeu x{}, x{}, 0x{:x}", rs1, rs2, b_imm)
            }
            rvsim::Op::Lb { rd, rs1, i_imm } => write!(f, "lb x{}, x{}, 0x{:x}", rd, rs1, i_imm),
            rvsim::Op::Lh { rd, rs1, i_imm } => write!(f, "lh x{}, x{}, 0x{:x}", rd, rs1, i_imm),
            rvsim::Op::Lw { rd, rs1, i_imm } => write!(f, "lw x{}, x{}, 0x{:x}", rd, rs1, i_imm),
            rvsim::Op::Lbu { rd, rs1, i_imm } => write!(f, "lbu x{}, x{}, 0x{:x}", rd, rs1, i_imm),
            rvsim::Op::Lhu { rd, rs1, i_imm } => write!(f, "lhu x{}, x{}, 0x{:x}", rd, rs1, i_imm),
            rvsim::Op::Sb { rs1, rs2, s_imm } => write!(f, "sb x{}, x{}, 0x{:x}", rs1, rs2, s_imm),
            rvsim::Op::Sh { rs1, rs2, s_imm } => write!(f, "sh x{}, x{}, 0x{:x}", rs1, rs2, s_imm),
            rvsim::Op::Sw { rs1, rs2, s_imm } => write!(f, "sw x{}, x{}, 0x{:x}", rs1, rs2, s_imm),
            rvsim::Op::Addi { rd, rs1, i_imm } => {
                write!(f, "addi x{}, x{}, 0x{:x}", rd, rs1, i_imm)
            }
            rvsim::Op::Slti { rd, rs1, i_imm } => {
                write!(f, "slti x{}, x{}, 0x{:x}", rd, rs1, i_imm)
            }
            rvsim::Op::Sltiu { rd, rs1, i_imm } => {
                write!(f, "sltiu x{}, x{}, 0x{:x}", rd, rs1, i_imm)
            }
            rvsim::Op::Xori { rd, rs1, i_imm } => {
                write!(f, "xori x{}, x{}, 0x{:x}", rd, rs1, i_imm)
            }
            rvsim::Op::Ori { rd, rs1, i_imm } => write!(f, "ori x{}, x{}, 0x{:x}", rd, rs1, i_imm),
            rvsim::Op::Andi { rd, rs1, i_imm } => {
                write!(f, "andi x{}, x{}, 0x{:x}", rd, rs1, i_imm)
            }
            rvsim::Op::Slli { rd, rs1, shamt } => write!(f, "slli x{}, x{}, {}", rd, rs1, shamt),
            rvsim::Op::Srli { rd, rs1, shamt } => write!(f, "srli x{}, x{}, {}", rd, rs1, shamt),
            rvsim::Op::Srai { rd, rs1, shamt } => write!(f, "srai x{}, x{}, {}", rd, rs1, shamt),
            rvsim::Op::Add { rd, rs1, rs2 } => write!(f, "add x{}, x{}, x{}", rd, rs1, rs2),
            rvsim::Op::Sll { rd, rs1, rs2 } => write!(f, "sll x{}, x{}, x{}", rd, rs1, rs2),
            rvsim::Op::Slt { rd, rs1, rs2 } => write!(f, "slt x{}, x{}, x{}", rd, rs1, rs2),
            rvsim::Op::Sltu { rd, rs1, rs2 } => write!(f, "sltu x{}, x{}, x{}", rd, rs1, rs2),
            rvsim::Op::Xor { rd, rs1, rs2 } => write!(f, "xor x{}, x{}, x{}", rd, rs1, rs2),
            rvsim::Op::Srl { rd, rs1, rs2 } => write!(f, "srl x{}, x{}, x{}", rd, rs1, rs2),
            rvsim::Op::Or { rd, rs1, rs2 } => write!(f, "or x{}, x{}, x{}", rd, rs1, rs2),
            rvsim::Op::And { rd, rs1, rs2 } => write!(f, "and x{}, x{}, x{}", rd, rs1, rs2),
            rvsim::Op::Sub { rd, rs1, rs2 } => write!(f, "sub x{}, x{}, x{}", rd, rs1, rs2),
            rvsim::Op::Sra { rd, rs1, rs2 } => write!(f, "sra x{}, x{}, x{}", rd, rs1, rs2),
            rvsim::Op::Fence { pred, succ } => write!(f, "fence {}, {}", pred, succ),
            rvsim::Op::FenceI => write!(f, "fence.i"),
            rvsim::Op::Ecall => write!(f, "ecall"),
            rvsim::Op::Ebreak => write!(f, "ebreak"),
            rvsim::Op::Csrrw { rd, rs1, csr } => write!(f, "csrrw x{}, x{}, {}", rd, rs1, csr),
            rvsim::Op::Csrrs { rd, rs1, csr } => write!(f, "csrrs x{}, x{}, {}", rd, rs1, csr),
            rvsim::Op::Csrrc { rd, rs1, csr } => write!(f, "csrrc x{}, x{}, {}", rd, rs1, csr),
            rvsim::Op::Csrrwi { rd, zimm, csr } => {
                write!(f, "csrrwi x{}, 0x{:x}, {}", rd, zimm, csr)
            }
            rvsim::Op::Csrrsi { rd, zimm, csr } => {
                write!(f, "csrrsi x{}, 0x{:x}, {}", rd, zimm, csr)
            }
            rvsim::Op::Csrrci { rd, zimm, csr } => {
                write!(f, "csrrci x{}, 0x{:x}, {}", rd, zimm, csr)
            }
            rvsim::Op::Mul { rd, rs1, rs2 } => write!(f, "mul x{}, x{}, x{}", rd, rs1, rs2),
            rvsim::Op::Mulh { rd, rs1, rs2 } => write!(f, "mulh x{}, x{}, x{}", rd, rs1, rs2),
            rvsim::Op::Mulhsu { rd, rs1, rs2 } => write!(f, "mulhsu x{}, x{}, x{}", rd, rs1, rs2),
            rvsim::Op::Mulhu { rd, rs1, rs2 } => write!(f, "mulhu x{}, x{}, x{}", rd, rs1, rs2),
            rvsim::Op::Div { rd, rs1, rs2 } => write!(f, "div x{}, x{}, x{}", rd, rs1, rs2),
            rvsim::Op::Divu { rd, rs1, rs2 } => write!(f, "divu x{}, x{}, x{}", rd, rs1, rs2),
            rvsim::Op::Rem { rd, rs1, rs2 } => write!(f, "rem x{}, x{}, x{}", rd, rs1, rs2),
            rvsim::Op::Remu { rd, rs1, rs2 } => write!(f, "remu x{}, x{}, x{}", rd, rs1, rs2),
            rvsim::Op::LrW { rd, rs1, aq, rl } => {
                write!(f, "lr.w x{}, x{}, {}, {}", rd, rs1, aq, rl)
            }
            rvsim::Op::ScW {
                rd,
                rs1,
                rs2,
                aq,
                rl,
            } => write!(f, "sc.w x{}, x{}, x{}, {}, {}", rd, rs1, rs2, aq, rl),
            rvsim::Op::AmoswapW {
                rd,
                rs1,
                rs2,
                aq,
                rl,
            } => write!(f, "amoswap.w x{}, x{}, x{}, {}, {}", rd, rs1, rs2, aq, rl),
            rvsim::Op::AmoaddW {
                rd,
                rs1,
                rs2,
                aq,
                rl,
            } => write!(f, "amoadd.w x{}, x{}, x{}, {}, {}", rd, rs1, rs2, aq, rl),
            rvsim::Op::AmoxorW {
                rd,
                rs1,
                rs2,
                aq,
                rl,
            } => write!(f, "amoxor.w x{}, x{}, x{}, {}, {}", rd, rs1, rs2, aq, rl),
            rvsim::Op::AmoandW {
                rd,
                rs1,
                rs2,
                aq,
                rl,
            } => write!(f, "amoand.w x{}, x{}, x{}, {}, {}", rd, rs1, rs2, aq, rl),
            rvsim::Op::AmoorW {
                rd,
                rs1,
                rs2,
                aq,
                rl,
            } => write!(f, "amoor.w x{}, x{}, x{}, {}, {}", rd, rs1, rs2, aq, rl),
            rvsim::Op::AmominW {
                rd,
                rs1,
                rs2,
                aq,
                rl,
            } => write!(f, "amomin.w x{}, x{}, x{}, {}, {}", rd, rs1, rs2, aq, rl),
            rvsim::Op::AmomaxW {
                rd,
                rs1,
                rs2,
                aq,
                rl,
            } => write!(f, "amomax.w x{}, x{}, x{}, {}, {}", rd, rs1, rs2, aq, rl),
            rvsim::Op::AmominuW {
                rd,
                rs1,
                rs2,
                aq,
                rl,
            } => write!(f, "amominu.w x{}, x{}, x{}, {}, {}", rd, rs1, rs2, aq, rl),
            rvsim::Op::AmomaxuW {
                rd,
                rs1,
                rs2,
                aq,
                rl,
            } => write!(f, "amomaxu.w x{}, x{}, x{}, {}, {}", rd, rs1, rs2, aq, rl),
            rvsim::Op::Flw { rd, rs1, i_imm } => write!(f, "flw x{}, x{}, 0x{:x}", rd, rs1, i_imm),
            rvsim::Op::Fsw { rs1, rs2, s_imm } => {
                write!(f, "fsw x{}, x{}, 0x{:x}", rs1, rs2, s_imm)
            }
            rvsim::Op::FmaddS {
                rd,
                rs1,
                rs2,
                rs3,
                rm,
            } => write!(f, "fmadd.s x{}, x{}, x{}, {}, {}", rd, rs1, rs2, rs3, rm),
            rvsim::Op::FmsubS {
                rd,
                rs1,
                rs2,
                rs3,
                rm,
            } => write!(f, "fmsub.s x{}, x{}, x{}, {}, {}", rd, rs1, rs2, rs3, rm),
            rvsim::Op::FnmsubS {
                rd,
                rs1,
                rs2,
                rs3,
                rm,
            } => write!(f, "fnmsub.s x{}, x{}, x{}, {}, {}", rd, rs1, rs2, rs3, rm),
            rvsim::Op::FnmaddS {
                rd,
                rs1,
                rs2,
                rs3,
                rm,
            } => write!(f, "fnmadd.s x{}, x{}, x{}, {}, {}", rd, rs1, rs2, rs3, rm),
            rvsim::Op::FaddS { rd, rs1, rs2, rm } => {
                write!(f, "fadd.s x{}, x{}, x{}, {}", rd, rs1, rs2, rm)
            }
            rvsim::Op::FsubS { rd, rs1, rs2, rm } => {
                write!(f, "fsub.s x{}, x{}, x{}, {}", rd, rs1, rs2, rm)
            }
            rvsim::Op::FmulS { rd, rs1, rs2, rm } => {
                write!(f, "fmul.s x{}, x{}, x{}, {}", rd, rs1, rs2, rm)
            }
            rvsim::Op::FdivS { rd, rs1, rs2, rm } => {
                write!(f, "fdiv.s x{}, x{}, x{}, {}", rd, rs1, rs2, rm)
            }
            rvsim::Op::FsqrtS { rd, rs1, rm } => write!(f, "fsqrt.s x{}, x{}, {}", rd, rs1, rm),
            rvsim::Op::FsgnjS { rd, rs1, rs2 } => write!(f, "fsgnj.s x{}, x{}, x{}", rd, rs1, rs2),
            rvsim::Op::FsgnjnS { rd, rs1, rs2 } => {
                write!(f, "fsgnjn.s x{}, x{}, x{}", rd, rs1, rs2)
            }
            rvsim::Op::FsgnjxS { rd, rs1, rs2 } => {
                write!(f, "fsgnjx.s x{}, x{}, x{}", rd, rs1, rs2)
            }
            rvsim::Op::FminS { rd, rs1, rs2 } => write!(f, "fmin.s x{}, x{}, x{}", rd, rs1, rs2),
            rvsim::Op::FmaxS { rd, rs1, rs2 } => write!(f, "fmax.s x{}, x{}, x{}", rd, rs1, rs2),
            rvsim::Op::FcvtWS { rd, rs1, rm } => write!(f, "fcvt.w.s x{}, x{}, {}", rd, rs1, rm),
            rvsim::Op::FcvtWuS { rd, rs1, rm } => write!(f, "fcvt.wu.s x{}, x{}, {}", rd, rs1, rm),
            rvsim::Op::FmvXW { rd, rs1 } => write!(f, "fmv.x.w x{}, x{}", rd, rs1),
            rvsim::Op::FeqS { rd, rs1, rs2 } => write!(f, "feq.s x{}, x{}, x{}", rd, rs1, rs2),
            rvsim::Op::FltS { rd, rs1, rs2 } => write!(f, "flt.s x{}, x{}, x{}", rd, rs1, rs2),
            rvsim::Op::FleS { rd, rs1, rs2 } => write!(f, "fle.s x{}, x{}, x{}", rd, rs1, rs2),
            rvsim::Op::FclassS { rd, rs1 } => write!(f, "fclass.s x{}, x{}", rd, rs1),
            rvsim::Op::FcvtSW { rd, rs1, rm } => write!(f, "fcvt.s.w x{}, x{}, {}", rd, rs1, rm),
            rvsim::Op::FcvtSWu { rd, rs1, rm } => write!(f, "fcvt.s.wu x{}, x{}, {}", rd, rs1, rm),
            rvsim::Op::FmvWX { rd, rs1 } => write!(f, "fmv.w.x x{}, x{}", rd, rs1),
            rvsim::Op::Fld { rd, rs1, i_imm } => write!(f, "fld x{}, x{}, 0x{:x}", rd, rs1, i_imm),
            rvsim::Op::Fsd { rs1, rs2, s_imm } => {
                write!(f, "fsd x{}, x{}, 0x{:x}", rs1, rs2, s_imm)
            }
            rvsim::Op::FmaddD {
                rd,
                rs1,
                rs2,
                rs3,
                rm,
            } => write!(f, "fmadd.d x{}, x{}, x{}, {}, {}", rd, rs1, rs2, rs3, rm),
            rvsim::Op::FmsubD {
                rd,
                rs1,
                rs2,
                rs3,
                rm,
            } => write!(f, "fmsub.d x{}, x{}, x{}, {}, {}", rd, rs1, rs2, rs3, rm),
            rvsim::Op::FnmsubD {
                rd,
                rs1,
                rs2,
                rs3,
                rm,
            } => write!(f, "fnmsub.d x{}, x{}, x{}, {}, {}", rd, rs1, rs2, rs3, rm),
            rvsim::Op::FnmaddD {
                rd,
                rs1,
                rs2,
                rs3,
                rm,
            } => write!(f, "fnmadd.d x{}, x{}, x{}, {}, {}", rd, rs1, rs2, rs3, rm),
            rvsim::Op::FaddD { rd, rs1, rs2, rm } => {
                write!(f, "fadd.d x{}, x{}, x{}, {}", rd, rs1, rs2, rm)
            }
            rvsim::Op::FsubD { rd, rs1, rs2, rm } => {
                write!(f, "fsub.d x{}, x{}, x{}, {}", rd, rs1, rs2, rm)
            }
            rvsim::Op::FmulD { rd, rs1, rs2, rm } => {
                write!(f, "fmul.d x{}, x{}, x{}, {}", rd, rs1, rs2, rm)
            }
            rvsim::Op::FdivD { rd, rs1, rs2, rm } => {
                write!(f, "fdiv.d x{}, x{}, x{}, {}", rd, rs1, rs2, rm)
            }
            rvsim::Op::FsqrtD { rd, rs1, rm } => write!(f, "fsqrt.d x{}, x{}, {}", rd, rs1, rm),
            rvsim::Op::FsgnjD { rd, rs1, rs2 } => write!(f, "fsgnj.d x{}, x{}, x{}", rd, rs1, rs2),
            rvsim::Op::FsgnjnD { rd, rs1, rs2 } => {
                write!(f, "fsgnjn.d x{}, x{}, x{}", rd, rs1, rs2)
            }
            rvsim::Op::FsgnjxD { rd, rs1, rs2 } => {
                write!(f, "fsgnjx.d x{}, x{}, x{}", rd, rs1, rs2)
            }
            rvsim::Op::FminD { rd, rs1, rs2 } => write!(f, "fmin.d x{}, x{}, x{}", rd, rs1, rs2),
            rvsim::Op::FmaxD { rd, rs1, rs2 } => write!(f, "fmax.d x{}, x{}, x{}", rd, rs1, rs2),
            rvsim::Op::FcvtWD { rd, rs1, rm } => write!(f, "fcvt.w.d x{}, x{}, {}", rd, rs1, rm),
            rvsim::Op::FcvtWuD { rd, rs1, rm } => write!(f, "fcvt.wu.d x{}, x{}, {}", rd, rs1, rm),
            rvsim::Op::FeqD { rd, rs1, rs2 } => write!(f, "feq.d x{}, x{}, x{}", rd, rs1, rs2),
            rvsim::Op::FltD { rd, rs1, rs2 } => write!(f, "flt.d x{}, x{}, x{}", rd, rs1, rs2),
            rvsim::Op::FleD { rd, rs1, rs2 } => write!(f, "fle.d x{}, x{}, x{}", rd, rs1, rs2),
            rvsim::Op::FclassD { rd, rs1 } => write!(f, "fclass.d x{}, x{}", rd, rs1),
            rvsim::Op::FcvtDW { rd, rs1, rm } => write!(f, "fcvt.d.w x{}, x{}, {}", rd, rs1, rm),
            rvsim::Op::FcvtDWu { rd, rs1, rm } => write!(f, "fcvt.d.wu x{}, x{}, {}", rd, rs1, rm),
            rvsim::Op::FcvtSD { rd, rs1, rm } => write!(f, "fcvt.s.d x{}, x{}, {}", rd, rs1, rm),
            rvsim::Op::FcvtDS { rd, rs1, rm } => write!(f, "fcvt.d.s x{}, x{}, {}", rd, rs1, rm),
        }
    }
}
