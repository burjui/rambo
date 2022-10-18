use crate::new_riscv_sim::cpu::Cpu;
use crate::new_riscv_sim::mmu::MemoryAccessFlags;
use crate::riscv_backend::write_code;
use crate::riscv_base::decoder::decode;
use crate::riscv_exe::Dram;
use crate::riscv_exe::MemoryMode;
use crate::riscv_exe::Simulator;
use byteorder::LittleEndian;
use byteorder::WriteBytesExt;
use risky::instructions;
use risky::instructions::*;
use rvsim::CpuError;
use rvsim::Op;
use std::convert::TryFrom;
use std::error::Error;
use std::io::Cursor;

use super::cpu;

#[test]
fn riscv_sim() -> Result<(), Box<dyn Error>> {
    const CODE_BASE: u64 = 0;
    const CODE_SIZE: u64 = 1024;
    const DATA_BASE: u64 = 0x0100_0000;
    const DATA_SIZE: u64 = 1024;

    let mut cpu = Cpu::new();
    let mmu = cpu.get_mut_mmu();

    let mut code = Vec::new();
    let mut code_cursor = Cursor::new(&mut code);
    let instructions = [
        lb(1, 1, 0),
        lb(2, 2, 0),
        add(1, 1, 2),
        lui(2, DATA_BASE as i32),
        lh(2, 2, 0),
        add(1, 1, 2),
        ebreak(),
    ];
    instructions
        .into_iter()
        .enumerate()
        .for_each(|(i, instruction)| code_cursor.write_u32::<LittleEndian>(instruction).unwrap());
    mmu.add(
        CODE_BASE,
        CODE_SIZE,
        MemoryAccessFlags::READ,
        Some(&[(0u64, code.as_slice())]),
    );

    mmu.add(
        DATA_BASE,
        DATA_SIZE,
        MemoryAccessFlags::READ | MemoryAccessFlags::WRITE,
        None,
    );

    mmu.store(DATA_BASE, 3);
    mmu.store(DATA_BASE + 1, 5);

    cpu.write_register(1, DATA_BASE as i64);
    cpu.write_register(2, (DATA_BASE + 1) as i64);

    assert_eq!(
        decode_pc(&mut cpu),
        Some(Op::Lb {
            rd: 1,
            rs1: 1,
            i_imm: 0
        })
    );
    cpu.tick();
    assert_eq!(cpu.read_register(1), 3);

    assert_eq!(
        decode_pc(&mut cpu),
        Some(Op::Lb {
            rd: 2,
            rs1: 2,
            i_imm: 0
        })
    );
    cpu.tick();
    assert_eq!(cpu.read_register(2), 5);

    assert_eq!(
        decode_pc(&mut cpu),
        Some(Op::Add {
            rd: 1,
            rs1: 1,
            rs2: 2
        })
    );
    cpu.tick();
    assert_eq!(cpu.read_register(1), 8);

    assert_eq!(
        decode_pc(&mut cpu),
        Some(Op::Lui {
            rd: 2,
            u_imm: 0x0100_0000
        })
    );
    cpu.tick();
    assert_eq!(cpu.read_register(2), 0x0100_0000);

    assert_eq!(
        decode_pc(&mut cpu),
        Some(Op::Lh {
            rd: 2,
            rs1: 2,
            i_imm: 0
        })
    );
    cpu.tick();
    assert_eq!(cpu.read_register(2), 1283);

    assert_eq!(
        decode_pc(&mut cpu),
        Some(Op::Add {
            rd: 1,
            rs1: 1,
            rs2: 2
        })
    );
    cpu.tick();
    assert_eq!(cpu.read_register(1), 1291);

    assert_eq!(decode_pc(&mut cpu), Some(Op::Ebreak));
    Ok(())
}

//TODO: get rid of Op and use the new Cpu instructions
fn decode_pc(cpu: &mut Cpu) -> Option<Op> {
    let pc = cpu.read_pc();
    cpu.get_mut_mmu().load_word(pc).ok().and_then(Op::parse)
}
