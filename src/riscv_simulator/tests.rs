use crate::riscv_backend::write_code;
use rambo_riscv::MemoryMode;
use rambo_riscv::Simulator;
use rambo_riscv::DRAM;
use risky::instructions::*;
use rvsim::CpuError;
use rvsim::Op;
use std::convert::TryFrom;
use std::error::Error;
use std::io::Cursor;

macro_rules! assert_step {
    ($simulator: ident, $op: pat) => {{
        match $simulator.step() {
            $op => (),
            r => panic!("unexpected result: {:?}", r),
        }
    }};
}

#[test]
fn riscv_simulator() -> Result<(), Box<dyn Error>> {
    const CODE_BASE: usize = 0;
    const CODE_SIZE: usize = 1024;
    const DATA_BASE: usize = 0x0100_0000;
    const DATA_SIZE: usize = 1024;

    let mut simulator = Simulator::new(
        0,
        DRAM::new(&[
            (CODE_BASE, CODE_SIZE, MemoryMode::EXECUTE),
            (DATA_BASE, DATA_SIZE, MemoryMode::READ),
        ]),
    );

    let data_bank = simulator.dram.find_bank_mut(DATA_BASE).unwrap();
    data_bank[0] = 3;
    data_bank[1] = 5;
    simulator.cpu.x[1] = u32::try_from(DATA_BASE).unwrap();
    simulator.cpu.x[2] = simulator.cpu.x[1] + 1;

    let code_bank = simulator.dram.find_bank_mut(CODE_BASE).unwrap();
    let mut cursor = Cursor::new(&mut **code_bank);
    write_code(
        &mut cursor,
        &[
            lb(1, 1, 0)?,
            lb(2, 2, 0)?,
            add(1, 1, 2)?,
            lui(2, 0x0100_0000)?,
            lh(2, 2, 0)?,
            add(1, 1, 2)?,
            ebreak()?,
        ],
    );

    assert_step!(
        simulator,
        Ok(Op::Lb {
            rd: 1,
            rs1: 1,
            i_imm: 0
        })
    );
    assert_eq!(simulator.cpu.x[1], 3);

    assert_step!(
        simulator,
        Ok(Op::Lb {
            rd: 2,
            rs1: 2,
            i_imm: 0
        })
    );
    assert_eq!(simulator.cpu.x[2], 5);

    assert_step!(
        simulator,
        Ok(Op::Add {
            rd: 1,
            rs1: 1,
            rs2: 2
        })
    );
    assert_eq!(simulator.cpu.x[1], 8);

    assert_step!(
        simulator,
        Ok(Op::Lui {
            rd: 2,
            u_imm: 0x0100_0000
        })
    );
    assert_eq!(simulator.cpu.x[2], 0x0100_0000);

    assert_step!(
        simulator,
        Ok(Op::Lh {
            rd: 2,
            rs1: 2,
            i_imm: 0
        })
    );
    assert_eq!(simulator.cpu.x[2], 1283);

    assert_step!(
        simulator,
        Ok(Op::Add {
            rd: 1,
            rs1: 1,
            rs2: 2
        })
    );
    assert_eq!(simulator.cpu.x[1], 1291);

    assert_step!(simulator, Err((CpuError::Ebreak, Some(Op::Ebreak))));
    Ok(())
}
