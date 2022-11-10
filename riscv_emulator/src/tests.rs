use byteorder::LittleEndian;
use byteorder::WriteBytesExt;
use risky::instructions::*;
use risky::X1;
use risky::X2;
use std::io::Cursor;

use crate::cpu::Cpu;
use crate::mmu::MemoryAccessFlags;

#[test]
fn riscv_sim() {
    const CODE_BASE: u64 = 0;
    const CODE_SIZE: u64 = 1024;
    const DATA_BASE: u64 = 0x0100_0000;
    const DATA_SIZE: u64 = 1024;

    let mut cpu = Cpu::new();
    let mmu = cpu.mmu_mut();

    let mut code = Vec::new();
    let mut code_cursor = Cursor::new(&mut code);
    let instructions = [
        lb(X1, X1, 0),
        lb(X2, X2, 0),
        add(X1, X1, X2),
        lui(X2, DATA_BASE as i32),
        lh(X2, X2, 0),
        add(X1, X1, X2),
        ebreak(),
    ];
    instructions
        .into_iter()
        .for_each(|instruction| code_cursor.write_u32::<LittleEndian>(instruction).unwrap());
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

    mmu.store(DATA_BASE, 3).unwrap();
    mmu.store(DATA_BASE + 1, 5).unwrap();

    cpu.write_register(1, DATA_BASE as i64);
    cpu.write_register(2, (DATA_BASE + 1) as i64);

    cpu.tick();
    assert_eq!(cpu.read_register(1), 3);

    cpu.tick();
    assert_eq!(cpu.read_register(2), 5);

    cpu.tick();
    assert_eq!(cpu.read_register(1), 8);

    cpu.tick();
    assert_eq!(cpu.read_register(2), 0x0100_0000);

    cpu.tick();
    assert_eq!(cpu.read_register(2), 1283);

    cpu.tick();
    assert_eq!(cpu.read_register(1), 1291);
}
