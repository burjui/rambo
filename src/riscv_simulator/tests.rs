use std::cell::UnsafeCell;
use std::error::Error;
use std::io::{Cursor, Read, Write};
use std::{io, fmt};
use std::ops::{Deref, DerefMut, Range};

use bitflags::bitflags;
use itertools::Itertools;
use rvsim::CpuError;
use rvsim::CpuState;
use rvsim::Interp;
use rvsim::Memory;
use rvsim::MemoryAccess;
use rvsim::Op;
use rvsim::SimpleClock;

use crate::utils::{intersection, write_u32};
use crate::riscv::{add, ebreak, lb};

macro_rules! assert_step {
    ($simulator: ident, $op: pat) => ({
        match $simulator.step() {
            $op => (),
            _ => panic!("unexpected result"),
        }
    });
}

#[test]
fn riscv_simulator() -> Result<(), Box<dyn Error>> {
    const CODE_BASE: u32 = 0;
    const CODE_SIZE: u32 = 1024;
    const DATA_BASE: u32 = 0x01000000;
    const DATA_SIZE: u32 = 1024;

    let mut simulator = Simulator::new(0, DRAM::new(&[
        (CODE_BASE, CODE_SIZE, AccessMode::EXECUTE),
        (DATA_BASE, DATA_SIZE, AccessMode::READ),
    ]));
    let code_bank = simulator.dram.find_bank_mut(CODE_BASE).unwrap();
    let mut cursor = Cursor::new(&mut **code_bank);
    let cursor = &mut cursor;
    write_u32(lb(1, 1, 0)?, cursor)?;
    write_u32(lb(2, 2, 0)?, cursor)?;
    write_u32(add(1, 1, 2)?, cursor)?;
    write_u32(ebreak()?, cursor)?;

    let data_bank = simulator.dram.find_bank_mut(DATA_BASE).unwrap();
    data_bank[0] = 3;
    data_bank[1] = 5;

    simulator.cpu.x[1] = DATA_BASE + 0;
    simulator.cpu.x[2] = DATA_BASE + 1;

    assert_step!(simulator, Ok(Op::Lb { rd: 1, rs1: 1, i_imm: 0 }));
    assert_eq!(simulator.cpu.x[1], 3);

    assert_step!(simulator, Ok(Op::Lb { rd: 2, rs1: 2, i_imm: 0 }));
    assert_eq!(simulator.cpu.x[2], 5);

    assert_step!(simulator, Ok(Op::Add { rd: 1, rs1: 1, rs2: 2 }));
    assert_eq!(simulator.cpu.x[1], 8);

    assert_step!(simulator, Err((CpuError::Ebreak, Some(Op::Ebreak))));
    Ok(())
}

struct Simulator {
    clock: SimpleClock,
    dram: DRAM,
    cpu: CpuState,
}

impl Simulator {
    fn new(pc: u32, dram: DRAM) -> Self {
        Self {
            clock: SimpleClock::new(),
            dram,
            cpu: CpuState::new(pc),
        }
    }

    fn step(&mut self) -> Result<Op, (CpuError, Option<Op>)> {
        Interp::new(&mut self.cpu, &mut self.dram, &mut self.clock).step()
    }

    fn run(&mut self) -> (CpuError, Option<Op>) {
        Interp::new(&mut self.cpu, &mut self.dram, &mut self.clock).run()
    }
}

bitflags! {
    struct AccessMode: u32 {
        const READ = 1 << 0;
        const WRITE = 1 << 1;
        const EXECUTE = 1 << 2;
    }
}

impl fmt::Display for AccessMode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}{}{}",
               if self.contains(Self::READ) { "r" } else { "" },
               if self.contains(Self::WRITE) { "w" } else { "" },
               if self.contains(Self::EXECUTE) { "x" } else { "" })
    }
}

struct DRAM(Box<[DRAMBank]>);

impl DRAM {
    fn new(banks: &[(u32, u32, AccessMode)]) -> Self {
        assert!(!banks.is_empty());
        let banks: Box<[DRAMBank]> = banks
            .iter()
            .map(|(base_address, size, access_mode)| DRAMBank::new(*base_address, *size, *access_mode))
            .sorted_by_key(|bank| bank.address_range.start)
            .collect_vec()
            .into_boxed_slice();
        for adjacent_banks in banks.windows(2) {
            assert!(intersection(&adjacent_banks[0].address_range, &adjacent_banks[1].address_range).is_none());
        }
        Self(banks)
    }

    fn find_bank_mut(&mut self, address: u32) -> Option<&mut DRAMBank> {
        let mut start = 0;
        let mut end = self.0.len();
        while start != end {
            let mid = start + (end - start) / 2;
            let bank = &mut self.0[mid];
            if address < bank.address_range.start {
                end = mid;
            } else if address >= bank.address_range.end {
                start = mid;
            } else {
                return Some(&mut self.0[mid]);
            }
            if mid == start {
                break;
            }
        }
        None
    }
}

impl Memory for DRAM {
    fn access<T: Copy>(&mut self, address: u32, access: MemoryAccess<'_, T>) -> bool {
        match self.find_bank_mut(address) {
            Some(bank) => {
                let access_granted = match &access {
                    MemoryAccess::Load(_) => bank.access_mode.contains(AccessMode::READ),
                    MemoryAccess::Store(_) => bank.access_mode.contains(AccessMode::WRITE),
                    MemoryAccess::Exec(_) => bank.access_mode.contains(AccessMode::EXECUTE),
                };
                let base_address = bank.address_range.start;
                access_granted && Memory::access(&mut **bank, address - base_address, access)
            },

            None => false,
        }
    }
}

struct DRAMBank {
    address_range: Range<u32>,
    memory: UnsafeCell<Box<[u8]>>,
    access_mode: AccessMode,
}

impl DRAMBank {
    fn new(base_address: u32, size: u32, access_mode: AccessMode) -> Self {
        Self {
            address_range: base_address .. base_address + size,
            memory: UnsafeCell::new(vec![0; size as usize].into_boxed_slice()),
            access_mode,
        }
    }
}

impl Deref for DRAMBank {
    type Target = [u8];

    fn deref(&self) -> &Self::Target {
        unsafe { &**self.memory.get() }
    }
}

impl DerefMut for DRAMBank {
    fn deref_mut(&mut self) -> &mut Self::Target {
        unsafe { &mut **self.memory.get() }
    }
}

impl Read for DRAMBank {
    fn read(&mut self, buf: &mut [u8]) -> io::Result<usize> {
        Cursor::new(&**self).read(buf)
    }
}

impl Write for DRAMBank {
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        Cursor::new(&mut **self).write(buf)
    }

    fn flush(&mut self) -> io::Result<()> {
        Ok(())
    }
}
