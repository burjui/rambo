use std::cell::UnsafeCell;
use std::error::Error;
use std::io::{Cursor, Read, Write};
use std::io;
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

use crate::utils::intersection;

#[test]
fn riscv_simulator() -> Result<(), Box<dyn Error>> {
    let mut dram = DRAM::new(&[
        (0, 1024, AccessMode::EXECUTE),
    ]);
    let code_bank = dram.find_bank_mut(0).unwrap();
    code_bank.write_all(&[0x73, 0x00, 0x10, 0x00])?; // EBREAK
    let mut clock = SimpleClock::new();
    let mut cpu_state = CpuState::new(0);
    let mut interpreter = Interp::new(&mut cpu_state, &mut dram, &mut clock);
    let (err, op) = interpreter.run();
    assert_eq!(err, CpuError::Ebreak);
    assert_eq!(op, Some(Op::Ebreak));
    Ok(())
}

bitflags! {
    struct AccessMode: u32 {
        const READ = 1 << 0;
        const WRITE = 1 << 1;
        const EXECUTE = 1 << 2;
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
        let index = match self.0.binary_search_by_key(&address, |bank| bank.address_range.start) {
            Ok(index) => index,
            Err(index) => index,
        };
        if index < self.0.len() {
            let bank = &mut self.0[index];
            if address >= bank.address_range.start && address < bank.address_range.end {
                return Some(bank)
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
                access_granted && Memory::access(&mut **bank, address, access)
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
