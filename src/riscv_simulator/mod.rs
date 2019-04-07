use std::cell::UnsafeCell;
use std::convert::TryFrom;
use std::error::Error;
use std::fmt;
use std::io;
use std::io::{Cursor, Read, Write};
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
use termcolor::{Color, ColorSpec, WriteColor};

use crate::riscv_backend::{registers, RICSVImage};
use crate::utils::{intersection, stderr};

#[cfg(test)]
mod tests;

#[derive(PartialOrd, PartialEq)]
pub(crate) enum DumpState {
    None,
    Instructions,
    Everything
}

pub(crate) fn run(image: &RICSVImage, dump_state: DumpState) -> Result<Simulator, Box<dyn Error>> {
    let mut dram = DRAM::new(&[
        (image.code_base_address, u32::try_from(image.code.len())?, AccessMode::EXECUTE),
        (image.ram_base_address, u32::try_from(image.data.len() + 10240)?, AccessMode::READ | AccessMode::WRITE),
    ]);
    dram.find_bank_mut(image.code_base_address).unwrap().write_all(&image.code)?;
    dram.find_bank_mut(image.ram_base_address).unwrap().write_all(&image.data)?;

    let mut simulator = Simulator::new(image.code_base_address, dram);
    let stderr = &mut stderr();
    loop {
        let pc = simulator.cpu.pc;
        match simulator.step() {
            Ok(op) => {
                if dump_state >= DumpState::Instructions {
                    stderr.set_color(ColorSpec::new()
                        .set_fg(Some(Color::White))
                        .set_bold(true))?;
                    write!(stderr, "[0x{:08}]", pc)?;
                    stderr.reset()?;
                    writeln!(stderr, " {:?}", op)?;
                }

                if dump_state >= DumpState::Everything {
                    dump_registers(&simulator.cpu)?;

                    let data_bank = simulator.dram.find_bank_mut(image.ram_base_address).unwrap();
                    dump_ram(&data_bank[.. image.data.len()], image.ram_base_address)?;
                }
            },

            Err((error, op)) => {
                match error {
                    CpuError::Ebreak => break,
                    _ => return Err(format!("op: {:?}, error: {:?}", op, error).into()),
                }
            },
        }
    }
    Ok(simulator)
}

fn dump_registers(cpu: &CpuState) -> io::Result<()> {
    const REGISTERS_PER_ROW: usize = 4;
    const REGISTERS_PER_COLUMN: usize = registers::COUNT / REGISTERS_PER_ROW;

    let stderr = &mut stderr();
    for i in 0 .. registers::COUNT {
        if i % REGISTERS_PER_ROW == 0 {
            if i > 0 {
                writeln!(stderr)?;
            }
        } else {
            write!(stderr, "  |  ")?;
        }

        let register = (i % REGISTERS_PER_ROW) * REGISTERS_PER_COLUMN + i / REGISTERS_PER_ROW;
        write!(stderr, "x{:<2}", register)?;
        stderr.set_color(ColorSpec::new()
            .set_fg(Some(Color::Black))
            .set_intense(true))?;
        write!(stderr, "  0x{:08x}", cpu.x[register])?;
        stderr.reset()?;
    }
    writeln!(stderr, "\n")
}

fn dump_ram(ram: &[u8], base_address: u32) -> io::Result<()> {
    let stderr = &mut stderr();
    for index in 0 .. ram.len() {
        if index % 4 == 0 {
            if index > 0 {
                writeln!(stderr)?;
            }
            write!(stderr, "[{:08x}] ", base_address + index as u32)?;
        } else {
            write!(stderr, " ")?;
        }
        stderr.set_color(ColorSpec::new()
            .set_fg(Some(Color::Black))
            .set_intense(true))?;
        write!(stderr, "{:02x}", ram[index])?;
        stderr.reset()?;
    }
    writeln!(stderr, "\n")
}

pub(crate) struct Simulator {
    pub(crate) clock: SimpleClock,
    pub(crate) dram: DRAM,
    pub(crate) cpu: CpuState,
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
}

bitflags! {
    pub(crate) struct AccessMode: u32 {
        const READ = 1;
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

pub(crate) struct DRAM(Box<[DRAMBank]>);

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
            let address_range1 = &adjacent_banks[0].address_range;
            let address_range2 = &adjacent_banks[1].address_range;
            assert!(intersection(address_range1, address_range2).is_none(),
                "bank address ranges intersect: 0x{:08x} .. 0x{:08x} and 0x{:08x} .. 0x{:08x}",
                address_range1.start, address_range1.end, address_range2.start, address_range2.end);
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
