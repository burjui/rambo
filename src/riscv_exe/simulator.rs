use crate::riscv_base::registers;
use crate::riscv_exe::executable::Executable;
use crate::riscv_exe::RelocationKind;
use bitflags::bitflags;
use byteorder::LittleEndian;
use byteorder::ReadBytesExt;
use byteorder::WriteBytesExt;
use core::mem::swap;
use core::num::TryFromIntError;
use core::ops::Deref;
use core::ops::DerefMut;
use core::ops::Range;
use itertools::Itertools;
use rvsim::CpuError;
use rvsim::CpuState;
use rvsim::Interp;
use rvsim::Memory;
use rvsim::MemoryAccess;
use rvsim::Op;
use rvsim::SimpleClock;
use std::convert::TryFrom;
use std::error::Error;
use std::fmt;
use std::io;
use std::io::Cursor;
use std::io::Read;
use std::io::Write;

pub fn load(
    executable: &Executable,
    config: &SimulatorConfig,
) -> Result<Simulator, Box<dyn Error>> {
    let ram_size = config.stack_size + executable.data.len();
    let mut dram = Dram::new(&[
        (
            config.code_start_address,
            executable.code.len(),
            MemoryMode::EXECUTE,
        ),
        (
            config.data_start_address,
            executable.data.len() + ram_size,
            MemoryMode::READ | MemoryMode::WRITE,
        ),
    ]);

    let data_bank = dram.find_bank_mut(config.data_start_address).unwrap();
    data_bank.write_all(&executable.data)?;

    let code_bank = dram.find_bank_mut(config.code_start_address).unwrap();
    code_bank.write_all(&executable.code)?;

    for relocation in &executable.relocations {
        let target_offset = match relocation.kind {
            RelocationKind::Function => config.code_start_address,
            RelocationKind::DataLoad | RelocationKind::DataStore => config.data_start_address,
        };
        let target = (relocation.target + target_offset) as i32;
        let immediate = ui_immediate(target).unwrap();

        let offset = relocation.offset as usize;
        let u_instruction_range = offset..offset + 4;
        let mut u_instruction = (&code_bank[u_instruction_range.clone()])
            .read_u32::<LittleEndian>()
            .unwrap();
        u_instruction = u_instruction & 0x0000_0FFF | immediate.upper as u32;
        (&mut code_bank[u_instruction_range])
            .write_u32::<LittleEndian>(u_instruction)
            .unwrap();

        let next_instruction_range = offset + 4..offset + 8;
        let next_instruction = (&code_bank[next_instruction_range.clone()])
            .read_u32::<LittleEndian>()
            .unwrap();
        let next_instruction = match relocation.kind {
            RelocationKind::Function | RelocationKind::DataLoad => {
                next_instruction & 0x000F_FFFF | ((immediate.lower as u32) << 20)
            }
            RelocationKind::DataStore => {
                const LOW_MASK: u32 = 0b11111;
                const LOW_OFFSET: u32 = 7;
                const HIGH_MASK: u32 = 0b111_1111;
                const HIGH_OFFSET: u32 = 25;
                next_instruction & !(LOW_MASK << LOW_OFFSET | HIGH_MASK << HIGH_OFFSET)
                    | ((immediate.lower as u32 & LOW_MASK) << LOW_OFFSET)
                    | (((immediate.lower >> 5) as u32 & HIGH_MASK) << HIGH_OFFSET)
            }
        };

        (&mut code_bank[next_instruction_range])
            .write_u32::<LittleEndian>(next_instruction)
            .unwrap();
    }

    let mut simulator = Simulator::new(config.code_start_address, dram);
    simulator.cpu.x.iter_mut().for_each(|x| *x = 0xAAAA_AAAA);
    simulator.cpu.x[registers::ZERO as usize] = 0;
    simulator.cpu.x[registers::SP as usize] =
        u32::try_from(config.data_start_address + ram_size - 4).unwrap();
    simulator.cpu.x[registers::FP as usize] = simulator.cpu.x[registers::SP as usize];

    let code_end = config.code_start_address + executable.code.len();
    simulator.cpu.x[registers::RA as usize] = u32::try_from(code_end).unwrap();
    simulator.cpu.pc = u32::try_from(config.code_start_address + executable.entry).unwrap();

    Ok(simulator)
}

pub struct SimulatorConfig {
    pub stack_size: usize,
    pub code_start_address: usize,
    pub data_start_address: usize,
}

pub struct Simulator {
    pub clock: SimpleClock,
    pub dram: Dram,
    pub cpu: CpuState,
}

impl Simulator {
    pub fn new(pc: usize, dram: Dram) -> Self {
        Self {
            clock: SimpleClock::new(),
            dram,
            cpu: CpuState::new(u32::try_from(pc).unwrap()),
        }
    }

    pub fn step(&mut self) -> Result<Op, (CpuError, Option<Op>)> {
        Interp::new(&mut self.cpu, &mut self.dram, &mut self.clock).step()
    }
}

bitflags! {
    pub struct MemoryMode: u8 {
        const READ = 1;
        const WRITE = 1 << 1;
        const EXECUTE = 1 << 2;
    }
}

impl fmt::Display for MemoryMode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}{}{}",
            if self.contains(Self::READ) { "r" } else { "" },
            if self.contains(Self::WRITE) { "w" } else { "" },
            if self.contains(Self::EXECUTE) {
                "x"
            } else {
                ""
            }
        )
    }
}

pub struct Dram(Box<[DramBank]>);

impl Dram {
    pub fn new(banks: &[(usize, usize, MemoryMode)]) -> Self {
        assert!(!banks.is_empty());
        let banks: Box<[DramBank]> = banks
            .iter()
            .map(|(base_address, size, access_mode)| {
                DramBank::new(*base_address, *size, *access_mode)
            })
            .sorted_by_key(|bank| bank.address_range.start)
            .collect_vec()
            .into_boxed_slice();
        for adjacent_banks in banks.windows(2) {
            let address_range1 = &adjacent_banks[0].address_range;
            let address_range2 = &adjacent_banks[1].address_range;
            assert!(
                intersection(address_range1, address_range2).is_none(),
                "bank address ranges intersect: 0x{:08x} .. 0x{:08x} and 0x{:08x} .. 0x{:08x}",
                address_range1.start,
                address_range1.end,
                address_range2.start,
                address_range2.end
            );
        }
        Self(banks)
    }

    pub fn find_bank_mut(&mut self, address: usize) -> Option<&mut DramBank> {
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

impl Memory for Dram {
    fn access<T: Copy>(&mut self, address: u32, access: MemoryAccess<'_, T>) -> bool {
        match self.find_bank_mut(usize::try_from(address).unwrap()) {
            Some(bank) => {
                let access_granted = match &access {
                    MemoryAccess::Load(_) => bank.access_mode.contains(MemoryMode::READ),
                    MemoryAccess::Store(_) => bank.access_mode.contains(MemoryMode::WRITE),
                    MemoryAccess::Exec(_) => bank.access_mode.contains(MemoryMode::EXECUTE),
                };
                let base_address = bank.address_range.start;
                access_granted
                    && Memory::access(
                        &mut **bank,
                        address - u32::try_from(base_address).unwrap(),
                        access,
                    )
            }

            None => false,
        }
    }
}

pub struct DramBank {
    address_range: Range<usize>,
    memory: Box<[u8]>,
    access_mode: MemoryMode,
}

impl DramBank {
    pub fn new(base_address: usize, size: usize, access_mode: MemoryMode) -> Self {
        Self {
            address_range: base_address..base_address + size,
            memory: vec![0; size as usize].into_boxed_slice(),
            access_mode,
        }
    }
}

impl Deref for DramBank {
    type Target = [u8];

    fn deref(&self) -> &Self::Target {
        &self.memory
    }
}

impl DerefMut for DramBank {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.memory
    }
}

impl Read for DramBank {
    fn read(&mut self, buf: &mut [u8]) -> io::Result<usize> {
        Cursor::new(&**self).read(buf)
    }
}

impl Write for DramBank {
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        Cursor::new(&mut **self).write(buf)
    }

    fn flush(&mut self) -> io::Result<()> {
        Ok(())
    }
}

fn intersection<'a, T: Ord + Copy>(mut r1: &'a Range<T>, mut r2: &'a Range<T>) -> Option<Range<T>> {
    if r1.start > r2.start {
        swap(&mut r1, &mut r2);
    }
    if r1.end <= r2.start {
        None
    } else {
        Some(r1.start.max(r2.start)..r1.end.min(r2.end))
    }
}

#[test]
fn range_intersection() {
    test_intersection(0..10, 10..20, None);
    test_intersection(5..15, 10..20, Some(10..15));
    test_intersection(10..20, 12..15, Some(12..15));
}

#[cfg(test)]
fn test_intersection(range1: Range<u32>, range2: Range<u32>, expected_result: Option<Range<u32>>) {
    assert_eq!(intersection(&range1, &range2), expected_result);
    assert_eq!(intersection(&range2, &range1), expected_result);
}

struct ImmediateI {
    upper: i32,
    lower: i16,
}

fn ui_immediate(value: i32) -> Result<ImmediateI, TryFromIntError> {
    if -(1 << 11) <= value && value < 1 << 11 {
        Ok(ImmediateI {
            upper: 0,
            lower: i16::try_from(value)?,
        })
    } else {
        let value = value as u32;
        let value_rounded = value & !((1 << 12) - 1);
        let offset = value - value_rounded;
        let (upper, lower) = if offset < (1 << 11) {
            (value_rounded as i32, i16::try_from(offset)?)
        } else {
            let value_rounded = value_rounded + (1 << 12);
            (value_rounded as i32, -i16::try_from(value_rounded - value)?)
        };
        Ok(ImmediateI { upper, lower })
    }
}
