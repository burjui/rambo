use bitflags::bitflags;
use byteorder::ReadBytesExt;
use byteorder::WriteBytesExt;
use itertools::Itertools;
use riscv::registers;
use riscv::registers::REGISTER_COUNT;
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
use std::ops::Deref;
use std::ops::DerefMut;
use std::ops::Range;
use termcolor::Color;
use termcolor::ColorSpec;
use termcolor::StandardStream;
use termcolor::WriteColor;

use crate::riscv_backend::RICSVImage;
use crate::riscv_backend::{ui_immediate, RelocationKind};
use crate::utils::intersection;
use crate::utils::stdout;

#[cfg(test)]
mod tests;

pub(crate) enum DumpState<'a> {
    None,
    Instructions(&'a mut StandardStream),
    Everything(&'a mut StandardStream),
}

pub(crate) fn run(
    image: &RICSVImage,
    mut dump_state: DumpState<'_>,
) -> Result<Simulator, Box<dyn Error>> {
    const STACK_SIZE: u32 = 1024;
    const CODE_START_ADDRESS: u32 = 0x2000_0000;
    const DATA_START_ADDRESS: u32 = 0x8000_0000;

    let ram_size = STACK_SIZE + u32::try_from(image.data.len()).unwrap();
    let mut dram = DRAM::new(&[
        (
            CODE_START_ADDRESS,
            u32::try_from(image.code.len())?,
            AccessMode::EXECUTE,
        ),
        (
            DATA_START_ADDRESS,
            u32::try_from(image.data.len())? + ram_size,
            AccessMode::READ | AccessMode::WRITE,
        ),
    ]);

    let data_bank = dram.find_bank_mut(DATA_START_ADDRESS).unwrap();
    data_bank.write_all(&image.data)?;

    let code_bank = dram.find_bank_mut(CODE_START_ADDRESS).unwrap();
    code_bank.write_all(&image.code)?;

    for relocation in &image.relocations {
        let target_offset = match relocation.kind {
            RelocationKind::Function => CODE_START_ADDRESS,
            RelocationKind::DataLoad | RelocationKind::DataStore => DATA_START_ADDRESS,
        };
        let target = (relocation.target + target_offset) as i32;
        let immediate = ui_immediate(target).unwrap();

        let offset = relocation.offset as usize;
        let u_instruction_range = offset..offset + 4;
        let mut u_instruction = (&code_bank[u_instruction_range.clone()])
            .read_u32::<riscv::InstructionByteOrder>()
            .unwrap();
        u_instruction = u_instruction & 0x0000_0FFF | immediate.upper as u32;
        (&mut code_bank[u_instruction_range])
            .write_u32::<riscv::InstructionByteOrder>(u_instruction)
            .unwrap();

        let next_instruction_range = offset + 4..offset + 8;
        let next_instruction = (&code_bank[next_instruction_range.clone()])
            .read_u32::<riscv::InstructionByteOrder>()
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
            .write_u32::<riscv::InstructionByteOrder>(next_instruction)
            .unwrap();
    }

    let stdout = &mut stdout();
    let mut simulator = Simulator::new(CODE_START_ADDRESS, dram);
    simulator.cpu.x.iter_mut().for_each(|x| *x = 0xAAAA_AAAA);
    simulator.cpu.x[registers::ZERO as usize] = 0;
    simulator.cpu.x[registers::SP as usize] = DATA_START_ADDRESS + ram_size - 4;
    simulator.cpu.x[registers::FP as usize] = simulator.cpu.x[registers::SP as usize];
    simulator.cpu.x[registers::GP as usize] = DATA_START_ADDRESS;

    let dump_simulator_state =
        |output: &mut StandardStream, simulator: &mut Simulator| -> Result<(), Box<dyn Error>> {
            dump_registers(output, &simulator.cpu)?;

            let data_bank = simulator.dram.find_bank_mut(DATA_START_ADDRESS).unwrap();
            if !image.data.is_empty() {
                write_title(output, "RAM")?;
                dump_memory(output, &data_bank[..image.data.len()], DATA_START_ADDRESS)?;
            }

            dump_stack(&simulator.cpu, &data_bank, DATA_START_ADDRESS, ram_size)?;
            Ok(())
        };

    let code_end = CODE_START_ADDRESS + u32::try_from(image.code.len()).unwrap();
    simulator.cpu.x[registers::RA as usize] = code_end;
    simulator.cpu.pc = CODE_START_ADDRESS + image.entry;
    loop {
        if let DumpState::Everything(output) = &mut dump_state {
            dump_simulator_state(*output, &mut simulator)?;
        }

        if let DumpState::Instructions(output) | DumpState::Everything(output) = &mut dump_state {
            if let Some(comments) = image.comments.find(simulator.cpu.pc - CODE_START_ADDRESS) {
                for comment in comments {
                    output.set_color(
                        ColorSpec::new()
                            .set_fg(Some(Color::Black))
                            .set_intense(true),
                    )?;
                    writeln!(output, "// {}", comment)?;
                }
            }
        }

        let pc = simulator.cpu.pc;
        if pc == code_end {
            break;
        }

        match simulator.step() {
            Ok(op) => {
                if let DumpState::Instructions(output) | DumpState::Everything(output) =
                    &mut dump_state
                {
                    output.set_color(ColorSpec::new().set_fg(Some(Color::White)).set_bold(true))?;
                    write!(output, "[0x{:08x}]", pc)?;
                    output.reset()?;
                    writeln!(output, " {:?}", riscv::decoder::Op(op))?;
                    output.flush()?;
                }
            }

            Err((error, op)) => match error {
                CpuError::Ebreak => break,
                CpuError::Ecall if simulator.cpu.x[registers::A7 as usize] == 93 => break,
                _ => {
                    dump_registers(stdout, &simulator.cpu)?;
                    let data_bank = simulator.dram.find_bank_mut(DATA_START_ADDRESS).unwrap();
                    dump_stack(&simulator.cpu, &data_bank, DATA_START_ADDRESS, ram_size)?;
                    write_title(stdout, "RAM")?;
                    dump_memory(stdout, &data_bank, DATA_START_ADDRESS)?;
                    return Err(format!(
                        "[0x{:08x}] op: {:?}, error: {:?}",
                        pc,
                        op.map(riscv::decoder::Op),
                        error
                    )
                    .into());
                }
            },
        }

        if simulator.cpu.x[registers::SP as usize] < DATA_START_ADDRESS + ram_size - STACK_SIZE {
            return Err("stack overflow".into());
        }

        simulator.cpu.x[registers::ZERO as usize] = 0;
    }

    Ok(simulator)
}

fn dump_registers(output: &mut StandardStream, cpu: &CpuState) -> io::Result<()> {
    const REGISTERS_PER_ROW: usize = 4;
    const REGISTERS_PER_COLUMN: usize = REGISTER_COUNT / REGISTERS_PER_ROW;

    for i in 0..REGISTER_COUNT {
        if i % REGISTERS_PER_ROW == 0 {
            if i > 0 {
                writeln!(output)?;
            }
        } else {
            write!(output, "  |  ")?;
        }

        let register = (i % REGISTERS_PER_ROW) * REGISTERS_PER_COLUMN + i / REGISTERS_PER_ROW;
        write!(output, "x{:<2}", register)?;
        output.set_color(
            ColorSpec::new()
                .set_fg(Some(Color::Black))
                .set_intense(true),
        )?;
        write!(output, "  0x{:08x}", cpu.x[register])?;
        output.reset()?;
    }
    writeln!(output, "\n")
}

fn dump_stack(
    cpu: &CpuState,
    ram: &DRAMBank,
    ram_base_address: u32,
    ram_size: u32,
) -> io::Result<()> {
    let stdout = &mut stdout();
    write_title(stdout, "STACK")?;
    let stack_pointer = cpu.x[registers::SP as usize];
    let stack_offset = stack_pointer - ram_base_address;
    dump_memory(
        stdout,
        &ram[stack_offset as usize..ram_size as usize],
        stack_pointer,
    )
}

fn write_title(stderr: &mut StandardStream, title: &str) -> io::Result<()> {
    stderr.set_color(
        ColorSpec::new()
            .set_fg(Some(Color::Yellow))
            .set_intense(false),
    )?;
    writeln!(stderr, "{}:", title)?;
    stderr.reset()
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

pub(crate) struct DRAM(Box<[DRAMBank]>);

impl DRAM {
    fn new(banks: &[(u32, u32, AccessMode)]) -> Self {
        assert!(!banks.is_empty());
        let banks: Box<[DRAMBank]> = banks
            .iter()
            .map(|(base_address, size, access_mode)| {
                DRAMBank::new(*base_address, *size, *access_mode)
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
            }

            None => false,
        }
    }
}

struct DRAMBank {
    address_range: Range<u32>,
    memory: Box<[u8]>,
    access_mode: AccessMode,
}

impl DRAMBank {
    fn new(base_address: u32, size: u32, access_mode: AccessMode) -> Self {
        Self {
            address_range: base_address..base_address + size,
            memory: vec![0; size as usize].into_boxed_slice(),
            access_mode,
        }
    }
}

impl Deref for DRAMBank {
    type Target = [u8];

    fn deref(&self) -> &Self::Target {
        &self.memory
    }
}

impl DerefMut for DRAMBank {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.memory
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

pub(crate) fn dump_memory(
    output: &mut StandardStream,
    ram: &[u8],
    base_address: u32,
) -> io::Result<()> {
    for (index, &byte) in ram.iter().enumerate() {
        if index % 4 == 0 {
            if index > 0 {
                writeln!(output)?;
            }
            write!(output, "[{:08x}] ", base_address + index as u32)?;
        } else {
            write!(output, " ")?;
        }
        output.set_color(
            ColorSpec::new()
                .set_fg(Some(Color::Black))
                .set_intense(true),
        )?;
        write!(output, "{:02x}", byte)?;
        output.reset()?;
    }
    writeln!(output, "\n")
}
