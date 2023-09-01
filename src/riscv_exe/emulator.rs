use core::num::TryFromIntError;
use std::{convert::TryFrom, io, io::Write, ops::Range};

use byteorder::{LittleEndian, ReadBytesExt, WriteBytesExt};
use riscv_emulator::{
    cpu::Cpu,
    mmu::{MemoryAccessFlags, Mmu},
};
use risky::{
    abi::{FP, RA, SP, ZERO},
    registers::NUMBER_OF_REGISTERS,
    rv32i::ebreak,
};
use termcolor::{Color, ColorSpec, StandardStream, WriteColor};

use crate::{
    riscv_exe::{executable::Executable, RelocationKind},
    utils::{stdout, GenericResult},
};

#[allow(clippy::cast_sign_loss)]
pub fn load(executable: &Executable, config: &SimulatorConfig) -> GenericResult<Cpu> {
    let mut cpu = Cpu::new();
    let mut code = executable.code.clone();

    for relocation in &executable.relocations {
        let target_offset = match relocation.kind {
            RelocationKind::Function => config.code_start_address,
            RelocationKind::DataLoad | RelocationKind::DataStore => config.data_start_address,
        };
        let target = i32::try_from(relocation.target + target_offset)?;
        let immediate = ui_immediate(target).unwrap();

        let offset = usize::try_from(relocation.offset)?;
        let u_instruction_range = offset..offset + 4;
        let mut u_instruction = (&code[u_instruction_range.clone()])
            .read_u32::<LittleEndian>()
            .unwrap();
        u_instruction = u_instruction & 0x0000_0FFF | immediate.upper as u32;
        (&mut code[u_instruction_range])
            .write_u32::<LittleEndian>(u_instruction)
            .unwrap();

        let next_instruction_range = offset + 4..offset + 8;
        let next_instruction = (&code[next_instruction_range.clone()])
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

        (&mut code[next_instruction_range])
            .write_u32::<LittleEndian>(next_instruction)
            .unwrap();
    }

    cpu.mmu_mut().add(
        config.code_start_address,
        u64::try_from(code.len())?,
        MemoryAccessFlags::READ,
        Some(&[(0u64, code.as_slice())]),
    );

    let ram_size = u64::try_from(executable.data.len())? + config.stack_size;
    cpu.mmu_mut().add(
        config.data_start_address,
        ram_size,
        MemoryAccessFlags::READ | MemoryAccessFlags::WRITE,
        Some(&[(0u64, executable.data.as_slice())]),
    );

    for register in 0..u8::try_from(NUMBER_OF_REGISTERS)? {
        cpu.write_register(register, 0xAAAA_AAAA);
    }
    cpu.write_register(ZERO.into(), 0);
    cpu.write_register(
        SP.into(),
        i64::try_from(config.data_start_address)? + i64::try_from(ram_size)? - 4,
    );
    cpu.write_register(FP.into(), cpu.read_register(SP.into()));

    let code_end =
        i64::try_from(config.code_start_address)? + i64::try_from(executable.code.len())?;
    cpu.write_register(RA.into(), code_end);
    cpu.update_pc(config.code_start_address + executable.entry);

    Ok(cpu)
}

pub(crate) enum DumpState<'a> {
    None,
    Instructions(&'a mut StandardStream),
    Everything(&'a mut StandardStream),
}

pub(crate) fn run(executable: &Executable, mut dump_state: DumpState<'_>) -> GenericResult<Cpu> {
    let config = SimulatorConfig {
        stack_size: 1024,
        code_start_address: 0x2000_0000,
        data_start_address: 0x4000_0000,
    };
    let mut cpu = load(executable, &config)?;
    let ram_size = u64::try_from(executable.data.len())? + config.stack_size;
    let dump_simulator_state = |output: &mut StandardStream, cpu: &mut Cpu| -> GenericResult<()> {
        dump_registers(output, cpu)?;

        if !executable.data.is_empty() {
            write_title(output, "RAM")?;
            dump_ram(
                output,
                cpu.mmu_mut(),
                config.data_start_address
                    ..config.data_start_address + u64::try_from(executable.data.len())?,
            )?;
        }

        dump_stack(cpu, config.data_start_address, ram_size)?;
        Ok(())
    };

    let code_end = config.code_start_address + u64::try_from(executable.code.len())?;
    loop {
        if let DumpState::Everything(output) = &mut dump_state {
            dump_simulator_state(output, &mut cpu)?;
        }

        if let DumpState::Instructions(output) | DumpState::Everything(output) = &mut dump_state {
            let current_code_offset = cpu.read_pc() - config.code_start_address;
            if let Some(comment) = executable.get_comment_at(current_code_offset) {
                output.set_color(
                    ColorSpec::new()
                        .set_fg(Some(Color::Black))
                        .set_intense(true),
                )?;
                writeln!(output, "{comment}")?;
            }
        }

        let pc = cpu.read_pc();
        if pc == code_end {
            break;
        }

        let raw_instruction = cpu.mmu_mut().load_word_raw(pc);
        if raw_instruction == ebreak() {
            break;
        }

        // Note: there's no point in printing raw_instruction before cpu.tick(),
        // since the latter will panic and print it anyway
        if let Some((instruction, raw_instruction)) = cpu.tick() {
            if let DumpState::Instructions(output) | DumpState::Everything(output) = &mut dump_state
            {
                output.set_color(ColorSpec::new().set_fg(Some(Color::White)).set_bold(true))?;
                write!(output, "[0x{pc:08x}]")?;
                output.reset()?;
                writeln!(
                    output,
                    " {} {}",
                    instruction.name,
                    (instruction.disassemble)(raw_instruction, pc, None)
                )?;
                output.flush()?;
            }
        }

        let stack_pointer = u64::try_from(cpu.read_register(SP.into())).unwrap();
        if stack_pointer < config.data_start_address + ram_size - config.stack_size {
            return Err("stack overflow".into());
        }
    }

    Ok(cpu)
}

pub(crate) fn dump_ram(
    output: &mut StandardStream,
    mmu: &mut Mmu,
    range: Range<u64>,
) -> GenericResult<()> {
    for (index, address) in range.enumerate() {
        if index % 4 == 0 {
            if index > 0 {
                writeln!(output)?;
            }
            write!(output, "[{address:08x}] ")?;
        } else {
            write!(output, " ")?;
        }
        output.set_color(
            ColorSpec::new()
                .set_fg(Some(Color::Black))
                .set_intense(true),
        )?;
        let byte = mmu
            .load(address)
            .map_err(|_| format!("Failed to load a byte from address 0x{address:08x}"))?;
        write!(output, "{byte:02x}")?;
        output.reset()?;
    }
    writeln!(output, "\n")?;
    Ok(())
}

pub struct SimulatorConfig {
    pub stack_size: u64,
    pub code_start_address: u64,
    pub data_start_address: u64,
}

struct ImmediateI {
    upper: i32,
    lower: i16,
}

#[allow(clippy::cast_sign_loss, clippy::cast_possible_wrap)]
fn ui_immediate(value: i32) -> Result<ImmediateI, TryFromIntError> {
    if (-(1 << 11)..1 << 11).contains(&value) {
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

fn dump_registers(output: &mut StandardStream, cpu: &Cpu) -> io::Result<()> {
    const REGISTERS_PER_ROW: usize = 4;
    const REGISTERS_PER_COLUMN: usize = NUMBER_OF_REGISTERS / REGISTERS_PER_ROW;

    for i in 0..NUMBER_OF_REGISTERS {
        if i % REGISTERS_PER_ROW == 0 {
            if i > 0 {
                writeln!(output)?;
            }
        } else {
            write!(output, "  |  ")?;
        }

        let register = (i % REGISTERS_PER_ROW) * REGISTERS_PER_COLUMN + i / REGISTERS_PER_ROW;
        write!(output, "x{register:<2}")?;
        output.set_color(
            ColorSpec::new()
                .set_fg(Some(Color::Black))
                .set_intense(true),
        )?;
        write!(
            output,
            "  0x{:08x}",
            cpu.read_register(u8::try_from(register).unwrap())
        )?;
        output.reset()?;
    }
    writeln!(output, "\n")
}

fn dump_stack(cpu: &mut Cpu, ram_base_address: u64, ram_size: u64) -> GenericResult<()> {
    let stdout = &mut stdout();
    write_title(stdout, "STACK")?;
    let stack_pointer = u64::try_from(cpu.read_register(SP.into()))?;
    dump_ram(
        stdout,
        cpu.mmu_mut(),
        stack_pointer..ram_base_address + ram_size,
    )
}

fn write_title(stderr: &mut StandardStream, title: &str) -> io::Result<()> {
    stderr.set_color(
        ColorSpec::new()
            .set_fg(Some(Color::Yellow))
            .set_intense(false),
    )?;
    writeln!(stderr, "{title}:")?;
    stderr.reset()
}
