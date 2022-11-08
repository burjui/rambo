use crate::riscv_base::registers;
use crate::riscv_base::registers::REGISTER_COUNT;
use crate::riscv_exe;
use crate::riscv_exe::Executable;
use crate::riscv_exe::SimulatorConfig;
use crate::utils::stdout;
use crate::utils::GenericResult;
use riscv_emulator::cpu::Cpu;
use riscv_emulator::mmu::Mmu;
use risky::instructions::ebreak;
use std::convert::TryFrom;
use std::error::Error;
use std::io;
use std::io::Write;
use std::ops::Range;
use termcolor::Color;
use termcolor::ColorSpec;
use termcolor::StandardStream;
use termcolor::WriteColor;

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
    let mut cpu = riscv_exe::load(executable, &config)?;
    let ram_size = u64::try_from(executable.data.len())? + config.stack_size;
    let dump_simulator_state =
        |output: &mut StandardStream, cpu: &mut Cpu| -> Result<(), Box<dyn Error>> {
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
                writeln!(output, "{}", comment)?;
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

        if let Some((instruction, raw_instruction)) = cpu.tick() {
            if let DumpState::Instructions(output) | DumpState::Everything(output) = &mut dump_state
            {
                output.set_color(ColorSpec::new().set_fg(Some(Color::White)).set_bold(true))?;
                write!(output, "[0x{:08x}]", pc)?;
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

        let stack_pointer = u64::try_from(cpu.read_register(registers::SP)).unwrap();
        if stack_pointer < config.data_start_address + ram_size - config.stack_size {
            return Err("stack overflow".into());
        }
    }

    Ok(cpu)
}

fn dump_registers(output: &mut StandardStream, cpu: &Cpu) -> io::Result<()> {
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
    let stack_pointer = u64::try_from(cpu.read_register(registers::SP))?;
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
    writeln!(stderr, "{}:", title)?;
    stderr.reset()
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
            write!(output, "[{:08x}] ", address)?;
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
            .map_err(|_| format!("Failed to load a byte from address 0x{:08x}", address))?;
        write!(output, "{:02x}", byte)?;
        output.reset()?;
    }
    writeln!(output, "\n")?;
    Ok(())
}
