use riscv::registers;
use riscv::registers::REGISTER_COUNT;
use rvsim::CpuError;
use rvsim::CpuState;
use std::convert::TryFrom;
use std::error::Error;
use std::io;
use std::io::Write;
use termcolor::Color;
use termcolor::ColorSpec;
use termcolor::StandardStream;
use termcolor::WriteColor;

use crate::utils::stdout;
use rambo_riscv::DRAMBank;
use rambo_riscv::Executable;
use rambo_riscv::Simulator;
use rambo_riscv::SimulatorConfig;

#[cfg(test)]
mod tests;

pub(crate) enum DumpState<'a> {
    None,
    Instructions(&'a mut StandardStream),
    Everything(&'a mut StandardStream),
}

pub(crate) fn run(
    executable: &Executable,
    mut dump_state: DumpState<'_>,
) -> Result<Simulator, Box<dyn Error>> {
    let config = SimulatorConfig {
        stack_size: 1024,
        code_start_address: 0x2000_0000,
        data_start_address: 0x8000_0000,
    };
    let mut simulator = rambo_riscv::load(executable, &config)?;

    let ram_size = config.stack_size + executable.data.len();
    let stdout = &mut stdout();
    let dump_simulator_state =
        |output: &mut StandardStream, simulator: &mut Simulator| -> Result<(), Box<dyn Error>> {
            dump_registers(output, &simulator.cpu)?;

            let data_bank = simulator
                .dram
                .find_bank_mut(config.data_start_address)
                .unwrap();
            if !executable.data.is_empty() {
                write_title(output, "RAM")?;
                dump_memory(
                    output,
                    &data_bank[..executable.data.len()],
                    config.data_start_address,
                )?;
            }

            dump_stack(
                &simulator.cpu,
                &data_bank,
                config.data_start_address,
                ram_size,
            )?;
            Ok(())
        };

    let code_end = config.code_start_address + executable.code.len();
    loop {
        if let DumpState::Everything(output) = &mut dump_state {
            dump_simulator_state(*output, &mut simulator)?;
        }

        if let DumpState::Instructions(output) | DumpState::Everything(output) = &mut dump_state {
            let current_code_offset =
                usize::try_from(simulator.cpu.pc).unwrap() - config.code_start_address;
            if let Some(comment) = executable.comment_at(current_code_offset) {
                output.set_color(
                    ColorSpec::new()
                        .set_fg(Some(Color::Black))
                        .set_intense(true),
                )?;
                writeln!(output, "{}", comment)?;
            }
        }

        let pc = simulator.cpu.pc;
        if usize::try_from(pc).unwrap() == code_end {
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
                    let data_bank = simulator
                        .dram
                        .find_bank_mut(config.data_start_address)
                        .unwrap();
                    dump_stack(
                        &simulator.cpu,
                        &data_bank,
                        config.data_start_address,
                        ram_size,
                    )?;
                    write_title(stdout, "RAM")?;
                    dump_memory(stdout, &data_bank, config.data_start_address)?;
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

        let stack_pointer = usize::try_from(simulator.cpu.x[registers::SP as usize]).unwrap();
        if stack_pointer < config.data_start_address + ram_size - config.stack_size {
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
    ram_base_address: usize,
    ram_size: usize,
) -> io::Result<()> {
    let stdout = &mut stdout();
    write_title(stdout, "STACK")?;
    let stack_pointer = usize::try_from(cpu.x[usize::from(registers::SP)]).unwrap();
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

pub(crate) fn dump_memory(
    output: &mut StandardStream,
    ram: &[u8],
    base_address: usize,
) -> io::Result<()> {
    for (index, &byte) in ram.iter().enumerate() {
        if index % 4 == 0 {
            if index > 0 {
                writeln!(output)?;
            }
            write!(output, "[{:08x}] ", base_address + index)?;
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
