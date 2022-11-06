use crate::riscv_base::registers;
use crate::riscv_base::registers::REGISTER_COUNT;
use crate::riscv_exe::executable::Executable;
use crate::riscv_exe::RelocationKind;
use crate::utils::GenericResult;
use byteorder::LittleEndian;
use byteorder::ReadBytesExt;
use byteorder::WriteBytesExt;
use core::num::TryFromIntError;
use riscv_emulator::cpu::Cpu;
use riscv_emulator::mmu::MemoryAccessFlags;
use std::convert::TryFrom;

pub fn load(executable: &Executable, config: &SimulatorConfig) -> GenericResult<Cpu> {
    let mut cpu = Cpu::new();

    let mut code = executable.code.clone();

    for relocation in &executable.relocations {
        let target_offset = match relocation.kind {
            RelocationKind::Function => config.code_start_address,
            RelocationKind::DataLoad | RelocationKind::DataStore => config.data_start_address,
        };
        let target = (relocation.target + target_offset) as i32;
        let immediate = ui_immediate(target).unwrap();

        let offset = relocation.offset as usize;
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

    for register in 0..u8::try_from(REGISTER_COUNT)? {
        cpu.write_register(register, 0xAAAA_AAAA);
    }
    cpu.write_register(registers::ZERO, 0);
    cpu.write_register(
        registers::SP,
        i64::try_from(config.data_start_address)? + i64::try_from(ram_size)? - 4,
    );
    cpu.write_register(registers::FP, cpu.read_register(registers::SP));

    let code_end =
        i64::try_from(config.code_start_address)? + i64::try_from(executable.code.len())?;
    cpu.write_register(registers::RA, code_end);
    cpu.update_pc(config.code_start_address + executable.entry);

    Ok(cpu)
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
