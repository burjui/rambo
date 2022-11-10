use crate::ir::value_storage::ValueId;
use crate::ir::FnId;
use crate::ir::IRModule;
use crate::ir::Phi;
use crate::ir::Statement;
use crate::ir::Value;
use crate::riscv_exe::Executable;
use crate::riscv_exe::Relocation;
use crate::riscv_exe::RelocationKind;
use crate::stable_graph::Direction;
use crate::stable_graph::NodeIndex;
use crate::utils::function;
use crate::utils::impl_deref_for_newtype;
use crate::utils::GenericResult;
use crate::utils::VecUtils;
use bimap::BiMap;
use byteorder::LittleEndian;
use byteorder::ReadBytesExt;
use byteorder::WriteBytesExt;
use itertools::Itertools;
use riscv_emulator::cpu::Cpu;
use risky::abi::*;
use risky::instructions::*;
use risky::registers::*;
use rustc_hash::FxHashMap;
use smallvec::SmallVec;
use std::collections::btree_set::BTreeSet;
use std::convert::TryFrom;
use std::error::Error;
use std::fmt;
use std::io::Cursor;
use std::io::Write;
use std::mem::replace;
use std::num::TryFromIntError;
use termcolor::Color;
use termcolor::ColorSpec;
use termcolor::StandardStream;
use termcolor::WriteColor;

#[cfg(test)]
mod tests;

// FIXME Dumping code should be separated from code generation
pub(crate) enum DumpCode<'a> {
    No,
    Yes(&'a mut StandardStream),
}

pub(crate) struct EnableImmediateIntegers(pub(crate) bool);
impl_deref_for_newtype!(EnableImmediateIntegers, bool);

pub(crate) struct EnableComments(pub(crate) bool);
impl_deref_for_newtype!(EnableComments, bool);

pub(crate) fn generate(
    module: &IRModule,
    dump_code: DumpCode<'_>,
    enable_immediate_integers: EnableImmediateIntegers,
    enable_comments: EnableComments,
) -> GenericResult<Executable> {
    let mut state = SharedState::new();
    let backend = Backend::new(
        &mut state,
        module,
        dump_code,
        enable_immediate_integers,
        enable_comments,
    );
    backend.generate()?;
    for (offset, fn_id) in &state.function_relocations {
        let target = state.function_offsets[fn_id];
        state
            .image
            .relocations
            .push(Relocation::new(*offset, target, RelocationKind::Function));
    }
    Ok(state.image)
}

struct SharedState {
    image: Executable,
    function_offsets: FxHashMap<FnId, u64>,
    function_relocations: Vec<(u64, FnId)>,
}

impl SharedState {
    fn new() -> Self {
        Self {
            image: Executable::new(),
            function_offsets: FxHashMap::default(),
            function_relocations: Vec::new(),
        }
    }
}

struct Backend<'a, 'output> {
    state: &'a mut SharedState,
    module: &'a IRModule,
    dump_code: DumpCode<'output>,
    enable_immediate_integers: bool,
    enable_comments: bool,
    registers: RegisterAllocator,
    data_offsets: FxHashMap<ValueId, u64>,
    no_spill: SmallVec<[Register; REGISTER_COUNT]>,
    function_id: Option<FnId>,
    phis: FxHashMap<ValueId, BTreeSet<(NodeIndex, ValueId)>>,
}

impl<'a: 'output, 'output> Backend<'a, 'output> {
    fn new(
        state: &'a mut SharedState,
        module: &'a IRModule,
        dump_code: DumpCode<'a>,
        EnableImmediateIntegers(enable_immediate_integers): EnableImmediateIntegers,
        EnableComments(enable_comments): EnableComments,
    ) -> Self {
        Self {
            state,
            module,
            dump_code,
            enable_immediate_integers,
            enable_comments,
            registers: RegisterAllocator::new(),
            data_offsets: FxHashMap::default(),
            no_spill: SmallVec::new(),
            function_id: None,
            phis: FxHashMap::default(),
        }
    }

    fn generate(mut self) -> GenericResult<()> {
        for block in self.module.cfg.node_indices() {
            for statement in self.module.cfg[block].iter() {
                if let Statement::Definition(value_id) = statement {
                    if let Value::Phi(Phi(operands)) = &self.module.values[*value_id] {
                        for operand in operands {
                            self.phis
                                .entry(*operand)
                                .or_insert_with(BTreeSet::new)
                                .insert((block, *value_id));
                        }
                    }
                }
            }
        }

        self.tc_comment(&format!("---- START {}", &self.module.name));

        let code_start = self.current_code_offset();
        let comments_start = self.state.image.comments.len();
        let relocations_start = self.state.image.relocations.len();
        let mut next_block = self.generate_block(self.module.entry_block)?;
        while let Some(block) = next_block {
            next_block = self.generate_block(block)?;
        }

        if self.function_id.is_some() && &self.module.name != "main" {
            let mut register_saving_code = Vec::new();
            let saved_registers = self.registers.used().collect_vec();
            let saved_register_list = saved_registers
                .iter()
                .map(|register| format!("x{}", register))
                .format(", ")
                .to_string();
            for (i, &register) in saved_registers.iter().enumerate() {
                write_code(
                    &mut register_saving_code,
                    &[sw(SP, -i16::try_from(i * 4)?, register)],
                );
            }
            let saved_registers_size = i16::try_from(saved_registers.len() * 4)?;
            write_code(
                &mut register_saving_code,
                &[addi(SP, SP, -saved_registers_size)],
            );

            let fixup = u64::try_from(register_saving_code.len())?;
            self.state
                .image
                .code
                .insert_slice(code_start.try_into()?, &register_saving_code);
            for (comment_offset, _) in &mut self.state.image.comments[comments_start..] {
                if *comment_offset >= code_start {
                    *comment_offset += fixup;
                }
            }
            for relocation in &mut self.state.image.relocations[relocations_start..] {
                if let RelocationKind::DataLoad | RelocationKind::DataStore = relocation.kind {
                    if relocation.offset >= code_start {
                        relocation.offset += fixup;
                    }
                }
            }

            if self.enable_comments {
                self.state.image.add_comment(
                    code_start,
                    &format!("Save registers {}", &saved_register_list),
                );
            }
        }

        if self.function_id.is_none() {
            self.push_code(&[ebreak()]);
        }
        self.tc_comment(&format!("---- END {}", &self.module.name));

        let functions = self
            .module
            .functions
            .iter()
            .sorted_by_key(|(fn_id, _)| *fn_id)
            .map(|(fn_id, module)| (fn_id.clone(), module))
            .collect_vec();
        for (fn_id, fn_cfg) in functions {
            let fn_offset = self.current_code_offset();
            self.state.function_offsets.insert(fn_id.clone(), fn_offset);

            let mut backend = Backend::new(
                self.state,
                fn_cfg,
                DumpCode::No,
                EnableImmediateIntegers(self.enable_immediate_integers),
                EnableComments(self.enable_comments),
            );
            backend.function_id = Some(fn_id);
            backend.generate()?;
        }

        if let DumpCode::Yes(output) = replace(&mut self.dump_code, DumpCode::No) {
            output.reset()?;
            writeln!(output, "---- CODE DUMP: {} ----", &self.module.name)?;

            const INSTRUCTION_SIZE: usize = 4;
            for i in 0..self.state.image.code.len() / INSTRUCTION_SIZE {
                let offset = i * INSTRUCTION_SIZE;
                let raw_instruction = (&self.state.image.code[offset..offset + INSTRUCTION_SIZE])
                    .read_u32::<LittleEndian>()?;
                let instruction = Cpu::decode_raw(raw_instruction).ok_or_else(|| {
                    format!(
                        "Unknown instruction: WORD = {:08x} = {:08b} {:08b} {:08b} {:08b}",
                        raw_instruction,
                        raw_instruction >> 24 & 0xFF,
                        raw_instruction >> 16 & 0xFF,
                        raw_instruction >> 8 & 0xFF,
                        raw_instruction & 0xFF,
                    )
                })?;
                // for (op, offset) in Cpu::decode_raw(&self.state.image.code)
                let offset_u64 = u64::try_from(offset)?;
                if let Some(comment) = self.state.image.get_comment_at(offset_u64) {
                    writeln!(output, "{}", comment)?;
                }
                output.set_color(
                    ColorSpec::new()
                        .set_fg(Some(Color::Black))
                        .set_intense(true),
                )?;
                writeln!(
                    output,
                    "[0x{:08x}]  {} {}",
                    offset_u64,
                    instruction.name,
                    (instruction.disassemble)(raw_instruction, offset_u64, None)
                )?;
                output.reset()?;
            }

            if !self.state.image.data.is_empty() {
                const BYTES_PER_LINE: usize = 8;

                writeln!(output, "---- DATA DUMP: {} ----", &self.module.name)?;

                for (index, chunk) in self
                    .state
                    .image
                    .data
                    .iter()
                    .chunks(BYTES_PER_LINE)
                    .into_iter()
                    .enumerate()
                {
                    write!(output, "[{:08x}] ", index * BYTES_PER_LINE)?;
                    output.set_color(
                        ColorSpec::new()
                            .set_fg(Some(Color::Black))
                            .set_intense(true),
                    )?;

                    let chunk = chunk.cloned().collect::<Box<[u8]>>();
                    for byte in chunk.iter() {
                        write!(output, " {:02x}", byte)?;
                    }
                    output.reset()?;

                    (0..BYTES_PER_LINE - chunk.len()).try_for_each(|_| write!(output, "   "))?;
                    write!(output, "  | ")?;
                    for byte in chunk.iter() {
                        let c = if byte.is_ascii_graphic() {
                            *byte as char
                        } else {
                            '.'
                        };
                        write!(output, "{}", c)?;
                    }
                    writeln!(output)?;
                }

                writeln!(output, "\n")?;
            }
        }
        Ok(())
    }

    fn generate_block(&mut self, block: NodeIndex) -> GenericResult<Option<NodeIndex>> {
        let mut next_block = Some(block);
        let basic_block = &self.module.cfg[block];
        for statement in basic_block.iter() {
            next_block = self.generate_statement(block, statement)?;
        }
        Ok(next_block)
    }

    fn generate_statement(
        &mut self,
        block: NodeIndex,
        statement: &Statement,
    ) -> GenericResult<Option<NodeIndex>> {
        match statement {
            Statement::Comment(comment) => {
                self.ir_comment(comment);
                Ok(None)
            }

            Statement::Definition(value_id) => {
                self.tc_comment(&format!("define {:?}", self.module.values[*value_id]));
                let register = self.generate_value(*value_id, None)?;
                let phi = self.phis.get(value_id).and_then(|phis_by_block| {
                    phis_by_block.iter().find_map(|(phi_block, phi)| {
                        if *phi_block >= block {
                            Some(*phi)
                        } else {
                            None
                        }
                    })
                });
                if let Some(phi) = phi {
                    let phi_offset = self.allocate_value(phi)?.unwrap();
                    self.store_u32(register, phi_offset);
                }
                self.tc_comment(&format!(
                    "{}: {:?} -> x{}",
                    value_id, self.module.values[*value_id], register
                ));
                Ok(None)
            }

            Statement::CondJump(condition, then_branch, else_branch) => {
                let condition = self.allocate_register(*condition, None)?;
                self.no_spill.push(condition);

                let jump_to_else_branch = self.push_code(&jump_placeholder());
                let then_next_block = self.generate_block(*then_branch)?;
                if let Some(then_next_block) = then_next_block {
                    self.generate_block(then_next_block)?;
                }
                let jump_to_end = self.push_code(&jump_placeholder());
                let after_jump_to_end = self.current_code_offset();
                let else_branch_offset =
                    i16::try_from(self.current_code_offset() - jump_to_else_branch)?;
                self.patch_at(
                    jump_to_else_branch,
                    &[
                        // FIXME branches have limited range (+-4KiB), consider jump tables
                        beq(else_branch_offset, ZERO, condition),
                    ],
                );

                let else_next_block = self.generate_block(*else_branch)?;
                if let Some(else_next_block) = then_next_block {
                    self.generate_block(else_next_block)?;
                }

                // TODO store code in a format suitable for deletion
                let end_offset = i32::try_from(self.current_code_offset())?
                    .checked_sub(i32::try_from(after_jump_to_end)?)
                    .and_then(|difference| difference.checked_add(4)) // j 4(pc)
                    .unwrap();
                self.patch_jump(jump_to_end, ZERO, end_offset);
                if after_jump_to_end == self.current_code_offset() {
                    for _ in 0..4 {
                        self.state.image.code.remove(jump_to_end as usize);
                    }
                    let else_branch_offset = else_branch_offset - 4;
                    self.patch_at(
                        jump_to_else_branch,
                        &[beq(else_branch_offset, ZERO, condition)],
                    );
                }

                let then_next_block = then_next_block.unwrap_or(*then_branch);
                let else_next_block = else_next_block.unwrap_or(*else_branch);
                // Check if they converge
                let successors = (
                    self.module
                        .cfg
                        .edges_directed(then_next_block, Direction::Outgoing)
                        .next()
                        .map(|edge| edge.target),
                    self.module
                        .cfg
                        .edges_directed(else_next_block, Direction::Outgoing)
                        .next()
                        .map(|edge| edge.target),
                );
                assert_eq!(successors.0, successors.1);
                Ok(successors.0)
            }

            Statement::Return(value_id) => {
                self.allocate_register(*value_id, Some(A0))?;

                if self.function_id.is_some() {
                    if &self.module.name != "main" {
                        let saved_registers = self.registers.used().collect_vec();
                        let saved_register_list = saved_registers
                            .iter()
                            .map(|register| format!("x{}", register))
                            .format(", ")
                            .to_string();
                        self.tc_comment(&format!("Restore registers {}", &saved_register_list));
                        let saved_registers_size = i16::try_from(saved_registers.len() * 4)?;
                        self.push_code(&[addi(SP, SP, saved_registers_size)]);
                        for (i, &register) in saved_registers.iter().enumerate() {
                            self.push_code(&[lw(register, SP, -i16::try_from(i * 4).unwrap())]);
                        }
                    }

                    self.push_code(&[jalr(ZERO, RA, 0)]);
                }
                Ok(None)
            }
        }
    }

    fn generate_value(
        &mut self,
        value_id: ValueId,
        target: Option<Register>,
    ) -> GenericResult<Register> {
        match &self.module.values[value_id] {
            Value::Unit => Ok(ZERO),

            Value::Int(_) | Value::String(_) | Value::Function(_, _) | Value::Phi(_) => {
                self.allocate_register(value_id, target)
            }

            Value::AddInt(left, right) => {
                self.generate_binary(value_id, *left, *right, add, target)
            }
            Value::SubInt(left, right) => {
                self.generate_binary(value_id, *left, *right, sub, target)
            }
            Value::MulInt(left, right) => {
                self.generate_binary(value_id, *left, *right, mul, target)
            }
            Value::DivInt(left, right) => {
                self.generate_binary(value_id, *left, *right, div, target)
            }

            Value::Arg(index) => {
                // FIXME this does not conform to the standard ABI.
                let register = self.allocate_register(value_id, target)?;
                self.push_code(&[lw(register, FP, -i16::try_from(*index * 4).unwrap())]);
                Ok(register)
            }

            Value::Call(function, arguments) => {
                let mut saved_bytes_total = 0;
                if self.function_id.is_some() {
                    self.tc_comment("Save link and frame");
                    self.push_code(&[sw(SP, 0, RA), sw(SP, -4, FP)]);
                    saved_bytes_total += 8;
                }

                let saved_bytes_env = saved_bytes_total;
                saved_bytes_total += arguments.len() * 4;
                self.tc_comment("Set up frame and stack");
                self.push_code(&[
                    addi(FP, SP, -i16::try_from(saved_bytes_env)?),
                    addi(SP, SP, -i16::try_from(saved_bytes_total)?),
                ]);

                let no_spill_length = self.no_spill.len();
                self.tc_comment("Generate arguments");
                for (index, argument) in arguments.iter().enumerate() {
                    let register = self.allocate_register(*argument, None)?;
                    self.push_code(&[sw(FP, -i16::try_from(index * 4)?, register)]);
                    self.no_spill.push(register);
                }

                self.tc_comment("Call the function");
                self.no_spill.truncate(no_spill_length);
                let fn_address = self.allocate_register(*function, None).unwrap();
                // TODO if function is Value::Function then generate a relocation and auipc + jalr instead
                self.push_code(&[jalr(RA, fn_address, 0)]);

                self.tc_comment("Restore stack pointer");
                self.push_code(&[addi(SP, SP, i16::try_from(saved_bytes_total).unwrap())]);

                if self.function_id.is_some() {
                    self.tc_comment("Restore link and frame");
                    self.push_code(&[lw(RA, SP, 0), lw(FP, SP, -4)]);
                }

                let register = self.allocate_register(value_id, None)?;
                self.push_code(&[mv(register, A0)]);
                Ok(register)
            }

            value => unimplemented!("value {}: {:?}", value_id, value),
        }
    }

    fn generate_immediate(&mut self, register: Register, value_id: ValueId) -> GenericResult<()> {
        let value = &self.module.values[value_id];
        let value_i32 = match value {
            Value::Int(value) => *value,
            _ => panic!(
                "cannot generate an immediate from value {}: {:?}",
                value_id, value
            ),
        };
        self.generate_immediate_i32(register, value_i32)
    }

    fn generate_immediate_i32(&mut self, register: Register, value: i32) -> GenericResult<()> {
        let value = ui_immediate(value)?;
        if value.upper == 0 {
            self.push_code(&[addi(register, ZERO, value.lower)]);
        } else if value.lower == 0 {
            self.push_code(&[lui(register, value.upper)]);
        } else {
            self.push_code(&[
                lui(register, value.upper),
                addi(register, register, value.lower),
            ]);
        }
        Ok(())
    }

    fn generate_binary(
        &mut self,
        value_id: ValueId,
        left: ValueId,
        right: ValueId,
        op: impl FnOnce(Register, Register, Register) -> u32,
        target: Option<Register>,
    ) -> GenericResult<Register> {
        let no_spill_length = self.no_spill.len();
        let left = self.allocate_register(left, None)?;
        self.no_spill.push(left);
        let right = self.allocate_register(right, None)?;
        self.no_spill.push(right);
        let result = self.allocate_register(value_id, target)?;
        self.no_spill.truncate(no_spill_length);
        self.push_code(&[op(result, left, right)]);
        Ok(result)
    }

    fn allocate_value(&mut self, value_id: ValueId) -> GenericResult<Option<u64>> {
        if let Some(offset) = self.data_offsets.get(&value_id) {
            Ok(Some(*offset))
        } else {
            let value = &self.module.values[value_id];
            let data_offset = match value {
                Value::Int(value) => {
                    if self.enable_immediate_integers {
                        None
                    } else {
                        Some(self.allocate_u32(*value as u32).unwrap())
                    }
                }

                Value::AddInt(_, _)
                | Value::SubInt(_, _)
                | Value::MulInt(_, _)
                | Value::DivInt(_, _)
                | Value::Phi(_)
                | Value::Call(_, _) => Some(self.allocate_u32(0xDEAD_BEEF)?),

                Value::String(s) => Some(self.allocate_bytes(s.as_bytes())?),
                Value::AddString(_, _) => unimplemented!("{}: Value::AddString", function!()),

                Value::Unit | Value::Arg(_) | Value::Function(_, _) => None,
            };
            if let Some(offset) = data_offset {
                self.data_offsets.insert(value_id, offset);
            }
            Ok(data_offset)
        }
    }

    fn allocate_u32(&mut self, data: u32) -> GenericResult<u64> {
        let offset = self.current_data_offset();
        self.state.image.data.write_u32::<LittleEndian>(data)?;
        Ok(offset)
    }

    fn allocate_bytes(&mut self, data: &[u8]) -> GenericResult<u64> {
        let offset = self.current_data_offset();
        self.state.image.data.write_all(data)?;
        Ok(offset)
    }

    fn load_u32(&mut self, register: Register, offset: u64) {
        let relocation_offset = self.current_code_offset();
        self.state.image.relocations.push(Relocation::new(
            relocation_offset,
            offset,
            RelocationKind::DataLoad,
        ));
        self.push_code(&[lui(register, 0), lw(register, register, 0)]);
    }

    fn store_u32(&mut self, register: Register, offset: u64) {
        let relocation_offset = self.current_code_offset();
        self.state.image.relocations.push(Relocation::new(
            relocation_offset,
            offset,
            RelocationKind::DataStore,
        ));
        self.push_code(&[lui(T0, 0), sw(T0, 0, register)]);
    }

    fn push_code(&mut self, instructions: &[u32]) -> u64 {
        let code_start = self.current_code_offset();
        let mut cursor = Cursor::new(&mut self.state.image.code);
        cursor.set_position(code_start);
        write_code(&mut cursor, instructions);
        code_start
    }

    fn patch_at(&mut self, patch_offset: u64, instructions: &[u32]) {
        let mut cursor = Cursor::new(&mut self.state.image.code);
        cursor.set_position(patch_offset);
        write_code(&mut cursor, instructions);
    }

    fn allocate_register(
        &mut self,
        value_id: ValueId,
        target: Option<Register>,
    ) -> GenericResult<Register> {
        if self.enable_immediate_integers {
            if let Value::Int(value) = &self.module.values[value_id] {
                if *value == 0 {
                    return Ok(ZERO);
                }
            }
        }

        let (register, source, previous_user) =
            self.registers.allocate(value_id, target, &self.no_spill)?;
        if let Some(previous_user) = previous_user {
            if previous_user != value_id {
                // Spill
                let data_offset;
                if let Some(offset) = self.data_offsets.get(&previous_user) {
                    data_offset = *offset
                } else {
                    // FIXME allocate_value(). The following is a hack for function calls, for which memory is not allocated.
                    data_offset = self.allocate_u32(0xBAD_F00D).unwrap();
                    self.data_offsets.insert(previous_user, data_offset);
                }
                self.tc_comment(&format!(
                    "SPILLED [r{}] {} in favor of {}",
                    register, previous_user, value_id
                ));
                self.store_u32(register, data_offset);
            }
        }

        if let Some(source) = source {
            if source != register {
                self.push_code(&[mv(register, source)]);
            }
        } else if let Some(data_offset) = self.allocate_value(value_id).unwrap() {
            self.load_u32(register, data_offset);
        } else {
            match &self.module.values[value_id] {
                Value::Int(_) => {
                    if self.enable_immediate_integers {
                        self.generate_immediate(register, value_id)?;
                    }
                }

                Value::Function(fn_id, _) => {
                    let relocation_offset = self.current_code_offset();
                    self.state
                        .function_relocations
                        .push((relocation_offset, fn_id.clone()));
                    self.push_code(&[lui(register, 0), addi(register, register, 0)]);
                }

                _ => (),
            }
        }

        Ok(register)
    }

    fn ir_comment(&mut self, comment: &str) {
        self.state
            .image
            .add_comment(self.current_code_offset(), &format!("// {}", comment));
    }

    fn tc_comment(&mut self, comment: &str) {
        if self.enable_comments {
            self.state
                .image
                .add_comment(self.current_code_offset(), comment);
        }
    }

    fn patch_jump(&mut self, patch_offset: u64, rd: Register, offset: i32) {
        self.patch_at(
            patch_offset,
            &match Self::jump_offset(offset) {
                JumpOffset::Short => [nop(), jal(rd, offset)],
                JumpOffset::Long(offset) => [auipc(T0, offset.upper), jalr(rd, T0, offset.lower)],
            },
        )
    }

    fn current_code_offset(&self) -> u64 {
        u64::try_from(self.state.image.code.len()).unwrap()
    }

    fn current_data_offset(&self) -> u64 {
        u64::try_from(self.state.image.data.len()).unwrap()
    }

    fn jump_offset(offset: i32) -> JumpOffset {
        if (-(1 << 20)..1 << 20).contains(&offset) {
            JumpOffset::Short
        } else {
            JumpOffset::Long(ui_immediate(offset).unwrap())
        }
    }
}

#[derive(Copy, Clone, PartialEq)]
enum Allocation {
    None,
    Temporary,
    Permanent,
}

#[derive(Copy, Clone)]
struct IsTarget(bool);

struct RegisterAllocator {
    states: [Allocation; REGISTER_COUNT],
    values: BiMap<ValueId, Register>,
    used: [bool; REGISTER_COUNT],
}

impl RegisterAllocator {
    const RESERVED: &'static [Register] = &[ZERO, RA, SP, GP, FP, A0, A1, T0];

    fn new() -> Self {
        let mut states = [Allocation::None; 32];
        for register in Self::RESERVED.iter() {
            states[usize::from(*register)] = Allocation::Permanent;
        }
        Self {
            states,
            values: BiMap::new(),
            used: [false; REGISTER_COUNT],
        }
    }

    // TODO target variants: Specific(reg), Saved, Temp
    fn allocate(
        &mut self,
        value_id: ValueId,
        target: Option<Register>,
        no_spill: &[Register],
    ) -> GenericResult<(Register, Option<Register>, Option<ValueId>)> {
        if let Some(target) = target {
            self.try_allocate(value_id, target, IsTarget(true), no_spill)
                .map(|(source, previous_user)| (target, source, previous_user))
                .map_err(From::from)
        } else {
            if let Some(register) = self.values.get_by_left(&value_id) {
                return Ok((*register, Some(*register), None));
            }
            for register_index in 0..REGISTER_COUNT {
                let register = Register::new(register_index)?;
                if let Allocation::None = self.states[register_index] {
                    if let Ok((source, previous_user)) =
                        self.try_allocate(value_id, register, IsTarget(false), no_spill)
                    {
                        return Ok((register, source, previous_user));
                    }
                }
            }
            for register_index in 0..REGISTER_COUNT {
                let register = Register::new(register_index)?;
                if let Allocation::Temporary = self.states[register_index] {
                    if let Ok((source, previous_user)) =
                        self.try_allocate(value_id, register, IsTarget(false), no_spill)
                    {
                        return Ok((register, source, previous_user));
                    }
                }
            }
            Err(From::from(RegisterAllocationError))
        }
    }

    fn try_allocate(
        &mut self,
        value_id: ValueId,
        register: Register,
        IsTarget(is_target): IsTarget,
        no_spill: &[Register],
    ) -> Result<(Option<Register>, Option<ValueId>), RegisterAllocationError> {
        if self.values.get_by_left(&value_id) == Some(&register) {
            return Ok((Some(register), None));
        }
        let register_index = usize::from(register);
        match self.states[register_index] {
            Allocation::None => {
                let previous_user = None;
                let source = self.associate(value_id, register, previous_user);
                self.states[register_index] = Allocation::Temporary;
                self.used[register_index] = true;
                Ok((source, previous_user))
            }

            Allocation::Temporary => {
                let previous_user = self.values.get_by_right(&register).cloned();
                if previous_user == Some(value_id) {
                    self.states[register_index] = Allocation::Temporary;
                    self.used[register_index] = true;
                    Ok((None, None))
                } else if no_spill.contains(&register) {
                    Err(RegisterAllocationError)
                } else {
                    let source = self.associate(value_id, register, previous_user);
                    self.states[register_index] = Allocation::Temporary;
                    self.used[register_index] = true;
                    Ok((source, previous_user))
                }
            }

            Allocation::Permanent => {
                if is_target {
                    let previous_user = self.values.get_by_right(&register).cloned();
                    let source = self.associate(value_id, register, previous_user);
                    Ok((source, previous_user))
                } else {
                    Err(RegisterAllocationError)
                }
            }
        }
    }

    fn associate(
        &mut self,
        value_id: ValueId,
        register: Register,
        previous_user: Option<ValueId>,
    ) -> Option<Register> {
        if let Some(previous_user) = &previous_user {
            self.values.remove_by_left(previous_user);
        }
        let source = self
            .values
            .remove_by_left(&value_id)
            .map(|(_, source)| source);
        if let Some(source) = &source {
            self.states[usize::from(*source)] = Allocation::None;
        }
        self.values.insert(value_id, register);
        source
    }

    fn used(&self) -> impl Iterator<Item = Register> + '_ {
        self.used
            .iter()
            .enumerate()
            .filter_map(|(i, used)| match used {
                true => Some(i.try_into().unwrap()),
                _ => None,
            })
    }
}

#[derive(Debug)]
struct RegisterAllocationError;

impl fmt::Display for RegisterAllocationError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "failed to allocate a register")
    }
}

impl Error for RegisterAllocationError {}

pub(crate) fn write_code(writer: &mut impl Write, code: &[u32]) {
    for instruction in code {
        writer.write_u32::<LittleEndian>(*instruction).unwrap();
    }
}

pub(crate) enum JumpOffset {
    Short,
    Long(ImmediateI),
}

pub(crate) struct ImmediateI {
    pub(crate) upper: i32,
    pub(crate) lower: i16,
}

pub(crate) fn ui_immediate(value: i32) -> Result<ImmediateI, TryFromIntError> {
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

fn jump_placeholder() -> [u32; 2] {
    [nop(), nop()]
}
