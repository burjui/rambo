use crate::ir::value_storage::ValueId;
use crate::ir::FnId;
use crate::ir::IRModule;
use crate::ir::Phi;
use crate::ir::Statement;
use crate::ir::Value;
use crate::utils::GenericResult;
use crate::utils::VecUtils;
use bimap::BiMap;
use byteorder::LittleEndian;
use byteorder::WriteBytesExt;
use itertools::Itertools;
use petgraph::stable_graph::NodeIndex;
use petgraph::visit::EdgeRef;
use petgraph::Direction;
use rambo_riscv::Executable;
use rambo_riscv::Relocation;
use rambo_riscv::RelocationKind;
use riscv::decoder::decode;
use riscv::registers;
use riscv::registers::REGISTER_COUNT;
use risky::instructions::*;
use smallvec::SmallVec;
use std::collections::btree_set::BTreeSet;
use std::collections::HashMap;
use std::convert::TryFrom;
use std::error::Error;
use std::fmt;
use std::io;
use std::io::Cursor;
use std::io::Write;
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

#[derive(Deref)]
pub(crate) struct EnableImmediateIntegers(pub(crate) bool);

pub(crate) fn generate(
    module: &IRModule,
    dump_code: DumpCode<'_>,
    enable_immediate_integers: EnableImmediateIntegers,
) -> GenericResult<Executable> {
    let mut state = SharedState::new();
    let backend = Backend::new(&mut state, module, dump_code, enable_immediate_integers);
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
    function_offsets: HashMap<FnId, u32>,
    function_relocations: Vec<(u32, FnId)>,
}

impl SharedState {
    fn new() -> Self {
        Self {
            image: Executable::new(),
            function_offsets: HashMap::new(),
            function_relocations: Vec::new(),
        }
    }
}

struct Backend<'a> {
    state: &'a mut SharedState,
    module: &'a IRModule,
    dump_code: DumpCode<'a>,
    enable_immediate_integers: bool,
    registers: RegisterAllocator,
    data_offsets: HashMap<ValueId, u32>,
    no_spill: SmallVec<[u8; REGISTER_COUNT]>,
    function_id: Option<FnId>,
    phis: HashMap<ValueId, BTreeSet<(NodeIndex, ValueId)>>,
}

impl<'a> Backend<'a> {
    fn new(
        state: &'a mut SharedState,
        module: &'a IRModule,
        dump_code: DumpCode<'a>,
        EnableImmediateIntegers(enable_immediate_integers): EnableImmediateIntegers,
    ) -> Self {
        Self {
            state,
            module,
            dump_code,
            enable_immediate_integers,
            registers: RegisterAllocator::new(),
            data_offsets: HashMap::new(),
            no_spill: SmallVec::new(),
            function_id: None,
            phis: HashMap::new(),
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

        self.comment(&format!("---- START {}", &self.module.name))?;

        let code_start = self.current_code_offset().unwrap();
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
            for (i, register) in saved_registers.iter().enumerate() {
                write_code(
                    &mut register_saving_code,
                    &[sw(registers::SP, -i16::try_from(i * 4)?, *register)?],
                )?;
            }
            let saved_registers_size = i16::try_from(saved_registers.len() * 4)?;
            write_code(
                &mut register_saving_code,
                &[addi(registers::SP, registers::SP, -saved_registers_size)?],
            )?;

            let comment_fixup_offset = register_saving_code.len() as u32;
            self.state
                .image
                .code
                .insert_slice(usize::try_from(code_start).unwrap(), &register_saving_code);
            for (comment_offset, _) in self.state.image.comments.iter_mut().rev() {
                if *comment_offset > code_start {
                    *comment_offset += comment_fixup_offset;
                } else {
                    break;
                }
            }
            for relocation in self.state.image.relocations.iter_mut().rev() {
                if relocation.offset > code_start {
                    if let RelocationKind::DataLoad | RelocationKind::DataStore = relocation.kind {
                        relocation.offset += comment_fixup_offset;
                    }
                } else {
                    break;
                }
            }

            self.last_comment_at(
                code_start,
                &format!("Save registers {}", &saved_register_list),
            )?;
        }

        if self.function_id.is_none() {
            self.push_code(&[
                // This ecall stops an sbk-vm
                addi(registers::A7, registers::ZERO, 93).unwrap(),
                ecall().unwrap(),
            ])
            .unwrap();
        }
        self.comment(&format!("---- END {}", &self.module.name))?;

        let functions = self
            .module
            .functions
            .iter()
            .sorted_by_key(|(fn_id, _)| *fn_id)
            .map(|(fn_id, module)| (fn_id.clone(), module))
            .collect_vec();
        for (fn_id, fn_cfg) in functions {
            let fn_offset = self.current_code_offset()?;
            self.state.function_offsets.insert(fn_id.clone(), fn_offset);

            let mut backend = Backend::new(
                self.state,
                &fn_cfg,
                DumpCode::No,
                EnableImmediateIntegers(self.enable_immediate_integers),
            );
            backend.function_id = Some(fn_id);
            backend.generate()?;
        }

        if let DumpCode::Yes(output) = self.dump_code {
            output.reset()?;
            writeln!(output, "---- CODE DUMP: {} ----", &self.module.name)?;
            for (op, offset) in decode(&self.state.image.code) {
                let offset = u32::try_from(offset)?;
                if let Some(comments) = self.state.image.comments.find(offset) {
                    for comment in comments {
                        writeln!(output, "// {}", comment)?;
                    }
                }
                output.set_color(
                    ColorSpec::new()
                        .set_fg(Some(Color::Black))
                        .set_intense(true),
                )?;
                writeln!(output, "[0x{:08x}]  {:?}", u32::try_from(offset)?, op)?;
                output.reset()?;
            }

            if !self.state.image.data.is_empty() {
                writeln!(output, "---- DATA DUMP: {} ----", &self.module.name)?;
                for (index, &byte) in self.state.image.data.iter().enumerate() {
                    if index % 4 == 0 {
                        if index > 0 {
                            writeln!(output)?;
                        }
                        write!(output, "[{:08x}] ", index)?;
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
                self.comment(comment)?;
                Ok(None)
            }

            Statement::Definition(value_id) => {
                self.comment(&format!("define {:?}", self.module.values[*value_id]))?;
                let register = self.generate_value(*value_id, None)?;
                let phi = self.phis.get(&value_id).and_then(|phis_by_block| {
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
                    self.store_u32(register, phi_offset)?;
                }
                self.comment(&format!(
                    "{}: {:?} -> x{}",
                    value_id, self.module.values[*value_id], register
                ))?;
                Ok(None)
            }

            Statement::CondJump(condition, then_branch, else_branch) => {
                let condition = self.allocate_register(*condition, None)?;
                self.no_spill.push(condition);

                let jump_to_else_branch = self.push_code(&jump_placeholder()?)?;
                let then_next_block = self.generate_block(*then_branch)?;
                if let Some(then_next_block) = then_next_block {
                    self.generate_block(then_next_block)?;
                }
                let jump_to_end = self.push_code(&jump_placeholder()?)?;
                let after_jump_to_end = self.current_code_offset()?;
                let else_branch_offset =
                    i16::try_from(self.current_code_offset()? - jump_to_else_branch)?;
                self.patch_at(
                    jump_to_else_branch,
                    &[
                        // FIXME branches have limited range (+-4KiB), consider jump tables
                        beq(else_branch_offset, registers::ZERO, condition)?,
                    ],
                )?;

                let else_next_block = self.generate_block(*else_branch)?;
                if let Some(else_next_block) = then_next_block {
                    self.generate_block(else_next_block)?;
                }

                // TODO store code in a format suitable for deletion
                let end_offset = i32::try_from(self.state.image.code.len())?
                    .checked_sub(i32::try_from(after_jump_to_end)?)
                    .and_then(|difference| difference.checked_add(4)) // j 4(pc)
                    .unwrap();
                self.patch_jump(jump_to_end, registers::ZERO, end_offset)?;
                if after_jump_to_end == self.current_code_offset()? {
                    for _ in 0..4 {
                        self.state.image.code.remove(jump_to_end as usize);
                    }
                    let else_branch_offset = else_branch_offset - 4;
                    self.patch_at(
                        jump_to_else_branch,
                        &[beq(else_branch_offset, registers::ZERO, condition)?],
                    )?;
                }

                let then_next_block = then_next_block.unwrap_or(*then_branch);
                let else_next_block = else_next_block.unwrap_or(*else_branch);
                // Check if they converge
                let successors = (
                    self.module
                        .cfg
                        .edges_directed(then_next_block, Direction::Outgoing)
                        .next()
                        .map(|edge| edge.target()),
                    self.module
                        .cfg
                        .edges_directed(else_next_block, Direction::Outgoing)
                        .next()
                        .map(|edge| edge.target()),
                );
                assert_eq!(successors.0, successors.1);
                Ok(successors.0)
            }

            Statement::Return(value_id) => {
                self.allocate_register(*value_id, Some(registers::A0))?;

                if self.function_id.is_some() {
                    if &self.module.name != "main" {
                        let saved_registers = self.registers.used().collect_vec();
                        let saved_register_list = saved_registers
                            .iter()
                            .map(|register| format!("x{}", register))
                            .format(", ")
                            .to_string();
                        self.comment(&format!("Restore registers {}", &saved_register_list))?;
                        let saved_registers_size = i16::try_from(saved_registers.len() * 4)?;
                        self.push_code(&[addi(
                            registers::SP,
                            registers::SP,
                            saved_registers_size,
                        )?])?;
                        for (i, register) in saved_registers.iter().enumerate() {
                            self.push_code(&[lw(
                                *register,
                                registers::SP,
                                -i16::try_from(i * 4).unwrap(),
                            )
                            .unwrap()])?;
                        }
                    }

                    self.push_code(&[jalr(registers::ZERO, registers::RA, 0)?])?;
                }
                Ok(None)
            }
        }
    }

    fn generate_value(&mut self, value_id: ValueId, target: Option<u8>) -> GenericResult<u8> {
        match &self.module.values[value_id] {
            Value::Unit => Ok(registers::ZERO),

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
                self.push_code(&[lw(
                    register,
                    registers::FP,
                    -i16::try_from(*index * 4).unwrap(),
                )
                .unwrap()])?;
                Ok(register)
            }

            Value::Call(function, arguments) => {
                let mut saved_bytes_total = 0;
                if self.function_id.is_some() {
                    self.comment("Save link and frame")?;
                    self.push_code(&[
                        sw(registers::SP, 0, registers::RA)?,
                        sw(registers::SP, -4, registers::FP)?,
                    ])?;
                    saved_bytes_total += 8;
                }

                let saved_bytes_env = saved_bytes_total;
                saved_bytes_total += arguments.len() * 4;
                self.comment("Set up frame and stack")?;
                self.push_code(&[
                    addi(
                        registers::FP,
                        registers::SP,
                        -i16::try_from(saved_bytes_env)?,
                    )?,
                    addi(
                        registers::SP,
                        registers::SP,
                        -i16::try_from(saved_bytes_total)?,
                    )?,
                ])?;

                let no_spill_length = self.no_spill.len();
                self.comment("Generate arguments")?;
                for (index, argument) in arguments.iter().enumerate() {
                    let register = self.allocate_register(*argument, None)?;
                    self.push_code(&[sw(registers::FP, -i16::try_from(index * 4)?, register)?])?;
                    self.no_spill.push(register);
                }

                self.comment("Call the function")?;
                self.no_spill.truncate(no_spill_length);
                let fn_address = self.allocate_register(*function, None).unwrap();
                // TODO if function is Value::Function then generate a relocation and auipc + jalr instead
                self.push_code(&[jalr(registers::RA, fn_address, 0).unwrap()])?;

                self.comment("Restore stack pointer")?;
                self.push_code(&[addi(
                    registers::SP,
                    registers::SP,
                    i16::try_from(saved_bytes_total)?,
                )?])?;

                if self.function_id.is_some() {
                    self.comment("Restore link and frame")?;
                    self.push_code(&[
                        lw(registers::RA, registers::SP, 0).unwrap(),
                        lw(registers::FP, registers::SP, -4).unwrap(),
                    ])?;
                }

                let register = self.allocate_register(value_id, None)?;
                self.push_code(&[addi(register, registers::A0, 0)?])?;
                Ok(register)
            }

            value => unimplemented!("value {}: {:?}", value_id, value),
        }
    }

    fn generate_immediate(&mut self, register: u8, value_id: ValueId) -> GenericResult<()> {
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

    fn generate_immediate_i32(&mut self, register: u8, value: i32) -> GenericResult<()> {
        let value = ui_immediate(value)?;
        if value.upper == 0 {
            self.push_code(&[addi(register, registers::ZERO, value.lower).unwrap()])?;
        } else if value.lower == 0 {
            self.push_code(&[lui(register, value.upper).unwrap()])?;
        } else {
            self.push_code(&[
                lui(register, value.upper).unwrap(),
                addi(register, register, value.lower).unwrap(),
            ])?;
        }
        Ok(())
    }

    fn generate_binary(
        &mut self,
        value_id: ValueId,
        left: ValueId,
        right: ValueId,
        op: impl FnOnce(u8, u8, u8) -> risky::Result<u32>,
        target: Option<u8>,
    ) -> GenericResult<u8> {
        let no_spill_length = self.no_spill.len();
        let left = self.allocate_register(left, None)?;
        self.no_spill.push(left);
        let right = self.allocate_register(right, None)?;
        self.no_spill.push(right);
        let result = self.allocate_register(value_id, target)?;
        self.no_spill.truncate(no_spill_length);
        self.push_code(&[op(result, left, right)?])?;
        Ok(result)
    }

    fn allocate_value(&mut self, value_id: ValueId) -> GenericResult<Option<u32>> {
        match self.data_offsets.get(&value_id) {
            Some(offset) => Ok(Some(*offset)),

            None => {
                let value = &self.module.values[value_id];
                let data_offset = match value {
                    Value::Unit => None,

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

                    Value::Arg(_) | Value::Function(_, _) => None,

                    Value::String(_) => unimplemented!("{}: Value::String", function!()),
                    Value::AddString(_, _) => unimplemented!("{}: Value::AddString", function!()),
                };
                if let Some(offset) = data_offset {
                    self.data_offsets.insert(value_id, offset);
                }
                Ok(data_offset)
            }
        }
    }

    fn allocate_u32(&mut self, data: u32) -> GenericResult<u32> {
        let offset = u32::try_from(self.state.image.data.len())?;
        self.state.image.data.write_u32::<LittleEndian>(data)?;
        Ok(offset)
    }

    fn load_u32(&mut self, rd: u8, offset: u32) -> GenericResult<()> {
        let relocation_offset = self.current_code_offset().unwrap();
        self.state.image.relocations.push(Relocation::new(
            relocation_offset,
            offset,
            RelocationKind::DataLoad,
        ));
        self.push_code(&[lui(rd, 0).unwrap(), lw(rd, rd, 0).unwrap()])
            .map(|_| ())
    }

    fn store_u32(&mut self, register: u8, offset: u32) -> GenericResult<()> {
        let relocation_offset = self.current_code_offset().unwrap();
        self.state.image.relocations.push(Relocation::new(
            relocation_offset,
            offset,
            RelocationKind::DataStore,
        ));
        self.push_code(&[
            lui(registers::T0, 0).unwrap(),
            sw(registers::T0, 0, register).unwrap(),
        ])
        .map(|_| ())
    }

    fn push_code(&mut self, instructions: &[u32]) -> GenericResult<u32> {
        let code_start = self.state.image.code.len();
        let mut cursor = Cursor::new(&mut self.state.image.code);
        cursor.set_position(u64::try_from(code_start)?);
        write_code(&mut cursor, instructions)?;
        u32::try_from(code_start).map_err(From::from)
    }

    fn patch_at(&mut self, patch_offset: u32, instructions: &[u32]) -> io::Result<()> {
        let mut cursor = Cursor::new(&mut self.state.image.code);
        cursor.set_position(u64::from(patch_offset));
        write_code(&mut cursor, instructions)
    }

    fn current_code_offset(&self) -> Result<u32, TryFromIntError> {
        u32::try_from(self.state.image.code.len())
    }

    fn allocate_register(&mut self, value_id: ValueId, target: Option<u8>) -> GenericResult<u8> {
        if self.enable_immediate_integers {
            if let Value::Int(value) = &self.module.values[value_id] {
                if *value == 0 {
                    return Ok(registers::ZERO);
                }
            }
        }

        let (register, source, previous_user) =
            self.registers.allocate(value_id, target, &*self.no_spill)?;
        if let Some(previous_user) = previous_user {
            if previous_user != value_id {
                // Spill
                let data_offset;
                match self.data_offsets.get(&previous_user) {
                    Some(offset) => data_offset = *offset,
                    None => {
                        // FIXME allocate_value(). The following is a hack for function calls, for which memory is not allocated.
                        data_offset = self.allocate_u32(0xBAD_F00D).unwrap();
                        self.data_offsets.insert(previous_user, data_offset);
                    }
                }
                self.comment(&format!(
                    "SPILLED [r{}] {} in favor of {}",
                    register, previous_user, value_id
                ))?;
                self.store_u32(register, data_offset)?;
            }
        }

        if let Some(source) = source {
            if source != register {
                self.push_code(&[addi(register, source, 0)?])?;
            }
        } else if let Some(data_offset) = self.allocate_value(value_id).unwrap() {
            self.load_u32(register, data_offset)?;
        } else {
            match &self.module.values[value_id] {
                Value::Int(_) => {
                    if self.enable_immediate_integers {
                        self.generate_immediate(register, value_id)?;
                    }
                }

                Value::Function(fn_id, _) => {
                    let relocation_offset = self.current_code_offset().unwrap();
                    self.state
                        .function_relocations
                        .push((relocation_offset, fn_id.clone()));
                    self.push_code(&[
                        lui(register, 0).unwrap(),
                        addi(register, register, 0).unwrap(),
                    ])
                    .unwrap();
                }

                _ => (),
            }
        }

        Ok(register)
    }

    fn comment(&mut self, comment: &str) -> GenericResult<()> {
        self.last_comment_at(self.current_code_offset()?, comment)
    }

    fn last_comment_at(&mut self, offset: u32, comment: &str) -> GenericResult<()> {
        self.state.image.comments.insert(offset, comment);
        Ok(())
    }

    fn patch_jump(&mut self, patch_offset: u32, rd: u8, offset: i32) -> GenericResult<()> {
        self.patch_at(
            patch_offset,
            &match self.check_jump_offset(offset)? {
                JumpOffset::Short => [nop()?, jal(rd, offset)?],
                JumpOffset::Long(offset) => [
                    auipc(registers::T0, offset.upper)?,
                    jalr(rd, registers::T0, offset.lower)?,
                ],
            },
        )
        .map_err(Into::into)
    }

    fn check_jump_offset(&self, offset: i32) -> Result<JumpOffset, TryFromIntError> {
        if -(1 << 20) <= offset && offset < 1 << 20 {
            Ok(JumpOffset::Short)
        } else {
            Ok(JumpOffset::Long(ui_immediate(offset)?))
        }
    }
}

#[derive(Copy, Clone, PartialEq)]
enum Allocation {
    None,
    Temporary,
    Permanent,
}

#[derive(Deref, Copy, Clone)]
struct IsTarget(bool);

struct RegisterAllocator {
    states: [Allocation; REGISTER_COUNT],
    values: BiMap<ValueId, u8>,
    used: [bool; REGISTER_COUNT],
}

impl RegisterAllocator {
    const RESERVED: &'static [u8] = &[
        registers::ZERO,
        registers::RA,
        registers::SP,
        registers::GP,
        registers::FP,
        registers::A0,
        registers::A1,
        registers::T0,
    ];

    fn new() -> Self {
        let mut states = [Allocation::None; 32];
        for register in Self::RESERVED.iter() {
            states[*register as usize] = Allocation::Permanent;
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
        target: Option<u8>,
        no_spill: &[u8],
    ) -> GenericResult<(u8, Option<u8>, Option<ValueId>)> {
        match target {
            Some(target) => self
                .try_allocate(value_id, target, IsTarget(true), no_spill)
                .map(|(source, previous_user)| (target, source, previous_user))
                .map_err(From::from),

            None => {
                if let Some(register) = self.values.get_by_left(&value_id) {
                    return Ok((*register, Some(*register), None));
                }
                for register in 0..u8::try_from(REGISTER_COUNT)? {
                    if let Allocation::None = self.states[register as usize] {
                        if let Ok((source, previous_user)) =
                            self.try_allocate(value_id, register, IsTarget(false), no_spill)
                        {
                            return Ok((register, source, previous_user));
                        }
                    }
                }
                for register in 0..u8::try_from(REGISTER_COUNT)? {
                    if let Allocation::Temporary = self.states[register as usize] {
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
    }

    fn try_allocate(
        &mut self,
        value_id: ValueId,
        register: u8,
        IsTarget(is_target): IsTarget,
        no_spill: &[u8],
    ) -> Result<(Option<u8>, Option<ValueId>), RegisterAllocationError> {
        if self.values.get_by_left(&value_id) == Some(&register) {
            return Ok((Some(register), None));
        }
        match self.states[register as usize] {
            Allocation::None => {
                let previous_user = None;
                let source = self.associate(value_id, register, previous_user);
                self.states[register as usize] = Allocation::Temporary;
                self.used[register as usize] = true;
                Ok((source, previous_user))
            }

            Allocation::Temporary => {
                let previous_user = self.values.get_by_right(&register).cloned();
                if previous_user == Some(value_id) {
                    self.states[register as usize] = Allocation::Temporary;
                    self.used[register as usize] = true;
                    Ok((None, None))
                } else if no_spill.contains(&register) {
                    Err(RegisterAllocationError)
                } else {
                    let source = self.associate(value_id, register, previous_user);
                    self.states[register as usize] = Allocation::Temporary;
                    self.used[register as usize] = true;
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
        register: u8,
        previous_user: Option<ValueId>,
    ) -> Option<u8> {
        if let Some(previous_user) = &previous_user {
            self.values.remove_by_left(previous_user);
        }
        let source = self
            .values
            .remove_by_left(&value_id)
            .map(|(_, source)| source);
        if let Some(source) = &source {
            self.states[*source as usize] = Allocation::None;
        }
        self.values.insert(value_id, register);
        source
    }

    fn used<'a>(&'a self) -> impl Iterator<Item = u8> + 'a {
        self.used
            .iter()
            .enumerate()
            .filter_map(|(i, used)| match used {
                true => Some(i as u8),
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

pub(crate) fn write_code(writer: &mut impl Write, code: &[u32]) -> io::Result<()> {
    for instruction in code {
        writer.write_u32::<LittleEndian>(*instruction)?;
    }
    Ok(())
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

fn nop() -> risky::Result<u32> {
    addi(0, 0, 0)
}

fn jump_placeholder() -> risky::Result<[u32; 2]> {
    Ok([nop()?, nop()?])
}
