use std::cmp::Ordering;
use std::collections::btree_set::BTreeSet;
use std::collections::HashMap;
use std::convert::TryFrom;
use std::error::Error;
use std::fmt;
use std::io;
use std::io::Cursor;
use std::io::Write;
use std::mem::replace;
use std::num::TryFromIntError;

use bimap::BiMap;
use byteorder::WriteBytesExt;
use itertools::Itertools;
use num_traits::cast::ToPrimitive;
use petgraph::Direction;
use petgraph::stable_graph::NodeIndex;
use petgraph::visit::EdgeRef;
use risky::instructions::*;
use smallvec::SmallVec;
use termcolor::{Color, ColorSpec, StandardStream, WriteColor};

use crate::ir::{ControlFlowGraph, Phi};
use crate::ir::FnId;
use crate::ir::Statement;
use crate::ir::Value;
use crate::ir::value_storage::ValueId;
use crate::riscv::{DataByteOrder, InstructionByteOrder, registers};
use crate::riscv::registers::REGISTER_COUNT;
use crate::riscv_decoder::decode;
use crate::utils::{GenericResult, stderr};

#[cfg(test)]
mod tests;

#[derive(Deref)]
pub(crate) struct DumpCode(pub(crate) bool);

pub(crate) fn generate(cfg: &ControlFlowGraph, dump_code: DumpCode) -> GenericResult<RICSVImage> {
    let data_start_address = 0xD000_0000;
    let (image, _) = Backend::new(cfg, 0x0000_0000, data_start_address, data_start_address, dump_code).generate()?;
    Ok(image)
}

pub(crate) struct RICSVImage {
    pub(crate) code: Vec<u8>,
    pub(crate) code_base_address: u32,
    pub(crate) data: Vec<u8>,
    pub(crate) ram_base_address: u32,
    pub(crate) comments: CommentVec,
}

#[derive(Copy, Clone, PartialEq, Eq)]
struct PhiValue {
    block: NodeIndex,
    value_id: ValueId,
}

impl PartialOrd for PhiValue {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        other.block.partial_cmp(&self.block)
    }
}

impl Ord for PhiValue {
    fn cmp(&self, other: &Self) -> Ordering {
        other.block.cmp(&self.block)
    }
}

struct Backend<'a> {
    cfg: &'a ControlFlowGraph,
    code_base_address: u32,
    ram_base_address: u32,
    data_start_address: u32,
    code: Vec<u8>,
    data: Vec<u8>,
    comments: CommentVec,
    dump_code: bool,
    registers: RegisterAllocator,
    value_addresses: HashMap<ValueId, u32>,
    no_spill: SmallVec<[u8; REGISTER_COUNT]>,
    function_id: Option<FnId>,
    stderr: StandardStream,
    phis: HashMap<ValueId, BTreeSet<(NodeIndex, ValueId)>>,
    function_offsets: HashMap<FnId, u32>,
    function_ids: HashMap<ValueId, FnId>,
    function_used_registers: HashMap<FnId, Box<[u8]>>,
}

impl<'a> Backend<'a> {
    fn new(cfg: &'a ControlFlowGraph,
           code_base_address: u32,
           ram_base_address: u32,
           data_start_address: u32,
           DumpCode(dump_code): DumpCode) -> Self
    {
        Self {
            cfg,
            code_base_address,
            ram_base_address,
            data_start_address,
            dump_code,
            code: Vec::new(),
            data: Vec::new(),
            comments: CommentVec::new(),
            registers: RegisterAllocator::new(),
            value_addresses: HashMap::new(),
            no_spill: SmallVec::new(),
            function_id: None,
            stderr: stderr(),
            phis: HashMap::new(),
            function_offsets: HashMap::new(),
            function_ids: HashMap::new(),
            function_used_registers: HashMap::new(),
        }
    }

    fn generate(mut self) -> GenericResult<(RICSVImage, Box<[u8]>)> {
        for block in self.cfg.graph.node_indices() {
            for statement in self.cfg.graph[block].iter() {
                if let Statement::Definition(value_id) = statement {
                    match &self.cfg.values[*value_id] {
                        Value::Phi(Phi(operands)) => {
                            for operand in operands {
                                self.phis
                                    .entry(*operand)
                                    .or_insert_with(BTreeSet::new)
                                    .insert((block, *value_id));
                            }
                        },

                        Value::Function(fn_id) => {
                            self.function_ids.insert(*value_id, *fn_id);
                        }

                        _ => (),
                    }
                }
            }
        }

        self.comment(&format!("---- START {}", &self.cfg.name))?;
        let jump_over_functions_offset = self.push_code(&jump_placeholder()?)?;
        let after_jump_over_functions_offset = self.current_code_offset()?;

        if self.function_id.is_none() {
            for (fn_id, fn_cfg) in &self.cfg.functions {
                let fn_offset = self.current_code_offset()?;
                self.function_offsets.insert(*fn_id, fn_offset);
                let fn_data_address = self.current_data_offset()?;
                let mut backend = Backend::new(
                    fn_cfg,
                    self.code_base_address + fn_offset,
                    self.ram_base_address + fn_data_address,
                    self.ram_base_address,
                    DumpCode(self.dump_code));
                backend.function_id = Some(*fn_id);
                backend.dump_code = false;

                let (fn_image, used_registers) = backend.generate()?;
                self.function_used_registers.insert(*fn_id, used_registers);
                self.code.extend(fn_image.code);
                self.data.extend(fn_image.data);
                self.comments.extend(fn_image.comments);
            }
        }

        let program_offset = self.current_code_offset()?;
        if program_offset > after_jump_over_functions_offset {
            let jump_offset = i32::try_from(program_offset)?
                .checked_sub(i32::try_from(after_jump_over_functions_offset)?)
                .and_then(|difference| difference.checked_add(4))
                .ok_or(OFFSET_OUT_OF_RANGE)?;
            self.patch_jump(jump_over_functions_offset, registers::ZERO, jump_offset)?;
        } else {
            self.code.clear();
        }

        let mut next_block = self.generate_block(self.cfg.entry_block)?;
        while let Some(block) = next_block {
            next_block = self.generate_block(block)?;
        }

        if self.function_id.is_some() {
            let code = replace(&mut self.code, Vec::new());
            let saved_registers = self.registers
                .used()
                .collect_vec();
            let saved_register_list = saved_registers
                .iter()
                .map(|register| format!("x{}", register))
                .format(", ")
                .to_string();
            let save_registers_comment_address = self.code_base_address + self.current_code_offset()?;
            self.comment(&format!("Save registers {}", &saved_register_list))?;
            for (i, register) in saved_registers.iter().enumerate() {
                self.push_code(&[
                    sw(registers::STACK_POINTER, -i16::try_from(i * 4)?, *register)?
                ])?;
            }
            let saved_registers_size = i16::try_from(saved_registers.len() * 4)?;
            self.push_code(&[
                addi(registers::STACK_POINTER, registers::STACK_POINTER, -saved_registers_size)?
            ])?;

            let comment_fixup_offset = u32::try_from(self.code.len())?;
            self.code.extend(code);
            for (comment_address, _) in self.comments.iter_mut() {
                if *comment_address != save_registers_comment_address {
                    *comment_address += comment_fixup_offset;
                }
            }
        }

        if self.function_id.is_none() {
            self.push_code(&[
                ebreak()?,
            ])?;
        }

        self.comment(&format!("---- END {}", &self.cfg.name))?;

        if self.dump_code {
            self.stderr.reset()?;
            writeln!(self.stderr, "---- CODE DUMP: {} ----", &self.cfg.name)?;
            for (op, offset) in decode(&self.code) {
                let offset = u32::try_from(offset)?;
                if let Some(comments) = self.comments.find(offset) {
                    for comment in comments {
                        writeln!(self.stderr, "// {}", comment)?;
                    }
                }
                self.stderr.set_color(ColorSpec::new()
                    .set_fg(Some(Color::Black))
                    .set_intense(true))?;
                writeln!(self.stderr, "[0x{:08x}]  {:?}", self.code_base_address + u32::try_from(offset)?, op)?;
                self.stderr.reset()?;
            }

            writeln!(self.stderr, "---- DATA DUMP: {} ----", &self.cfg.name)?;

            for (index, &byte) in self.data.iter().enumerate() {
                if index % 4 == 0 {
                    if index > 0 {
                        writeln!(self.stderr)?;
                    }
                    write!(self.stderr, "[{:08x}] ", self.ram_base_address + index as u32)?;
                } else {
                    write!(self.stderr, " ")?;
                }
                self.stderr.set_color(ColorSpec::new()
                    .set_fg(Some(Color::Black))
                    .set_intense(true))?;
                write!(self.stderr, "{:02x}", byte)?;
                self.stderr.reset()?;
            }
            writeln!(self.stderr, "\n")?;
        }

        let image = RICSVImage {
            code: self.code,
            code_base_address: self.code_base_address,
            data: self.data,
            ram_base_address: self.ram_base_address,
            comments: self.comments,
        };
        let used_registers = self.registers
            .used()
            .collect_vec()
            .into_boxed_slice();
        Ok((image, used_registers))
    }

    fn generate_block(&mut self, block: NodeIndex) -> GenericResult<Option<NodeIndex>> {
        let mut next_block = Some(block);
        let basic_block = &self.cfg.graph[block];
        let basic_block_length = basic_block.len();
        for (index, statement) in basic_block.iter().enumerate() {
            next_block = self.generate_statement(block, statement)?;
            assert!(next_block.is_none() || index == basic_block_length - 1);
        }
        Ok(next_block)
    }

    fn generate_statement(&mut self, block: NodeIndex, statement: &Statement) -> GenericResult<Option<NodeIndex>> {
        match statement {
            Statement::Comment(comment) => {
                self.comment(comment)?;
                Ok(None)
            },

            Statement::Definition(value_id) => {
                self.comment(&format!("define {:?}", self.cfg.values[*value_id]))?;
                let register = self.generate_value(*value_id, None)?;
                let phi = self.phis
                    .get(&value_id)
                    .and_then(|phis_by_block| phis_by_block
                        .iter()
                        .find_map(|(phi_block, phi)| if *phi_block >= block {
                            Some(*phi)
                        } else {
                            None
                        }));
                if let Some(phi) = phi {
                    let phi_address = self.allocate_value(phi)?.unwrap();
                    self.store(register, phi_address, AccessWidth::Word)?;
                }
                self.comment(&format!("{}: {:?} -> x{}", value_id, self.cfg.values[*value_id], register))?;
                Ok(None)
            },

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
                let else_branch_offset = i16::try_from(self.current_code_offset()? - jump_to_else_branch)?;
                self.patch_at(jump_to_else_branch, &[
                    // FIXME branches have limited range (+-4KiB), consider jump tables
                    beq(else_branch_offset, registers::ZERO, condition)?
                ])?;

                let else_next_block = self.generate_block(*else_branch)?;
                if let Some(else_next_block) = then_next_block {
                    self.generate_block(else_next_block)?;
                }

                // TODO store code in a format suitable for deletion
                let end_offset = i32::try_from(self.code.len())?
                    .checked_sub(i32::try_from(after_jump_to_end)?)
                    .and_then(|difference| difference.checked_add(4)) // j 4(pc)
                    .ok_or(OFFSET_OUT_OF_RANGE)?;
                self.patch_jump(jump_to_end, registers::ZERO, end_offset)?;
                if after_jump_to_end == self.current_code_offset()? {
                    for _ in 0 .. 4 {
                        self.code.remove(jump_to_end as usize);
                    }
                    let else_branch_offset = else_branch_offset - 4;
                    self.patch_at(jump_to_else_branch, &[
                        beq(else_branch_offset, registers::ZERO, condition)?
                    ])?;
                }

                let then_next_block = then_next_block.unwrap_or(*then_branch);
                let else_next_block = else_next_block.unwrap_or(*else_branch);
                // Check if they converge
                let successors = (
                    self.cfg.graph
                        .edges_directed(then_next_block, Direction::Outgoing)
                        .next()
                        .map(|edge| edge.target()),
                    self.cfg.graph.edges_directed(else_next_block, Direction::Outgoing)
                        .next()
                        .map(|edge| edge.target()),
                );
                assert_eq!(successors.0, successors.1);
                Ok(successors.0)
            },

            Statement::Return(value_id) => {
                self.allocate_register(*value_id, Some(registers::RETURN_VALUE0))?;

                if self.function_id.is_some() {
                    let saved_registers = self.registers
                        .used()
                        .collect_vec();
                    let saved_register_list = saved_registers
                        .iter()
                        .map(|register| format!("x{}", register))
                        .format(", ")
                        .to_string();
                    self.comment(&format!("Restore registers {}", &saved_register_list))?;
                    let saved_registers_size = i16::try_from(saved_registers.len() * 4)?;
                    self.push_code(&[
                        addi(registers::STACK_POINTER, registers::STACK_POINTER, saved_registers_size)?
                    ])?;
                    for (i, register) in saved_registers.iter().enumerate() {
                        self.push_code(&[
                            lw(*register, registers::STACK_POINTER, -i16::try_from(i * 4)?)?
                        ])?;
                    }

                    self.push_code(&[
                        jalr(registers::ZERO, registers::LINK, 0)?,
                    ])?;
                }
                Ok(None)
            },
        }
    }

    fn generate_value(&mut self, value_id: ValueId, target: Option<u8>) -> GenericResult<u8> {
        self.allocate_value(value_id)?;
        match &self.cfg.values[value_id] {
            Value::Unit => Ok(registers::ZERO),

            Value::Int(_) |
            Value::String(_) |
            Value::Function(_) |
            Value::Phi(_) => self.allocate_register(value_id, target),

            Value::AddInt(left, right) => self.generate_binary(value_id, *left, *right, add, target),
            Value::SubInt(left, right) => self.generate_binary(value_id, *left, *right, sub, target),
            Value::MulInt(left, right) => self.generate_binary(value_id, *left, *right, mul, target),
            Value::DivInt(left, right) => self.generate_binary(value_id, *left, *right, div, target),

            Value::Arg(index) => {
                // FIXME this does not conform to the standard ABI.
                let register = self.allocate_register(value_id, target)?;
                self.push_code(&[
                    lw(register, registers::FRAME_POINTER, -i16::try_from(*index * 4)?)?,
                ])?;
                Ok(register)
            },

            Value::Call(function, arguments) => {
                let mut saved_bytes_total = 0;
                if self.function_id.is_some() {
                    self.comment("Save link and frame")?;
                    self.push_code(&[
                        sw(registers::STACK_POINTER, 0, registers::LINK)?,
                        sw(registers::STACK_POINTER, -4, registers::FRAME_POINTER)?,
                    ])?;
                    saved_bytes_total += 8;
                }

                let saved_bytes_env = saved_bytes_total;
                saved_bytes_total += arguments.len() * 4;
                self.comment("Set up frame and stack")?;
                self.push_code(&[
                    addi(registers::FRAME_POINTER, registers::STACK_POINTER, -i16::try_from(saved_bytes_env)?)?,
                    addi(registers::STACK_POINTER, registers::STACK_POINTER, -i16::try_from(saved_bytes_total)?)?,
                ])?;

                let no_spill_length = self.no_spill.len();
                self.comment("Generate arguments")?;
                for (index, argument) in arguments.iter().enumerate() {
                    let register = self.allocate_register(*argument, None)?;
                    self.push_code(&[
                        sw(registers::FRAME_POINTER, -i16::try_from(index * 4)?, register)?,
                    ])?;
                    self.no_spill.push(register);
                }

                self.comment("Call the function")?;
                self.no_spill.truncate(no_spill_length);
                let function = self.allocate_register(*function, None)?;
                self.push_code(&[
                    jalr(registers::LINK, function, 0)?,
                ])?;

                self.comment("Restore stack pointer")?;
                self.push_code(&[
                    addi(registers::STACK_POINTER, registers::STACK_POINTER, i16::try_from(saved_bytes_total)?)?,
                ])?;

                if self.function_id.is_some() {
                    self.comment("Restore link and frame")?;
                    self.push_code(&[
                        lw(registers::LINK, registers::STACK_POINTER, 0)?,
                        lw(registers::FRAME_POINTER, registers::STACK_POINTER, -4)?,
                    ])?;
                }

                let register = self.allocate_register(value_id, None)?;
                self.push_code(&[
                    addi(register, registers::RETURN_VALUE0, 0)?
                ])?;
                Ok(register)
            },

            value => unimplemented!("value {}: {:?}", value_id, value),
        }
    }

    fn generate_binary(
        &mut self, value_id: ValueId, left: ValueId, right: ValueId,
        op: impl FnOnce(u8, u8, u8) -> risky::Result<u32>, target: Option<u8>) -> GenericResult<u8>
    {
        let no_spill_length = self.no_spill.len();
        let left = self.allocate_register(left, None)?;
        self.no_spill.push(left);
        let right = self.allocate_register(right, None)?;
        self.no_spill.push(right);
        let result = self.allocate_register(value_id, target)?;
        self.no_spill.truncate(no_spill_length);
        self.push_code(&[
            op(result, left, right)?
        ])?;
        Ok(result)
    }

    fn allocate_value(&mut self, value_id: ValueId) -> GenericResult<Option<u32>> {
        match self.value_addresses.get(&value_id) {
            Some(address) => Ok(Some(*address)),

            None => {
                let value = &self.cfg.values[value_id];
                let address = match value {
                    Value::Int(n) => {
                        let value = n
                            .to_u32()
                            .ok_or_else(|| format!("number out of range: {}", n))?;
                        Some(self.allocate_u32(value)?)
                    },

                    Value::String(_) => unimplemented!("allocate_value(): Value::String"),

                    Value::Function(fn_id) => {
                        let value = self.code_base_address + self.function_offsets[fn_id];
                        Some(self.allocate_u32(value)?)
                    },

                    Value::Phi(_) => Some(self.allocate_u32(0xDEAD_BEEF)?),

                    Value::AddInt(_, _) |
                    Value::SubInt(_, _) |
                    Value::MulInt(_, _) |
                    Value::DivInt(_, _) => Some(self.allocate_u32(0)?),

                    _ => None,
                };
                if let Some(address) = address {
                    self.value_addresses.insert(value_id, address);
                }
                Ok(address)
            },
        }
    }

    fn allocate_u32(&mut self, data: u32) -> GenericResult<u32> {
        let offset = u32::try_from(self.data.len())?;
        self.data.write_u32::<DataByteOrder>(data)?;
        Ok(self.ram_base_address + offset)
    }

    fn load(&mut self, rd: u8, address: u32, width: AccessWidth) -> GenericResult<()> {
        let load = match width {
            AccessWidth::Byte => lb,
            AccessWidth::HalfWord => lh,
            AccessWidth::Word => lw,
        };
        let offset = i32::try_from(address - self.data_start_address)?;
        let result = match self.check_offset(offset)? {
            Offset::Short => self.push_code(&[
                load(rd, registers::GLOBAL_POINTER, offset as i16)?,
            ]),

            Offset::Long => self.push_code(&[
                lui(rd, address as i32)?,
                load(rd, rd, (address & 0xFFF) as i16)?,
            ]),
        };
        result.map(|_| ())
    }

    fn store(&mut self, register: u8, address: u32, width: AccessWidth) -> GenericResult<()> {
        let store = match width {
            AccessWidth::Byte => sb,
            AccessWidth::HalfWord => sh,
            AccessWidth::Word => sw,
        };
        let offset = i32::try_from(address - self.ram_base_address)?;
        let result = match self.check_offset(offset)? {
            Offset::Short => self.push_code(&[
                store(registers::GLOBAL_POINTER, offset as i16, register)?,
            ]),

            Offset::Long => self.push_code(&[
                lui(registers::TMP0, address as i32)?,
                store(registers::TMP0, (address & 0xFFF) as i16, register)?,
            ]),
        };
        result.map(|_| ())
    }

    fn push_code(&mut self, instructions: &[u32]) -> GenericResult<u32> {
        let code_start = self.code.len();
        let mut cursor = Cursor::new(&mut self.code);
        cursor.set_position(u64::try_from(code_start)?);
        write_code(&mut cursor, instructions)?;
        u32::try_from(code_start).map_err(From::from)
    }

    fn patch_at(&mut self, patch_offset: u32, instructions: &[u32]) -> io::Result<()> {
        let mut cursor = Cursor::new(&mut self.code);
        cursor.set_position(u64::from(patch_offset));
        write_code(&mut cursor, instructions)
    }

    fn current_code_offset(&self) -> Result<u32, TryFromIntError> {
        u32::try_from(self.code.len())
    }

    fn current_data_offset(&self) -> Result<u32, TryFromIntError> {
        u32::try_from(self.data.len())
    }

    fn allocate_register(&mut self, value_id: ValueId, target: Option<u8>) -> GenericResult<u8> {
        let (register, source, previous_user) = self.registers.allocate(value_id, target, &*self.no_spill)?;
        if let Some(previous_user) = previous_user {
            if previous_user != value_id {
                if let Some(address) = self.value_addresses.get(&previous_user) {
                    self.store(register, *address, AccessWidth::Word)?;
                }
            }
        }
        if let Some(source) = source {
            if source != register {
                self.push_code(&[
                    addi(register, source, 0)?,
                ])?;
            }
        } else if let Some(address) = self.value_addresses.get(&value_id) {
            self.load(register, *address, AccessWidth::Word)?;
        }
        Ok(register)
    }

    fn comment(&mut self, comment: &str) -> GenericResult<()> {
        let code_offset = self.code_base_address + self.current_code_offset()?;
        self.comments.insert(code_offset, comment);
        Ok(())
    }

    fn patch_jump(&mut self, patch_offset: u32, rd: u8, offset: i32) -> GenericResult<()> {
        self.patch_at(patch_offset, &match self.check_offset(offset)? {
            Offset::Short => [
                nop()?,
                jal(rd, offset)?,
            ],

            Offset::Long => [
                auipc(registers::TMP0, offset)?,
                jalr(rd, registers::TMP0, offset as i16)?,
            ],
        }).map_err(Into::into)
    }

    fn check_offset(&self, offset: i32) -> Result<Offset, TryFromIntError> {
        // FIXME "- 4" part is probably not portable, check the compressed format manual
        if -(1 << 20) <= offset && offset < 1 << 20 {
            Ok(Offset::Short)
        } else {
            Ok(Offset::Long)
        }
    }
}

#[derive(Deref, DerefMut)]
pub(crate) struct CommentVec(Vec<(u32, Vec<String>)>);

impl CommentVec {
    fn new() -> Self {
        Self(Vec::new())
    }

    fn insert(&mut self, code_offset: u32, comment: &str) {
        let index = match self.0.binary_search_by_key(&code_offset, |(offset, _)| *offset) {
            Ok(index) => index,
            Err(index) => {
                self.0.insert(index, (code_offset, Vec::new()));
                index
            },
        };
        self.0[index].1.push(comment.to_owned());
    }

    pub(crate) fn find(&self, code_offset: u32) -> Option<impl Iterator<Item = &String>> {
        self.0.binary_search_by_key(&code_offset, |(offset, _)| *offset)
            .map(|index| self.0[index].1.iter())
            .ok()
    }
}

impl IntoIterator for CommentVec {
    type Item = (u32, Vec<String>);
    type IntoIter = std::vec::IntoIter<Self::Item>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

enum AccessWidth { Byte, HalfWord, Word }

enum Offset { Short, Long }

#[derive(Copy, Clone, PartialEq)]
enum Allocation { None, Temporary, Permanent }

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
        registers::LINK,
        registers::STACK_POINTER,
        registers::GLOBAL_POINTER,
        registers::FRAME_POINTER,
        registers::RETURN_VALUE0,
        registers::RETURN_VALUE1,
        registers::TMP0,
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
    fn allocate(&mut self, value_id: ValueId, target: Option<u8>, no_spill: &[u8]) -> GenericResult<(u8, Option<u8>, Option<ValueId>)> {
        match target {
            Some(target) => self.try_allocate(value_id, target, IsTarget(true), no_spill)
                .map(|(source, previous_user)| (target, source, previous_user))
                .map_err(From::from),

            None => {
                if let Some(register) = self.values.get_by_left(&value_id) {
                    return Ok((*register, Some(*register), None));
                }
                for register in 0 .. u8::try_from(REGISTER_COUNT)? {
                    if let Allocation::None = self.states[register as usize] {
                        if let Ok((source, previous_user)) = self.try_allocate(value_id, register, IsTarget(false), no_spill) {
                            return Ok((register, source, previous_user));
                        }
                    }
                }
                for register in 0 .. u8::try_from(REGISTER_COUNT)? {
                    if let Allocation::Temporary = self.states[register as usize] {
                        if let Ok((source, previous_user)) = self.try_allocate(value_id, register, IsTarget(false), no_spill) {
                            return Ok((register, source, previous_user));
                        }
                    }
                }
                Err(From::from(RegisterAllocationError))
            }
        }
    }

    fn try_allocate(&mut self, value_id: ValueId, register: u8, IsTarget(is_target): IsTarget, no_spill: &[u8]) -> Result<(Option<u8>, Option<ValueId>), RegisterAllocationError> {
        if self.values.get_by_left(&value_id) == Some(&register) {
            return Ok((Some(register), None));
        }
        let result = match self.states[register as usize] {
            Allocation::None => {
                let previous_user = None;
                let source = self.associate(value_id, register, previous_user);
                self.states[register as usize] = Allocation::Temporary;
                self.used[register as usize] = true;
                Ok((source, previous_user))
            },

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
            },

            Allocation::Permanent => {
                if is_target {
                    let previous_user = self.values.get_by_right(&register).cloned();
                    let source = self.associate(value_id, register, previous_user);
                    Ok((source, previous_user))
                } else {
                    Err(RegisterAllocationError)
                }
            },
        };
        result
    }

    fn associate(&mut self, value_id: ValueId, register: u8, previous_user: Option<ValueId>) -> Option<u8> {
        if let Some(previous_user) = &previous_user {
            self.values.remove_by_left(previous_user);
        }
        let source = self.values
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
        writer.write_u32::<InstructionByteOrder>(*instruction)?;
    }
    Ok(())
}

fn nop() -> risky::Result<u32> {
    addi(0, 0, 0)
}

fn jump_placeholder() -> risky::Result<[u32; 2]> {
    Ok([
        nop()?,
        nop()?,
    ])
}

const OFFSET_OUT_OF_RANGE: &str = "offset out of range";
