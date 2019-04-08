/*
Rules of the game
=================

x0: hardwired to constant zero
x1: function call return value
x2: stack pointer,
x3: frame pointer
x4: bump memory allocator pointer

When image is executed, data is copied to RAM as-is, and x4 is set to
the end of the data in RAM. Subsequent RAM allocations will just use x4 as
the allocation address and add the allocation size to x4.
*/

use std::cmp::Ordering;
use std::collections::btree_set::BTreeSet;
use std::collections::HashMap;
use std::convert::TryFrom;
use std::error::Error;
use std::fmt;
use std::io;
use std::io::Cursor;
use std::io::Write;
use std::mem::size_of_val;
use std::num::TryFromIntError;

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
use crate::ir::Statement;
use crate::ir::Value;
use crate::ir::value_storage::ValueId;
use crate::riscv::{DataByteOrder, InstructionByteOrder};
use crate::riscv_decoder::decode;
use crate::utils::{GenericResult, stderr};

#[cfg(test)]
mod tests;

pub(crate) struct DumpCode(pub(crate) bool);

pub(crate) fn generate(cfg: &ControlFlowGraph, dump_code: DumpCode) -> GenericResult<RICSVImage> {
    Backend::new(cfg, dump_code).generate()
}

pub(crate) struct RICSVImage {
    pub(crate) code: Vec<u8>,
    pub(crate) code_base_address: u32,
    pub(crate) data: Vec<u8>,
    pub(crate) ram_base_address: u32,
    pub(crate) comments: HashMap<u32, Vec<String>>,
}

impl RICSVImage {
    fn new() -> Self {
        Self {
            code: Vec::new(),
            code_base_address: 0x0000_0000,
            data: Vec::new(),
            ram_base_address: 0xD000_0000,
            comments: HashMap::new(),
        }
    }
}

#[derive(Copy, Clone, Eq, PartialEq, Hash)]
struct ValueState {
    address: u32,
    register: Option<u8>,
}

impl fmt::Debug for ValueState {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "ValueState {{ 0x{:08x}, {:?} }}", self.address, self.register)
    }
}

impl ValueState {
    fn new(address: u32) -> Self {
        Self {
            address,
            register: None,
        }
    }
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

#[derive(Deref, Copy, Clone, Eq, PartialEq)]
struct Reused(bool);

struct Backend<'a> {
    cfg: &'a ControlFlowGraph,
    dump_code: bool,
    image: RICSVImage,
    registers: RegisterAllocator,
    values: HashMap<ValueId, ValueState>,
    registers_used: HashMap<u8, ValueId>,
    no_spill: SmallVec<[u8; registers::COUNT]>,
    is_in_function: bool,
    stderr: StandardStream,
    phis: HashMap<ValueId, BTreeSet<(NodeIndex, ValueId)>>,
}

impl<'a> Backend<'a> {
    fn new(cfg: &'a ControlFlowGraph, DumpCode(dump_code): DumpCode) -> Self {
        Self {
            cfg,
            dump_code,
            image: RICSVImage::new(),
            registers: RegisterAllocator::new(),
            values: HashMap::new(),
            registers_used: HashMap::new(),
            no_spill: SmallVec::new(),
            is_in_function: false,
            stderr: stderr(),
            phis: HashMap::new(),
        }
    }

    fn generate(mut self) -> GenericResult<RICSVImage> {
        for block in self.cfg.graph.node_indices() {
            for statement in self.cfg.graph[block].iter() {
                if let Statement::Definition(phi) = statement {
                    if let Value::Phi(Phi(operands)) = &self.cfg.values[*phi] {
                        for operand in operands {
                            self.phis
                                .entry(*operand)
                                .or_insert_with(BTreeSet::new)
                                .insert((block, *phi));
                        }
                    }
                }
            }
        }
        let mut next_block = self.generate_block(self.cfg.entry_block)?;
        while let Some(block) = next_block {
            next_block = self.generate_block(block)?;
        }
        self.push_code(&[
            ebreak()?,
        ])?;
        Ok(self.image)
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
                self.comment(&format!("{:?}", self.cfg.values[*value_id]))?;
                let register = self.generate_value(block, *value_id)?;
                self.comment(&format!("{:?} -> x{}", self.cfg.values[*value_id], register))?;
                Ok(None)
            },

            Statement::CondJump(condition, then_branch, else_branch) => {
                let condition = self.generate_value(block, *condition)?;
                let jump_to_else_branch = self.push_code(&[ebreak()?])?;
                let then_next_block = self.generate_block(*then_branch)?;
                if let Some(then_next_block) = then_next_block {
                    self.generate_block(then_next_block)?;
                }
                let jump_to_end = self.push_code(&[ebreak()?])?;
                let after_jump_to_end = self.current_code_offset()?;
                let else_branch_offset = u16::try_from(self.current_code_offset()? - jump_to_else_branch)?;
                self.patch_at(jump_to_else_branch, &[
                    beq(else_branch_offset, registers::ZERO, condition)?
                ])?;

                let else_next_block = self.generate_block(*else_branch)?;
                if let Some(else_next_block) = then_next_block {
                    self.generate_block(else_next_block)?;
                }

                // TODO store code in a format suitable for deletion
                self.patch_at(jump_to_end, &[
                    jal(registers::TMP0, self.current_code_offset()? - jump_to_end)?
                ])?;
                if after_jump_to_end == self.current_code_offset()? {
                    for _ in 0 .. 4 {
                        self.image.code.remove(jump_to_end as usize);
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
                let register = self.generate_value(block, *value_id)?;
                self.push_code(&[
                    add(registers::RESULT, register, registers::ZERO)?,
                ])?;
                if self.is_in_function {
                    unimplemented!("return from a function")
                }
                Ok(None)
            },
        }
    }

    fn generate_value(&mut self, block: NodeIndex, value_id: ValueId) -> GenericResult<u8> {
        let phi = self.phis
            .get(&value_id)
            .and_then(|phis_by_block| phis_by_block
                .iter()
                .find_map(|(phi_block, phi)| if *phi_block >= block {
                    Some(*phi)
                } else {
                    None
                }));

        let address = self.allocate_value(value_id)?;
        match &self.cfg.values[value_id] {
            Value::Unit => Ok(registers::ZERO),

            Value::Int(_) |
            Value::String(_) |
            Value::Function(_) |
            Value::Phi(_) => {
                match &self.values[&value_id].register {
                    Some(register) => Ok(*register),
                    None => {
                        let (register, reused) = self.allocate_register(value_id)?;
                        if !*reused  {
                            self.load(register, address, AccessWidth::Word)?;
                        }
                        if let Some(phi) = phi {
                            let phi_address = self.allocate_value(phi)?;
                            self.store(register, phi_address, AccessWidth::Word)?;
                        }
                        Ok(register)
                    },
                }
            },

            Value::AddInt(left, right) => self.generate_binary(block, value_id, *left, *right, add),
            Value::SubInt(left, right) => self.generate_binary(block, value_id, *left, *right, sub),
            Value::MulInt(left, right) => self.generate_binary(block, value_id, *left, *right, mul),
            Value::DivInt(left, right) => self.generate_binary(block, value_id, *left, *right, div),

            value => unimplemented!("value {}: {:?}", value_id, value),
        }
    }

    fn generate_binary(
        &mut self, block: NodeIndex, value_id: ValueId, left: ValueId, right: ValueId,
        op: impl FnOnce(u8, u8, u8) -> risky::Result<u32>) -> GenericResult<u8>
    {
        let left = self.generate_value(block, left)?;

        let no_spill_len = self.no_spill.len();
        self.no_spill.push(left);

        let right = self.generate_value(block, right)?;
        self.no_spill.push(right);

        let (result, _) = self.allocate_register(value_id)?;
        self.push_code(&[
            op(result, left, right)?
        ])?;

        self.no_spill.truncate(no_spill_len);
        Ok(result)
    }

    fn allocate_value(&mut self, value_id: ValueId) -> GenericResult<u32> {
        match self.values.get(&value_id) {
            Some(state) => Ok(state.address),

            None => {
                let value = &self.cfg.values[value_id];
                let address = match value {
                    Value::Unit => u32::max_value(),
                    Value::Int(n) => self.allocate_u32(n.to_u32()
                        .unwrap_or_else(|| panic!("number out of range: {}", n)))?,

                    Value::String(_) |
                    Value::Function(_) => unimplemented!(),

                    _ => self.allocate_u32(0)?,
                };
                self.values.insert(value_id, ValueState::new(address));
                Ok(address)
            },
        }
    }

    fn allocate_u32(&mut self, data: u32) -> GenericResult<u32> {
        let offset = u32::try_from(self.image.data.len())?;
        self.image.data.write_u32::<DataByteOrder>(data)?;
        Ok(self.image.ram_base_address + offset)
    }

    fn load(&mut self, rd: u8, address: u32, width: AccessWidth) -> GenericResult<()> {
        let load = match width {
            AccessWidth::Byte => lb,
            AccessWidth::HalfWord => lh,
            AccessWidth::Word => lw,
        };
        let (address_low, address_high) = split_immediate_12_20(address);
        self.push_code(&[
            lui(rd, address_high)?,
            load(rd, rd, address_low)?,
        ])?;
        Ok(())
    }

    fn store(&mut self, register: u8, address: u32, width: AccessWidth) -> GenericResult<()> {
        let store = match width {
            AccessWidth::Byte => sb,
            AccessWidth::HalfWord => sh,
            AccessWidth::Word => sw,
        };
        let (address_low, address_high) = split_immediate_12_20(address);
        self.push_code(&[
            lui(registers::TMP0, address_high)?,
            store(address_low, registers::TMP0, register)?,
        ])?;
        Ok(())
    }

    fn push_code(&mut self, instructions: &[u32]) -> GenericResult<u32> {
        let code = &mut self.image.code;
        let encoded_start = code.len();
        push_code(code, instructions)?;
        let encoded_end = code.len();
        if self.dump_code {
            self.stderr.set_color(ColorSpec::new()
                .set_fg(Some(Color::Black))
                .set_intense(true))?;
            let code_base_address = self.image.code_base_address;
            writeln!(self.stderr, "{}", decode(&code[encoded_start .. encoded_end])
                .map(|(op, offset)| format!("[0x{:08x}]  {:?}",
                    code_base_address as usize + encoded_start + offset as usize, op))
                .format("\n"))?;
            self.stderr.reset()?;
        }
        let code_offset = u32::try_from(encoded_start)?;
        Ok(code_offset)
    }

    fn patch_at(&mut self, code_offset: u32, instructions: &[u32]) -> io::Result<()> {
        let code = &mut self.image.code;
        let encoded_start = code_offset as usize;
        let encoded_end = encoded_start + instructions.len() * size_of_val(&instructions[0]);
        let patch_area = &mut code[encoded_start .. encoded_end];
        let mut cursor = Cursor::new(patch_area);
        for instruction in instructions {
            cursor.write_u32::<InstructionByteOrder>(*instruction)?;
        }
        Ok(())
    }

    fn current_code_offset(&self) -> Result<u32, TryFromIntError> {
        u32::try_from(self.image.code.len())
    }

    fn allocate_register(&mut self, value_id: ValueId) -> GenericResult<(u8, Reused)> {
        let previously_allocated_register =
            self.values.get(&value_id).and_then(|state| state.register);
        match previously_allocated_register {
            Some(register) => Ok((register, Reused(true))),

            None => {
                let (register, spilled) = self.registers.allocate(&*self.no_spill)?;
                if *spilled {
                    let spilled_value_id = self.registers_used[&register];
                    let spilled_value = self.values.get_mut(&spilled_value_id).unwrap();
                    spilled_value.register = None;

                    let spilled_value_address = spilled_value.address;
                    if self.dump_code {
                        writeln!(self.stderr, "// spilling {}: 0x{:08x} <- x{}",
                                 spilled_value_id, spilled_value_address, register)?;
                    }
                    self.store(register, spilled_value_address, AccessWidth::Word)?;
                }

                self.registers_used.insert(register, value_id);
                self.values.get_mut(&value_id).unwrap().register = Some(register);
                Ok((register, Reused(false)))
            },
        }
    }

    fn comment(&mut self, comment: &str) -> GenericResult<()> {
        if self.dump_code {
            writeln!(self.stderr, "// {}", comment)?;
        }
        let code_offset = self.current_code_offset()?;
        self.image.comments
            .entry(code_offset)
            .or_insert_with(Vec::new)
            .push(comment.to_owned());
        Ok(())
    }
}

enum AccessWidth { Byte, HalfWord, Word }

pub(crate) mod registers {
    macro_rules! reserved_registers {
        ($($name: ident = $value: literal,)+) => {
            $(pub(crate) const $name: u8 = $value;)+
            pub(crate) const COUNT: usize = 32;
            pub(crate) const RESERVED: &[u8] = &[$($value,)+];
        };
    }

    reserved_registers! {
        ZERO = 0,
        RESULT = 1,
//        STACK_POINTER = 2,
//        FRAME_POINTER = 3,
//        ALLOCATION_POINTER = 4,
        TMP0 = 31,
    }
}

#[derive(Copy, Clone, PartialEq)]
enum Allocation { None, Temporary, Permanent }

struct RegisterAllocator {
    states: [Allocation; 32],
}

#[derive(Deref, Copy, Clone)]
struct Spilled(bool);

impl RegisterAllocator {
    fn new() -> Self {
        let mut states = [Allocation::None; 32];
        for register in registers::RESERVED.iter() {
            states[*register as usize] = Allocation::Permanent;
        }
        Self { states }
    }

    fn allocate(&mut self, no_spill: &[u8]) -> Result<(u8, Spilled), RegisterAllocationError> {
        for (register, state) in self.states_mut() {
            if *state == Allocation::None {
                *state = Allocation::Temporary;
                return Ok((register as u8, Spilled(false)))
            }
        }
        for (register, state) in self.states_mut() {
            if *state == Allocation::Temporary && !no_spill.contains(&register) {
                return Ok((register as u8, Spilled(true)))
            }
        }
        Err(RegisterAllocationError)
    }

    fn states_mut(&mut self) -> impl Iterator<Item = (u8, &mut Allocation)> {
        self.states
            .iter_mut()
            .enumerate()
            .map(|(i, state)| (i as u8, state))
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

fn split_immediate_12_20(imm: u32) -> (u16, u32) {
    (imm as u16 & 0xFFF, imm >> 12)
}

pub(crate) fn push_code(writer: &mut impl Write, code: &[u32]) -> io::Result<()> {
    for instruction in code {
        writer.write_u32::<InstructionByteOrder>(*instruction)?;
    }
    Ok(())
}
