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

use std::collections::HashMap;
use std::convert::TryFrom;
use std::error::Error;
use std::fmt;
use std::io;
use std::io::Cursor;
use std::io::Write;

use byteorder::LittleEndian;
use byteorder::ReadBytesExt;
use byteorder::WriteBytesExt;
use itertools::Itertools;
use num_traits::cast::ToPrimitive;
use petgraph::stable_graph::NodeIndex;
use risky::instructions::*;
use rvsim::Op;
use smallvec::SmallVec;
use termcolor::{Color, ColorSpec, StandardStream, WriteColor};

use crate::ir::ControlFlowGraph;
use crate::ir::Statement;
use crate::ir::Value;
use crate::ir::value_storage::ValueId;
use crate::utils::{GenericResult, stderr};

#[cfg(test)]
mod tests;

type InstructionByteOrder = LittleEndian;
type DataByteOrder = LittleEndian;

pub(crate) struct DumpCode(pub(crate) bool);

pub(crate) fn generate(cfg: &ControlFlowGraph, dump_code: DumpCode) -> GenericResult<RICSVImage> {
    Backend::new(cfg, dump_code).generate()
}

pub(crate) struct RICSVImage {
    pub(crate) code: Vec<u8>,
    pub(crate) code_base_address: u32,
    pub(crate) data: Vec<u8>,
    pub(crate) ram_base_address: u32,
}

impl RICSVImage {
    fn new() -> Self {
        Self {
            code: Vec::new(),
            code_base_address: 0x0000_0000,
            data: Vec::new(),
            ram_base_address: 0xD000_0000,
        }
    }
}

#[derive(Copy, Clone, Eq, PartialEq, Hash)]
struct ValueState {
    address: u32,
    register: Option<u8>,
}

impl ValueState {
    fn new(address: u32) -> Self {
        Self {
            address,
            register: None,
        }
    }
}

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
        }
    }

    fn generate(mut self) -> GenericResult<RICSVImage> {
        let mut next_block = Some(self.cfg.entry_block);
        while let Some(block) = next_block {
            next_block = self.generate_block(block)?;
        }
        self.push_code(&[
            ebreak()?,
        ])?;
        Ok(self.image)
    }

    fn generate_block(&mut self, block: NodeIndex) -> GenericResult<Option<NodeIndex>> {
        let basic_block = &self.cfg.graph[block];
        let mut next_block = None;
        for (index, statement) in basic_block.iter().enumerate() {
            next_block = self.generate_statement(statement)?;
            if next_block.is_some() {
                assert_eq!(index, basic_block.len() - 1);
                break;
            }
        }
        Ok(next_block)
    }

    fn generate_statement(&mut self, statement: &Statement) -> GenericResult<Option<NodeIndex>> {
        match statement {
            Statement::Comment(_) => (),
            Statement::Definition(value_id) => {
                self.generate_value(*value_id)?;
            },
            // Statement::CondJump(ValueId, NodeIndex, NodeIndex),
             Statement::Return(value_id) => {
                 let register = self.generate_value(*value_id)?;
                 self.push_code(&[
                     add(registers::RESULT, register, registers::ZERO)?,
                 ])?;

                 if self.is_in_function {
                     unimplemented!("return from a function")
                 }
             },
            _ => unimplemented!("statement: {:?}", statement),
        }
        Ok(None)
    }

    fn generate_value(&mut self, value_id: ValueId) -> GenericResult<u8> {
        let address = self.allocate_value(value_id)?;
        let value = &self.cfg.values[value_id];
        match value {
            Value::Unit |
            Value::Int(_) |
            Value::String(_) |
            Value::Function(_) => {
                match &self.values[&value_id].register {
                    Some(register) => Ok(*register),
                    None => {
                        let register = self.allocate_register(value_id)?;
                        self.load(register, address, AccessWidth::Word)?;
                        Ok(register)
                    },
                }
            },

            Value::AddInt(left, right) => self.generate_binary(value_id, *left, *right, "+", add),
            Value::SubInt(left, right) => self.generate_binary(value_id, *left, *right, "-", sub),
            Value::MulInt(left, right) => self.generate_binary(value_id, *left, *right, "*", mul),
            Value::DivInt(left, right) => self.generate_binary(value_id, *left, *right, "/", div),

            _ => unimplemented!("value {}: {:?}", value_id, value),
        }
    }

    fn generate_binary(
        &mut self, value_id: ValueId, left: ValueId, right: ValueId,
        op_name: &'static str,
        op: impl FnOnce(u8, u8, u8) -> risky::Result<u32>) -> GenericResult<u8>
    {
        if self.dump_code {
            writeln!(self.stderr, "// {} {} {}", left, op_name, right)?;
        }
        let left = self.generate_value(left)?;

        let no_spill_len = self.no_spill.len();
        self.no_spill.push(left);

        let right = self.generate_value(right)?;
        self.no_spill.push(right);

        let result = self.allocate_register(value_id)?;
        self.push_code(&[
            op(result, left, right)?
        ])?;

        self.no_spill.truncate(no_spill_len);
        Ok(result)
    }

    fn allocate_value(&mut self, value_id: ValueId) -> GenericResult<u32> {
        match self.values.get(&value_id) {
            Some(ValueState { address, .. }) => Ok(*address),
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

    fn push_code(&mut self, instructions: &[u32]) -> io::Result<()> {
        let code = &mut self.image.code;
        let encoded_start = code.len();
        push_code(code, instructions)?;
        let encoded_end = code.len();
        if self.dump_code {
            self.stderr.set_color(ColorSpec::new()
                .set_fg(Some(Color::Black))
                .set_intense(true))?;
            writeln!(self.stderr, "{:?}", decode(&code[encoded_start..encoded_end]).format("\n"))?;
            self.stderr.reset()?;
        }
        Ok(())
    }

    fn allocate_register(&mut self, value_id: ValueId) -> GenericResult<u8> {
        let (register, spilled) = self.registers.allocate(&*self.no_spill)?;
        if *spilled {
            let value_id = self.registers_used[&register];
            let spilled_value = self.values.get_mut(&value_id).unwrap();
            spilled_value.register = None;
            let spilled_value_address = spilled_value.address;
            if self.dump_code {
                writeln!(self.stderr, "// spilling {}: 0x{:08x} <- r{}",
                         value_id, spilled_value_address, register)?;
            }
            self.store(register, spilled_value_address, AccessWidth::Word)?;
        }
        self.registers_used.insert(register, value_id);
        self.values.get_mut(&value_id).unwrap().register = Some(register);
        Ok(register)
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

pub(crate) struct Decoder<'a>(Cursor<&'a [u8]>);

impl Iterator for Decoder<'_> {
    type Item = Op;

    fn next(&mut self) -> Option<Self::Item> {
        self.0
            .read_u32::<InstructionByteOrder>()
            .map(|instruction| Op::parse(instruction)
                .unwrap_or_else(|| panic!("failed to parse instruction: 0x{:08x}", instruction)))
            .ok()
    }
}

pub(crate) fn decode(code: &[u8]) -> Decoder<'_> {
    Decoder(Cursor::new(code))
}

fn split_immediate_12_20(imm: u32) -> (u16, u32) {
    (imm as u16 & 0xFFF, imm >> 12)
}

pub(crate) fn push_code(writer: &mut impl Write, code: &[u32]) -> io::Result<()> {
    for instruction in code {
        writer.write_u32::<InstructionByteOrder>(*instruction)?;
    }
    Ok(())
}
