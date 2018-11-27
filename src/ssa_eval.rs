use std::cell::RefCell;
use std::collections::HashMap;
use std::mem::size_of;
use std::rc::Rc;

use num_bigint::BigInt;
use num_traits::cast::ToPrimitive;
use num_traits::Zero;

use crate::codegen::SSAId;
use crate::codegen::SSAIdName;
use crate::codegen::SSAOp;
use crate::codegen::SSAStatement;

#[cfg(test)] mod tests;

crate struct SSAEvaluator {
    values: HashMap<SSAIdName, Value>,
    phi: HashMap<SSAId, Rc<RefCell<SSAId>>>,
    stack: Vec<Value>,
    stack_frames: Vec<usize>,
    pc: usize,
    current_value: Value,
    stopped: bool,
    ram: Vec<u8>
}

crate struct EvalResult {
    value: Value,
    ram: Box<[u8]>
}

impl SSAEvaluator {
    crate fn new() -> Self {
        Self {
            values: HashMap::new(),
            phi: HashMap::new(),
            stack: Vec::new(),
            stack_frames: Vec::new(),
            pc: 0,
            current_value: Value::Unit,
            stopped: false,
            ram: Vec::new()
        }
    }

    crate fn eval(mut self, ssa: &[SSAStatement]) -> EvalResult {
        for (index, statement) in ssa.iter().enumerate() {
            match &statement.op {
                SSAOp::Label => self.set_value(&statement.target.id, Value::Label(index)),
                SSAOp::Phi(id1, id2) => {
                    let cell = Rc::new(RefCell::new(id1.clone()));
                    self.phi.insert(id1.clone(), cell.clone());
                    self.phi.insert(id2.clone(), cell);
                },
                _ => ()
            }
        }
        let statement_count = ssa.len();
        while self.pc < statement_count && !self.stopped {
            let statement = &ssa[self.pc];
            self.pc += 1;
            self.eval_statement(statement);
        }
        EvalResult {
            value: self.current_value,
            ram: self.ram.into_boxed_slice()
        }
    }

    fn eval_statement(&mut self, statement: &SSAStatement) {
        match &statement.op {
            SSAOp::Id(id) => self.set_statement_result(statement, self.value(id).clone()),
            SSAOp::Unit => self.set_statement_result(statement, Value::Unit),
            SSAOp::Int(value) => self.set_statement_result(statement, Value::Int(value.clone())),
            SSAOp::SubInt(left, right) => self.set_statement_result(statement, Value::Int(self.value(left).int() - self.value(right).int())),
            SSAOp::AddInt(left, right) => self.set_statement_result(statement, Value::Int(self.value(left).int() + self.value(right).int())),
            SSAOp::MulInt(left, right) => self.set_statement_result(statement, Value::Int(self.value(left).int() * self.value(right).int())),
            SSAOp::DivInt(left, right) => self.set_statement_result(statement, Value::Int(self.value(left).int() / self.value(right).int())),
            SSAOp::Call(function, arguments) => {
                self.stack.push(Value::CallResult(self.pc, statement.target.id.clone()));
                self.stack_frames.push(self.stack.len());
                for argument in arguments.iter().rev() {
                    self.stack.push(self.value(argument).clone());
                }
                self.pc = self.value(function).label();
            },
            SSAOp::Arg(index) => self.set_statement_result(statement, self.stack[self.stack.len() - 1 - index].clone()),
            SSAOp::Return(value) => {
                self.stack.resize(self.stack_frames.pop().unwrap(), Value::Unit);
                let (pc, result_id) = self.stack.pop().unwrap().call_result();
                self.pc = pc;
                let result = self.value(value).clone();
                self.set_value(&result_id, result);
            },
            SSAOp::End(id) => {
                self.current_value = self.value(id).clone();
                self.stopped = true;
            },
            SSAOp::Str(s) => {
                let data = s.as_bytes();
                let size = data.len();
                let address = self.alloc(size);
                self.ram[address .. address + size].copy_from_slice(data);
                self.set_statement_result(statement, Value::Int(address.into()))
            },
            SSAOp::Br(label) => self.pc = self.value(label).label(),
            SSAOp::Cbz(value, label) =>
                if self.value(value).int() == &Zero::zero() {
                    self.pc = self.value(label).label()
                },
            SSAOp::Alloc(size) => {
                let size = self.value(size).int().to_usize().unwrap();
                let address = self.alloc(size);
                self.set_statement_result(statement, Value::Int(address.into()));
            },
            SSAOp::Copy(src, dst, size) => {
                let src = self.value(src).int().to_usize().unwrap();
                let dst = self.value(dst).int().to_usize().unwrap();
                let size = self.value(size).int().to_usize().unwrap();
                let (src, dst) = if src < dst {
                    let (left, right) = self.ram.split_at_mut(dst);
                    (&mut left[src .. src + size], &mut right[0 .. size])
                } else if src > dst {
                    let (left, right) = self.ram.split_at_mut(src);
                    (&mut left[0 .. size], &mut right[dst .. size])
                } else {
                    return;
                };
                dst.copy_from_slice(src);
            },
            SSAOp::Length(address) => {
                let address = self.value(address).int().to_usize().unwrap();
                let length = *Self::block_length(&mut self.ram, address);
                self.set_statement_result(statement, Value::Int(length.into()));
            },
            SSAOp::Phi(id, _) => {
                let id = self.phi[id].borrow().clone();
                self.set_statement_result(statement, self.value(&id).clone())
            },
            SSAOp::Label => ()
        };
    }

    fn set_statement_result(&mut self, statement: &SSAStatement, value: Value) {
        self.set_value(&statement.target.id, value);
    }

    fn set_value(&mut self, id: &SSAId, value: Value) {
        self.values.insert(id.name.clone(), value);
        if let Some(cell) = self.phi.get(id) {
            *cell.borrow_mut() = id.clone();
        }
    }

    fn value(&self, id: &SSAId) -> &Value {
        &self.values[&id.name]
    }

    fn alloc(&mut self, size: usize) -> usize {
        let address = self.ram.len() + Self::BLOCK_LENGTH_SIZE;
        self.ram.resize(address + size, 0);
        *Self::block_length_mut(&mut self.ram, address) = size;
        address
    }

    const BLOCK_LENGTH_SIZE: usize = size_of::<usize>();

    crate fn block_length(ram: &[u8], address: usize) -> &usize {
        unsafe { &*(&ram[address - Self::BLOCK_LENGTH_SIZE] as *const u8 as *const usize) }
    }

    fn block_length_mut(ram: &mut [u8], address: usize) -> &mut usize {
        unsafe { &mut *(&mut ram[address - Self::BLOCK_LENGTH_SIZE] as *mut u8 as *mut usize) }
    }

    crate fn str(ram: &[u8], address: usize) -> &str {
        std::str::from_utf8(&ram[address .. address + *Self::block_length(ram, address)]).unwrap()
    }
}

#[derive(PartialEq, Debug, Clone)]
crate enum Value {
    Unit,
    Int(BigInt),
    Label(usize),
    CallResult(usize, SSAId),
}

impl Value {
    crate fn int(&self) -> &BigInt {
        match self {
            Value::Int(value) => value,
            _ => unreachable!("\n{:?}", self)
        }
    }

    fn label(&self) -> usize {
        match self {
            Value::Label(index) => *index,
            _ => unreachable!("\n{:?}", self)
        }
    }

    fn call_result(&self) -> (usize, SSAId) {
        match self {
            Value::CallResult(pc, id) => (*pc, id.clone()),
            _ => unreachable!("\n{:?}", self)
        }
    }
}
