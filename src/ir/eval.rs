use std::mem::replace;
use std::ops::Index;
use std::ops::IndexMut;
use std::rc::Rc;

use itertools::Itertools;
use num_bigint::BigInt;
use num_traits::identities::Zero;
use petgraph::graph::NodeIndex;
use petgraph::prelude::Direction::Outgoing;
use petgraph::visit::EdgeRef;

use crate::ir::BasicBlock;
use crate::ir::ControlFlowGraph;
use crate::ir::Ident;
use crate::ir::Statement;
use crate::ir::Value;

pub(crate) struct EvalContext<'a> {
    cfg: &'a ControlFlowGraph,
    env: EvalEnv,
    stack: Vec<Ident>,
    timestamp: usize,
}

impl<'a> EvalContext<'a> {
    pub(crate) fn new(cfg: &'a ControlFlowGraph) -> Self {
        Self {
            cfg,
            env: EvalEnv::new(cfg.id_count, RuntimeValue::undefined()),
            stack: Vec::new(),
            timestamp: 0,
        }
    }

    pub(crate) fn eval(mut self) -> Value {
        self.eval_impl(self.cfg.entry_block)
    }

    fn eval_impl(&mut self, block: NodeIndex) -> Value {
        let mut result = &self.cfg.undefined;
        let mut state = self.new_state(block);
        loop {
            if state.statement_index >= state.basic_block.len() {
                let mut outgoing_edges = self.cfg.graph.edges_directed(state.block, Outgoing);
                match outgoing_edges.next() {
                    Some(edge) => state = self.new_state(edge.target()),
                    None => break,
                };
                debug_assert_eq!(outgoing_edges.next(), None, "detected branching without branching instruction");
            }

            match &state.basic_block[state.statement_index] {
                Statement::Comment(_) => (),

                Statement::Definition { ident, value } => {
                    let value = self.eval_value(value);
                    self.define(ident, value);
                    result = ident;
                }

                Statement::CondJump(var, then_block, else_block) => {
                    let value = &self.env[var].value;
                    match value {
                        Value::Int(n) => {
                            let block = if n.is_zero() { else_block } else { then_block };
                            state = self.new_state(*block);
                            continue;
                        }

                        _ => unreachable!("var `{}' is not a number: {:?}", var, value),
                    }
                }
            }
            state.statement_index += 1;
        }
        replace(&mut self.env[result], RuntimeValue::undefined()).value
    }

    fn new_state(&self, block: NodeIndex) -> LocalEvalState<'a> {
        LocalEvalState {
            block,
            basic_block: self.cfg.graph.block(block),
            statement_index: 0
        }
    }

    fn eval_value(&mut self, value: &Value) -> Value {
        match value {
            Value::Undefined |
            Value::Unit |
            Value::Int(_) |
            Value::String(_) |
            Value::Function { .. } => value.clone(),

            Value::AddInt(left, right) => Value::Int(self.int(left) + self.int(right)),
            Value::SubInt(left, right) => Value::Int(self.int(left) - self.int(right)),
            Value::MulInt(left, right) => Value::Int(self.int(left) * self.int(right)),
            Value::DivInt(left, right) => Value::Int(self.int(left) / self.int(right)),

            Value::AddString(left, right) => Value::String(Rc::new(
                (*self.string(left)).clone() + &self.string(right))),

            Value::Phi(operands) => operands.iter()
                .filter_map(|operand| self.env.get(*operand))
                .minmax_by(|value1, value2| value1.timestamp.cmp(&value2.timestamp))
                .into_option()
                .unwrap()
                .1
                .value
                .clone(),

            Value::Call(function, arguments) => {
                let value = &self.runtime_value(function).value;
                let entry_block = match value {
                    Value::Function(entry_block) => *entry_block,
                    _ => unreachable!("`{}' is not a function: {:?}", function, value),
                };
                let stack_size = self.stack.len();
                for argument in arguments.iter().rev() {
                    self.stack.push(*argument);
                }
                let result = self.eval_impl(entry_block);
                self.stack.resize(stack_size, self.cfg.undefined);
                result
            }

            Value::Arg(index) => self.runtime_value(&self.stack[self.stack.len() - 1 - index])
                .value
                .clone(),

            Value::Return(result) => self.runtime_value(result).value.clone(),
        }
    }

    #[allow(clippy::trivially_copy_pass_by_ref)]
    fn define(&mut self, ident: &Ident, value: Value) {
        let next_timestamp = self.timestamp + 1;
        self.env[ident] = RuntimeValue {
            value,
            timestamp: replace(&mut self.timestamp, next_timestamp),
        };
    }

    #[allow(clippy::trivially_copy_pass_by_ref)]
    fn int(&self, ident: &Ident) -> BigInt {
        self.runtime_value(ident).into()
    }

    #[allow(clippy::trivially_copy_pass_by_ref)]
    fn string(&self, ident: &Ident) -> Rc<String> {
        self.runtime_value(ident).into()
    }

    fn runtime_value(&self, ident: &Ident) -> &RuntimeValue {
        &self.env[ident]
    }
}

#[derive(Deref, DerefMut)]
struct EvalEnv(Vec<RuntimeValue>);

impl EvalEnv {
    fn new(length: usize, default_value: RuntimeValue) -> Self {
        let mut vec = Vec::with_capacity(length);
        vec.resize(length, default_value);
        Self(vec)
    }

    fn get(&self, ident: Ident) -> Option<&RuntimeValue> {
        self.0.get(ident.0)
    }
}

impl Index<&Ident> for EvalEnv {
    type Output = RuntimeValue;

    fn index(&self, ident: &Ident) -> &Self::Output {
        self.index(*ident)
    }
}

impl Index<Ident> for EvalEnv {
    type Output = RuntimeValue;

    fn index(&self, ident: Ident) -> &Self::Output {
        &self.0[ident.0]
    }
}

impl IndexMut<&Ident> for EvalEnv {
    fn index_mut(&mut self, ident: &Ident) -> &mut Self::Output {
        self.index_mut(*ident)
    }
}

impl IndexMut<Ident> for EvalEnv {
    fn index_mut(&mut self, ident: Ident) -> &mut Self::Output {
        &mut self.0[ident.0]
    }
}

struct LocalEvalState<'a> {
    block: NodeIndex,
    basic_block: &'a BasicBlock,
    statement_index: usize,
}

#[derive(Debug, Clone)]
struct RuntimeValue {
    value: Value,
    timestamp: usize,
}

impl RuntimeValue {
    fn undefined() -> Self {
        Self {
            value: Value::Undefined,
            timestamp: 0,
        }
    }
}

impl From<&RuntimeValue> for BigInt {
    fn from(value: &RuntimeValue) -> Self {
        match &value.value {
            Value::Int(n) => n.clone(),
            _ => unreachable!("BigInt::from(): {:?}", value),
        }
    }
}

impl From<&RuntimeValue> for Rc<String> {
    fn from(value: &RuntimeValue) -> Self {
        match &value.value {
            Value::String(s) => s.clone(),
            _ => unreachable!("BigInt::from(): {:?}", value),
        }
    }
}
