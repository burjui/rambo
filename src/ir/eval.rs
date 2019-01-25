use std::mem::replace;
use std::rc::Rc;

use hashbrown::HashMap;
use itertools::Itertools;
use num_bigint::BigInt;
use num_traits::identities::Zero;
use petgraph::graph::NodeIndex;
use petgraph::prelude::Direction::Outgoing;
use petgraph::visit::EdgeRef;

use crate::ir::ControlFlowGraph;
use crate::ir::Ident;
use crate::ir::Statement;
use crate::ir::Value;

#[derive(Debug)]
struct RuntimeValue {
    value: Value,
    timestamp: usize,
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

pub(crate) struct EvalContext<'a> {
    cfg: &'a ControlFlowGraph,
    env: HashMap<Ident, RuntimeValue>,
    stack: Vec<Ident>,
    timestamp: usize,
}

impl<'a> EvalContext<'a> {
    pub(crate) fn new(cfg: &'a ControlFlowGraph) -> Self {
        Self {
            cfg,
            env: HashMap::new(),
            stack: Vec::new(),
            timestamp: 0,
        }
    }

    pub(crate) fn eval(mut self) -> Value {
        self.eval_impl(self.cfg.entry_block)
    }

    fn eval_impl(&mut self, block: NodeIndex) -> Value {
        let mut result = self.cfg.undefined;
        let (mut block, mut index) = (block, 0);
        loop {
            let basic_block = self.cfg.graph.block(block);
            if index >= basic_block.len() {
                let outgoing_edges = self.cfg.graph.0
                    .edges_directed(block, Outgoing)
                    .collect_vec();
                debug_assert!(outgoing_edges.len() <= 1, "detected branching without branching instruction");
                if outgoing_edges.is_empty() {
                    break;
                } else {
                    block = outgoing_edges[0].target();
                    index = 0;
                }
            }

            match &basic_block[index] {
                Statement::Comment(_) => (),

                Statement::Definition { ident, value } => {
                    let value = self.eval_value(value);
                    result = *ident;
                    self.define(result, value);
                }

                Statement::CondJump(var, then_block, else_block) => {
                    let value = &self.env[var].value;
                    match value {
                        Value::Int(n) => {
                            block = if n.is_zero() { *else_block } else { *then_block };
                            index = 0;
                            continue;
                        }

                        _ => unreachable!("var `{:?}' is not a number: {:?}", var, value),
                    }
                }
            }
            index += 1;
        }
        self.env.remove(&result).unwrap().value
    }

    fn eval_value(&mut self, value: &Value) -> Value {
        match value {
            Value::Undefined |
            Value::Unit |
            Value::Int(_) |
            Value::String(_) |
            Value::Function {..} => value.clone(),

            Value::AddInt(left, right) => Value::Int(self.int(left) + self.int(right)),
            Value::SubInt(left, right) => Value::Int(self.int(left) - self.int(right)),
            Value::MulInt(left, right) => Value::Int(self.int(left) * self.int(right)),
            Value::DivInt(left, right) => Value::Int(self.int(left) / self.int(right)),

            Value::AddString(left, right) => Value::String(Rc::new(
                (*self.string(left)).clone() + &self.string(right))),

            Value::Phi(operands) => operands.iter()
                .filter_map(|operand| self.env.get(operand))
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
                    _ => unreachable!("`{:?}' is not a function: {:?}", function, value),
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
        }
    }

    fn define(&mut self, ident: Ident, value: Value) {
        let next_timestamp = self.timestamp + 1;
        self.env.insert(ident, RuntimeValue {
            value,
            timestamp: replace(&mut self.timestamp, next_timestamp),
        });
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
