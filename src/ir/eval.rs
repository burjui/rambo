use std::collections::HashMap;
use std::mem::replace;
use std::ops::Deref;
use std::ops::DerefMut;
use std::ops::Index;
use std::ops::IndexMut;
use std::rc::Rc;

use itertools::Itertools;
use num_bigint::BigInt;
use num_traits::identities::Zero;
use petgraph::graph::NodeIndex;
use petgraph::prelude::Direction::Outgoing;
use petgraph::visit::EdgeRef;

use crate::ir::{ControlFlowGraph, FunctionMap};
use crate::ir::Statement;
use crate::ir::Value;
use crate::ir::VarId;

pub(crate) struct EvalContext<'a> {
    cfg: &'a ControlFlowGraph,
    functions: &'a FunctionMap,
    env: EvalEnv,
    stack: Vec<Value>,
    timestamp: usize,
    unit: Value,
}

impl<'a> EvalContext<'a> {
    pub(crate) fn new(cfg: &'a ControlFlowGraph, functions: &'a FunctionMap) -> Self {
        Self {
            cfg,
            functions,
            env: EvalEnv::new(),
            stack: Vec::new(),
            timestamp: 0,
            unit: Value::Unit,
        }
    }

    pub(crate) fn eval(mut self) -> Value {
        self.eval_impl(self.cfg.entry_block)
    }

    fn eval_impl(&mut self, block: NodeIndex) -> Value {
        let mut result = &self.unit;
        let mut state = self.new_state(block);
        loop {
            let statement = state.statements.next();
            if statement.is_none() {
                let mut outgoing_edges = self.cfg.graph.edges_directed(state.block, Outgoing);
                match outgoing_edges.next() {
                    Some(edge) => state = self.new_state(edge.target()),
                    None => break,
                };
                debug_assert_eq!(outgoing_edges.next(), None, "detected branching without branching instruction");
            }

            match statement {
                Some(Statement::Comment(_)) => (),

                Some(Statement::Definition { ident, value_index }) => {
                    let value = self.eval_value(&self.cfg.values[*value_index]);
                    self.define(ident, value);
                    result = &self.env[ident].value;
                }

                Some(Statement::CondJump(var, then_block, else_block)) => {
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

                Some(Statement::Return(id)) => {
                    result = &self.env[id].value;
                    break;
                }

                None => break,
            }
        }
        result.clone()
    }

    fn new_state(&self, block: NodeIndex) -> LocalEvalState<'a> {
        LocalEvalState {
            block,
            statements: Box::new(self.cfg.graph[block].iter()),
        }
    }

    fn eval_value(&mut self, value: &Value) -> Value {
        match value {
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
                let fn_id = match value {
                    Value::Function(fn_id) => fn_id,
                    _ => unreachable!("`{}' is not a function: {:?}", function, value),
                };
                let function_cfg = &self.functions[fn_id];
                let mut function_context = EvalContext::new(function_cfg, &self.cfg.functions);
                for argument in arguments.iter() {
                    let runtime_value = self.env[argument].clone();
                    let value = runtime_value.value.clone();
                    function_context.stack.push(value);
                }
                function_context.eval()
            }

            Value::Arg(index) => self.stack[*index].clone(),
        }
    }

    #[allow(clippy::trivially_copy_pass_by_ref)]
    fn define(&mut self, ident: &VarId, value: Value) {
        let next_timestamp = self.timestamp + 1;
        self.env[ident] = RuntimeValue {
            value,
            timestamp: replace(&mut self.timestamp, next_timestamp),
        };
    }

    #[allow(clippy::trivially_copy_pass_by_ref)]
    fn int(&self, ident: &VarId) -> BigInt {
        self.runtime_value(ident).into()
    }

    #[allow(clippy::trivially_copy_pass_by_ref)]
    fn string(&self, ident: &VarId) -> Rc<String> {
        self.runtime_value(ident).into()
    }

    fn runtime_value(&self, ident: &VarId) -> &RuntimeValue {
        &self.env[ident]
    }
}

struct EvalEnv(HashMap<VarId, RuntimeValue>);

impl EvalEnv {
    fn new() -> Self {
        Self(HashMap::new())
    }

    fn get(&self, ident: VarId) -> Option<&RuntimeValue> {
        self.0.get(&ident)
    }
}

impl Deref for EvalEnv {
    type Target = HashMap<VarId, RuntimeValue>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl DerefMut for EvalEnv {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl Index<&VarId> for EvalEnv {
    type Output = RuntimeValue;

    fn index(&self, ident: &VarId) -> &Self::Output {
        self.index(*ident)
    }
}

impl Index<VarId> for EvalEnv {
    type Output = RuntimeValue;

    fn index(&self, ident: VarId) -> &Self::Output {
        &self.0[&ident]
    }
}

impl IndexMut<&VarId> for EvalEnv {
    fn index_mut(&mut self, ident: &VarId) -> &mut Self::Output {
        &mut self[*ident]
    }
}

impl IndexMut<VarId> for EvalEnv {
    fn index_mut(&mut self, ident: VarId) -> &mut Self::Output {
        self.entry(ident).or_default()
    }
}

struct LocalEvalState<'a> {
    block: NodeIndex,
    statements: Box<dyn Iterator<Item = &'a Statement> + 'a>,
}

#[derive(Debug, Clone)]
struct RuntimeValue {
    value: Value,
    timestamp: usize,
}

impl Default for RuntimeValue {
    fn default() -> Self {
        Self {
            value: Value::Unit,
            timestamp: 0,
        }
    }
}

impl From<&RuntimeValue> for BigInt {
    fn from(value: &RuntimeValue) -> Self {
        match &value.value {
            Value::Int(n) => n.clone(),
            _ => unreachable!("BigInt::from(): {:?}", &value.value),
        }
    }
}

impl From<&RuntimeValue> for Rc<String> {
    fn from(value: &RuntimeValue) -> Self {
        match &value.value {
            Value::String(s) => s.clone(),
            _ => unreachable!("Rc<String>::from(): {:?}", &value.value),
        }
    }
}
