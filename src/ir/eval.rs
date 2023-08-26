use crate::ir::value_storage::ValueId;
use crate::ir::FunctionMap;
use crate::ir::IRModule;
use crate::ir::Statement;
use crate::ir::Value;
use crate::stable_graph::Direction;
use crate::stable_graph::NodeIndex;
use core::convert::TryFrom;
use core::convert::TryInto;
use itertools::Itertools;
use rustc_hash::FxHashMap;
use std::mem::replace;
use std::ops::Deref;
use std::ops::DerefMut;
use std::ops::Index;
use std::ops::IndexMut;
use std::rc::Rc;

pub(crate) fn eval(module: &IRModule) -> Value {
    EvalContext::new(module, &module.functions).eval()
}

struct EvalContext<'a> {
    module: &'a IRModule,
    functions: &'a FunctionMap,
    env: EvalEnv,
    stack: Vec<Value>,
    timestamp: usize,
    unit: Value,
}

impl<'a> EvalContext<'a> {
    fn new(module: &'a IRModule, functions: &'a FunctionMap) -> Self {
        Self {
            module,
            functions,
            env: EvalEnv::new(),
            stack: Vec::new(),
            timestamp: 0,
            unit: Value::Unit,
        }
    }

    fn eval(mut self) -> Value {
        self.eval_impl(self.module.entry_block)
    }

    fn eval_impl(&mut self, block: NodeIndex) -> Value {
        let mut result = &self.unit;
        let mut state = self.new_state(block);

        loop {
            let basic_block = &self.module.cfg[state.block];
            let Some(statement_index) = state.statement_index else {
                let mut outgoing_edges = self
                    .module
                    .cfg
                    .edges_directed(state.block, Direction::Outgoing);
                let outgoing_edge = outgoing_edges.next();

                match outgoing_edge {
                    Some(edge) => {
                        state = self.new_state(edge.target);
                        state.statement_index = self.module.cfg[state.block].indices().next();
                        continue;
                    }

                    None => break,
                };
            };

            let statement = match basic_block.get(statement_index) {
                Some(statement) => {
                    state.statement_index = basic_block.next_index(statement_index);
                    statement
                }

                None => break,
            };

            match statement {
                Statement::Comment(_) => (),

                Statement::Definition(value_id) => {
                    let value = self.eval_value(&self.module.values[*value_id]);
                    self.define(*value_id, value);
                    result = &self.env[value_id].value;
                }

                Statement::CondJump(var, then_block, else_block) => {
                    let value = &self.env[var].value;
                    match value {
                        Value::Int(value) => {
                            let branch = if *value == 0 { else_block } else { then_block };
                            state = self.new_state(*branch);
                            continue;
                        }

                        _ => unreachable!("var `{}' is not a number: {:?}", var, value),
                    }
                }

                Statement::Return(id) => {
                    result = &self.env[id].value;
                    break;
                }
            }
        }

        result.clone()
    }

    fn new_state(&self, block: NodeIndex) -> LocalEvalState {
        LocalEvalState {
            block,
            statement_index: self.module.cfg[block].indices().next(),
        }
    }

    fn eval_value(&mut self, value: &Value) -> Value {
        match value {
            Value::Unit | Value::Int(_) | Value::String(_) | Value::Function { .. } => {
                value.clone()
            }

            Value::AddInt(left, right) => Value::Int(self.int(*left) + self.int(*right)),
            Value::SubInt(left, right) => Value::Int(self.int(*left) - self.int(*right)),
            Value::MulInt(left, right) => Value::Int(self.int(*left) * self.int(*right)),
            Value::DivInt(left, right) => Value::Int(self.int(*left) / self.int(*right)),

            Value::AddString(left, right) => Value::String(Rc::new(
                (*self.string(*left)).clone() + &self.string(*right),
            )),

            Value::Phi(operands) => operands
                .iter()
                .filter_map(|operand| self.env.get(*operand))
                .minmax_by(|value1, value2| value1.timestamp.cmp(&value2.timestamp))
                .into_option()
                .unwrap()
                .1
                .value
                .clone(),

            Value::Call(function, arguments) => {
                let value = &self.runtime_value(*function).value;
                let Value::Function(fn_id, _) = value else {
                    unreachable!("`{}' is not a function: {:?}", function, value)
                };

                let function_cfg = &self.functions[fn_id];
                let mut functions = self.functions.clone();
                functions.extend(function_cfg.functions.clone());
                let mut function_context = EvalContext::new(function_cfg, &functions);
                for argument in arguments {
                    let runtime_value = self.env[argument].clone();
                    let value = runtime_value.value.clone();
                    function_context.stack.push(value);
                }
                function_context.eval()
            }

            Value::Arg(index) => self.stack[*index].clone(),
        }
    }

    fn define(&mut self, value_id: ValueId, value: Value) {
        let next_timestamp = self.timestamp + 1;
        self.env[value_id] = RuntimeValue {
            value,
            timestamp: replace(&mut self.timestamp, next_timestamp),
        };
    }

    fn int(&self, value_id: ValueId) -> i32 {
        self.runtime_value(value_id).try_into().unwrap()
    }

    fn string(&self, value_id: ValueId) -> Rc<String> {
        self.runtime_value(value_id).try_into().unwrap()
    }

    fn runtime_value(&self, value_id: ValueId) -> &RuntimeValue {
        &self.env[value_id]
    }
}

struct EvalEnv(FxHashMap<ValueId, RuntimeValue>);

impl EvalEnv {
    fn new() -> Self {
        Self(FxHashMap::default())
    }

    fn get(&self, value_id: ValueId) -> Option<&RuntimeValue> {
        self.0.get(&value_id)
    }
}

impl Deref for EvalEnv {
    type Target = FxHashMap<ValueId, RuntimeValue>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl DerefMut for EvalEnv {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl Index<&ValueId> for EvalEnv {
    type Output = RuntimeValue;

    fn index(&self, value_id: &ValueId) -> &Self::Output {
        self.index(*value_id)
    }
}

impl Index<ValueId> for EvalEnv {
    type Output = RuntimeValue;

    fn index(&self, value_id: ValueId) -> &Self::Output {
        &self.0[&value_id]
    }
}

impl IndexMut<&ValueId> for EvalEnv {
    fn index_mut(&mut self, value_id: &ValueId) -> &mut Self::Output {
        &mut self[*value_id]
    }
}

impl IndexMut<ValueId> for EvalEnv {
    fn index_mut(&mut self, value_id: ValueId) -> &mut Self::Output {
        self.entry(value_id).or_default()
    }
}

struct LocalEvalState {
    block: NodeIndex,
    statement_index: Option<usize>,
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

impl TryFrom<&RuntimeValue> for i32 {
    type Error = String;

    fn try_from(value: &RuntimeValue) -> Result<Self, Self::Error> {
        if let Value::Int(value) = &value.value {
            Ok(*value)
        } else {
            Err(format!("i32::from(): {:?}", &value.value))
        }
    }
}

impl TryFrom<&RuntimeValue> for Rc<String> {
    type Error = String;

    fn try_from(value: &RuntimeValue) -> Result<Self, Self::Error> {
        if let Value::String(s) = &value.value {
            Ok(s.clone())
        } else {
            Err(format!("Rc<String>::from(): {:?}", &value.value))
        }
    }
}
