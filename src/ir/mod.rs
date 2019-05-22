use std::collections::HashMap;
use std::fmt;
use std::hash::Hash;
use std::hash::Hasher;
use std::io::Write;
use std::iter::empty;
use std::iter::once;
use std::mem::replace;
use std::ops::Deref;
use std::ops::DerefMut;
use std::rc::Rc;

use itertools::Itertools;
use num_bigint::BigInt;
use petgraph::graph::NodeIndex;
use petgraph::stable_graph::StableDiGraph;
use stable_vec::StableVec;

use crate::ir::value_storage::ValueId;
use crate::ir::value_storage::ValueStorage;

pub(crate) mod eval;
pub(crate) mod value_storage;

#[derive(Clone)]
pub(crate) struct BasicBlock(StableVec<Statement>);

impl BasicBlock {
    pub(crate) fn new() -> Self {
        Self(StableVec::new())
    }

    pub(crate) fn len(&self) -> usize {
        self.0.num_elements()
    }
}

impl IntoIterator for BasicBlock {
    type Item = Statement;
    type IntoIter = std::vec::IntoIter<Statement>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_vec().into_iter()
    }
}

impl fmt::Debug for BasicBlock {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "{:?}", self.iter().format("\n"))
    }
}

impl Deref for BasicBlock {
    type Target = StableVec<Statement>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl DerefMut for BasicBlock {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

#[derive(Clone)]
pub(crate) struct ControlFlowGraph(StableDiGraph<BasicBlock, ()>);

impl ControlFlowGraph {
    pub(crate) fn new() -> Self {
        Self(StableDiGraph::new())
    }
}

impl Deref for ControlFlowGraph {
    type Target = StableDiGraph<BasicBlock, ()>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl DerefMut for ControlFlowGraph {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

pub(crate) type FunctionMap = HashMap<FnId, IRModule>;

#[derive(Clone)]
pub(crate) struct IRModule {
    pub(crate) name: String,
    pub(crate) cfg: ControlFlowGraph,
    pub(crate) entry_block: NodeIndex,
    pub(crate) exit_block: NodeIndex,
    pub(crate) definitions: HashMap<ValueId, StatementLocation>,
    pub(crate) values: ValueStorage,
    pub(crate) functions: FunctionMap,
    pub(crate) parameters: Vec<ValueId>,
    pub(crate) result: ValueId,
}

pub(crate) trait Ident: Sized + Copy {
    const UNDEFINED: Self;
    fn next(&self) -> Self;
    fn prefix(&self) -> &'static str;
    fn index(&self) -> usize;
}

macro_rules! define_ident {
    ($visibility: vis $name: ident $prefix: literal) => {
        #[derive(Copy, Clone, Eq, PartialEq, Hash, Ord, PartialOrd)]
        $visibility struct $name(usize);

        impl crate::ir::Ident for $name {
            const UNDEFINED: Self = $name(0);

            fn next(&self) -> Self {
                $name(self.0 + 1)
            }

            fn prefix(&self) -> &'static str {
                $prefix
            }

            fn index(&self) -> usize {
                self.0
            }
        }

        impl Default for $name {
            fn default() -> Self {
                crate::ir::Ident::UNDEFINED
            }
        }

        impl std::fmt::Display for $name {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                write!(f, "{}{}", $prefix, self.0)
            }
        }

        impl std::fmt::Debug for $name {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                std::fmt::Display::fmt(self, f)
            }
        }
    }
}

#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash, Ord, PartialOrd)]
pub(crate) struct StatementLocation {
    pub(crate) block: NodeIndex,
    pub(crate) index: usize,
}

impl StatementLocation {
    pub(crate) fn new(block: NodeIndex, index: usize) -> Self {
        Self { block, index }
    }
}

define_ident!{ pub(crate) FnId "λ" }

pub(crate) struct IdentGenerator<Id: Ident>(Id);

impl<Id: Ident> IdentGenerator<Id> {
    pub(crate) fn new() -> Self {
        Self(Id::UNDEFINED.next())
    }

    pub(crate) fn next_id(&mut self) -> Id {
        let next_id = self.0.next();
        replace(&mut self.0, next_id)
    }
}

#[derive(Clone)]
pub(crate) enum Statement {
    Comment(String),
    Definition(ValueId),
    CondJump(ValueId, NodeIndex, NodeIndex),
    Return(ValueId),
}

impl fmt::Debug for Statement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Statement::Comment(comment) => writeln!(f, "// {}", comment.split(|c| c == '\n').format("\n// ")),
            Statement::Definition(value_id) => write!(f, "define {}", value_id),
            Statement::CondJump(condition, then_branch, else_branch) =>
                write!(f, "condjump {}, {}, {}", condition, then_branch.index(), else_branch.index()),
            Statement::Return(value_id) => write!(f, "return {}", value_id)
        }
    }
}

pub(crate) fn fmt_statement(sink: &mut impl Write, statement: &Statement, values: &ValueStorage) -> std::io::Result<()> {
    match statement {
        Statement::Comment(comment) => writeln!(sink, "// {}", comment.split(|c| c == '\n').format("\n// ")),
        Statement::Definition(value_id) => write!(sink, "{} ← {:?}", value_id, &values[*value_id]),
        Statement::CondJump(value_id, then_branch, else_branch) =>
            write!(sink, "condjump {}, {}, {}", value_id, then_branch.index(), else_branch.index()),
        Statement::Return(value_id) => write!(sink, "return {}", value_id)
    }
}

#[derive(Clone, Eq)]
pub(crate) struct Phi(pub(crate) Vec<ValueId>);

impl Deref for Phi {
    type Target = [ValueId];

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl DerefMut for Phi {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl PartialEq for Phi {
    fn eq(&self, Phi(other): &Phi) -> bool {
        !self.0.is_empty() && &self.0 == other
    }
}

impl Hash for Phi {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.0.hash(state);
    }
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub(crate) enum Value {
    Unit,
    Int(BigInt), // TODO i32
    String(Rc<String>),
    Function(FnId),
    AddInt(ValueId, ValueId),
    SubInt(ValueId, ValueId),
    MulInt(ValueId, ValueId),
    DivInt(ValueId, ValueId),
    AddString(ValueId, ValueId),
    Phi(Phi),
    Call(ValueId, Vec<ValueId>),
    Arg(usize),
}

impl Value {
    pub(crate) fn is_constant(&self) -> bool {
        matches!(self, Value::Unit, Value::Int(_), Value::String(_), Value::Function(_))
    }
}

impl fmt::Debug for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Unit => f.write_str("()"),
            Value::Int(n) => write!(f, "{}", n),
            Value::String(s) => write!(f, "\"{}\"", s),
            Value::Function(id) => write!(f, "{}", id),
            Value::AddInt(left, right) => write!(f, "{} + {}", left, right),
            Value::SubInt(left, right) => write!(f, "{} - {}", left, right),
            Value::MulInt(left, right) => write!(f, "{} * {}", left, right),
            Value::DivInt(left, right) => write!(f, "{} / {}", left, right),
            Value::AddString(left, right) => write!(f, "{} + {}", left, right),
            Value::Phi(Phi(operands)) => write!(f, "ϕ({})", operands.iter().format(", ")),
            Value::Call(function, arguments) => write!(f, "call {}({})", function, arguments.iter().format(", ")),
            Value::Arg(index) => write!(f, "arg[{}]", index),
        }
    }
}

pub(crate) fn get_statement_value_operands<'a>(
    values: &'a ValueStorage,
    statement: &'a Statement) -> Box<dyn Iterator<Item = ValueId> + 'a>
{
    match statement {
        Statement::Comment(_) => Box::new(empty()),
        Statement::Definition(value_id) => get_value_operands(&values[*value_id]),
        Statement::CondJump(condition, _, _) => Box::new(once(*condition)),
        Statement::Return(result) => Box::new(once(*result)),
    }
}

pub(crate) fn get_statement_operands_mut<'a>(statement: &'a mut Statement)
    -> Box<dyn Iterator<Item = &'a mut ValueId> + 'a>
{
    match statement {
        Statement::Comment(_) => Box::new(empty()),
        Statement::Definition(value_id) => Box::new(once(value_id)),
        Statement::CondJump(condition, _, _) => Box::new(once(condition)),
        Statement::Return(result) => Box::new(once(result)),
    }
}

pub(crate) fn replace_value_id(value: &mut Value, value_id: ValueId, replacement: ValueId) {
    let operands: Box<dyn Iterator<Item = &mut ValueId>> = match value {
        Value::Unit |
        Value::Int(_) |
        Value::String(_) |
        Value::Function {..} |
        Value::Arg(_) => Box::new(empty()),

        Value::AddInt(left, right) |
        Value::SubInt(left, right) |
        Value::MulInt(left, right) |
        Value::DivInt(left, right) |
        Value::AddString(left, right) => Box::new(once(left).chain(once(right))),
        Value::Phi(operands) => Box::new(operands.iter_mut()),
        Value::Call(function, arguments) => Box::new(once(function).chain(arguments.iter_mut())),
    };
    for operand in operands {
        if *operand == value_id {
            *operand = replacement;
        }
    }
}

pub(crate) fn get_value_operands<'a>(value: &'a Value) -> Box<dyn Iterator<Item =ValueId> + 'a> {
    match value {
        Value::Unit |
        Value::Int(_) |
        Value::String(_) |
        Value::Function {..} |
        Value::Arg(_) => Box::new(empty()),

        Value::AddInt(left, right) |
        Value::SubInt(left, right) |
        Value::MulInt(left, right) |
        Value::DivInt(left, right) |
        Value::AddString(left, right) => Box::new(once(*left).chain(once(*right))),

        Value::Phi(phi) => Box::new(phi.iter().cloned()),
        Value::Call(function, arguments) => Box::new(once(*function).chain(arguments.iter().cloned())),
    }
}

pub(crate) fn get_value_operands_mut<'a>(value: &'a mut Value) -> Box<dyn Iterator<Item = &mut ValueId> + 'a> {
    match value {
        Value::Unit |
        Value::Int(_) |
        Value::String(_) |
        Value::Function {..} |
        Value::Arg(_) => Box::new(empty()),

        Value::AddInt(left, right) |
        Value::SubInt(left, right) |
        Value::MulInt(left, right) |
        Value::DivInt(left, right) |
        Value::AddString(left, right) => Box::new(once(left).chain(once(right))),

        Value::Phi(phi) => Box::new(phi.iter_mut()),
        Value::Call(function, arguments) => Box::new(once(function).chain(arguments.iter_mut())),
    }
}

pub(crate) fn normalize(value: Value) -> Value {
    match &value {
        Value::Unit |
        Value::Int(_) |
        Value::String(_) |
        Value::Function {..} |
        Value::SubInt(_, _) |
        Value::DivInt(_, _) |
        Value::AddString(_, _) |
        Value::Call(_, _) |
        Value::Arg(_) => value,

        Value::AddInt(left, right) => Value::AddInt(*left.min(&right), *left.max(&right)),
        Value::MulInt(left, right) => Value::MulInt(*left.min(&right), *left.max(&right)),

        Value::Phi(Phi(operands)) => {
            let sorted_operands = operands
                .iter()
                .cloned()
                .sorted()
                .collect_vec();
            Value::Phi(Phi(sorted_operands))
        },
    }
}
