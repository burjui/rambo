use std::collections::HashMap;
use std::fmt;
use std::hash::Hash;
use std::hash::Hasher;
use std::io::Write;
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
use crate::semantics::BindingRef;

pub(crate) mod eval;
pub(crate) mod value_storage;

#[derive(Debug, Eq, PartialEq, Hash, Clone)]
pub(crate) enum Variable {
    Value(ValueId),
    Binding(BindingRef),
}

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
pub(crate) struct BasicBlockGraph(StableDiGraph<BasicBlock, ()>);

impl BasicBlockGraph {
    pub(crate) fn new() -> Self {
        Self(StableDiGraph::new())
    }
}

impl Deref for BasicBlockGraph {
    type Target = StableDiGraph<BasicBlock, ()>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl DerefMut for BasicBlockGraph {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

pub(crate) type FunctionMap = HashMap<FnId, ControlFlowGraph>;

#[derive(Clone)]
pub(crate) struct ControlFlowGraph {
    pub(crate) name: String,
    pub(crate) graph: BasicBlockGraph,
    pub(crate) entry_block: NodeIndex,
    pub(crate) definitions: HashMap<ValueId, StatementLocation>,
    pub(crate) values: ValueStorage,
    pub(crate) functions: FunctionMap,
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
            Statement::Definition(value_index) => write!(f, "define {}", value_index),
            Statement::CondJump(condition, true_branch, false_branch) =>
                write!(f, "condjump {}, {}, {}", condition, true_branch.index(), false_branch.index()),
            Statement::Return(value_id) => write!(f, "return {}", value_id)
        }
    }
}

pub(crate) fn fmt_statement(sink: &mut impl Write, statement: &Statement, values: &ValueStorage) -> std::io::Result<()> {
    match statement {
        Statement::Comment(comment) => writeln!(sink, "// {}", comment.split(|c| c == '\n').format("\n// ")),
        Statement::Definition(value_index) => write!(sink, "{} ← {:?}", value_index, &values[*value_index]),
        Statement::CondJump(value_id, true_branch, false_branch) =>
            write!(sink, "condjump {}, {}, {}", value_id, true_branch.index(), false_branch.index()),
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
    Int(BigInt),
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
