use std::fmt;
use std::hash::Hash;
use std::hash::Hasher;
use std::mem::replace;
use std::ops::Deref;
use std::ops::DerefMut;
use std::rc::Rc;

use itertools::Itertools;
use num_bigint::BigInt;
use petgraph::graph::NodeIndex;
use petgraph::stable_graph::StableDiGraph;

use crate::ir::value_storage::ValueIndex;
use crate::ir::value_storage::ValueStorage;
use crate::semantics::BindingRef;
use crate::unique_rc::UniqueRc;
use crate::utils::WHITESPACE_REGEX;

pub(crate) mod eval;
pub(crate) mod value_storage;

#[derive(Debug, Eq, PartialEq, Hash, Clone)]
pub(crate) enum Variable {
    Value(ValueIndex),
    Binding(BindingRef),
}

pub(crate) struct BasicBlock(Vec<Statement>);

impl BasicBlock {
    pub(crate) fn new() -> Self {
        Self(Vec::new())
    }
}

impl fmt::Debug for BasicBlock {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "{:?}", self.iter().format("\n"))
    }
}

impl Deref for BasicBlock {
    type Target = Vec<Statement>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl DerefMut for BasicBlock {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

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

pub(crate) struct ControlFlowGraph {
    pub(crate) name: String,
    pub(crate) graph: BasicBlockGraph,
    pub(crate) entry_block: NodeIndex,
    pub(crate) exit_block: NodeIndex,
    pub(crate) undefined: VarId,
    pub(crate) values: ValueStorage,
}

pub(crate) trait Ident: Sized {
    fn new(id: usize) -> Self;
    fn id(&self) -> usize;
}

macro_rules! define_ident {
    ($name: ident, $prefix: literal) => {
        #[derive(Copy, Clone, Eq, PartialEq, Hash, Ord, PartialOrd)]
        pub(crate) struct $name(usize);

        impl Ident for $name {
            fn new(id: usize) -> Self {
                $name(id)
            }

            fn id(&self) -> usize {
                self.0
            }
        }

        impl Default for $name {
            fn default() -> Self {
                $name(0)
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

define_ident!(VarId, "v");
define_ident!(FnId, "λ");

pub(crate) struct IdentGenerator<Id: Ident>(Id);

impl<Id: Ident> IdentGenerator<Id> {
    pub(crate) fn new() -> Self {
        Self(Id::new(0))
    }

    pub(crate) fn next_id(&mut self) -> Id {
        let next_id = Id::new(self.0.id() + 1);
        replace(&mut self.0, next_id)
    }
}

#[derive(Clone)]
pub(crate) enum Statement {
    Comment(String),
    Definition {
        ident: VarId,
        value_index: ValueIndex,
    },
    CondJump(VarId, NodeIndex, NodeIndex),
    Return(VarId),
}

impl fmt::Debug for Statement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Statement::Comment(comment) => {
                let comment = comment.replace("\n", ";");
                write!(f, "// {}", WHITESPACE_REGEX.replace_all(&comment, " "))
            },
            Statement::Definition { ident, value_index } => write!(f, "{} ← {}", ident, value_index),
            Statement::CondJump(ident, positive, negative) =>
                write!(f, "condjump {}, {}, {}", ident, positive.index(), negative.index()),
            Statement::Return(ident) => write!(f, "return {}", ident)
        }
    }
}

#[derive(Clone, Eq)]
pub(crate) struct Phi(pub(crate) Vec<VarId>);

impl Deref for Phi {
    type Target = [VarId];

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
    Undefined,
    Unit,
    Int(BigInt),
    String(Rc<String>),
    Function(FnId, UniqueRc<ControlFlowGraph>),
    AddInt(VarId, VarId),
    SubInt(VarId, VarId),
    MulInt(VarId, VarId),
    DivInt(VarId, VarId),
    AddString(VarId, VarId),
    Phi(Phi),
    Call(VarId, Vec<VarId>),
    Arg(usize),
}

impl fmt::Debug for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Undefined => f.write_str("<undefined>"),
            Value::Unit => f.write_str("()"),
            Value::Int(n) => write!(f, "{}", n),
            Value::String(s) => write!(f, "\"{}\"", s),
            Value::Function(id, _) => write!(f, "{}", id),
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
