use std::fmt;
use std::hash::Hash;
use std::hash::Hasher;
use std::mem::replace;
use std::ops::Deref;
use std::ops::DerefMut;
use std::rc::Rc;

use itertools::Itertools;
use num_bigint::BigInt;
use petgraph::graph::DiGraph;
use petgraph::graph::NodeIndex;

use crate::ir::value_storage::ValueIndex;
use crate::ir::value_storage::ValueStorage;
use crate::semantics::BindingRef;
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

pub(crate) struct BasicBlockGraph(DiGraph<BasicBlock, ()>);

impl BasicBlockGraph {
    pub(crate) fn new() -> Self {
        Self(DiGraph::new())
    }

    pub(crate) fn block(&self, block: NodeIndex) -> &BasicBlock {
        self.node_weight(block).unwrap()
    }

    pub(crate) fn block_mut(&mut self, block: NodeIndex) -> &mut BasicBlock {
        self.node_weight_mut(block).unwrap()
    }
}

impl Deref for BasicBlockGraph {
    type Target = DiGraph<BasicBlock, ()>;

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
    pub(crate) graph: BasicBlockGraph,
    pub(crate) entry_block: NodeIndex,
    pub(crate) exit_block: NodeIndex,
    pub(crate) undefined: Ident,
    pub(crate) values: ValueStorage,
    pub(crate) id_count: usize,
}


#[derive(Copy, Clone, Eq, PartialEq, Hash)]
pub(crate) struct Ident(usize);

impl Default for Ident {
    fn default() -> Self {
        Ident(0)
    }
}

pub(crate) struct IdentGenerator(usize);

impl IdentGenerator {
    pub(crate) fn new() -> Self {
        Self(0)
    }

    pub(crate) fn next_id(&mut self) -> Ident {
        let next_id = self.0 + 1;
        Ident(replace(&mut self.0, next_id))
    }

    pub(crate) fn id_count(&self) -> usize {
        self.0
    }
}

impl fmt::Display for Ident {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "v{}", self.0)
    }
}

impl fmt::Debug for Ident {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(self, f)
    }
}

#[derive(Clone)]
pub(crate) enum Statement {
    Comment(String),
    Definition {
        ident: Ident,
        value_index: ValueIndex,
    },
    CondJump(Ident, NodeIndex, NodeIndex),
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
        }
    }
}

#[derive(Clone, Eq)]
pub(crate) struct Phi(pub(crate) Vec<Ident>);

impl Deref for Phi {
    type Target = [Ident];

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
    Function(NodeIndex),
    AddInt(Ident, Ident),
    SubInt(Ident, Ident),
    MulInt(Ident, Ident),
    DivInt(Ident, Ident),
    AddString(Ident, Ident),
    Phi(Phi),
    Call(Ident, Vec<Ident>),
    Arg(usize),
    Return(Ident),
}

impl fmt::Debug for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Undefined => f.write_str("<undefined>"),
            Value::Unit => f.write_str("()"),
            Value::Int(n) => write!(f, "{}", n),
            Value::String(s) => write!(f, "\"{}\"", s),
            Value::Function(entry_block) => write!(f, "λ{} ", entry_block.index()),
            Value::AddInt(left, right) => write!(f, "{} + {}", left, right),
            Value::SubInt(left, right) => write!(f, "{} - {}", left, right),
            Value::MulInt(left, right) => write!(f, "{} * {}", left, right),
            Value::DivInt(left, right) => write!(f, "{} / {}", left, right),
            Value::AddString(left, right) => write!(f, "{} + {}", left, right),
            Value::Phi(Phi(operands)) => write!(f, "ϕ({})", operands.iter().format(", ")),
            Value::Call(function, arguments) => write!(f, "call {}({})", function, arguments.iter().format(", ")),
            Value::Arg(index) => write!(f, "arg[{}]", index),
            Value::Return(result) => write!(f, "return {}", result),
        }
    }
}
