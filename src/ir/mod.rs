use core::cmp;
use std::{
    fmt,
    hash::{Hash, Hasher},
    io::Write,
    iter::{empty, once},
    ops::{Deref, DerefMut, Index, IndexMut},
    rc::Rc,
};

use itertools::Itertools;
use rustc_hash::FxHashMap;

use crate::{
    ir::value_storage::{ValueId, ValueStorage},
    source::Source,
    stable_graph::{Direction, Edge, EdgeIndex, NodeIndex, StableGraph},
    stable_vec::StableVec,
};

pub(crate) mod eval;
pub(crate) mod value_storage;

#[derive(Clone)]
pub(crate) struct BasicBlock(StableVec<Statement>);

impl BasicBlock {
    pub(crate) fn new() -> Self {
        Self(StableVec::new())
    }

    pub(crate) fn into_iter(self) -> impl IntoIterator<Item = Statement> {
        self.0.into_iter()
    }

    pub(crate) fn iter(&self) -> impl Iterator<Item = &Statement> {
        self.0.iter()
    }

    pub(crate) fn find_last(&self) -> Option<&Statement> {
        self.0.find_last()
    }

    pub(crate) fn pop(&mut self) {
        self.0.pop()
    }

    pub(crate) fn push(&mut self, statement: Statement) -> usize {
        self.0.push(statement)
    }

    pub(crate) fn len(&self) -> usize {
        self.0.len()
    }

    pub(crate) fn iter_mut(&mut self) -> impl Iterator<Item = &mut Statement> {
        self.0.iter_mut()
    }

    pub(crate) fn remove(&mut self, location: StatementLocation) {
        self.0.remove(location.index)
    }

    pub(crate) fn get(&self, index: usize) -> Option<&Statement> {
        self.0.get(index)
    }

    pub(crate) fn next_index(&self, index: usize) -> Option<usize> {
        self.0.next_index(index)
    }

    pub(crate) fn indices(&self) -> impl Iterator<Item = usize> + '_ {
        self.0.indices()
    }
}

impl fmt::Debug for BasicBlock {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "{:?}", self.iter().format("\n"))
    }
}

impl Index<StatementLocation> for BasicBlock {
    type Output = Statement;

    fn index(&self, location: StatementLocation) -> &Self::Output {
        &self.0[location.index]
    }
}

#[derive(Clone)]
pub(crate) struct ControlFlowGraph {
    graph: StableGraph<BasicBlock>,
}

impl ControlFlowGraph {
    pub(crate) fn new() -> Self {
        Self {
            graph: StableGraph::new(),
        }
    }

    pub(crate) fn add_edge(&mut self, from: NodeIndex, to: NodeIndex) {
        self.graph.add_edge(from, to);
    }

    pub(crate) fn remove_node(&mut self, node: NodeIndex) {
        self.graph.remove_node(node);
    }

    pub(crate) fn edges_directed(
        &self,
        node: NodeIndex,
        direction: Direction,
    ) -> impl Iterator<Item = Edge> + '_ {
        self.graph.edges_directed(node, direction)
    }

    pub(crate) fn has_edge(&self, from: NodeIndex, to: NodeIndex) -> bool {
        self.graph.has_edge(from, to)
    }

    pub(crate) fn edge_indices(&self) -> impl Iterator<Item = EdgeIndex> + '_ {
        self.graph.edge_indices()
    }

    pub(crate) fn node_indices(&self) -> impl Iterator<Item = NodeIndex> + '_ {
        self.graph.node_indices()
    }

    pub(crate) fn add_new_block(&mut self) -> NodeIndex {
        self.graph.add_node(BasicBlock::new())
    }
}

impl Index<NodeIndex> for ControlFlowGraph {
    type Output = BasicBlock;

    fn index(&self, index: NodeIndex) -> &Self::Output {
        &self.graph[index]
    }
}

impl IndexMut<NodeIndex> for ControlFlowGraph {
    fn index_mut(&mut self, index: NodeIndex) -> &mut Self::Output {
        &mut self.graph[index]
    }
}

impl Index<EdgeIndex> for ControlFlowGraph {
    type Output = Edge;

    fn index(&self, index: EdgeIndex) -> &Self::Output {
        &self.graph[index]
    }
}

impl IndexMut<EdgeIndex> for ControlFlowGraph {
    fn index_mut(&mut self, index: EdgeIndex) -> &mut Self::Output {
        &mut self.graph[index]
    }
}

pub(crate) type FunctionMap = FxHashMap<FnId, IRModule>;

#[derive(Clone)]
pub(crate) struct IRModule {
    pub(crate) name: String,
    pub(crate) cfg: ControlFlowGraph,
    pub(crate) entry_block: NodeIndex,
    pub(crate) values: ValueStorage,
    pub(crate) functions: FunctionMap,
    pub(crate) parameters: Vec<ValueId>,
    pub(crate) result: ValueId,
}

#[derive(Clone, Eq)]
pub(crate) struct FnId {
    id: usize,
    source: Source,
}

impl PartialEq for FnId {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}

impl Hash for FnId {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.id.hash(state)
    }
}

impl PartialOrd for FnId {
    fn partial_cmp(&self, other: &Self) -> Option<cmp::Ordering> {
        self.id.partial_cmp(&other.id)
    }
}

impl Ord for FnId {
    fn cmp(&self, other: &Self) -> cmp::Ordering {
        self.id.cmp(&other.id)
    }
}

impl fmt::Display for FnId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "λ{}<{}>", self.id, self.source)
    }
}

impl fmt::Debug for FnId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(self, f)
    }
}

pub(crate) struct FnIdGenerator(usize);

impl FnIdGenerator {
    pub(crate) const fn new() -> Self {
        Self(0)
    }

    pub(crate) fn new_id(&mut self, source: Source) -> FnId {
        let id = self.0;
        self.0 += 1;
        FnId { id, source }
    }
}

#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash, Ord, PartialOrd)]
pub(crate) struct StatementLocation {
    pub(crate) block: NodeIndex,
    pub(crate) index: usize,
}

impl StatementLocation {
    pub(crate) const fn new(block: NodeIndex, index: usize) -> Self {
        Self { block, index }
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
            Self::Comment(comment) => {
                writeln!(f, "// {}", comment.split(|c| c == '\n').format("\n// "))
            }
            Self::Definition(value_id) => write!(f, "define {}", value_id),
            Self::CondJump(condition, then_branch, else_branch) => {
                write!(
                    f,
                    "condjump {}, {}, {}",
                    condition, then_branch, else_branch
                )
            }
            Self::Return(value_id) => write!(f, "return {}", value_id),
        }
    }
}

pub(crate) fn fmt_statement(
    sink: &mut impl Write,
    statement: &Statement,
    values: &ValueStorage,
) -> std::io::Result<()> {
    match statement {
        Statement::Comment(comment) => {
            writeln!(sink, "// {}", comment.split(|c| c == '\n').format("\n// "))
        }
        Statement::Definition(value_id) => write!(sink, "{} ← {:?}", value_id, &values[*value_id]),
        Statement::CondJump(value_id, then_branch, else_branch) => {
            write!(
                sink,
                "condjump {}, {}, {}",
                value_id, then_branch, else_branch
            )
        }
        Statement::Return(value_id) => write!(sink, "return {}", value_id),
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
    fn eq(&self, Self(other): &Self) -> bool {
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
    Int(i32),
    String(Rc<String>),
    Function(FnId, Rc<String>),
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
        matches!(
            self,
            Self::Unit | Self::Int(_) | Self::String(_) | Self::Function(_, _)
        )
    }
}

impl fmt::Debug for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Unit => f.write_str("()"),
            Self::Int(value) => write!(f, "{}", value),
            Self::String(s) => write!(f, "\"{}\"", s),
            Self::Function(_, name) => write!(f, "{}", name),
            Self::AddInt(left, right) | Self::AddString(left, right) => {
                write!(f, "{} + {}", left, right)
            }
            Self::SubInt(left, right) => write!(f, "{} - {}", left, right),
            Self::MulInt(left, right) => write!(f, "{} * {}", left, right),
            Self::DivInt(left, right) => write!(f, "{} / {}", left, right),
            Self::Phi(Phi(operands)) => write!(f, "ϕ({})", operands.iter().format(", ")),
            Self::Call(function, arguments) => {
                write!(f, "call {}({})", function, arguments.iter().format(", "))
            }
            Self::Arg(index) => write!(f, "arg[{}]", index),
        }
    }
}

pub(crate) fn get_statement_value_operands<'a>(
    values: &'a ValueStorage,
    statement: &'a Statement,
) -> Box<dyn Iterator<Item = ValueId> + 'a> {
    match statement {
        Statement::Comment(_) => Box::new(empty()),
        Statement::Definition(value_id) => get_value_operands(&values[*value_id]),
        Statement::CondJump(condition, _, _) => Box::new(once(*condition)),
        Statement::Return(result) => Box::new(once(*result)),
    }
}

pub(crate) fn get_statement_operands_mut<'a>(
    statement: &'a mut Statement,
) -> Box<dyn Iterator<Item = &'a mut ValueId> + 'a> {
    match statement {
        Statement::Comment(_) => Box::new(empty()),
        Statement::Definition(value_id) => Box::new(once(value_id)),
        Statement::CondJump(condition, _, _) => Box::new(once(condition)),
        Statement::Return(result) => Box::new(once(result)),
    }
}

pub(crate) fn replace_value_id(value: &mut Value, value_id: ValueId, replacement: ValueId) {
    let operands: Box<dyn Iterator<Item = &mut ValueId>> = match value {
        Value::Unit | Value::Int(_) | Value::String(_) | Value::Function { .. } | Value::Arg(_) => {
            Box::new(empty())
        }

        Value::AddInt(left, right)
        | Value::SubInt(left, right)
        | Value::MulInt(left, right)
        | Value::DivInt(left, right)
        | Value::AddString(left, right) => Box::new(once(left).chain(once(right))),
        Value::Phi(operands) => Box::new(operands.iter_mut()),
        Value::Call(function, arguments) => Box::new(once(function).chain(arguments.iter_mut())),
    };
    for operand in operands {
        if *operand == value_id {
            *operand = replacement;
        }
    }
}

pub(crate) fn get_value_operands<'a>(value: &'a Value) -> Box<dyn Iterator<Item = ValueId> + 'a> {
    match value {
        Value::Unit | Value::Int(_) | Value::String(_) | Value::Function { .. } | Value::Arg(_) => {
            Box::new(empty())
        }

        Value::AddInt(left, right)
        | Value::SubInt(left, right)
        | Value::MulInt(left, right)
        | Value::DivInt(left, right)
        | Value::AddString(left, right) => Box::new(once(*left).chain(once(*right))),

        Value::Phi(phi) => Box::new(phi.iter().cloned()),
        Value::Call(function, arguments) => {
            Box::new(once(*function).chain(arguments.iter().cloned()))
        }
    }
}

pub(crate) fn get_value_operands_mut<'a>(
    value: &'a mut Value,
) -> Box<dyn Iterator<Item = &mut ValueId> + 'a> {
    match value {
        Value::Unit | Value::Int(_) | Value::String(_) | Value::Function { .. } | Value::Arg(_) => {
            Box::new(empty())
        }

        Value::AddInt(left, right)
        | Value::SubInt(left, right)
        | Value::MulInt(left, right)
        | Value::DivInt(left, right)
        | Value::AddString(left, right) => Box::new(once(left).chain(once(right))),

        Value::Phi(phi) => Box::new(phi.iter_mut()),
        Value::Call(function, arguments) => Box::new(once(function).chain(arguments.iter_mut())),
    }
}
