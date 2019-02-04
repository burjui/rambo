use std::fmt;
use std::iter::empty;
use std::iter::once;
use std::mem::replace;
use std::rc::Rc;

use hashbrown::HashMap;
use hashbrown::HashSet;
use itertools::Itertools;
use multimap::MultiMap;
use num_bigint::BigInt;
use once_cell::sync;
use petgraph::graph::DiGraph;
use petgraph::graph::NodeIndex;
use petgraph::prelude::Direction::Incoming;
use petgraph::visit::EdgeRef;

use crate::ir::idgen::IdentGenerator;
use crate::semantics::BindingKind;
use crate::semantics::BindingRef;
use crate::semantics::ExprRef;
use crate::semantics::TypedExpr;
use crate::semantics::TypedStatement;
use crate::source::Source;
#[cfg(test)] use crate::utils::TestResult;
use crate::utils::WHITESPACE_REGEX;

pub(crate) mod eval;
mod idgen;

pub(crate) struct EnableWarnings(pub(crate) bool);

#[derive(Debug, Eq, PartialEq, Hash, Clone)]
pub(crate) enum Variable {
    Value(Value),
    Binding(BindingRef),
}

pub(crate) struct FrontEnd {
    enable_warnings: bool,
    graph: BasicBlockGraph,
    idgen: IdentGenerator,
    variables: HashMap<Variable, HashMap<NodeIndex, Ident>>,
    variables_read: HashSet<Variable>,
    sealed_blocks: HashSet<NodeIndex>,
    incomplete_phis: HashMap<NodeIndex, HashMap<Variable, Ident>>,
    phi_locations: HashMap<Ident, StatementLocation>,
    phi_users: MultiMap<Ident, StatementLocation>,
    trivial_phis: HashSet<StatementLocation>,
    entry_block: NodeIndex,
    undefined: Ident,
    undefined_users: Vec<StatementLocation>,
}

/*
The algorithm used here to produce IR in minimal SSA form is from the following paper:

Braun M., Buchwald S., Hack S., Leißa R., Mallon C., Zwinkau A.
(2013) Simple and Efficient Construction of Static Single Assignment Form.
In: Jhala R., De Bosschere K. (eds)
Compiler Construction. CC 2013.
Lecture Notes in Computer Science, vol 7791. Springer, Berlin, Heidelberg
*/
impl FrontEnd {
    pub(crate) fn new(EnableWarnings(enable_warnings): EnableWarnings) -> Self {
        let mut instance = Self {
            enable_warnings,
            graph: BasicBlockGraph::new(),
            idgen: IdentGenerator::new(),
            variables: HashMap::new(),
            variables_read: HashSet::new(),
            sealed_blocks: HashSet::new(),
            incomplete_phis: HashMap::new(),
            phi_locations: HashMap::new(),
            phi_users: MultiMap::new(),
            trivial_phis: HashSet::new(),
            entry_block: NodeIndex::new(0),
            undefined: Ident(0),
            undefined_users: Vec::new(),
        };
        instance.entry_block = instance.new_block();
        instance.seal_block(instance.entry_block);
        instance.undefined = instance.define(instance.entry_block, Value::Undefined);
        instance
    }

    pub(crate) fn build(mut self, code: &ExprRef) -> ControlFlowGraph {
        let (exit_block, _) = self.process_expr(code, self.entry_block);
        self.assert_no_undefined_variables();
        if self.enable_warnings {
            warn_about_redundant_bindings(self.variables, self.variables_read);
        }
        remove_statements(&mut self.graph, self.trivial_phis.into_iter());

        ControlFlowGraph {
            graph: self.graph,
            entry_block: self.entry_block,
            exit_block,
            undefined: self.undefined,
            id_count: self.idgen.id_count()
        }
    }

    fn assert_no_undefined_variables(&self) {
        assert!(self.undefined_users.is_empty(), "found undefined usages:\n{:?}\n",
                self.undefined_users.iter()
                    .map(|user| &self.graph.block(user.block)[user.index])
                    .format("\n"));
    }

    fn process_expr(&mut self, expr: &ExprRef, block: NodeIndex) -> (NodeIndex, Ident) {
        match &**expr {
            TypedExpr::Block(block_data) => {
                let mut current_block = block;
                let mut current_ident = None;
                for statement in &block_data.statements {
                    let (block, ident) = self.process_statement(statement, current_block);
                    current_block = block;
                    current_ident = Some(ident);
                }
                let ident = current_ident.unwrap_or_else(|| self.define(block, Value::Unit));
                (current_block, ident)
            }

            TypedExpr::Unit(source) => {
                self.comment(block, source.text());
                (block, self.define(block, Value::Unit))
            }

            TypedExpr::Int(value, source) => {
                self.comment(block, source.text());
                (block, self.define(block, Value::Int(value.clone())))
            }

            TypedExpr::String(value, source) => {
                self.comment(block, source.text());
                (block, self.define(block, Value::String(value.clone())))
            }

            TypedExpr::AddInt(left, right, source) => self.process_binary(block, left, right, source, Value::AddInt),
            TypedExpr::SubInt(left, right, source) => self.process_binary(block, left, right, source, Value::SubInt),
            TypedExpr::MulInt(left, right, source) => self.process_binary(block, left, right, source, Value::MulInt),
            TypedExpr::DivInt(left, right, source) => self.process_binary(block, left, right, source, Value::DivInt),
            TypedExpr::AddStr(left, right, source) => self.process_binary(block, left, right, source, Value::AddString),

            TypedExpr::Reference(binding, source) => {
                self.comment(block, source.text());
                (block, self.read_variable(&Variable::Binding(binding.clone()), block))
            }

            TypedExpr::Conditional { condition, positive, negative, source, .. } => {
                self.comment(block, source.text());
                let (block, condition) = self.process_expr(condition, block);
                let positive_block = self.new_block();
                let negative_block = self.new_block();

                let statement_location = StatementLocation::new(block, self.graph.block(block).len());
                self.record_phi_and_undefined_usages(statement_location, once(condition));
                self.graph.block_mut(block).push(
                    Statement::CondJump(condition, positive_block, negative_block));

                self.graph.add_edge(block, positive_block, ());
                self.graph.add_edge(block, negative_block, ());
                self.seal_block(positive_block);
                self.seal_block(negative_block);
                let (positive_exit_block, positive) = self.process_expr(positive, positive_block);
                let (negative_exit_block, negative) = self.process_expr(negative, negative_block);

                let exit = self.new_block();
                self.graph.add_edge(positive_exit_block, exit, ());
                self.graph.add_edge(negative_exit_block, exit, ());
                self.seal_block(exit);
                let result_phi = self.define_phi(exit, &[positive, negative]);
                (exit, self.try_remove_trivial_phi(result_phi))
            }

            TypedExpr::Assign(binding, value, source) => {
                self.comment(block, source.text());
                let variable = Variable::Binding(binding.clone());
                let (block, ident) = self.process_expr(value, block);
                self.write_variable(&variable, block, ident);
                (block, ident)
            }

            TypedExpr::Lambda(lambda, source) => {
                let entry_block = self.new_block();
                self.seal_block(entry_block);
                self.comment(entry_block, source.text());
                for parameter in &lambda.parameters {
                    self.comment(entry_block, &parameter.name);
                    let index = if let BindingKind::Arg(index) = parameter.kind {
                        index
                    } else {
                        unreachable!()
                    };
                    let declaration = self.define(entry_block, Value::Arg(index));
                    self.write_variable(&Variable::Binding(parameter.clone()), entry_block, declaration);
                }
                let _ = self.process_expr(&lambda.body, entry_block);
                (block, self.define(block, Value::Function(entry_block)))
            }

            TypedExpr::Application { function, arguments, source, .. } => {
                self.comment(block, source.text());
                let (block, function_ident) = self.process_expr(function, block);
                let mut current_block = block;
                let mut argument_idents = vec![];
                for argument in arguments {
                    let (block, ident) = self.process_expr(argument, current_block);
                    argument_idents.push(ident);
                    current_block = block;
                }
                (block, self.define(current_block, Value::Call(function_ident, argument_idents)))
            }

            _ => unimplemented!("process_expr: {:?}", expr)
        }
    }

    fn process_binary(&mut self, block: NodeIndex, left: &ExprRef, right: &ExprRef, source: &Source,
                      value_constructor: impl Fn(Ident, Ident) -> Value) -> (NodeIndex, Ident)
    {
        let (block, left_ident) = self.process_expr(left, block);
        let (block, right_ident) = self.process_expr(right, block);
        self.comment(block, source.text());
        (block, self.define(block, value_constructor(left_ident, right_ident)))
    }

    fn process_statement(&mut self, statement: &TypedStatement, block: NodeIndex) -> (NodeIndex, Ident) {
        match statement {
            TypedStatement::Binding(binding) => {
                self.comment(block, binding.source.text());
                let (block, ident) = self.process_expr(&binding.data, block);
                self.write_variable(&Variable::Binding(binding.clone()), block, ident);
                (block, ident)
            },
            TypedStatement::Expr(expr) => {
                self.process_expr(expr, block)
            }
        }
    }

    fn write_variable(&mut self, variable: &Variable, block: NodeIndex, ident: Ident) {
        self.variables
            .entry(variable.clone())
            .or_insert_with(HashMap::new)
            .insert(block, ident);
    }

    fn read_variable(&mut self, variable: &Variable, block: NodeIndex) -> Ident {
        if !self.variables_read.contains(variable) {
            self.variables_read.insert(variable.clone());
        }

        self.variables
            .get(variable)
            .and_then(|map| map.get(&block))
            .map(Clone::clone)
            .unwrap_or_else(|| self.read_variable_recursive(variable, block))
    }

    fn read_variable_recursive(&mut self, variable: &Variable, block: NodeIndex) -> Ident {
        let ident = if !self.sealed_blocks.contains(&block) {
            // Incomplete CFG
            let phi = self.define_phi(block, &[]);
            self.incomplete_phis
                .entry(block)
                .or_insert_with(HashMap::new)
                .insert(variable.clone(), phi);
            phi
        } else {
            let mut predecessors = self.graph
                .edges_directed(block, Incoming)
                .map(|edge| edge.source());
            let first_predecessor = predecessors.next();
            let second_predecessor = predecessors.next();
            if let (Some(predecessor), None) = (first_predecessor, second_predecessor) {
                // Optimize the common case of one predecessor: no phi needed
                self.read_variable(variable, predecessor)
            } else {
                // Break potential cycles with operandless phi
                let phi = self.define_phi(block, &[]);
                self.write_variable(variable, block, phi);
                self.add_phi_operands(variable, phi)
            }
        };
        self.write_variable(variable, block, ident);
        ident
    }

    fn add_phi_operands(&mut self, variable: &Variable, phi: Ident) -> Ident {
        let predecessors = self.graph
            .edges_directed(self.phi_locations[&phi].block, Incoming)
            .map(|edge| edge.source())
            .collect_vec();
        for predecessor in predecessors {
            let operand = self.read_variable(variable, predecessor);
            self.phi_operands_mut(phi).push(operand);
        }
        self.try_remove_trivial_phi(phi)
    }

    fn try_remove_trivial_phi(&mut self, phi: Ident) -> Ident {
        let mut same = None;
        for &operand in self.phi_operands(phi) {
            if operand == self.undefined {
                same = Some(self.undefined);
                break;
            }
            if Some(operand) == same || operand == phi {
                // Unique value or self−reference
                continue;
            }
            if same.is_some() {
                // The phi merges at least two values: not trivial
                return phi;
            }
            same = Some(operand);
        }

        let location = self.phi_locations[&phi];
        self.trivial_phis.insert(location);

        let same = match same {
            Some(ident) => ident,
            None => self.undefined
        };

        if let Some(vec) = self.phi_users.get_vec_mut(&phi) {
            if let Some((index, _)) = vec.iter().find_position(|user| **user == location) {
                vec.remove(index);
            }
        }

        static EMPTY_USERS: sync::Lazy<Vec<StatementLocation>> = once_cell::sync_lazy!(vec![]);
        let users = self.phi_users
            .get_vec(&phi)
            .unwrap_or_else(|| &*EMPTY_USERS);
        let mut possible_trivial_phis = vec![];
        for user in users {
            if let Statement::Definition { ident, value } = &mut self.graph.block_mut(user.block)[user.index] {
                replace_phi(value, phi, same);
                if let Value::Phi(_) = value {
                    possible_trivial_phis.push(*ident);
                }
            }
        }

        for phi in possible_trivial_phis {
            self.try_remove_trivial_phi(phi);
        }

        same
    }

    fn seal_block(&mut self, block: NodeIndex) {
        let variables = self.incomplete_phis
            .get(&block)
            .map(|incomplete_phis| incomplete_phis
                .keys()
                .cloned()
                .collect_vec());
        if let Some(variables) = variables {
            for variable in &variables {
                self.add_phi_operands(variable, self.incomplete_phis[&block][variable]);
            }
        }
        self.sealed_blocks.insert(block);
    }

    fn define(&mut self, block: NodeIndex, value: Value) -> Ident {
        let variable = Variable::Value(value.clone());
        let value_ident = (|| {
            if let Variable::Value(value) = &variable {
                if let Value::Arg(_) = value {
                    return self.undefined;
                } else if let Value::Phi(operands) = value {
                    if operands.is_empty() {
                        return self.undefined;
                    }
                }
            }
            self.read_variable(&variable, block)
        })();

        if value_ident != self.undefined {
            value_ident
        } else {
            let definition_location = StatementLocation::new(block, self.graph.block(block).len());
            self.record_phi_and_undefined_usages(definition_location, get_value_operands(&value));
            let ident = self.idgen.new_id();
            self.graph.block_mut(block).push(Statement::Definition {
                ident,
                value
            });
            self.write_variable(&variable, block, ident);
            ident
        }
    }

    fn define_phi(&mut self, block: NodeIndex, operands: &[Ident]) -> Ident {
        let phi = self.define(block, Value::Phi(operands.to_vec()));
        let index = self.graph.block(block).len() - 1;
        self.phi_locations.insert(phi, StatementLocation::new(block, index));
        phi
    }

    fn record_phi_and_undefined_usages(&mut self, location: StatementLocation, idents: impl Iterator<Item = Ident>) {
        let (is_using_undefined, phis_used) = idents
            .fold((false, vec![]), |(mut is_using_undefined, mut phis_used), ident| {
                if ident == self.undefined {
                    is_using_undefined = true;
                } else if self.is_a_phi(ident) {
                    phis_used.push(ident);
                }
                (is_using_undefined, phis_used)
            });
        if is_using_undefined {
            self.undefined_users.push(location);
        }
        for phi in phis_used {
            self.phi_users.insert(phi, location);
        }
    }

    fn is_a_phi(&self, ident: Ident) -> bool {
        ident != self.undefined && self.phi_locations.contains_key(&ident)
    }

    fn phi_operands(&self, phi: Ident) -> &[Ident] {
        let location = self.phi_locations[&phi];
        match &self.graph.block(location.block)[location.index] {
            Statement::Definition { value: Value::Phi(operands), .. } => &operands,
            _ => unreachable!()
        }
    }

    fn phi_operands_mut(&mut self, phi: Ident) -> &mut Vec<Ident> {
        let location = self.phi_locations[&phi];
        match &mut self.graph.block_mut(location.block)[location.index] {
            Statement::Definition { value: Value::Phi(operands), .. } => operands,
            _ => unreachable!()
        }
    }

    fn comment(&mut self, block: NodeIndex, comment: &str) {
        self.graph.block_mut(block).push(Statement::Comment(comment.to_owned()))
    }

    fn new_block(&mut self) -> NodeIndex {
        let block = self.graph.add_node(BasicBlock(vec![]));
        self.comment(block, &format!("--- BLOCK {} ---", block.index()));
        block
    }
}

fn remove_statements(graph: &mut BasicBlockGraph, locations: impl Iterator<Item = StatementLocation>) {
    let locations_sorted = locations
        .sorted_by(|a, b| a.block.cmp(&b.block).then_with(|| a.index.cmp(&b.index)));
    let indices_by_block = locations_sorted
        .into_iter()
        .group_by(|location| location.block);
    for (block, locations) in &indices_by_block {
        let mut indices = locations.map(|location| location.index);
        let mut index_to_be_removed = indices.next();
        let basic_block = graph.block_mut(block);
        let pruned = replace(basic_block, BasicBlock(vec![]))
            .0
            .into_iter()
            .enumerate()
            .filter_map(|(statement_index, statement)| {
                if let Some(to_be_removed) = index_to_be_removed {
                    if statement_index == to_be_removed {
                        index_to_be_removed = indices.next();
                        return None;
                    }
                }
                Some(statement)
            })
            .collect_vec();
        *basic_block = BasicBlock(pruned);
    }
}

fn get_value_operands<'a>(value: &'a Value) -> Box<dyn Iterator<Item = Ident> + 'a> {
    match value {
        Value::Undefined |
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

        Value::Phi(operands) => Box::new(operands.iter().cloned()),
        Value::Call(function, arguments) => Box::new(once(*function).chain(arguments.iter().cloned())),
    }
}

fn warn_about_redundant_bindings(variables: HashMap<Variable, HashMap<NodeIndex, Ident>>, variables_read: HashSet<Variable>) {
    let variables = variables
        .into_iter()
        .map(|(variable, _)| variable)
        .collect::<HashSet<_>>();
    let redundant_bindings = variables
        .difference(&variables_read)
        .filter_map(|variable| match variable {
            Variable::Binding(binding) => Some(binding.clone()),
            Variable::Value(_) => None,
        })
        .sorted_by(|a, b| a.source.range().start().cmp(&b.source.range().start()));
    for binding in redundant_bindings {
        warning_at!(binding.source, "unused definition: {}", binding.source.text());
    }
}

fn replace_phi(value: &mut Value, phi: Ident, replacement: Ident) {
    let operands: Box<dyn Iterator<Item = &mut Ident>> = match value {
        Value::Undefined |
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
        if *operand == phi {
            *operand = replacement;
        }
    }
}

#[derive(Deref, DerefMut)]
pub(crate) struct BasicBlock(Vec<Statement>);

impl fmt::Debug for BasicBlock {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "{:?}", self.iter().format("\n"))
    }
}

#[derive(Deref, DerefMut)]
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

pub(crate) struct ControlFlowGraph {
    pub(crate) graph: BasicBlockGraph,
    pub(crate) entry_block: NodeIndex,
    pub(crate) exit_block: NodeIndex,
    pub(crate) undefined: Ident,
    pub(crate) id_count: usize,
}

#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
struct StatementLocation {
    block: NodeIndex,
    index: usize
}

impl StatementLocation {
    fn new(block: NodeIndex, index: usize) -> Self {
        Self { block, index }
    }
}

#[derive(Copy, Clone, Eq, PartialEq, Hash)]
pub(crate) struct Ident(usize);

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

pub(crate) enum Statement {
    Comment(String),
    Definition {
        ident: Ident,
        value: Value
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
            Statement::Definition { ident, value } => write!(f, "{} ← {:?}", ident, value),
            Statement::CondJump(ident, positive, negative) =>
                write!(f, "condjump {}, {}, {}", ident, positive.index(), negative.index()),
        }
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
    Phi(Vec<Ident>),
    Call(Ident, Vec<Ident>),
    Arg(usize),
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
            Value::Phi(operands) => write!(f, "ϕ({})", operands.iter().format(", ")),
            Value::Call(function, arguments) => write!(f, "call {}({})", function, arguments.iter().format(", ")),
            Value::Arg(index) => write!(f, "arg[{}]", index),
        }
    }
}

#[test]
fn gen_ir() -> TestResult {
    let code = typecheck!("
    let x = 47
    let y = 29
    let z = y
    1
    let r = if 0 {
        let a = z
        let b = a
        z = 22
        x + b + y
        1
    } else {
        z = 33
        1
    }
    z
    r + 1
    z + 1

    z = 6
    z + 7
    z = 8
    z + 9

    let f = λ (g: λ (u:str, v:str) -> str, u:str, v:str) -> g u v
    let s1 = f (λ (u:str, v:str) -> \"(\" + u + \", \" + v + \")\") \"hello\" \"world\"
    let s2 = f (λ (u:str, v:str) -> \"<\" + u + \"; \" + v + \">\") (\"bye, \" + \"world\") \"seeya\"
    s1 + s2
    ")?;

    let cfg = FrontEnd::new(EnableWarnings(false)).build(&code);
    let mut output = std::fs::File::create("ir_cfg.dot")?;
    crate::graphviz::Graphviz::new()
        .include_comments(true)
        .fmt(&mut output, &cfg.graph)?;

    let value = eval::EvalContext::new(&cfg).eval();
    assert_eq!(value, Value::String(Rc::new("(hello, world)<bye, world; seeya>".to_owned())));

    Ok(())
}
