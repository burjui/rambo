use std::fmt;
use std::iter::once;
use std::mem::replace;

use hashbrown::HashMap;
use hashbrown::HashSet;
use itertools::Itertools;
use multimap::MultiMap;
use num_bigint::BigInt;
use once_cell::sync;
use once_cell::unsync;
use petgraph::graph::DiGraph;
use petgraph::graph::NodeIndex;
use petgraph::prelude::Direction::Incoming;
use petgraph::visit::EdgeRef;

use crate::semantics::BindingRef;
use crate::semantics::ExprRef;
use crate::semantics::TypedExpr;
use crate::semantics::TypedStatement;
use crate::source::Source;
#[cfg(test)] use crate::utils::TestResult;
use crate::utils::WHITESPACE_REGEX;

// TODO peephole optimizations

#[derive(Deref, DerefMut)]
crate struct BasicBlock(Vec<Statement>);

impl fmt::Debug for BasicBlock {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "{:?}", self.iter().format("\n"))
    }
}

type BasicBlockGraph = DiGraph<BasicBlock, ()>;

crate struct ControlFlowGraph {
    crate graph: BasicBlockGraph,
    crate entry_block: NodeIndex,
    crate exit_block: NodeIndex,
}

#[derive(Copy, Clone, Debug, PartialEq)]
struct StatementLocation {
    block: NodeIndex,
    index: usize
}

impl StatementLocation {
    fn new(block: NodeIndex, index: usize) -> Self {
        Self { block, index }
    }
}

crate struct FrontEnd {
    graph: BasicBlockGraph,
    next_id: usize,
    variables: HashMap<BindingRef, HashMap<NodeIndex, Ident>>,
    sealed_blocks: HashSet<NodeIndex>,
    incomplete_phis: HashMap<NodeIndex, HashMap<BindingRef, Ident>>,
    phi_locations: HashMap<Ident, StatementLocation>,
    phi_users: MultiMap<Ident, StatementLocation>,
    trivial_phis: HashSet<Ident>,
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
    crate fn new() -> Self {
        Self {
            graph:  DiGraph::new(),
            next_id: 0,
            variables: HashMap::new(),
            sealed_blocks: HashSet::new(),
            incomplete_phis: HashMap::new(),
            phi_locations: HashMap::new(),
            phi_users: MultiMap::new(),
            trivial_phis: HashSet::new(),
        }
    }

    crate fn build(mut self, code: &ExprRef) -> ControlFlowGraph {
        let entry_block = self.new_block();
        self.seal_block(entry_block);
        let (exit_block, _) = self.process_expr(code, entry_block);
        self.remove_trivial_phi_statements();

        ControlFlowGraph {
            graph: self.graph,
            entry_block,
            exit_block,
        }
    }

    fn process_expr(&mut self, expr: &ExprRef, block: NodeIndex) -> (NodeIndex, Ident) {
        match &**expr {
            TypedExpr::Block(block_data) => {
                let mut current_block = block;
                let mut current_value = None;
                for statement in &block_data.statements {
                    let (block, ident) = self.process_statement(statement, current_block);
                    current_block = block;
                    current_value = Some(ident);
                }
                let value = current_value.unwrap_or_else(|| self.define(block, Value::Unit));
                (current_block, value)
            }

            TypedExpr::Unit(source) => {
                self.comment(block, source.text());
                (block, self.define(block, Value::Unit))
            }

            TypedExpr::Int(value, source) => {
                self.comment(block, source.text());
                (block, self.define(block, Value::Int(value.clone())))
            }

            TypedExpr::AddInt(left, right, source) => self.process_binary(block, left, right, source, Value::AddInt),
            TypedExpr::SubInt(left, right, source) => self.process_binary(block, left, right, source, Value::SubInt),
            TypedExpr::MulInt(left, right, source) => self.process_binary(block, left, right, source, Value::MulInt),
            TypedExpr::DivInt(left, right, source) => self.process_binary(block, left, right, source, Value::DivInt),

            TypedExpr::Reference(binding, source) => {
                self.comment(block, source.text());
                (block, self.read_variable(binding, block))
            }

            // TODO try rearranging seal_block() calls

            TypedExpr::Conditional { condition, positive, negative, source, .. } => {
                self.comment(block, source.text());
                let (block, condition) = self.process_expr(condition, block);
                let positive_block = self.new_block();
                let negative_block = self.new_block();
                block_mut(&mut self.graph, block).push(
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
                let result_phi = self.try_remove_trivial_phi(result_phi);
                (exit, result_phi)
            }

            TypedExpr::Assign(binding, value, source) => {
                self.comment(block, source.text());
                let (block, value) = self.process_expr(value, block);
                self.write_variable(binding, block, value);
                (block, value)
            }

            _ => unimplemented!("process_expr: {:?}", expr)
        }
    }

    fn process_binary(&mut self, block: NodeIndex, left: &ExprRef, right: &ExprRef, source: &Source,
                      value_constructor: impl Fn(Ident, Ident) -> Value) -> (NodeIndex, Ident)
    {
        let (block, left_value) = self.process_expr(left, block);
        let (block, right_value) = self.process_expr(right, block);
        self.comment(block, source.text());
        (block, self.define(block, value_constructor(left_value, right_value)))
    }

    fn process_statement(&mut self, statement: &TypedStatement, block: NodeIndex) -> (NodeIndex, Ident) {
        match statement {
            TypedStatement::Binding(binding) => {
                self.comment(block, binding.source.text());
                let (block, ident) = self.process_expr(&binding.data, block);
                self.write_variable(binding, block, ident);
                (block, ident)
            },
            TypedStatement::Expr(expr) => {
                self.process_expr(expr, block)
            }
        }
    }

    fn remove_trivial_phi_statements(&mut self) {
        // Sort locations by block
        let mut trivial_phi_locations = self.trivial_phis.iter()
            .map(|phi| self.phi_locations[phi])
            .collect::<Vec<_>>();
        trivial_phi_locations.sort_unstable_by(|a, b| a.block.cmp(&b.block));
        // Then group by block and sort by index
        let trivial_phis_by_block: Vec<(NodeIndex, Vec<usize>)> = trivial_phi_locations.iter()
            .group_by(|location| location.block)
            .into_iter()
            .map(|(block, locations)| {
                let mut indices: Vec<usize> = locations
                    .map(|location| location.index)
                    .collect();
                indices.sort_unstable();
                (block, indices)
            })
            .collect();
        for (block, indices) in trivial_phis_by_block {
            let basic_block = self.block(block);
            let block_length = basic_block.len();
            let indices_length = indices.len();
            let mut pruned = Vec::with_capacity(block_length - indices_length);
            pruned.extend_from_slice(&basic_block[0 .. indices[0]]);
            for (from_excluded, to) in indices.into_iter().chain(once(block_length)).tuple_windows() {
                pruned.extend_from_slice(&basic_block[from_excluded + 1 .. to]);
            }
            *block_mut(&mut self.graph, block) = BasicBlock(pruned);
        }
    }

    fn write_variable(&mut self, variable: &BindingRef, block: NodeIndex, value: Ident) {
        self.variables
            .entry(variable.clone())
            .or_insert_with(HashMap::new)
            .insert(block, value);
    }

    fn read_variable(&mut self, variable: &BindingRef, block: NodeIndex) -> Ident {
        self.variables
            .get(variable)
            .and_then(|map| map.get(&block))
            .map(Clone::clone)
            .unwrap_or_else(|| self.read_variable_recursive(variable, block))
    }

    fn read_variable_recursive(&mut self, variable: &BindingRef, block: NodeIndex) -> Ident {
        let predecessors = unsync::Lazy::new(|| self.graph
            .edges_directed(block, Incoming)
            .collect::<Vec<_>>());
        let value = if !self.sealed_blocks.contains(&block) {
            // Incomplete CFG
            let phi = self.define_phi(block, &[]);
            self.incomplete_phis
                .entry(block)
                .or_insert_with(HashMap::new)
                .insert(variable.clone(), phi);
            phi
        } else if predecessors.len() == 1 {
            // Optimize the common case of one predecessor: no phi needed
            self.read_variable(variable, predecessors[0].source())
        } else {
            // Break potential cycles with operandless phi
            let phi = self.define_phi(block, &[]);
            self.write_variable(variable, block, phi);
            self.add_phi_operands(variable, phi)
        };
        self.write_variable(variable, block, value);
        value
    }

    fn add_phi_operands(&mut self, variable: &BindingRef, phi: Ident) -> Ident {
        let predecessors = self.graph
            .edges_directed(self.phi_locations[&phi].block, Incoming)
            .map(|edge| edge.source())
            .collect::<Vec<_>>();
        for predecessor in predecessors {
            let operand = self.read_variable(variable, predecessor);
            self.phi_operands_mut(phi).push(operand);
        }
        self.try_remove_trivial_phi(phi)
    }

    fn try_remove_trivial_phi(&mut self, phi: Ident) -> Ident {
        let mut same = None;
        for operand in self.phi_operands(phi) {
            if Some(*operand) == same || *operand == phi {
                // Unique value or self−reference
                continue;
            }
            if same.is_some() {
                // The phi merges at least two values: not trivial
                return phi;
            }
            same = Some(*operand);
        }
        self.trivial_phis.insert(phi);

        /* NOTE: this part from the original algorithm from the paper is not implemented:
        if same = None:
            same ← new Undef(); # The phi is unreachable or in the start block
        */
        if let Some(vec) = self.phi_users.get_vec_mut(&phi) {
            vec.remove_item(&self.phi_locations[&phi]);
        }

        static EMPTY_USERS: sync::Lazy<Vec<StatementLocation>> = once_cell::sync_lazy!(vec![]);
        let users = self.phi_users
            .get_vec(&phi)
            .unwrap_or_else(|| &*EMPTY_USERS);
        let same = same.unwrap();
        let mut possible_trivial_phis = vec![];
        for user in users {
            if let Statement::Definition { ident, value } = &mut block_mut(&mut self.graph, user.block)[user.index] {
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
                .collect::<Vec<_>>()
            );
        if let Some(variables) = variables {
            for variable in &variables {
                self.add_phi_operands(variable, self.incomplete_phis[&block][variable]);
            }
        }
        self.sealed_blocks.insert(block);
        self.comment(block, "--- SEALED ---");
    }

    fn define(&mut self, block: NodeIndex, value: Value) -> Ident {
        let phi_usages = self.get_phis(&value);
        let ident = self.new_id();
        let basic_block = block_mut(&mut self.graph, block);
        basic_block.push(Statement::Definition {
            ident,
            value
        });

        if !phi_usages.is_empty() {
            let index = basic_block.len() - 1;
            let this = StatementLocation::new(block, index);
            for phi in phi_usages {
                self.phi_users.insert(phi, this);
            }
        }

        ident
    }

    fn define_phi(&mut self, block: NodeIndex, operands: &[Ident]) -> Ident {
        let phi = self.define(block, Value::Phi(operands.to_vec()));
        let index = self.block(block).len() - 1;
        self.phi_locations.insert(phi, StatementLocation::new(block, index));
        phi
    }

    fn get_phis(&self, value: &Value) -> Vec<Ident> {
        let operands: Vec<Ident> = match value {
            Value::AddInt(left, right) |
            Value::SubInt(left, right) |
            Value::MulInt(left, right) |
            Value::DivInt(left, right) => vec![*left, *right],

            Value::Call(function, arguments) => {
                let mut operands = vec![*function];
                operands.extend_from_slice(&arguments);
                operands
            }

            Value::Return(value) => vec![*value],
            Value::Phi(operands) => operands.clone(),
            _ => vec![]
        };
        operands.into_iter()
            .filter(|operand| self.phi_locations.contains_key(operand))
            .collect()
    }

    fn phi_operands(&self, phi: Ident) -> &[Ident] {
        let location = self.phi_locations[&phi];
        match &self.block(location.block)[location.index] {
            Statement::Definition { value: Value::Phi(operands), .. } => &operands,
            _ => unreachable!()
        }
    }

    fn phi_operands_mut(&mut self, phi: Ident) -> &mut Vec<Ident> {
        let location = self.phi_locations[&phi];
        match &mut block_mut(&mut self.graph, location.block)[location.index] {
            Statement::Definition { value: Value::Phi(operands), .. } => operands,
            _ => unreachable!()
        }
    }

    fn comment(&mut self, block: NodeIndex, comment: &str) {
        block_mut(&mut self.graph, block).push(Statement::Comment(comment.to_owned()))
    }

    fn new_id(&mut self) -> Ident {
        let next_id = self.next_id + 1;
        Ident(replace(&mut self.next_id, next_id))
    }

    fn new_block(&mut self) -> NodeIndex {
        let block = self.graph.add_node(BasicBlock(vec![]));
        self.comment(block, &format!("--- BLOCK {} ---", block.index()));
        block
    }

    fn block(&self, block: NodeIndex) -> &BasicBlock {
        self.graph.node_weight(block).unwrap()
    }
}

fn block_mut(graph: &mut BasicBlockGraph, block: NodeIndex) -> &mut BasicBlock {
    graph.node_weight_mut(block).unwrap()
}

fn replace_phi(value: &mut Value, phi: Ident, replacement: Ident) {
    let operands: Vec<&mut Ident> = match value {
        Value::AddInt(left, right) |
        Value::SubInt(left, right) |
        Value::MulInt(left, right) |
        Value::DivInt(left, right) => vec![left, right],

        Value::Call(function, arguments) => {
            let mut operands = vec![function];
            operands.extend(arguments.iter_mut());
            operands
        },
        Value::Return(value) => vec![value],
        Value::Phi(operands) => operands.iter_mut().collect(),
        _ => vec![]
    };
    for operand in operands {
        if *operand == phi {
            *operand = replacement;
        }
    }
}

#[derive(Copy, Clone, Eq, PartialEq, Hash)]
crate struct Ident(usize);

impl fmt::Debug for Ident {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "v{:?}", self.0)
    }
}

#[derive(Clone)]
crate enum Statement {
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
            Statement::Definition { ident, value } => write!(f, "{:?} ← {:?}", ident, value),
            Statement::CondJump(ident, positive, negative) => write!(f, "condjump {:?}, {}, {}", ident, positive.index(), negative.index()),
        }
    }
}

#[derive(Clone)]
crate enum Value {
    Unit,
    Int(BigInt),
//    String(String),
    AddInt(Ident, Ident),
    SubInt(Ident, Ident),
    MulInt(Ident, Ident),
    DivInt(Ident, Ident),
//    Offset(Ident, Ident),
    Arg(usize),
//    Alloc(Ident),
//    Length(Ident),
//    Copy {
//        src: Ident,
//        dst: Ident,
//        size: Ident
//    },
    Call(Ident, Vec<Ident>),
    Return(Ident),
    Phi(Vec<Ident>),
}

impl fmt::Debug for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Unit => f.write_str("()"),
            Value::Int(n) => write!(f, "{}", n),
            Value::AddInt(left, right) => write!(f, "{:?} + {:?}", left, right),
            Value::SubInt(left, right) => write!(f, "{:?} - {:?}", left, right),
            Value::MulInt(left, right) => write!(f, "{:?} * {:?}", left, right),
            Value::DivInt(left, right) => write!(f, "{:?} / {:?}", left, right),
            Value::Arg(index) => write!(f, "arg[{:?}]", index),
            Value::Call(callee, arguments) => write!(f, "call {:?}({:?})", callee, arguments.iter().format(", ")),
            Value::Return(result) => write!(f, "return {:?}", result),
            Value::Phi(operands) => write!(f, "ϕ({:?})", operands.iter().format(", ")),
        }
    }
}

#[test]
fn gen_ir() -> TestResult {
    use std::fs::File;
    use petgraph::dot::Config::EdgeNoLabel;
    use petgraph::dot::Dot;
    use std::io::Write;

    let code = typecheck!("
    let x = 47
    let y = 29
    let z = y
    let r = if 0 {
        let a = z
        let b = a
        z = 22
        x + b + y
    } else {
        z = 33
        999
    }
    z
    r + 1

//    let z = \\ (a:num, b:num) -> {
//        if a {
//            a
//        } else {
//            a + 1
//        } + b
//    }
//    let s = {
//        let a = (z x y)
//        z a 3
//    }
//    let nn = 1
//    let a1 = 0
//    let a2 = a1
//    let a3 = a1 + a2
//    let c = \\ (a:num, b:num) -> 4
//    c 1 2
//    a1 = 10
//    (0 - a1) * (0 - 1)
//    (a1 + a1)
//    (a1 - a1)
//    s
    ")?;
    let cfg = FrontEnd::new().build(&code);
    let mut output = File::create("ir_cfg.dot")?;
    write!(output, "{:?}", Dot::with_config(&cfg.graph, &[EdgeNoLabel]))?;
    Ok(())
}
