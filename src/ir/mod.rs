use std::cell::RefCell;
use std::fmt;
use std::iter::once;

use hashbrown::HashMap;
use hashbrown::HashSet;
use itertools::Itertools;
use multimap::MultiMap;
use num_bigint::BigInt;
use once_cell::unsync::Lazy;
use petgraph::graph::DiGraph;
use petgraph::graph::NodeIndex;
use petgraph::prelude::Direction::Incoming;
use petgraph::visit::EdgeRef;
use rc_arena::Arena;
use rc_arena::Rc;

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

type ControlFlowGraph = DiGraph<BasicBlock, ()>;

trait ControlFlowGraphUtils {
    fn new_block(&mut self) -> NodeIndex;
    fn block(&self, block: NodeIndex) -> &BasicBlock;
    fn block_mut(&mut self, block: NodeIndex) -> &mut BasicBlock;
}

impl ControlFlowGraphUtils for ControlFlowGraph {
    fn new_block(&mut self) -> NodeIndex {
        let block = self.add_node(BasicBlock(vec![]));
        self.node_weight_mut(block).unwrap().push(
            Statement::Comment(format!("*** BLOCK #{} ***", block.index())));
        block
    }

    fn block(&self, block: NodeIndex<u32>) -> &BasicBlock {
        self.node_weight(block).unwrap()
    }

    fn block_mut(&mut self, block: NodeIndex<u32>) -> &mut BasicBlock {
        self.node_weight_mut(block).unwrap()
    }
}

crate struct CFGSSA {
    crate graph: ControlFlowGraph,
    crate entry_block: NodeIndex,
    crate exit_block: NodeIndex,
}

// TODO ger rid of cells
type PhiOperandsCell = RefCell<Vec<Ident>>;
type PhiOperands = Rc<PhiOperandsCell>;

#[derive(Copy, Clone, Debug)]
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
    cfg: ControlFlowGraph,
    entry_block: NodeIndex,
    idgen: IdentGenerator,
    values: HashMap<BindingRef, HashMap<NodeIndex, Ident>>,
    sealed_blocks: HashSet<NodeIndex>,
    incomplete_phis: HashMap<NodeIndex, HashMap<BindingRef, Ident>>,
    phi_operands_arena: Arena<PhiOperandsCell>,
    phi_operands: HashMap<Ident, PhiOperands>,
    phi_locations: HashMap<Ident, StatementLocation>,
    phi_users: MultiMap<Ident, StatementLocation>,
    trivial_phis: HashSet<Ident>,
}

impl FrontEnd {
    crate fn new() -> Self {
        let mut cfg = ControlFlowGraph::new();
        let entry_block = cfg.new_block();
        Self {
            cfg,
            entry_block,
            idgen: IdentGenerator::new(),
            values: HashMap::new(),
            sealed_blocks: HashSet::new(),
            incomplete_phis: HashMap::new(),
            phi_operands_arena: Arena::new(),
            phi_operands: HashMap::new(),
            phi_locations: HashMap::new(),
            phi_users: MultiMap::new(),
            trivial_phis: HashSet::new(),
        }
    }

    crate fn build(mut self, code: &ExprRef) -> CFGSSA {
        let (exit_block, _) = self.process_expr(code, self.entry_block);
        self.seal_block(exit_block);
        self.remove_trivial_phis();

        CFGSSA {
            graph: self.cfg,
            entry_block: self.entry_block,
            exit_block
        }
    }

    fn remove_trivial_phis(&mut self) {
        // Sort localtions by block
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
            eprintln!("!! removing({}): {}", block.index(), indices.iter().format("\n"));
            let basic_block = self.block_mut(block);
            let block_length = basic_block.len();
            let indices_length = indices.len();
            let mut pruned = Vec::with_capacity(block_length - indices_length);
            pruned.extend_from_slice(&basic_block[0 .. indices[0]]);
            for (from_excluded, to) in indices.into_iter().chain(once(block_length)).tuple_windows() {
                pruned.extend_from_slice(&basic_block[from_excluded + 1 .. to]);
            }
            *self.block_mut(block) = BasicBlock(pruned);
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
                let value = current_value.unwrap_or_else(|| self.define(block, Value::Unit).0);
                (current_block, value)
            }

            TypedExpr::Unit(source) => {
                self.comment(block, source.text());
                (block, self.define(block, Value::Unit).0)
            }

            TypedExpr::Int(value, source) => {
                self.comment(block, source.text());
                let (ident, _) = self.define(block, Value::Int(value.clone()));
                (block, ident)
            }

            TypedExpr::AddInt(left, right, source) => self.process_binary(block, left, right, source, Value::AddInt),
            TypedExpr::SubInt(left, right, source) => self.process_binary(block, left, right, source, Value::SubInt),
            TypedExpr::MulInt(left, right, source) => self.process_binary(block, left, right, source, Value::MulInt),
            TypedExpr::DivInt(left, right, source) => self.process_binary(block, left, right, source, Value::DivInt),

            TypedExpr::Reference(binding, source) => {
                self.comment(block, source.text());
                let ident = self.read_variable(binding, block);
                (block, ident)
            }

            TypedExpr::Conditional { condition, positive, negative, source, .. } => {
                let (block, condition) = self.process_expr(condition, block);
                let positive_block = self.cfg.new_block();
                let negative_block = self.cfg.new_block();
                let basic_block = self.block_mut(block);
                basic_block.push(Statement::CondJump(condition, positive_block, negative_block));
                self.seal_block(block);

                self.cfg.add_edge(block, positive_block, ());
                self.cfg.add_edge(block, negative_block, ());
                let (positive_exit_block, positive) = self.process_expr(positive, positive_block);
                let (negative_exit_block, negative) = self.process_expr(negative, negative_block);
                eprintln!("positive_block:\n.. {:?}", self.block_mut(positive_block).iter().format("\n.. "));
                eprintln!("negative_block:\n.. {:?}", self.block_mut(negative_block).iter().format("\n.. "));
                let exit = self.cfg.new_block();
                self.cfg.add_edge(positive_exit_block, exit, ());
                self.cfg.add_edge(negative_exit_block, exit, ());
                self.seal_block(positive_exit_block);
                self.seal_block(negative_exit_block);

                self.comment(block, source.text());
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
        let (ident, _) = self.define(block, value_constructor(left_value, right_value));
        (block, ident)
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

    fn write_variable(&mut self, variable: &BindingRef, block: NodeIndex, value: Ident) {
        self.values
            .entry(variable.clone())
            .or_insert_with(HashMap::new)
            .insert(block, value);
    }

    fn read_variable(&mut self, variable: &BindingRef, block: NodeIndex) -> Ident {
        self.values
            .get(variable)
            .and_then(|map| map.get(&block))
            .map(Clone::clone)
            .unwrap_or_else(|| self.read_variable_recursive(variable, block))
    }

    fn read_variable_recursive(&mut self, variable: &BindingRef, block: NodeIndex) -> Ident {
        let predecessors = Lazy::new(|| self.cfg.edges_directed(block, Incoming).collect::<Vec<_>>());
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
        let predecessors = self.cfg
            .edges_directed(self.phi_locations[&phi].block, Incoming)
            .map(|edge| edge.source())
            .collect::<Vec<_>>();
        for predecessor in predecessors {
            let operand = self.read_variable(variable, predecessor);
            self.phi_operands[&phi].borrow_mut().push(operand);
        }
        self.try_remove_trivial_phi(phi)
    }

    fn try_remove_trivial_phi(&mut self, phi: Ident) -> Ident {
        eprintln!("try_remove_trivial_phi: {:?}", phi);
        let mut same = None;
        let operands = self.phi_operands[&phi].borrow();
        for operand in operands.iter() {
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
        drop(operands);
        self.trivial_phis.insert(phi);

        /*
        Next part differs from the algorithm from the paper
        Original code:

            if same = None:
                same ← new Undef(); # The phi is unreachable or in the start block
            users ← phi.users.remove(phi) # Remember all users except the phi itself
            phi.replaceBy(same) # Reroute all uses of phi to same and remove phi
        */
        let users = self.phi_users
            .get_vec(&phi)
            .map(|vec| vec.to_vec())
            .unwrap_or_else(Vec::new);
        eprintln!("users({:?}):\n  {:?}", phi, users.iter()
            .map(|user| &self.cfg.node_weight(user.block).unwrap()[user.index])
            .format("\n  "));
        let same = same.unwrap();
        let mut possible_trivial_phis = vec![];
        for user in &users {
            if let Statement::Definition { ident, value } = &mut self.block_mut(user.block)[user.index] {
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
        self.comment(block, "*** SEALED ***");
    }

    fn define(&mut self, block: NodeIndex, value: Value) -> (Ident, usize) {
        let phi_usages = self.get_phis(&value);
        let ident = self.idgen.new_id();
        let definition = Statement::Definition {
            ident,
            value
        };

        let basic_block = self.block_mut(block);
        let index = basic_block.len();
        basic_block.push(definition);

        if !phi_usages.is_empty() {
            let user = StatementLocation::new(block, index);
            for phi in phi_usages {
                self.phi_users.insert(phi, user);
            }
        }

        (ident, index)
    }

    fn define_phi(&mut self, block: NodeIndex, operands: &[Ident]) -> Ident {
        let operands = self.phi_operands_arena.alloc(RefCell::new(Vec::from(operands)));
        let (phi, index) = self.define(block, Value::Phi(operands.clone()));
        self.phi_operands.insert(phi, operands);
        self.phi_locations.insert(phi, StatementLocation::new(block, index));
        phi
    }

    fn get_phis(&self, value: &Value) -> Vec<Ident> {
        let operands: Vec<&Ident> = match value {
            Value::AddInt(left, right) |
            Value::SubInt(left, right) |
            Value::MulInt(left, right) |
            Value::DivInt(left, right) => vec![left, right],

            Value::Call(function, arguments) => {
                let mut operands = vec![function];
                operands.extend(arguments.iter());
                operands
            },
            Value::Return(value) => vec![value],
            Value::Phi(operands) => {
                let borrowed = operands.borrow();
                borrowed.iter()
                    .map(|operand| operand as *const Ident)
                    .map(|ptr| unsafe { &*ptr })
                    .collect()
            },
            _ => vec![]
        };
        operands.into_iter()
            .filter(|operand| self.phi_operands.contains_key(operand))
            .cloned()
            .collect()
    }

    fn comment(&mut self, block: NodeIndex, comment: &str) {
        self.cfg.block_mut(block).push(Statement::Comment(comment.to_owned()))
    }

    fn block_mut(&mut self, node: NodeIndex) -> &mut BasicBlock {
        self.cfg.node_weight_mut(node).unwrap()
    }
}

fn replace_phi(phi_user: &mut Value, phi: Ident, value: Ident) {
    let operands: Vec<&mut Ident> = match phi_user {
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
        Value::Phi(operands) => {
            let mut borrowed = operands.borrow_mut();
            borrowed.iter_mut()
                .map(|operand| operand as *mut Ident)
                .map(|ptr| unsafe { &mut *ptr })
                .collect()
        },
        _ => vec![]
    };
    for operand in operands {
        if *operand == phi {
            *operand = value;
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

crate struct IdentGenerator(usize);

impl IdentGenerator {
    crate fn new() -> Self {
        Self(0)
    }

    crate fn new_id(&mut self) -> Ident {
        let id = self.0;
        self.0 += 1;
        Ident(id)
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
    Phi(PhiOperands),
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
            Value::Phi(operands) => write!(f, "ϕ({:?})", operands.borrow().iter().format(", ")),
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
