use std::cell::RefCell;
use std::collections::HashSet;
use std::fmt::Debug;
use std::fmt::Formatter;
use std::rc::Rc;

use petgraph::Direction;
use petgraph::dot::Config;
use petgraph::dot::Dot;
use petgraph::Graph;
use petgraph::graph::NodeIndex;
use petgraph::visit::EdgeRef;

use crate::semantics::Block;
use crate::semantics::ExprRef;
use crate::semantics::TypedExpr;
use crate::semantics::TypedStatement;
use itertools::Itertools;

crate fn construct_cfg(code: &ExprRef) -> CFG {
    let mut cfg = CFG::new();
    let entry_node = cfg.add_node(CFGNode::Entry);
    let exit_node = cfg.add_node(CFGNode::Exit);
    let last_node = scan_basic_blocks_expr(&mut cfg, entry_node, code)
        .map(|(_, block)| block)
        .unwrap_or(entry_node);

    // If the last block before exit is empty, remove it (can be leftover from a conditional)
    let last_block_is_empty = match cfg.node_weight(last_node) {
        Some(CFGNode::BasicBlock(block)) => block.block.borrow().statements.is_empty(),
        _ => false,
    };
    if last_block_is_empty {
        let incoming_nodes = cfg.edges_directed(last_node, Direction::Incoming)
            .map(|edge| edge.source())
            .collect::<Vec<_>>();
        for node in incoming_nodes {
            cfg.add_edge(node, exit_node, ());
        }
        cfg.remove_node(last_node);
    } else {
        cfg.add_edge(last_node, exit_node, ());
    }

    verify_cfg(&cfg);
    cfg
}

fn verify_cfg(cfg: &CFG) {
    let mut visited_edges: HashSet<(NodeIndex, NodeIndex)> = HashSet::new();
    for edge in cfg.raw_edges() {
        let source = cfg.node_weight(edge.source()).unwrap();
        let target = cfg.node_weight(edge.target()).unwrap();
        let edge = (edge.source(), edge.target());
        assert!(!visited_edges.contains(&edge), "redundant edge from: {:?}\nto: {:?}", source, target);
        visited_edges.insert(edge);
    }
}

fn scan_basic_blocks(cfg: &mut CFG, entry: NodeIndex, code: &[TypedStatement]) -> (BasicBlockRef, NodeIndex) {
    let mut initial_block = Some(BasicBlockRef::new(BasicBlock::new()));
    let mut current_block = initial_block.as_ref().unwrap().clone();
    let mut current_node = entry;
    for statement in code {
        match statement {
            TypedStatement::Binding(_) => current_block.push(statement.clone()),
            TypedStatement::Expr(expr) => {
                if let Some(initial_block) = initial_block.take() {
                    if !initial_block.block.borrow().statements.is_empty() {
                        current_node = cfg.add_node(CFGNode::BasicBlock(initial_block.clone()));
                        cfg.add_edge(entry, current_node, ());
                    }
                }
                if let Some((block, node)) = scan_basic_blocks_expr(cfg, current_node, expr) {
                    current_block = block;
                    current_node = node;
                } else {
                    current_block.push(statement.clone());
                }
            }
        }
    }
    (current_block, current_node)
}

fn scan_basic_blocks_expr(cfg: &mut CFG, entry: NodeIndex, expr: &ExprRef) -> Option<(BasicBlockRef, NodeIndex)> {
    match expr as &TypedExpr {
        TypedExpr::Conditional { condition, positive, negative, .. } => {
            let condition_node = if let Some((_, node)) = scan_basic_blocks_expr(cfg, entry, condition) {
                node
            } else {
                let (block, node) = new_basic_block(cfg);
                block.push(TypedStatement::Expr(condition.clone()));
                cfg.add_edge(entry, node, ());
                node
            };

            let positive_node = if let Some((_, node)) = scan_basic_blocks_expr(cfg, condition_node, positive) {
                node
            } else {
                let (block, node) = new_basic_block(cfg);
                block.push(TypedStatement::Expr(positive.clone()));
                cfg.add_edge(condition_node, node, ());
                node
            };

            let negative_node = negative.as_ref().map(|clause| {
                if let Some((_, node)) = scan_basic_blocks_expr(cfg, condition_node, clause) {
                    node
                } else {
                    let (block, node) = new_basic_block(cfg);
                    block.push(TypedStatement::Expr(clause.clone()));
                    cfg.add_edge(condition_node, node, ());
                    node
                }
            });

            let (exit_block, exit_node) = new_basic_block(cfg);
            cfg.add_edge(positive_node, exit_node, ());
            negative_node.map(|negative_node| cfg.add_edge(negative_node, exit_node, ()));
            Some((exit_block, exit_node))
        },
        TypedExpr::Application { function, .. } => scan_basic_blocks_expr(cfg, entry, function),
        TypedExpr::Block(Block { statements, .. }) => Some(scan_basic_blocks(cfg, entry, statements)),
        _ => {
            let (block, node) = new_basic_block(cfg);
            block.push(TypedStatement::Expr(expr.clone()));
            cfg.add_edge(entry, node, ());
            Some((block, node))
        }
    }
}

fn new_basic_block(cfg: &mut CFG) -> (BasicBlockRef, NodeIndex) {
    let block = BasicBlockRef::new(BasicBlock::new());
    let node = cfg.add_node(CFGNode::BasicBlock(block.clone()));
    (block, node)
}

crate struct BasicBlock {
    statements: Vec<TypedStatement>
}

impl BasicBlock {
    fn new() -> Self {
        Self { statements: vec![] }
    }

    fn push(&mut self, statement: TypedStatement) {
        self.statements.push(statement);
    }
}

impl Debug for BasicBlock {
    fn fmt(&self, formatter: &mut Formatter<'_>) -> std::fmt::Result {
        write!(formatter, "{:?}", self.statements.iter().format("\n"))
    }
}

#[derive(Clone)]
crate struct BasicBlockRef {
    block: Rc<RefCell<BasicBlock>>
}

impl BasicBlockRef {
    fn new(block: BasicBlock) -> Self {
        Self { block: Rc::new(RefCell::new(block)) }
    }

    fn push(&self, statement: TypedStatement) {
        self.block.borrow_mut().push(statement);
    }
}

impl Debug for BasicBlockRef {
    fn fmt(&self, formatter: &mut Formatter<'_>) -> std::fmt::Result {
        write!(formatter, "{:?}", self.block.borrow())
    }
}

crate enum CFGNode {
    Entry,
    Exit,
    BasicBlock(BasicBlockRef)
}

impl Debug for CFGNode {
    fn fmt(&self, formatter: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            CFGNode::Entry => formatter.write_str("entry"),
            CFGNode::Exit => formatter.write_str("exit"),
            CFGNode::BasicBlock(block) => write!(formatter, "{:?}", block)
        }
    }
}

type CFG = Graph<CFGNode, ()>;

#[allow(unused)]
crate fn dump_graph(graph: &CFG, filename: &str) {
    use std::fs::File;
    use std::io::BufWriter;
    use std::io::Write;
    let file = File::create(filename).unwrap();
    let x = Dot::with_config(graph, &[Config::EdgeNoLabel]);
    writeln!(BufWriter::new(file), "{:#?}", x);
}
