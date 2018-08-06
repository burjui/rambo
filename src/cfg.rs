use crate::semantics::*;
use std::rc::Rc;
use std::cell::RefCell;
use std::fmt;
use petgraph::{ Graph, Direction, visit::EdgeRef, graph::NodeIndex, dot::{ Dot, Config } };
use crate::utils::*;

crate fn construct_cfg(code: &[TypedStatement]) -> CFG {
    let mut cfg = CFG::new();
    let entry_block = cfg.add_node(CFGNode::Entry);
    let exit_block = cfg.add_node(CFGNode::Exit);
    let last_block = scan_basic_blocks(&mut cfg, entry_block, code);
    let last_block_length = if let CFGNode::BasicBlock(block) = cfg.node_weight(last_block).unwrap() {
        block.block.borrow().statements.len()
    } else {
        unreachable!()
    };
    if last_block_length == 0 {
        let incoming = cfg.edges_directed(last_block, Direction::Incoming).collect::<Vec<_>>();
        for edge in incoming {
            cfg.add_edge(edge.source(), exit_block, 0);
            cfg.remove_edge(edge.id());
        }
    } else {
        cfg.add_edge(last_block, exit_block, 0);
    }
    cfg
}

fn scan_basic_blocks(cfg: &mut CFG, entry: NodeIndex, code: &[TypedStatement]) -> NodeIndex {
    let (mut current_block, mut current_node) = new_basic_block(cfg);
    cfg.add_edge(entry, current_node, 0);
    for statement in code {
        match statement {
            TypedStatement::Binding(_) => current_block.push(statement.clone()),
            TypedStatement::Expr(expr) => {
                if let TypedExpr::Conditional { positive, negative, .. } = expr as &TypedExpr {
                    current_block.push(statement.clone());
                    let (positive_block, positive_node) = new_basic_block(cfg);
                    positive_block.push(TypedStatement::Expr(positive.clone()));
                    cfg.add_edge(current_node, positive_node, 0);
                    let negative_node = negative.as_ref().map(|clause| {
                        let (block, node) = new_basic_block(cfg);
                        block.push(TypedStatement::Expr(clause.clone()));
                        cfg.add_edge(current_node, node, 0);
                        node
                    });

                    let (block, node) = new_basic_block(cfg);
                    current_block = block;
                    current_node = node;
                    cfg.add_edge(positive_node, current_node, 0);
                    negative_node.map(|node| cfg.add_edge(node, current_node, 0));
                } else {
                    current_block.push(statement.clone());
                }
            }
        }
    }
    current_node
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

impl fmt::Display for BasicBlock {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(formatter, "{}", self.statements.iter().join_as_strings("\n"))
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

impl fmt::Display for BasicBlockRef {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(formatter, "{}", self.block.borrow())
    }
}

crate enum CFGNode {
    Entry,
    Exit,
    BasicBlock(BasicBlockRef)
}

impl fmt::Display for CFGNode {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            CFGNode::Entry => formatter.write_str("entry"),
            CFGNode::Exit => formatter.write_str("exit"),
            CFGNode::BasicBlock(block) => write!(formatter, "{}", block)
        }
    }
}

type CFG = Graph<CFGNode, u8>;

#[allow(unused)]
crate fn display_graph(graph: &CFG, filename: &str) {
    use std::fs::File;
    use std::io::BufWriter;
    use std::io::Write;
    use std::process::Command;
    {
        let file = File::create(filename).unwrap();
        let x = Dot::with_config(graph, &[Config::EdgeNoLabel]);
        writeln!(BufWriter::new(file), "{}", x);
    }
    let _ = Command::new("dot").arg("-Txlib").arg(filename).spawn();
}
