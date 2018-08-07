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
    let (_, last_block) = scan_basic_blocks(&mut cfg, entry_block, code);
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

fn scan_basic_blocks_expr(cfg: &mut CFG, entry: NodeIndex, expr: &ExprRef) -> Option<(BasicBlockRef, NodeIndex)> {
    match expr as &TypedExpr {
        TypedExpr::Conditional { condition, positive, negative, .. } => {
            let condition_node = if let Some((_, node)) = scan_basic_blocks_expr(cfg, entry, condition) {
                node
            } else {
                let (block, node) = new_basic_block(cfg);
                block.push(TypedStatement::Expr(condition.clone()));
                node
            };
            cfg.add_edge(entry, condition_node, 0);

            let positive_node = if let Some((_, node)) = scan_basic_blocks_expr(cfg, condition_node, positive) {
                node
            } else {
                let (block, node) = new_basic_block(cfg);
                block.push(TypedStatement::Expr(positive.clone()));
                cfg.add_edge(condition_node, node, 0);
                node
            };

            let negative_node = negative.as_ref().map(|clause| {
                if let Some((_, node)) = scan_basic_blocks_expr(cfg, condition_node, clause) {
                    node
                } else {
                    let (block, node) = new_basic_block(cfg);
                    block.push(TypedStatement::Expr(clause.clone()));
                    cfg.add_edge(condition_node, node, 0);
                    node
                }
            });

            let (exit_block, exit_node) = new_basic_block(cfg);
            cfg.add_edge(positive_node, exit_node, 0);
            negative_node.map(|negative_node| cfg.add_edge(negative_node, exit_node, 0));
            Some((exit_block, exit_node))
        },
        TypedExpr::Application { function, .. } => {
            let binding = if let TypedExpr::Deref(binding, _) = function as &TypedExpr {
                binding
            } else {
                unreachable!()
            };
            let lambda = if let BindingValue::Var(value) = &binding.borrow().value {
                if let TypedExpr::Lambda(lambda, _) = value as &TypedExpr {
                    lambda.clone()
                } else {
                    unreachable!()
                }
            } else {
                unreachable!()
            };
            println!("##### {:?}", lambda);
            scan_basic_blocks_expr(cfg, entry, &lambda.body)
        },
        TypedExpr::Block(statements, _) => Some(scan_basic_blocks(cfg, entry, statements)),
        _ => None
    }
}

fn scan_basic_blocks(cfg: &mut CFG, entry: NodeIndex, code: &[TypedStatement]) -> (BasicBlockRef, NodeIndex) {
    let (mut current_block, mut current_node) = new_basic_block(cfg);
    cfg.add_edge(entry, current_node, 0);
    for statement in code {
        match statement {
            TypedStatement::Binding(_) => current_block.push(statement.clone()),
            TypedStatement::Expr(expr) => {
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
