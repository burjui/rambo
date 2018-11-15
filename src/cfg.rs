use std::cell::RefCell;
use std::collections::HashSet;
use std::fmt::Debug;
use std::fmt::Display;
use std::fmt::Formatter;
use std::rc::Rc;

use itertools::Itertools;
use petgraph::dot::Dot;
use petgraph::Graph;
use petgraph::graph::NodeIndex;

use crate::env::Environment;
use crate::semantics::Binding;
use crate::semantics::BindingRef;
use crate::semantics::Block;
use crate::semantics::ExprRef;
use crate::semantics::TypedExpr;
use crate::semantics::TypedStatement;
use crate::unique_rc::UniqueRc;

// TODO rewrite this for VM code or some IR

crate struct CFGBuilder {
    cfg: CFG,
    env: Environment<Rc<String>, BindingRef>
}

impl CFGBuilder {
    crate fn new() -> Self {
        Self {
            cfg: CFG::new(),
            env: Environment::new()
        }
    }

    crate fn scan(mut self, code: &ExprRef) -> CFG {
        let entry_node = self.cfg.add_node(CFGNode::Entry);
        let (first_block, first_nodes) = self.new_basic_block(&[entry_node], "scan");
        let exit_node = self.cfg.add_node(CFGNode::Exit);
        let (_, last_nodes) = self.scan_expr(code, first_block, &first_nodes);
        self.cfg.link(&last_nodes, &[exit_node], "");
        verify_cfg(&self.cfg);
        self.cfg
    }

    fn scan_block(&mut self, code: &[TypedStatement], mut block: BasicBlockRef, entries: &[NodeIndex]) -> (BasicBlockRef, Vec<NodeIndex>) {
        self.env.push();
        let mut entries = entries.to_vec();
        let mut previous_statement: Option<TypedStatement> = None;
        for statement in code {
            if let Some(TypedStatement::Expr(expr)) = previous_statement {
                if let TypedExpr::Application {..} = *expr {
                    let (new_block, new_entries) = self.new_basic_block(&entries, "application exit");
                    block = new_block;
                    entries = new_entries;
                }
            }
            match statement {
                TypedStatement::Binding(binding) => {
                    self.env.bind(binding.name.clone(), binding.clone());
                    block.push(statement.clone());
                },
                TypedStatement::Expr(expr) => {
                    let (b, e) = self.scan_expr(expr, block.clone(), &entries);
                    block = b;
                    entries = e;
                }
            }
            previous_statement = Some(statement.clone());
        }
        self.env.pop();
        (block, entries)
    }

    fn scan_expr(&mut self, expr: &ExprRef, block: BasicBlockRef, entries: &[NodeIndex]) -> (BasicBlockRef, Vec<NodeIndex>) {
        match expr as &TypedExpr {
            TypedExpr::Conditional { condition, positive, negative, .. } => {
                let (_, condition) = self.scan_expr(condition, block.clone(), entries);
                let (positive_block, positive_nodes) = self.new_basic_block(&condition, "positive");
                let (_, mut nodes) = self.scan_expr(positive, positive_block, &positive_nodes);

                if let Some(negative) = negative.as_ref() {
                    let (negative_block, negative_nodes) = self.new_basic_block(&condition, "negative");
                    let (_, mut negative) = self.scan_expr(negative, negative_block, &negative_nodes);
                    nodes.append(&mut negative);
                }
                (BasicBlockRef::new(BasicBlock::new()), nodes)
            },
            TypedExpr::Application { function, .. } => {
                block.push(TypedStatement::Expr(expr.clone()));
                let (application_block, application_nodes) = self.new_basic_block(&entries, "application entry");
                self.scan_expr(function, application_block, &application_nodes)
            }
            TypedExpr::Block(Block { statements, .. }) => self.scan_block(statements, block, entries),
            TypedExpr::Lambda(lambda, _) => {
                self.env.push();
                for parameter in &lambda.parameters {
                    let parameter_name = Rc::new(parameter.name.text().to_owned());
                    self.env.bind(parameter_name.clone(), BindingRef::from(Binding {
                        name: parameter_name.clone(),
                        data: ExprRef::from(TypedExpr::ArgumentPlaceholder(parameter_name, parameter.type_.clone())),
                        source: parameter.source.clone()
                    }));
                }
                let result = self.scan_expr(&lambda.body, block, entries);
                self.env.pop();
                result
            },
            TypedExpr::Deref(name, _, _) => {
                let binding = self.env.resolve(name).unwrap();
                self.scan_expr(&binding.data, block, entries)
            },
            TypedExpr::AddInt(left, right, _) |
            TypedExpr::SubInt(left, right, _) |
            TypedExpr::MulInt(left, right, _) |
            TypedExpr::DivInt(left, right, _) |
            TypedExpr::AddStr(left, right, _) => {
                let (block, entries) = self.scan_expr(left, block, entries);
                let (block, entries) = self.scan_expr(right, block, &entries);
                (block, entries)
            },
            TypedExpr::Assign(_left, _right, _) => {
                block.push(TypedStatement::Expr(expr.clone()));
                (block, entries.to_vec())
            },
            TypedExpr::ArgumentPlaceholder(_, _) |
            TypedExpr::Unit(_) |
            TypedExpr::Int(_, _) |
            TypedExpr::String(_, _) => {
                block.push(TypedStatement::Expr(expr.clone()));
                (block, entries.to_vec())
            }
        }
    }

    fn new_basic_block(&mut self, entries: &[NodeIndex], source: &'static str) -> (BasicBlockRef, Vec<NodeIndex>) {
        let block = BasicBlockRef::new(BasicBlock::new());
        let node = self.cfg.add_node(CFGNode::BasicBlock(source, block.clone()));
        self.cfg.link(entries, &[node], "");
        (block, vec![node])
    }
}

fn verify_cfg(cfg: &CFG) {
    let mut visited_edges: HashSet<(NodeIndex, NodeIndex)> = HashSet::new();
    for edge in cfg.raw_edges() {
        let source = cfg.node_weight(edge.source()).unwrap();
        let target = cfg.node_weight(edge.target()).unwrap();
        let edge = (edge.source(), edge.target());
        assert!(!visited_edges.contains(&edge), "redundant edge from: {}\nto: {}", source, target);
        visited_edges.insert(edge);
    }
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

crate type BasicBlockRef = UniqueRc<RefCell<BasicBlock>>;

impl BasicBlockRef {
    fn new(block: BasicBlock) -> Self {
        Self::from(RefCell::new(block))
    }

    fn push(&self, statement: TypedStatement) {
        self.borrow_mut().push(statement);
    }
}

impl Debug for BasicBlockRef {
    fn fmt(&self, formatter: &mut Formatter<'_>) -> std::fmt::Result {
        write!(formatter, "{:?}", self.borrow())
    }
}

crate enum CFGNode {
    Entry,
    Exit,
    BasicBlock(&'static str, BasicBlockRef)
}

impl Display for CFGNode {
    fn fmt(&self, formatter: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            CFGNode::Entry => formatter.write_str("entry"),
            CFGNode::Exit => formatter.write_str("exit"),
            CFGNode::BasicBlock(source, block) => {
                if !source.is_empty() {
                    writeln!(formatter, "[{}]", source)?;
                }
                write!(formatter, "{:?}", block)
            }
        }
    }
}

crate type CFG = Graph<CFGNode, &'static str>;

trait GraphUtils {
    type EdgeWeight;

    fn link(&mut self, from: &[NodeIndex], to: &[NodeIndex], weight: Self::EdgeWeight);
}

impl GraphUtils for CFG {
    type EdgeWeight = &'static str;

    fn link(&mut self, from: &[NodeIndex], to: &[NodeIndex], weight: Self::EdgeWeight) {
        for from in from {
            for to in to {
                self.add_edge(*from, *to, weight);
            }
        }
    }
}

#[allow(unused)]
crate fn dump_graph(graph: &CFG, filename: &str) {
    use std::fs::File;
    use std::io::BufWriter;
    use std::io::Write;
    let file = File::create(filename).unwrap();
    writeln!(BufWriter::new(file), "{:#}", Dot::new(graph));
}
