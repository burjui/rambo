use std::fmt;
use std::ops::Deref;
use std::ops::DerefMut;

use hashbrown::HashMap;
use itertools::Itertools;
use num_bigint::BigInt;
use petgraph::graph::DiGraph;
use petgraph::graph::NodeIndex;

use crate::semantics::BindingRef;
use crate::semantics::ExprRef;
use crate::semantics::TypedExpr;
use crate::semantics::TypedStatement;
use crate::source::Source;
#[cfg(test)]
use crate::utils::TestResult;

type BasicBlockStatements = Vec<Statement>;
// TODO: #[derive(Deref, DerefMut)]
crate struct BasicBlock(BasicBlockStatements);

impl BasicBlock {
    crate fn new() -> Self {
        Self(Vec::new())
    }
}

impl Deref for BasicBlock {
    type Target = BasicBlockStatements;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl DerefMut for BasicBlock {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl fmt::Debug for BasicBlock {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "{:?}", self.iter().format("\n"))
    }
}

#[derive(Copy, Clone)]
crate struct Boundary {
    entry: NodeIndex,
    exit: NodeIndex
}

type ControlFlowGraph = DiGraph<BasicBlock, ()>;

crate struct CFGSSA {
    crate graph: ControlFlowGraph,
    crate boundary: Boundary
}

enum BoundaryType { Single, Split }

crate struct FrontEnd {
    cfg: ControlFlowGraph,
    initial_boundary: Boundary,
    idgen: IdentGenerator,
    values: HashMap<BindingRef, HashMap<NodeIndex, Ident>>,
    unit: Ident
}

impl FrontEnd {
    crate fn new() -> Self {
        let mut cfg = DiGraph::new();
        let mut entry_block = BasicBlock::new();
        let mut idgen = IdentGenerator::new();
        let unit = idgen.new_id();
        entry_block.push(Statement::Definition {
            ident: unit,
            value: Value::Unit
        });
        let entry = cfg.add_node(entry_block);
        Self {
            cfg,
            initial_boundary: Boundary {
                entry,
                exit: entry
            },
            idgen,
            values: HashMap::new(),
            unit
        }
    }

    crate fn build(mut self, code: &ExprRef) -> CFGSSA {
        let boundary = self.new_boundary(BoundaryType::Single);
        let _ = self.process_expr(code, boundary);
        CFGSSA {
            graph: self.cfg,
            boundary
        }
    }

    fn process_expr(&mut self, expr: &ExprRef, boundary: Boundary) -> (Boundary, Ident) {
        match &**expr {
            TypedExpr::Block(block) => {
                let mut current_boundary = boundary;
                let mut current_value = None;
                for statement in &block.statements {
                    let (boundary, ident) = self.process_statement(statement, current_boundary);
                    current_boundary = boundary;
                    current_value = Some(ident);
                }
                let value = match current_value {
                    Some(ident) => ident,
                    None => {
                        let (definition, ident) = self.new_definition(Value::Unit);
                        self.block_mut(boundary.entry).push(definition);
                        ident
                    }
                };
                (boundary, value)
            }

            TypedExpr::Unit(source) => {
                self.block_mut(boundary.entry).push(Statement::Comment(source.text().to_owned()));
                (boundary, self.unit)
            }

            TypedExpr::Int(value, source) => {
                let (definition, ident) = self.new_definition(Value::Int(value.clone()));
                let block = self.block_mut(boundary.entry);
                block.push(Statement::Comment(source.text().to_owned()));
                block.push(definition);
                (boundary, ident)
            }

            TypedExpr::AddInt(left, right, source) => self.process_binary(boundary, left, right, source, Value::AddInt),
            TypedExpr::SubInt(left, right, source) => self.process_binary(boundary, left, right, source, Value::SubInt),
            TypedExpr::MulInt(left, right, source) => self.process_binary(boundary, left, right, source, Value::MulInt),
            TypedExpr::DivInt(left, right, source) => self.process_binary(boundary, left, right, source, Value::DivInt),

            TypedExpr::Reference(binding, _) => {
                let ident = self.read_variable(binding, boundary.entry);
                (boundary, ident)
            }

            TypedExpr::Conditional { condition, positive, negative, source, .. } => {
                let (boundary, condition) = self.process_expr(condition, boundary);
                let positive_boundary = self.new_boundary(BoundaryType::Single);
                let negative_boundary = self.new_boundary(BoundaryType::Single);
                let block = self.block_mut(boundary.exit);
                block.push(Statement::CondJump(condition));

                self.cfg.add_edge(boundary.exit, positive_boundary.entry, ());
                self.cfg.add_edge(boundary.exit, negative_boundary.entry, ());
                let (positive_boundary, positive) = self.process_expr(positive, positive_boundary);
                let (negative_boundary, negative) = self.process_expr(negative, positive_boundary);
                let exit = self.new_boundary(BoundaryType::Single);
                self.cfg.add_edge(positive_boundary.exit, exit.entry, ());
                self.cfg.add_edge(negative_boundary.exit, exit.entry, ());

                let block = self.block_mut(exit.entry);
                block.push(Statement::Comment(source.text().to_owned()));
                let (definition, dummy_phi) = self.new_definition(Value::Phi(MISSING_IDENT, MISSING_IDENT));
                (exit, dummy_phi)
            }

            _ => unimplemented!("process_expr: {:?}", expr)
        }
    }

    fn process_binary(&mut self, boundary: Boundary, left: &ExprRef, right: &ExprRef, source: &Source, value_constructor: impl Fn(Ident, Ident) -> Value) -> (Boundary, Ident) {
        let (boundary, left_value) = self.process_expr(left, boundary);
        let (boundary, right_value) = self.process_expr(right, boundary);
        let (definition, ident) = self.new_definition(value_constructor(left_value, right_value));
        let block = self.block_mut(boundary.entry);
        block.push(Statement::Comment(source.text().to_owned()));
        block.push(definition);
        (boundary, ident)
    }

    fn process_statement(&mut self, statement: &TypedStatement, boundary: Boundary) -> (Boundary, Ident) {
        match statement {
            TypedStatement::Binding(binding) => {
                let (boundary, ident) = self.process_expr(&binding.data, boundary);
                self.write_variable(binding, boundary.entry, ident);
                (boundary, ident)
            },
            TypedStatement::Expr(expr) => {
                self.process_expr(expr, boundary)
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
        unimplemented!("read_variable_recursive: {:?}", variable)
    }

    fn block_mut(&mut self, node: NodeIndex) -> &mut BasicBlock {
        self.cfg.node_weight_mut(node).unwrap()
    }

    fn new_boundary(&mut self, type_: BoundaryType) -> Boundary {
        let entry = self.cfg.add_node(BasicBlock::new());
        let exit = match type_ {
            BoundaryType::Single => entry,
            BoundaryType::Split => self.cfg.add_node(BasicBlock::new())
        };
        Boundary { entry, exit }
    }

    fn new_definition(&mut self, value: Value) -> (Statement, Ident) {
        let ident = self.idgen.new_id();
        let definition = Statement::Definition {
            ident,
            value
        };
        (definition, ident)
    }
}

#[derive(Copy, Clone)]
crate struct Ident(usize);

impl fmt::Debug for Ident {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "v{:?}", self.0)
    }
}

const MISSING_IDENT: Ident = Ident(0);

crate struct IdentGenerator(usize);

impl IdentGenerator {
    crate fn new() -> Self {
        Self(1)
    }

    crate fn new_id(&mut self) -> Ident {
        let id = self.0;
        self.0 += 1;
        Ident(id)
    }
}

//#[derive(Debug)]
crate enum Statement {
    Comment(String),
    Definition {
        ident: Ident,
        value: Value
    },
    CondJump(Ident),
}


impl fmt::Debug for Statement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Statement::Comment(comment) => write!(f, "// {}", comment),
            Statement::Definition { ident, value } => write!(f, "{:?} ← {:?}", ident, value),
            Statement::CondJump(ident) => write!(f, "condjump {:?}", ident),
        }
    }
}

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
    Phi(Ident, Ident),
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
            Value::Phi(id1, id2) => write!(f, "ϕ({:?}, {:?})", id1, id2),
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
    if 0 {
        let a = z
        let b = a
        x + b + y
    }
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
