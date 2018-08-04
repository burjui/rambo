use petgraph::{ Graph, Direction, graph::NodeIndex, dot::{ Dot, Config } };
use crate::semantics::*;
use crate::source::Source;
use std::collections::hash_map::HashMap;

crate fn remove_reduntant_bindings(code: &[TypedStatement]) -> Vec<TypedStatement> {
    let (graph, map) = Reachability::compute(code);
    {
        use std::fs::File;
        use std::io::BufWriter;
        use std::io::Write;
        use std::process::Command;
        const FILENAME: &str = "reachability.dot";
        {
            let file = File::create(FILENAME).unwrap();
            let x = Dot::with_config(&graph, &[Config::EdgeNoLabel]);
            writeln!(BufWriter::new(file), "{}", x);
        }
        let _ = Command::new("dot").arg("-Txlib").arg(FILENAME).spawn();
    }
    remove_reduntant_bindings_with(graph, map, code)
}

fn remove_reduntant_bindings_with(mut graph: ReachabilityGraph, mut map: BindingToNodeMap, code: &[TypedStatement]) -> Vec<TypedStatement> {
    let iter_map = map.clone();
    for (binding, node) in &iter_map {
        let incoming = graph.edges_directed(*node, Direction::Incoming).collect::<Vec<_>>();
        if incoming.len() == 0 {
            unsafe {
                let binding = (**binding).borrow();
                warning_at!(binding.source, "redundant binding: {}", binding.source.text())
            }
            graph.remove_node(*node);
            map.remove(binding);
        }
    }
    code.to_vec()
}

type ReachabilityGraph = Graph<SourcePrinter, u8>;
type BindingToNodeMap = HashMap<BindingPtr, NodeIndex>;

struct Reachability {
    graph: ReachabilityGraph,
    map: BindingToNodeMap
}

impl Reachability {
    fn compute(code: &[TypedStatement]) -> (ReachabilityGraph, BindingToNodeMap) {
        let mut computer = Reachability {
            graph: ReachabilityGraph::new(),
            map: HashMap::new()
        };
        computer.compute_reachability(code);
        (computer.graph, computer.map)
    }

    fn compute_reachability(&mut self, code: &[TypedStatement]) {
        for statement in code {
            match statement {
                TypedStatement::Binding(binding) => self.register(binding),
                TypedStatement::Expr(expr) => self.expr_reachability(expr)
            }
        }
    }

    fn expr_reachability(&mut self, expr: &ExprRef) {
        match expr as &TypedExpr {
            TypedExpr::Phantom |
            TypedExpr::Unit(_) |
            TypedExpr::Int(_, _) |
            TypedExpr::String(_, _) => {},
            TypedExpr::Deref(binding, source) => {
                let src = self.graph.add_node(SourcePrinter(source.clone()));
                let dst = self.map.get(&binding.ptr()).unwrap();
                self.graph.add_edge(src, *dst, 0);
            },
            TypedExpr::Lambda(lambda, _) => {
                lambda.parameters.iter().for_each(|parameter| self.register(parameter));
                self.expr_reachability(&lambda.body);
            },
            TypedExpr::Application { function, arguments, .. } => {
                self.expr_reachability(function);
                arguments.iter().for_each(|argument| self.expr_reachability(argument));
            },
            TypedExpr::AddInt(left, right, _) |
            TypedExpr::SubInt(left, right, _) |
            TypedExpr::MulInt(left, right, _) |
            TypedExpr::DivInt(left, right, _) |
            TypedExpr::AddStr(left, right, _) |
            TypedExpr::Assign(left, right, _) => {
                self.expr_reachability(left);
                self.expr_reachability(right);
            },
            TypedExpr::Conditional { condition, positive, negative, .. } => {
                self.expr_reachability(condition);
                self.expr_reachability(positive);
                negative.as_ref().map(|clause| self.expr_reachability(clause));
            },
            TypedExpr::Block(statements, _) => self.compute_reachability(statements.as_slice())
        }
    }

    fn register(&mut self, binding: &BindingRef) {
        let node = self.graph.add_node(SourcePrinter(binding.borrow().source.clone()));
        self.map.insert(binding.ptr(), node);
        if let BindingValue::Var(value) = &binding.borrow().value {
             self.expr_reachability(value);
        }
    }
}

use std::fmt;
struct SourcePrinter(Source);
impl fmt::Display for SourcePrinter {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let source = &self.0;
        write!(f, "{} [{:?}]", source.text(), source.file.position(source.range.start).unwrap())
    }
}
