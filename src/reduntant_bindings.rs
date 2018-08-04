use petgraph::{ Graph, Direction, visit::EdgeRef, graph::NodeIndex, dot::{ Dot, Config } };
use crate::semantics::*;
use crate::source::Source;
use std::collections::{ hash_map::HashMap, hash_set::HashSet };
use crate::typed_visitor::{ TypedVisitor, NoOpTypedVisitor };

#[derive(PartialEq)]
crate enum Warnings { On, Off }

crate struct RedundantBindings {
    warnings: Warnings,
    graph: ReachabilityGraph,
    map: BindingToNodeMap
}

struct ReduntantBindingRemover {

}

impl ReduntantBindingRemover {

}

impl TypedVisitor for ReduntantBindingRemover {

}

impl RedundantBindings {
    crate fn remove(code: &[TypedStatement], warnings: Warnings) -> Vec<TypedStatement> {
        let (graph, map) = Reachability::compute(code);
        let redundant = RedundantBindings { warnings, graph, map };
        redundant.remove_unreachable(code)
    }

    fn remove_unreachable(mut self, code: &[TypedStatement]) -> Vec<TypedStatement> {
        let mut disconnected_nodes = HashSet::<NodeIndex>::new();
        let binding_nodes = self.map.values().map(|v| *v).collect::<Vec<_>>();
        binding_nodes.into_iter().for_each(|node| Self::process_node(&mut self.graph, node, &mut disconnected_nodes));
        {
            let mut reduntant_sources = disconnected_nodes.iter()
                .filter_map(|node| self.graph.node_weight(*node))
                .map(|SourcePrinter(source)| source)
                .collect::<Vec<_>>();
            reduntant_sources.sort_by_key(|source| source.range.start);
            if self.warnings == Warnings::On {
                for source in reduntant_sources {
                    warning_at!(source, "redundant binding: {}", source.text());
                }
            }
        }
        NoOpTypedVisitor::new().visit_statements(code)
    }

    fn process_node(graph: &mut ReachabilityGraph, node: NodeIndex, disconnected_nodes: &mut HashSet<NodeIndex>) {
        let incoming_count = graph.edges_directed(node, Direction::Incoming).count();
        if incoming_count == 0 {
            disconnected_nodes.insert(node);
            let outgoing = graph.edges_directed(node, Direction::Outgoing).map(|edge| (edge.id(), edge.target())).collect::<Vec<_>>();
            for (edge, dst) in &outgoing {
                graph.remove_edge(*edge);
                Self::process_node(graph, *dst, disconnected_nodes);
            }
        }
    }
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
        computer.compute_reachability(code, &None);
        (computer.graph, computer.map)
    }

    fn compute_reachability(&mut self, code: &[TypedStatement], origin: &Option<BindingRef>) {
        for statement in code {
            match statement {
                TypedStatement::Binding(binding) => self.register(binding),
                TypedStatement::Expr(expr) => self.expr_reachability(expr, origin)
            }
        }
    }

    fn expr_reachability(&mut self, expr: &ExprRef, origin: &Option<BindingRef>) {
        match expr as &TypedExpr {
            TypedExpr::Phantom |
            TypedExpr::Unit(_) |
            TypedExpr::Int(_, _) |
            TypedExpr::String(_, _) => {},
            TypedExpr::Deref(binding, source) => {
                let src = origin.as_ref()
                    .map(|origin| *self.map.get(&origin.ptr()).unwrap())
                    .unwrap_or_else(|| self.graph.add_node(SourcePrinter(source.clone())));
                let dst = self.map.get(&binding.ptr()).unwrap();
                self.graph.add_edge(src, *dst, 0);
            },
            TypedExpr::Lambda(lambda, _) => {
                lambda.parameters.iter().for_each(|parameter| self.register(parameter));
                self.expr_reachability(&lambda.body, origin);
            },
            TypedExpr::Application { function, arguments, .. } => {
                self.expr_reachability(function, origin);
                arguments.iter().for_each(|argument| self.expr_reachability(argument, origin));
            },
            TypedExpr::AddInt(left, right, _) |
            TypedExpr::SubInt(left, right, _) |
            TypedExpr::MulInt(left, right, _) |
            TypedExpr::DivInt(left, right, _) |
            TypedExpr::AddStr(left, right, _) |
            TypedExpr::Assign(left, right, _) => {
                self.expr_reachability(left, origin);
                self.expr_reachability(right, origin);
            },
            TypedExpr::Conditional { condition, positive, negative, .. } => {
                self.expr_reachability(condition, origin);
                self.expr_reachability(positive, origin);
                negative.as_ref().map(|clause| self.expr_reachability(clause, origin));
            },
            TypedExpr::Block(statements, _) => self.compute_reachability(statements.as_slice(), origin)
        }
    }

    fn register(&mut self, binding: &BindingRef) {
        let node = self.graph.add_node(SourcePrinter(binding.borrow().source.clone()));
        self.map.insert(binding.ptr(), node);
        if let BindingValue::Var(value) = &binding.borrow().value {
             self.expr_reachability(value, &Some(binding.clone()));
        }
    }
}

use std::fmt;
struct SourcePrinter(Source);

impl fmt::Debug for SourcePrinter {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let source = &self.0;
        write!(f, "{} [{:?}]", source.text(), source.file.position(source.range.start).unwrap())
    }
}

impl fmt::Display for SourcePrinter {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let source = &self.0;
        write!(f, "{} [{:?}]", source.text(), source.file.position(source.range.start).unwrap())
    }
}

#[allow(unused)]
fn display_graph(graph: &ReachabilityGraph, filename: &str) {
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
