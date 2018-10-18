use crate::semantics::BindingRef;
use crate::semantics::BindingValue;
use crate::semantics::Block;
use crate::semantics::ExprRef;
use crate::semantics::TypedExpr;
use crate::semantics::TypedStatement;
use crate::source::Source;
use crate::typed_visitor::TypedVisitor;
use petgraph::Direction;
use petgraph::Graph;
use petgraph::graph::NodeIndex;
use petgraph::visit::EdgeRef;
use std::collections::hash_map::HashMap;
use std::collections::hash_set::HashSet;
use std::fmt;

#[derive(PartialEq)]
crate enum Warnings { On, Off }

crate struct RedundantBindings {
    warnings: Warnings,
    graph: ReachabilityGraph,
    map: BindingToNodeMap
}

impl RedundantBindings {
    crate fn remove(code: &Block, warnings: Warnings) -> Block {
        let (graph, map) = Reachability::compute(code);
        let redundant = RedundantBindings { warnings, graph, map };
        redundant.remove_unreachable(code)
    }

    fn remove_unreachable(mut self, code: &Block) -> Block {
        let mut disconnected_nodes = HashSet::<NodeIndex>::new();
        let binding_nodes = self.map.values().cloned().collect::<Vec<_>>();
        binding_nodes.into_iter().for_each(|node| Self::process_node(&mut self.graph, node, &mut disconnected_nodes));
        let mut redundant_sources = disconnected_nodes.iter()
            .filter_map(|node| self.graph.node_weight(*node))
            .map(|SourcePrinter(source)| source)
            .collect::<Vec<_>>();
        redundant_sources.sort_by_key(|source| source.range.start);
        if self.warnings == Warnings::On {
            for source in redundant_sources {
                warning_at!(source, "redundant definition: {}", source.text());
            }
        }
        let redundant_bindings = self.map.into_iter()
            .filter_map(|(binding, node)| if disconnected_nodes.contains(&node) { Some(binding) } else { None })
            .collect::<HashSet<_>>();
        RedundantBindingRemover::new(redundant_bindings).visit_block(code)
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
type BindingToNodeMap = HashMap<BindingRef, NodeIndex>;

struct Reachability {
    graph: ReachabilityGraph,
    map: BindingToNodeMap
}

struct RedundantBindingRemover {
    redundant_bindings: HashSet<BindingRef>
}

impl RedundantBindingRemover {
    fn new(redundant_bindings: HashSet<BindingRef>) -> Self {
        Self { redundant_bindings }
    }
}

impl TypedVisitor for RedundantBindingRemover {
    fn post_statements(&mut self, statements: Vec<TypedStatement>) -> Vec<TypedStatement> {
        // Remove consecutive `()' left from redundant bindings
        let mut previous_expr_is_unit = false;
        statements.into_iter()
            .filter_map(|statement| match &statement {
                TypedStatement::Binding(_) => Some(statement),
                TypedStatement::Expr(expr) => {
                    previous_expr_is_unit = match expr as &TypedExpr {
                        TypedExpr::Unit(_) => {
                            if previous_expr_is_unit {
                                return None;
                            }
                            true
                        },
                        _ => false
                    };
                    Some(statement)
                }
            })
            .collect()
    }

    fn visit_statement(&mut self, statement: &TypedStatement) -> Option<TypedStatement> {
        match statement {
            TypedStatement::Binding(binding) =>
                if self.redundant_bindings.contains(binding) {
                    let unit = TypedExpr::Unit(binding.borrow().source.clone());
                    Some(TypedStatement::Expr(ExprRef::from(unit)))
                } else {
                    Some(TypedStatement::Binding(self.visit_binding(binding)))
                },
            TypedStatement::Expr(expr) => Some(TypedStatement::Expr(self.visit(expr)))
        }
    }
}

impl Reachability {
    fn compute(code: &Block) -> (ReachabilityGraph, BindingToNodeMap) {
        let mut computer = Reachability {
            graph: ReachabilityGraph::new(),
            map: HashMap::new()
        };
        computer.compute_reachability(&code.statements, &None);
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
                    .map(|origin| self.map[origin])
                    .unwrap_or_else(|| self.graph.add_node(SourcePrinter(source.clone())));
                let dst = self.map[binding];
                self.graph.add_edge(src, dst, 0);
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
            TypedExpr::AddStr(left, right, _) => {
                self.expr_reachability(left, origin);
                self.expr_reachability(right, origin);
            },
            TypedExpr::Assign(_, value, _) => {
                self.expr_reachability(value, origin);
            }
            TypedExpr::Conditional { condition, positive, negative, .. } => {
                self.expr_reachability(condition, origin);
                self.expr_reachability(positive, origin);
                if let Some(clause) = negative.as_ref() {
                    self.expr_reachability(clause, origin)
                }
            },
            TypedExpr::Block(Block { statements, .. }) => self.compute_reachability(statements.as_slice(), origin)
        }
    }

    fn register(&mut self, binding: &BindingRef) {
        let node = self.graph.add_node(SourcePrinter(binding.borrow().source.clone()));
        self.map.insert(binding.clone(), node);
        if let BindingValue::Var(value) = &binding.borrow().data {
             self.expr_reachability(value, &Some(binding.clone()));
        }
    }
}

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
