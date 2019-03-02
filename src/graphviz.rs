use std::io;
use std::mem::replace;

use askama_escape::escape;
use itertools::Itertools;
use petgraph::graph::NodeIndex;

use crate::ir::BasicBlockGraph;
use crate::ir::ControlFlowGraph;
use crate::ir::Phi;
use crate::ir::Statement;
use crate::ir::Value;
use crate::ir::value_storage::ValueStorage;

pub(crate) struct Graphviz<'a> {
    id: usize,
    name: String,
    graph: &'a BasicBlockGraph,
    values: &'a ValueStorage,
    is_subgraph: bool,
}

macro_rules! colorize {
    ($text: expr, $color: literal) => (format_args!("<font color=\"{}\">{}</font>", $color, $text));
    (comment $text: expr) => (colorize!($text, "grey"));
    (constant $text: expr) => (colorize!($text, "darkorange"));
    (keyword $text: expr) => (colorize!($text, "navy"));
    (undefined $text: expr) => (colorize!($text, "red"));
}

impl<'a> Graphviz<'a> {
    pub(crate) fn write(sink: &mut impl io::Write, cfg: &'a ControlFlowGraph, name: String) -> Result<(), io::Error> {
        let mut state = Self {
            id: 0,
            name,
            graph: &cfg.graph,
            values: &cfg.values,
            is_subgraph: false,
        };
        state.fmt_impl(sink)
    }

    fn fmt_impl(&mut self, sink: &mut impl io::Write) -> Result<(), io::Error> {
        if !self.is_subgraph {
            writeln!(sink, "digraph {{")?;
            writeln!(sink, "compound=true")?;
            writeln!(sink, "node [ fontname=\"Fira Code\" ]")?;
            writeln!(sink)?;
        }
        writeln!(sink, "subgraph cluster{} {{", self.id)?;
        writeln!(sink, "label=\"{}\";", escape(&self.name))?;
        for node in self.graph.node_indices() {
            self.fmt_node(sink, node)?;
        }
        for edge in self.graph.edge_indices().map(|edge| self.graph.edge_endpoints(edge).unwrap()) {
            self.fmt_edge(sink, edge)?;
        }
        writeln!(sink, "}}")?;

        let is_subgraph = replace(&mut self.is_subgraph, true);
        let functions = self.values
            .iter()
            .filter_map(|value| match value {
                Value::Function(id, cfg) => Some((id, cfg)),
                _ => None,
            });
        for (id, cfg) in functions {
            writeln!(sink)?;
            self.id += 1;
            self.name = id.to_string();
            self.graph = &cfg.graph;
            self.values = &cfg.values;
            self.fmt_impl(sink)?;
        }

        if !is_subgraph {
            writeln!(sink, "}}")?;
        }
        Ok(())
    }

    fn fmt_node(&self, sink: &mut impl io::Write, block: NodeIndex) -> Result<(), io::Error> {
        write!(sink, "\"{}_{}\" [ shape=box xlabel=\"{}_{}\" label=<", self.name, block.index(), self.name, block.index())?;
        self.fmt_block(sink, &**self.graph[block])?;
        writeln!(sink, ">];")
    }

    fn fmt_edge(&self, sink: &mut impl io::Write, (source, target): (NodeIndex, NodeIndex)) -> Result<(), io::Error> {
        writeln!(sink, "\"{}_{}\" -> \"{}_{}\";", self.name, source.index(), self.name, target.index())
    }

    fn fmt_block(&self, sink: &mut impl io::Write, block: &[Statement]) -> Result<(), io::Error> {
        writeln!(sink, "<table border=\"0\" cellspacing=\"0\" cellpadding=\"0\"><tr><td>")?;
        let block = block
            .iter()
            .fold(Vec::new(), |mut block, statement| {
                if let (Some(Statement::Comment(s1)), Statement::Comment(s2)) = (block.last_mut(), statement) {
                    s1.push('\n');
                    s1.push_str(s2);
                } else {
                    block.push(statement.clone());
                }
                block
            });
        for statement in block {
            self.fmt_statement(sink, &statement)?;
            writeln!(sink, "<br align=\"left\"/>")?;
        }
        writeln!(sink, "</td></tr></table>")?;
        Ok(())
    }

    fn fmt_statement(&self, sink: &mut impl io::Write, statement: &Statement) -> Result<(), io::Error> {
        match statement {
            Statement::Comment(comment) => {
                let comment = comment
                    .split(|c| c == '\n')
                    .map(escape)
                    .map(|e| e.to_string())
                    .filter(|s| !s.is_empty())
                    .format("<br align=\"left\"/>\n// ");
                write!(sink, "{}", colorize!(comment &format!("// {}", comment)))
            }

            Statement::Definition { ident, value_index } => {
                write!(sink, "{} ← ", ident)?;
                self.fmt_value(sink, &self.values[*value_index])
            }

            Statement::CondJump(var, then_block, else_block) => {
                write!(sink, "{} {}, {}, {}", colorize!(keyword "condjump"),
                       var, then_block.index(), else_block.index())
            }

            Statement::Return(ident) => write!(sink, "{} {}", colorize!(keyword "return"), ident),
        }
    }

    fn fmt_value(&self, sink: &mut impl io::Write, value: &Value) -> Result<(), io::Error> {
        match value {
            Value::Unit => write!(sink, "{}", colorize!(constant "()")),
            Value::Int(n) => write!(sink, "{}", colorize!(constant n.to_string())),
            Value::String(s) => write!(sink, "{}", colorize!(constant escape(&format!("\"{}\"", s)))),
            Value::Function(id, _) => write!(sink, "{}", colorize!(keyword escape(&id.to_string()))),
            Value::AddInt(left, right) => write!(sink, "{} + {}", left, right),
            Value::SubInt(left, right) => write!(sink, "{} - {}", left, right),
            Value::MulInt(left, right) => write!(sink, "{} * {}", left, right),
            Value::DivInt(left, right) => write!(sink, "{} / {}", left, right),
            Value::AddString(left, right) => write!(sink, "{} ++ {}", left, right),
            Value::Phi(Phi(operands)) => write!(sink, "{}({})", colorize!(keyword "ϕ"), operands.iter().format(", ")),
            Value::Call(function, arguments) => write!(sink, "{} {}({})", colorize!(keyword "call"), function, arguments.iter().format(", ")),
            Value::Arg(index) => write!(sink, "{}[{}]", colorize!(keyword "arg"), index),
        }
    }
}
