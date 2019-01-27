use std::io;

use askama_escape::escape;
use itertools::Itertools;
use petgraph::graph::Edge;
use petgraph::graph::NodeIndex;

use crate::ir::BasicBlockGraph;
use crate::ir::Statement;
use crate::ir::Value;
use crate::utils::WHITESPACE_REGEX;

pub(crate) struct Graphviz {
    include_comments: bool
}

macro_rules! colorize {
    ($text: expr, $color: literal) => (format_args!("<font color=\"{}\">{}</font>", $color, $text));
    (comment $text: expr) => (colorize!($text, "grey"));
    (constant $text: expr) => (colorize!($text, "darkorange"));
    (keyword $text: expr) => (colorize!($text, "navy"));
    (undefined $text: expr) => (colorize!($text, "red"));
}

impl Graphviz {
    pub(crate) fn new() -> Self {
        Self { include_comments: false }
    }

    pub(crate) fn include_comments(self, include_comments: bool) -> Self {
        Self { include_comments }
    }

    pub(crate) fn fmt(&self, sink: &mut impl io::Write, graph: &BasicBlockGraph) -> Result<(), io::Error> {
        writeln!(sink, "digraph {{\nnode [ fontname = \"Fira Code\" ]")?;
        for node in graph.node_indices() {
            self.fmt_node(sink, node, graph)?;
        }
        for edge in graph.raw_edges().iter() {
            self.fmt_edge(sink, edge)?;
        }
        writeln!(sink, "}}")
    }

    fn fmt_node(&self, sink: &mut impl io::Write, block: NodeIndex, graph: &BasicBlockGraph) -> Result<(), io::Error> {
        write!(sink, "{} [ shape=box label=<", block.index())?;
        self.fmt_block(sink, &**graph.block(block))?;
        writeln!(sink, ">]\n")
    }

    fn fmt_edge(&self, sink: &mut impl io::Write, edge: &Edge<()>) -> Result<(), io::Error> {
        writeln!(sink, "{} -> {}", edge.source().index(), edge.target().index())
    }

    fn fmt_block(&self, sink: &mut impl io::Write, block: &[Statement]) -> Result<(), io::Error> {
        writeln!(sink, "<table border=\"0\" cellspacing=\"0\" cellpadding=\"0\"><tr><td>")?;
        for statement in block {
            let is_a_comment = match statement {
                Statement::Comment(_) => true,
                _ => false,
            };
            if !is_a_comment || self.include_comments {
                self.fmt_statement(sink, statement)?;
                writeln!(sink, "<br align=\"left\"/>")?;
            }
        }
        writeln!(sink, "</td></tr></table>")?;
        Ok(())
    }

    fn fmt_statement(&self, sink: &mut impl io::Write, statement: &Statement) -> Result<(), io::Error> {
        match statement {
            Statement::Comment(comment) => {
                if self.include_comments {
                    write!(sink, "{}", colorize!(comment &format!("// {}",
                        escape(&WHITESPACE_REGEX.replace_all(comment, " ")).to_string())))?;
                }
            }

            Statement::Definition { ident, value } => {
                write!(sink, "{} ← ", ident)?;
                self.fmt_value(sink, value)?;
            }

            Statement::CondJump(var, then_block, else_block) => {
                write!(sink, "{} {}, {}, {}", colorize!(keyword "condjump"),
                       var, then_block.index(), else_block.index())?;
            }
        }
        Ok(())
    }

    fn fmt_value(&self, sink: &mut impl io::Write, value: &Value) -> Result<(), io::Error> {
        match value {
            Value::Undefined => write!(sink, "&lt;{}&gt;", colorize!(undefined "undefined")),
            Value::Unit => write!(sink, "{}", colorize!(constant "()")),
            Value::Int(n) => write!(sink, "{}", colorize!(constant n.to_string())),
            Value::String(s) => write!(sink, "{}", colorize!(constant escape(&format!("\"{}\"", s)))),
            Value::Function(entry_block) => write!(sink, "{}", colorize!(keyword format!("λ{}", entry_block.index()))),
            Value::AddInt(left, right) => write!(sink, "{} + {}", left, right),
            Value::SubInt(left, right) => write!(sink, "{} - {}", left, right),
            Value::MulInt(left, right) => write!(sink, "{} * {}", left, right),
            Value::DivInt(left, right) => write!(sink, "{} - {}", left, right),
            Value::AddString(left, right) => write!(sink, "{} ++ {}", left, right),
            Value::Phi(operands) => write!(sink, "{}({})", colorize!(keyword "ϕ"), operands.iter().format(", ")),
            Value::Call(function, arguments) => write!(sink, "{} {}({})", colorize!(keyword "call"), function, arguments.iter().format(", ")),
            Value::Arg(index) => write!(sink, "{}[{}]", colorize!(keyword "arg"), index),
        }
    }
}
