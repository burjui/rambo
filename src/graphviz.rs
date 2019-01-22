use core::fmt::Write;
use std::error::Error;
use std::fmt;
use std::fmt::Display;
use std::io;

use askama_escape::escape;
use itertools::Itertools;
use petgraph::graph::NodeIndex;

use crate::control_flow::CFGNode;
use crate::control_flow::ControlFlowGraph;
use crate::ssa::SSAOp;
use crate::ssa::SSAStatement;
use crate::utils::WHITESPACE_REGEX;

pub(crate) struct Graphviz {
    include_comments: bool
}

impl Graphviz {
    pub(crate) fn new() -> Self {
        Self { include_comments: false }
    }

    pub(crate) fn include_comments(self, include_comments: bool) -> Self {
        Self { include_comments }
    }

    pub(crate) fn fmt(&self, sink: &mut impl io::Write, graph: &ControlFlowGraph<'_>) -> Result<(), Box<dyn Error>> {
        let nodes = graph.node_indices()
            .map(|node| self.fmt_node(node, graph))
            .collect::<Result<Vec<_>, fmt::Error>>()?;
        let edges = graph.raw_edges().iter()
            .map(|edge| format!("  {:?} -> {:?}", edge.source().index(), edge.target().index()));
        let fontspec = "  node [ fontname = \"Fira Code\"  ]";
        writeln!(sink, "digraph {{\n{}\n{}\n{}\n}}", fontspec, nodes.iter().format("\n"), edges.format("\n"))?;
        Ok(())
    }

    fn fmt_node(&self, node: NodeIndex, graph: &ControlFlowGraph<'_>) -> Result<String, fmt::Error> {
        let (label, shape) = match graph.node_weight(node).unwrap() {
            CFGNode::Entry => ("entry".to_owned(), "ellipse"),
            CFGNode::Exit => ("exit".to_owned(), "ellipse"),
            CFGNode::BasicBlock(block) => (self.fmt_block(block)?, "box")
        };
        Ok(format!("  {} [ shape={} label=<{}> ]", node.index(), shape, label))
    }

    fn fmt_block(&self, block: &[SSAStatement]) -> Result<String, fmt::Error> {
        let code = block.iter().map(|s| fmt_statement(s) + "<br align=\"left\"/>").join("");
        let mut s = format!("<table border=\"0\" cellspacing=\"0\" cellpadding=\"0\"><tr><td>{}</td>", code);
        if self.include_comments {
            let comments = format!("{}", block.iter()
                .map(|statement| &statement.target.comment)
                .map(|comment| WHITESPACE_REGEX.replace_all(comment, " "))
                .map(|comment| escape(&comment).to_string())
                .map(|comment| if comment.is_empty() { "".to_owned() } else { colorize_comment(&format!("// {}<br align=\"left\"/>", comment)) })
                .format(""));
            s.write_fmt(format_args!("<td fixedsize=\"true\" width=\"30\"></td><td>{}</td>", comments))?;
        }
        s.write_str("</tr></table>")?;
        Ok(s)
    }
}

fn fmt_statement(statement: &SSAStatement) -> String {
    if let SSAOp::Label = &statement.op {
        format!("{:?}:", &statement.target.id)
    } else {
        let op = fmt_op(&statement.op);
        format!("    {}", match &statement.op {
            SSAOp::Br(_) | SSAOp::Cbz(_, _) | SSAOp::Return(_) | SSAOp::Copy(_, _, _) => op,
            _ => format!("<font color=\"gray\"><b>{:?} ←</b></font> {}", &statement.target.id, &op)
        })
    }
}

fn fmt_op(op: &SSAOp) -> String {
    match op {
        SSAOp::Unit => "()".to_owned(),
        SSAOp::Int(value) => colorize_constant(&value),
        SSAOp::Str(value) => colorize_constant(&format!("\"{}\"", value)),
        SSAOp::AddInt(left, right) => format!("{:?} + {:?}", left, right),
        SSAOp::SubInt(left, right) => format!("{:?} - {:?}", left, right),
        SSAOp::MulInt(left, right) => format!("{:?} * {:?}", left, right),
        SSAOp::DivInt(left, right) => format!("{:?} / {:?}", left, right),
        SSAOp::Offset(left, right) => format!("&amp;[{:?} + {:?}]", left, right),
        SSAOp::Label => unreachable!(),
        SSAOp::Br(label) => format!("{} {:?}", colorize_keyword("br"), label),
        SSAOp::Cbz(condition, label) => format!("{} {:?}, {:?}", colorize_keyword("cbz"), condition, label),
        SSAOp::Arg(index) => format!("{}[{}] ", colorize_keyword("arg"), colorize_constant(&index)),
        SSAOp::Return(id) => format!("{} {:?}", colorize_keyword("ret"), id),
        SSAOp::Call(id, args) => format!("{} {:?}, {:?}", colorize_keyword("call"), id, args.iter().format(", ")),
        SSAOp::Alloc(size) => format!("{} {:?}", colorize_keyword("alloc"), size),
        SSAOp::Copy(src, dst, size) => format!("{} {:?}, {:?}, {:?}", colorize_keyword("copy"), src, dst, size),
        SSAOp::Length(str) => format!("{} {:?}", colorize_keyword("length"), str),
        SSAOp::Phi(left, right) => format!("{}({:?}, {:?})", colorize_keyword("ϕ"), left, right),
        SSAOp::End(value) => format!("{} {:?}", colorize_keyword("end"), value)
    }
}

fn colorize_comment(s: &str) -> String {
    colorize(s, "grey")
}

fn colorize_constant(n: &impl Display) -> String {
    colorize(&format!("{}", n), "darkorange")
}

fn colorize_keyword(s: &str) -> String {
    colorize(s, "navy")
}

fn colorize(s: &str, color: &str) -> String {
    format!("<font color=\"{}\">{}</font>", color, s)
}
