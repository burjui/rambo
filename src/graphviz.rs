use core::fmt::Write;
use std::fmt;
use std::io;
use std::iter::FlatMap;
use std::iter::once;
use std::iter::Once;
use std::str::Chars;

use itertools::Itertools;
use petgraph::graph::NodeIndex;

use crate::ir::BasicBlockGraph;
use crate::ir::ControlFlowGraph;
use crate::ir::FnId;
use crate::ir::Ident;
use crate::ir::IdentGenerator;
use crate::ir::Phi;
use crate::ir::Statement;
use crate::ir::Value;
use crate::ir::value_storage::ValueStorage;
use crate::ir::VarId;

define_ident!{ ClusterId "cluster" }

pub(crate) fn graphviz_dot_write(output: &mut impl io::Write, cfg: &ControlFlowGraph) -> io::Result<()> {
    writeln!(output, "digraph {{")?;
    writeln!(output, "node [ ")?;
    writeln_attribute(output, "fontname", NODE_FONT)?;
    writeln_attribute(output, "fontsize", NODE_FONT_SIZE)?;
    writeln!(output, "]\n")?;
    let mut cluster_idgen = IdentGenerator::new();
    let label = CfgLabel::Ordinary(&cfg.name);
    write_cfg(output, cfg, label, &mut cluster_idgen)?;
    writeln!(output, "}}")
}

fn write_cfg(
    output: &mut impl io::Write,
    cfg: &ControlFlowGraph,
    label: CfgLabel<'_>,
    cluster_idgen: &mut IdentGenerator<ClusterId>)
    -> io::Result<()>
{
    let cluster_id = cluster_idgen.next_id();
    writeln!(output, "subgraph {} {{", cluster_id)?;
    writeln_cluster_label(output, label)?;
    for block in cfg.graph.node_indices() {
        let block_id = BlockId::new(cluster_id, block);
        write_block(output, block, block_id, &cfg.graph, &cfg.values)?;
    }
    let edges = cfg.graph
        .edge_indices()
        .map(|edge| cfg.graph.edge_endpoints(edge).unwrap())
        .collect_vec();
    for (source, target) in edges {
        let source_id = BlockId::new(cluster_id, source);
        let target_id = BlockId::new(cluster_id, target);
        write_edge(output, source_id, target_id)?;
    }
    writeln!(output, "}}")?;

    let functions = cfg.values
        .iter()
        .filter_map(|value| match value {
            Value::Function(id, cfg) => Some((*id, cfg)),
            _ => None,
        });
    for (id, cfg) in functions {
        writeln!(output)?;
        write_cfg(output, cfg, CfgLabel::Function(id), cluster_idgen)?;
    }
    Ok(())
}

fn writeln_cluster_label(output: &mut impl io::Write, label: CfgLabel<'_>) -> io::Result<()> {
    let label_end = write_html_attribute_start(output, "label")?;
    let font_tag_end = write_html_start_tag(output, "font", &[
        ("color", CLUSTER_TITLE_COLOR),
        ("point-size", CLUSTER_TITLE_FONT_SIZE),
    ])?;
    let bold_tag_end = write_html_start_tag(output, "b", &[])?;
    match label {
        CfgLabel::Ordinary(label) => write!(output, "{}", escape_html_text(label))?,
        CfgLabel::Function(function_id) => write_ident(output, function_id)?,
    }

    bold_tag_end.write(output)?;
    font_tag_end.write(output)?;
    label_end.write(output)?;
    writeln!(output)
}

fn write_block(
    output: &mut impl io::Write,
    block: NodeIndex,
    block_id: BlockId,
    graph: &BasicBlockGraph,
    values: &ValueStorage)
    -> io::Result<()>
{
    writeln!(output, "{} [", block_id)?;
    writeln_attribute(output, "shape", "box")?;
    writeln_node_xlabel_attribute(output, &block.index().to_string())?;
    let label_end = write_html_attribute_start(output, "label")?;
    write_basic_block(output, graph[block].iter(), values)?;
    label_end.write(output)?;
    writeln!(output, "\n]")
}

fn write_edge(output: &mut impl io::Write, source_id: BlockId, target_id: BlockId) -> io::Result<()> {
    writeln!(output, "{} -> {}", source_id, target_id)
}

fn write_basic_block<'a>(
    output: &mut impl io::Write,
    statements: impl IntoIterator<Item = &'a Statement>,
    values: &ValueStorage) -> io::Result<()>
{
    let table_end_tag = write_html_start_tag(output, "table", &[ ("border", "0"), ("cellspacing", "0"), ("cellpadding", "0")])?;
    let row_end_tag = write_html_start_tag(output, "tr", &[])?;
    let cell_end_tag = write_html_start_tag(output, "td", &[])?;
    let statements = statements
        .into_iter()
        .fold(Vec::new(), |mut statements, statement| {
            if let (Some(Statement::Comment(s1)), Statement::Comment(s2)) = (statements.last_mut(), statement) {
                s1.push('\n');
                s1.push_str(s2);
            } else {
                statements.push(statement.clone());
            }
            statements
        });
    for statement in statements {
        write_statement(output, &statement, values)?;
        writeln_line_break_align_left(output)?;
    }
    cell_end_tag.write(output)?;
    row_end_tag.write(output)?;
    table_end_tag.write(output)
}

fn write_statement(output: &mut impl io::Write, statement: &Statement, values: &ValueStorage) -> io::Result<()> {
    match statement {
        Statement::Comment(comment) => {
            let font_color_end = write_font_color_start(output, COMMENT_COLOR)?;
            let mut lines = comment
                .split(|c| c == '\n')
                .filter(|s| !s.is_empty())
                .peekable();
            while let Some(line) = lines.next() {
                write!(output, "// {}", escape_html_text(line))?;
                if lines.peek().is_some() {
                    writeln_line_break_align_left(output)?;
                }
            }
            font_color_end.write(output)
        }

        Statement::Definition { ident, value_index } => {
            write_ident(output, *ident)?;
            write!(output, " ← ")?;
            write_value(output, &values[*value_index])
        }

        Statement::CondJump(condition, then_block, else_block) => {
            write_with_font_color(output, KEYWORD_COLOR, "condjump")?;
            write!(output, " ")?;
            write_ident(output, *condition)?;
            write!(output, ", {}, {}", then_block.index(), else_block.index())
        }

        Statement::Return(ident) => {
            write_with_font_color(output, KEYWORD_COLOR, "return")?;
            write!(output, " ")?;
            write_ident(output, *ident)
        }
    }
}

fn write_ident(output: &mut impl io::Write, ident: impl Ident) -> io::Result<()> {
    write!(output, "{}", ident.prefix())?;
    let sub_tag_end = write_html_start_tag(output, "sub", &[])?;
    write!(output, "{}", ident.index())?;
    sub_tag_end.write(output)
}

fn write_value(output: &mut impl io::Write, value: &Value) -> io::Result<()> {
    match value {
        Value::Unit => write_with_font_color(output, CONSTANT_COLOR, "()"),
        Value::Int(n) => write_with_font_color(output, CONSTANT_COLOR, &n.to_string()),
        Value::String(s) => write_with_font_color(output, CONSTANT_COLOR, &format!("\"{}\"", s)),
        Value::Function(function_id, _) => {
            let font_color_end = write_font_color_start(output, LAMBDA_COLOR)?;
            write_ident(output, *function_id)?;
            font_color_end.write(output)
        },
        Value::AddInt(left, right) => write_binary(output, "+", *left, *right),
        Value::SubInt(left, right) => write_binary(output, "-", *left, *right),
        Value::MulInt(left, right) => write_binary(output, "*", *left, *right),
        Value::DivInt(left, right) => write_binary(output, "/", *left, *right),
        Value::AddString(left, right) => write_binary(output, "++", *left, *right),
        Value::Phi(Phi(operands)) => {
            write_with_font_color(output, KEYWORD_COLOR, "ϕ")?;
            write_arguments(output, operands)
        },
        Value::Call(function, arguments) => {
            write_with_font_color(output, KEYWORD_COLOR, "call")?;
            write!(output, " ")?;
            write_ident(output, *function)?;
            write_arguments(output, arguments)
        },
        Value::Arg(index) => {
            write_with_font_color(output, KEYWORD_COLOR, "arg")?;
            write!(output, "[{}]", index)
        }
    }
}

fn write_binary(
    output: &mut impl io::Write,
    operator: &'static str,
    left: VarId, right: VarId)
    -> io::Result<()>
{
    write_ident(output, left)?;
    write!(output, " ")?;
    write_with_font_color(output, OPERATOR_COLOR, operator)?;
    write!(output, " ")?;
    write_ident(output, right)
}

fn write_arguments(output: &mut impl io::Write, operands: &[VarId]) -> io::Result<()> {
    write!(output, "(")?;
    let mut operands = operands.iter().peekable();
    while let Some(operand) = operands.next() {
        write_ident(output, *operand)?;
        if operands.peek().is_some() {
            write!(output, ", ")?;
        }
    }
    write!(output, ")")
}

fn write_with_font_color(output: &mut impl io::Write, color: &'static str, content: &str) -> io::Result<()> {
    let font_color_end = write_font_color_start(output, color)?;
    write!(output, "{}", escape_html_text(content))?;
    font_color_end.write(output)
}

fn write_font_color_start<Output: io::Write>(output: &mut Output, color: &'static str) -> io::Result<EndTag> {
    write_html_start_tag(output, "font", &[ ("color", color) ])
}

fn writeln_line_break_align_left(output: &mut impl io::Write) -> io::Result<()> {
    write_html_standalone_tag(output, "br", &[ ("align", "left") ])?;
    writeln!(output)
}

fn writeln_node_xlabel_attribute<Output: io::Write>(output: &mut Output, text: &str) -> io::Result<()> {
    let label_end = write_html_attribute_start(output, "xlabel")?;
    let font_tag_end = write_html_start_tag(output, "font", &[
        ("color", BLOCK_TITLE_COLOR),
        ("point-size", BLOCK_TITLE_FONT_SIZE),
    ])?;
    let bold_tag_end = write_html_start_tag(output, "b", &[])?;
    write!(output, "{}", escape_html_text(text))?;
    bold_tag_end.write(output)?;
    font_tag_end.write(output)?;
    label_end.write(output)?;
    writeln!(output)
}

fn write_html_standalone_tag(output: &mut impl io::Write, name: &'static str, attributes: &[(&'static str, &str)]) -> io::Result<()> {
    write!(output, "<{}", name)?;
    write_attributes(output, attributes)?;
    write!(output, "/>")?;
    Ok(())
}

fn write_html_start_tag(
    output: &mut impl io::Write,
    name: &'static str,
    attributes: &[ (&'static str, &str) ])
    -> io::Result<EndTag>
{
    write!(output, "<{}", name)?;
    write_attributes(output, attributes)?;
    write!(output, ">")?;
    Ok(EndTag(name))
}

fn write_attributes(
    output: &mut impl io::Write,
    attributes: &[ (&'static str, &str) ])
    -> io::Result<()>
{
    for (name, value) in attributes {
        write!(output, " ")?;
        write_attribute(output, name, value)?;
    }
    Ok(())
}

fn writeln_attribute(output: &mut impl io::Write, name: &'static str, value: &str) -> io::Result<()> {
    write_attribute(output, name, value)?;
    writeln!(output)
}

fn write_attribute(output: &mut impl io::Write, name: &'static str, value: &str) -> io::Result<()> {
    write!(output, "{}=", name)?;
    write_attribute_value(output, value)
}

fn write_attribute_value(output: &mut impl io::Write, value: &str) -> io::Result<()> {
    write!(output, "\"{}\"", escape_attribute_value(value))
}

fn write_html_attribute_start(
    output: &mut impl io::Write,
    name: &'static str)
    -> io::Result<HtmlAttributeValueEnd>
{
    write!(output, "{}=", name)?;
    write!(output, "<")?;
    Ok(HtmlAttributeValueEnd)
}

fn escape_html_text(s: &str) -> EscapedString<'_> {
    EscapedString::new(s, escape_html_text_char)
}

fn escape_html_text_char(c: char) -> EscapedChar {
    match c {
        '&' => EscapedChar::Escaped("&amp;".chars()),
        '<' => EscapedChar::Escaped("&lt;".chars()),
        '>' => EscapedChar::Escaped("&gt;".chars()),
        _ => EscapedChar::Original(once(c)),
    }
}

fn escape_attribute_value(s: &str) -> EscapedString<'_> {
    EscapedString::new(s, escape_attribute_char)
}

fn escape_attribute_char(c: char) -> EscapedChar {
    match c {
        '\"' => EscapedChar::Escaped("&quot;".chars()),
        '\'' => EscapedChar::Escaped("&#39;".chars()),
        _ => EscapedChar::Original(once(c)),
    }
}

enum CfgLabel<'a> {
    Ordinary(&'a str),
    Function(FnId),
}

#[must_use]
#[derive(Copy, Clone)]
struct BlockId {
    cluster_id: ClusterId,
    block: NodeIndex,
}

impl BlockId {
    fn new(cluster_id: ClusterId, block: NodeIndex) -> Self {
        Self { cluster_id, block }
    }
}

impl fmt::Display for BlockId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}_{}", self.cluster_id, self.block.index())
    }
}

#[must_use]
struct EndTag(&'static str);

impl EndTag {
    fn write(self, output: &mut impl io::Write) -> io::Result<()> {
        write!(output, "</{}>", self.0)
    }
}

#[must_use]
struct HtmlAttributeValueEnd;

impl HtmlAttributeValueEnd {
    fn write(self, output: &mut impl io::Write) -> io::Result<()> {
        write!(output, ">")
    }
}

#[derive(Clone)]
struct EscapedString<'a> {
    chars: FlatMap<Chars<'a>, EscapedChar, fn(char) -> EscapedChar>,
}

impl<'a> EscapedString<'a> {
    fn new(s: &'a str, escape: fn(char) -> EscapedChar) -> Self {
        Self { chars: s.chars().flat_map(escape) }
    }
}

impl Iterator for EscapedString<'_> {
    type Item = char;

    fn next(&mut self) -> Option<Self::Item> {
        self.chars.next()
    }
}

impl<'a> fmt::Display for EscapedString<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for c in self.clone() {
            f.write_char(c)?;
        }
        Ok(())
    }
}

#[derive(Clone)]
enum EscapedChar {
    Original(Once<char>),
    Escaped(Chars<'static>),
}

impl Iterator for EscapedChar {
    type Item = char;

    fn next(&mut self) -> Option<Self::Item> {
        match self {
            EscapedChar::Original(iter) => iter.next(),
            EscapedChar::Escaped(iter) => iter.next(),
        }
    }
}

static CLUSTER_TITLE_COLOR: &str = "black";
static CLUSTER_TITLE_FONT_SIZE: &str = "14";

static BLOCK_TITLE_COLOR: &str = "cornsilk4";
static BLOCK_TITLE_FONT_SIZE: &str = "10";

static NODE_FONT: &str = "Fira Code";
static NODE_FONT_SIZE: &str = "10";

static COMMENT_COLOR: &str = "cornsilk3";
static KEYWORD_COLOR: &str = "dodgerblue3";
static CONSTANT_COLOR: &str = "darkgoldenrod3";
static LAMBDA_COLOR: &str = "olivedrab4";
static OPERATOR_COLOR: &str = "bisque4";
