use core::fmt::Write;
use std::{
    fmt,
    fs::File,
    io,
    io::{BufWriter, Write as _},
    iter::{once, FlatMap, Once},
    mem::replace,
    path::Path,
    str::Chars,
};

use itertools::Itertools;

use crate::{
    ir::{
        value_storage::{ValueId, ValueStorage},
        ControlFlowGraph, IRModule, Phi, Statement, Value,
    },
    stable_graph::NodeIndex,
};

pub(crate) struct IrGraphvizFile {
    output: BufWriter<File>,
    cluster_idgen: ClusterIdGenerator,
}

impl IrGraphvizFile {
    pub(crate) fn create(path: impl AsRef<Path>) -> Result<Self, io::Error> {
        Ok(Self {
            output: BufWriter::new(File::create(path)?),
            cluster_idgen: ClusterIdGenerator::new(),
        })
    }

    pub(crate) fn write(mut self, module: &IRModule) -> io::Result<()> {
        writeln!(self.output, "digraph {{")?;
        writeln!(self.output, "node [ ")?;
        self.writeln_html_attribute("fontname", NODE_FONT)?;
        self.writeln_html_attribute("fontsize", NODE_FONT_SIZE)?;
        writeln!(self.output, "]\n")?;
        self.write_module(module, &module.name)?;
        writeln!(self.output, "}}")
    }

    fn write_module(&mut self, module: &IRModule, label: &str) -> io::Result<()> {
        let cluster_id = self.cluster_idgen.next();
        writeln!(self.output, "subgraph {} {{", cluster_id)?;
        self.writeln_cluster_label(label)?;
        for block in module.cfg.node_indices() {
            let block_id = BlockId::new(cluster_id, block);
            self.write_block(block, block_id, &module.cfg, &module.values)?;
        }
        let edges = module
            .cfg
            .edge_indices()
            .map(|edge| &module.cfg[edge])
            .collect_vec();
        for edge in edges {
            let source_id = BlockId::new(cluster_id, edge.source);
            let target_id = BlockId::new(cluster_id, edge.target);
            self.write_edge(source_id, target_id)?;
        }
        writeln!(self.output, "}}")?;

        let functions = module.values.iter().filter_map(|(value, _)| match value {
            Value::Function(fn_id, name) => Some((name.as_ref(), &module.functions[fn_id])),
            _ => None,
        });
        for (name, module) in functions {
            writeln!(self.output)?;
            self.write_module(module, name)?;
        }
        Ok(())
    }

    fn writeln_cluster_label(&mut self, label: &str) -> io::Result<()> {
        self.write_start_html_attribute("label")?;
        let font_tag_end = self.write_start_html_tag(
            "font",
            &[
                ("color", CLUSTER_TITLE_COLOR),
                ("point-size", CLUSTER_TITLE_FONT_SIZE),
            ],
        )?;
        let bold_tag_end = self.write_start_html_tag("b", &[])?;
        write!(self.output, "{}", escape_html_text(label))?;
        self.write_end_html_tag(bold_tag_end)?;
        self.write_end_html_tag(font_tag_end)?;
        self.write_end_html_attribute()?;
        writeln!(self.output)
    }

    fn write_block(
        &mut self,
        block: NodeIndex,
        block_id: BlockId,
        cfg: &ControlFlowGraph,
        values: &ValueStorage,
    ) -> io::Result<()> {
        writeln!(self.output, "{} [", block_id)?;
        self.writeln_html_attribute("shape", "box")?;
        self.writeln_node_xlabel_attribute(&block.to_string())?;
        self.write_start_html_attribute("label")?;
        self.write_basic_block(cfg[block].iter(), values)?;
        self.write_end_html_attribute()?;
        writeln!(self.output, "\n]")
    }

    fn write_basic_block<'a>(
        &mut self,
        statements: impl IntoIterator<Item = &'a Statement>,
        values: &ValueStorage,
    ) -> io::Result<()> {
        let table_end_tag = self.write_start_html_tag(
            "table",
            &[("border", "0"), ("cellspacing", "0"), ("cellpadding", "0")],
        )?;
        let row_end_tag = self.write_start_html_tag("tr", &[])?;
        let cell_end_tag = self.write_start_html_tag("td", &[])?;
        let statements = statements
            .into_iter()
            .fold(Vec::new(), |mut statements, statement| {
                if let (Some(Statement::Comment(s1)), Statement::Comment(s2)) =
                    (statements.last_mut(), statement)
                {
                    s1.push('\n');
                    s1.push_str(s2);
                } else {
                    statements.push(statement.clone());
                }
                statements
            });
        for statement in statements {
            self.write_statement(&statement, values)?;
            self.writeln_line_break_align_left()?;
        }
        self.write_end_html_tag(cell_end_tag)?;
        self.write_end_html_tag(row_end_tag)?;
        self.write_end_html_tag(table_end_tag)
    }

    fn write_statement(&mut self, statement: &Statement, values: &ValueStorage) -> io::Result<()> {
        match statement {
            Statement::Comment(comment) => {
                let font_color_end = self.write_font_color_start(COMMENT_COLOR)?;
                let mut lines = comment
                    .split(|c| c == '\n')
                    .filter(|s| !s.is_empty())
                    .peekable();
                while let Some(line) = lines.next() {
                    write!(self.output, "// {}", escape_html_text(line))?;
                    if lines.peek().is_some() {
                        self.writeln_line_break_align_left()?;
                    }
                }
                self.write_end_html_tag(font_color_end)
            }

            Statement::Definition(value_id) => {
                self.write_value_id(*value_id)?;
                write!(self.output, " ← ")?;
                self.write_value(&values[*value_id])
            }

            Statement::CondJump(condition, then_block, else_block) => {
                self.write_with_font_color(KEYWORD_COLOR, "condjump")?;
                write!(self.output, " ")?;
                self.write_value_id(*condition)?;
                write!(self.output, ", {}, {}", then_block, else_block)
            }

            Statement::Return(value_id) => {
                self.write_with_font_color(KEYWORD_COLOR, "return")?;
                write!(self.output, " ")?;
                self.write_value_id(*value_id)
            }
        }
    }

    fn write_value(&mut self, value: &Value) -> io::Result<()> {
        match value {
            Value::Unit => self.write_with_font_color(CONSTANT_COLOR, "()"),
            Value::Int(value) => self.write_with_font_color(CONSTANT_COLOR, &value.to_string()),
            Value::String(s) => self.write_with_font_color(CONSTANT_COLOR, &format!("\"{}\"", s)),
            Value::Function(_, name) => {
                let font_color_end = self.write_font_color_start(LAMBDA_COLOR)?;
                write!(self.output, "{}", escape_html_text(name))?;
                self.write_end_html_tag(font_color_end)
            }
            Value::AddInt(left, right) => self.write_binary_operation("+", *left, *right),
            Value::SubInt(left, right) => self.write_binary_operation("-", *left, *right),
            Value::MulInt(left, right) => self.write_binary_operation("*", *left, *right),
            Value::DivInt(left, right) => self.write_binary_operation("/", *left, *right),
            Value::AddString(left, right) => self.write_binary_operation("++", *left, *right),
            Value::Phi(Phi(operands)) => {
                self.write_with_font_color(KEYWORD_COLOR, "ϕ")?;
                self.write_arguments(operands)
            }
            Value::Call(function, arguments) => {
                self.write_with_font_color(KEYWORD_COLOR, "call")?;
                write!(self.output, " ")?;
                self.write_value_id(*function)?;
                self.write_arguments(arguments)
            }
            Value::Arg(index) => {
                self.write_with_font_color(KEYWORD_COLOR, "arg")?;
                write!(self.output, "[{}]", index)
            }
        }
    }

    fn write_binary_operation(
        &mut self,
        operator: &'static str,
        left: ValueId,
        right: ValueId,
    ) -> io::Result<()> {
        self.write_value_id(left)?;
        write!(self.output, " ")?;
        self.write_with_font_color(OPERATOR_COLOR, operator)?;
        write!(self.output, " ")?;
        self.write_value_id(right)
    }

    fn write_arguments(&mut self, operands: &[ValueId]) -> io::Result<()> {
        write!(self.output, "(")?;
        let mut operands = operands.iter().peekable();
        while let Some(operand) = operands.next() {
            self.write_value_id(*operand)?;
            if operands.peek().is_some() {
                write!(self.output, ", ")?;
            }
        }
        write!(self.output, ")")
    }

    fn write_value_id(&mut self, value_id: ValueId) -> io::Result<()> {
        write!(self.output, "v")?;
        let sub_tag_end = self.write_start_html_tag("sub", &[])?;
        write!(self.output, "{}", value_id.0)?;
        self.write_end_html_tag(sub_tag_end)
    }

    fn write_edge(&mut self, source_id: BlockId, target_id: BlockId) -> io::Result<()> {
        writeln!(self.output, "{} -> {}", source_id, target_id)
    }

    fn write_start_html_attribute(&mut self, name: &'static str) -> io::Result<()> {
        write!(self.output, "{}=", name)?;
        write!(self.output, "<")
    }

    fn write_end_html_attribute(&mut self) -> io::Result<()> {
        write!(self.output, ">")
    }

    fn write_start_html_tag(
        &mut self,
        name: &'static str,
        attributes: &[(&'static str, &str)],
    ) -> io::Result<EndTag> {
        write!(self.output, "<{}", name)?;
        self.write_html_attributes(attributes)?;
        write!(self.output, ">")?;
        Ok(EndTag { name })
    }

    fn write_end_html_tag(&mut self, tag: EndTag) -> io::Result<()> {
        write!(self.output, "</{}>", tag.name)
    }

    fn write_standalone_html_tag(
        &mut self,
        name: &'static str,
        attributes: &[(&'static str, &str)],
    ) -> io::Result<()> {
        write!(self.output, "<{}", name)?;
        self.write_html_attributes(attributes)?;
        write!(self.output, "/>")?;
        Ok(())
    }

    fn write_html_attributes(&mut self, attributes: &[(&'static str, &str)]) -> io::Result<()> {
        for (name, value) in attributes {
            write!(self.output, " ")?;
            self.write_html_attribute(name, value)?;
        }
        Ok(())
    }

    fn write_html_attribute(&mut self, name: &'static str, value: &str) -> io::Result<()> {
        write!(self.output, "{}=", name)?;
        self.write_html_attribute_value(value)
    }

    fn writeln_html_attribute(&mut self, name: &'static str, value: &str) -> io::Result<()> {
        self.write_html_attribute(name, value)?;
        writeln!(self.output)
    }

    fn write_html_attribute_value(&mut self, value: &str) -> io::Result<()> {
        write!(self.output, "\"{}\"", escape_attribute_value(value))
    }

    fn write_with_font_color(&mut self, color: &'static str, content: &str) -> io::Result<()> {
        let font_color_end = self.write_font_color_start(color)?;
        write!(self.output, "{}", escape_html_text(content))?;
        self.write_end_html_tag(font_color_end)
    }

    fn write_font_color_start(&mut self, color: &'static str) -> io::Result<EndTag> {
        self.write_start_html_tag("font", &[("color", color)])
    }

    fn writeln_node_xlabel_attribute(&mut self, text: &str) -> io::Result<()> {
        self.write_start_html_attribute("xlabel")?;
        let font_tag_end = self.write_start_html_tag(
            "font",
            &[
                ("color", BLOCK_TITLE_COLOR),
                ("point-size", BLOCK_TITLE_FONT_SIZE),
            ],
        )?;
        let bold_tag_end = self.write_start_html_tag("b", &[])?;
        write!(self.output, "{}", escape_html_text(text))?;
        self.write_end_html_tag(bold_tag_end)?;
        self.write_end_html_tag(font_tag_end)?;
        self.write_end_html_attribute()?;
        writeln!(self.output)
    }

    fn writeln_line_break_align_left(&mut self) -> io::Result<()> {
        self.write_standalone_html_tag("br", &[("align", "left")])?;
        writeln!(self.output)
    }
}

const CLUSTER_TITLE_COLOR: &str = "black";
const CLUSTER_TITLE_FONT_SIZE: &str = "14";

const BLOCK_TITLE_COLOR: &str = "cornsilk4";
const BLOCK_TITLE_FONT_SIZE: &str = "10";

const NODE_FONT: &str = "Fira Code";
const NODE_FONT_SIZE: &str = "10";

const COMMENT_COLOR: &str = "cornsilk3";
const KEYWORD_COLOR: &str = "dodgerblue3";
const CONSTANT_COLOR: &str = "darkgoldenrod3";
const LAMBDA_COLOR: &str = "olivedrab4";
const OPERATOR_COLOR: &str = "bisque4";

struct ClusterIdGenerator(usize);

impl ClusterIdGenerator {
    const fn new() -> Self {
        Self(0)
    }

    fn next(&mut self) -> ClusterId {
        let next_id = self.0 + 1;
        ClusterId(replace(&mut self.0, next_id))
    }
}

#[must_use]
#[derive(Copy, Clone)]
struct ClusterId(usize);

impl fmt::Display for ClusterId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        write!(f, "cluster{}", self.0)
    }
}

#[must_use]
#[derive(Copy, Clone)]
struct BlockId {
    cluster_id: ClusterId,
    block: NodeIndex,
}

impl BlockId {
    const fn new(cluster_id: ClusterId, block: NodeIndex) -> Self {
        Self { cluster_id, block }
    }
}

impl fmt::Display for BlockId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}_{}", self.cluster_id, self.block)
    }
}

#[must_use]
struct EndTag {
    name: &'static str,
}

#[derive(Clone)]
struct EscapedString<'a> {
    chars: FlatMap<Chars<'a>, EscapedChar, fn(char) -> EscapedChar>,
}

impl<'a> EscapedString<'a> {
    fn new(s: &'a str, escape: fn(char) -> EscapedChar) -> Self {
        Self {
            chars: s.chars().flat_map(escape),
        }
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
            Self::Original(iter) => iter.next(),
            Self::Escaped(iter) => iter.next(),
        }
    }
}

fn escape_attribute_value(s: &str) -> EscapedString<'_> {
    EscapedString::new(s, escape_attribute_char)
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

fn escape_attribute_char(c: char) -> EscapedChar {
    match c {
        '\"' => EscapedChar::Escaped("&quot;".chars()),
        '\'' => EscapedChar::Escaped("&#39;".chars()),
        _ => EscapedChar::Original(once(c)),
    }
}
