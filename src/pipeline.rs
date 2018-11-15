use std::error::Error;
use std::io::Write;

use itertools::Itertools;
use termcolor::Color;
use termcolor::ColorSpec;
use termcolor::StandardStream;
use termcolor::WriteColor;

use crate::eval::Evaluator;
use crate::eval::Evalue;
use crate::lexer::Lexer;
use crate::parser::Block as ASTBlock;
use crate::parser::Parser;
use crate::redundant_bindings::report_redundant_bindings;
use crate::redundant_bindings::Warnings;
use crate::semantics::check_module;
use crate::semantics::ExprRef;
use crate::source::SourceFile;
use crate::source::SourceFileRef;

type PipelineInput<'a, T> = Option<(T, &'a mut StandardStream)>;

#[derive(Clone)]
crate struct PipelineOptions {
    crate warnings: bool,
    crate dump_intermediate: bool,
    crate dump_cfg: bool,
    crate max_pass: PassId
}

crate struct Pipeline<'a, T> {
    input: PipelineInput<'a, T>,
    options: &'a PipelineOptions
}

impl<'a, T> Pipeline<'a, T> {
    crate fn new(input: T, stdout: &'a mut StandardStream, options: &'a PipelineOptions) -> Self {
        Self { input: Some((input, stdout)), options }
    }

    crate fn map<U, Pass: CompilerPass<T, U>>(self, _: Pass) -> Result<Pipeline<'a, U>, Box<dyn Error>> {
        let max_pass = self.options.max_pass;
        let options = self.options;
        self.input
            .filter(|_| max_pass >= Pass::ID)
            .map(|(data, stdout)| Pass::apply(data, stdout, options))
            .transpose()
            .map(|output| Pipeline { input: output, options })
    }
}

crate struct Load;
crate struct Parse;
crate struct VerifySemantics;
crate struct ReportRedundantBindings;
crate struct Evaluate;

#[derive(PartialOrd, PartialEq, Copy, Clone)]
crate enum PassId {
    Load,
    Parse,
    VerifySemantics,
    ReportRedundantBindings,
    Evaluate
}

crate static ALL_PASS_IDS: [PassId; 5] = [
    PassId::Load,
    PassId::Parse,
    PassId::VerifySemantics,
    PassId::ReportRedundantBindings,
    PassId::Evaluate
];

impl PassId {
    crate fn name(&self) -> &str {
        match self {
            PassId::Load => "load",
            PassId::Parse => "parse",
            PassId::VerifySemantics => "sem",
            PassId::ReportRedundantBindings => "rb",
            PassId::Evaluate => "eval"
        }
    }

    fn title(&self) -> &str {
        match self {
            PassId::Load => "Loading",
            PassId::Parse => "Parsing",
            PassId::VerifySemantics => "Verifying semantics",
            PassId::ReportRedundantBindings => "Detecting redundant bindings",
            PassId::Evaluate => "Evaluating"
        }
    }
}

crate trait CompilerPass<Input, Output> {
    const ID: PassId;
    fn apply_impl(input: Input, stdout: &mut StandardStream, options: &PipelineOptions) -> Result<Output, Box<dyn Error>>;

    fn apply<'stdout>(input: Input, stdout: &'stdout mut StandardStream, options: &PipelineOptions) -> Result<(Output, &'stdout mut StandardStream), Box<dyn Error>> {
        stdout.write_title("::", Self::ID.title(), Color::White)?;
        Self::apply_impl(input, stdout, options).map(|output| (output, stdout))
    }
}

impl Load {
    fn file_size_pretty(size: usize) -> String {
        [(usize::pow(2, 30), "G"),
            (usize::pow(2, 20), "M"),
            (usize::pow(2, 10), "K")]
            .iter()
            .find(|(unit, _)| size >= *unit)
            .map(|&(unit, name)| format!("{:.2} {}iB", size as f32 / unit as f32, name))
            .unwrap_or_else(|| format!("{} bytes", size))
    }
}

impl CompilerPass<String, SourceFileRef> for Load {
    const ID: PassId = PassId::Load;

    fn apply_impl(path: String, stdout: &mut StandardStream, _: &PipelineOptions) -> Result<SourceFileRef, Box<dyn Error>> {
        let source_file = SourceFile::load(&path)?;
        let line_count = source_file.lines().len();
        writeln!(stdout, "{}, {} lines", Self::file_size_pretty(source_file.size()), line_count)?;
        Ok(source_file)
    }
}

impl CompilerPass<SourceFileRef, ASTBlock> for Parse {
    const ID: PassId = PassId::Parse;

    fn apply_impl(source_file: SourceFileRef, stdout: &mut StandardStream, options: &PipelineOptions) -> Result<ASTBlock, Box<dyn Error>> {
        let lexer = Lexer::new(source_file);
        let mut parser = Parser::new(lexer);
        let ast = parser.parse()?;
        let stats = { parser.lexer_stats() };
        writeln!(stdout, "{} lexemes", stats.lexeme_count)?;
        if options.dump_intermediate {
            writeln!(stdout, "{:?}", ast.statements.iter().format("\n"))?;
        }
        Ok(ast)
    }
}

impl CompilerPass<ASTBlock, ExprRef> for VerifySemantics {
    const ID: PassId = PassId::VerifySemantics;

    fn apply_impl(ast: ASTBlock, stdout: &mut StandardStream, options: &PipelineOptions) -> Result<ExprRef, Box<dyn Error>> {
        let hir = check_module(&ast)?;
        if options.dump_intermediate {
            writeln!(stdout, "{:?}", hir)?;
        }
        Ok(hir)
    }
}

impl CompilerPass<ExprRef, ExprRef> for ReportRedundantBindings {
    const ID: PassId = PassId::ReportRedundantBindings;

    fn apply_impl(hir: ExprRef, _stdout: &mut StandardStream, options: &PipelineOptions) -> Result<ExprRef, Box<dyn Error>> {
        // TODO pass stdout to report_redundant_bindings()
        report_redundant_bindings(&hir, Warnings(options.warnings));
        Ok(hir)
    }
}

impl CompilerPass<ExprRef, Evalue> for Evaluate {
    const ID: PassId = PassId::Evaluate;

    fn apply_impl(hir: ExprRef, stdout: &mut StandardStream, _: &PipelineOptions) -> Result<Evalue, Box<dyn Error>> {
        let mut evaluator = Evaluator::new();
        let value = evaluator.eval(&hir)?;
        writeln!(stdout, "{:?}", value)?;
        Ok(value)
    }
}

crate trait StandardStreamUtils {
    fn write_title(&mut self, prefix: &str, title: &str, color: Color) -> std::io::Result<()>;
    fn write_colored(&mut self, text: &str, color: Color) -> std::io::Result<()>;
}

impl StandardStreamUtils for StandardStream {
    fn write_title(&mut self, prefix: &str, title: &str, color: Color) -> std::io::Result<()> {
        self.write_colored(prefix, Color::Blue)?;
        write!(self, " ")?;
        self.write_colored(title, color)?;
        writeln!(self)?;
        self.set_color(ColorSpec::new()
            .set_fg(Some(Color::Black))
            .set_intense(true))
    }

    fn write_colored(&mut self, text: &str, color: Color) -> std::io::Result<()> {
        self.set_color(ColorSpec::new()
            .set_fg(Some(color))
            .set_bold(true))?;
        write!(self, "{}", text)
    }
}
