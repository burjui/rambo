use std::error::Error;
use std::io::Write;

use itertools::Itertools;
use termcolor::Color;
use termcolor::ColorSpec;
use termcolor::StandardStream;
use termcolor::WriteColor;

use crate::codegen::Codegen;
use crate::codegen::SSAStatement;
use crate::eval::Evaluator;
use crate::eval::Evalue;
use crate::lexer::Lexer;
use crate::parser::Block as ASTBlock;
use crate::parser::Parser;
use crate::redundant_bindings::report_redundant_bindings;
use crate::redundant_bindings::Warnings;
use crate::semantics::ExprRef;
use crate::semantics::SemanticsChecker;
use crate::source::SourceFile;
use crate::source::SourceFileRef;

#[derive(Clone)]
crate struct PipelineOptions {
    crate warnings: bool,
    crate dump_intermediate: bool,
    crate dump_cfg: bool,
    crate max_pass_name: String
}

crate struct Pipeline<'a, T> {
    input: Option<T>,
    stdout: &'a mut StandardStream,
    options: &'a PipelineOptions
}

impl<'a, T> Pipeline<'a, T> {
    crate fn new(input: T, stdout: &'a mut StandardStream, options: &'a PipelineOptions) -> Self {
        Self { input: Some(input), stdout, options }
    }

    crate fn map<U, Pass: CompilerPass<T, U>>(self, _: Pass) -> Result<Pipeline<'a, U>, Box<dyn Error>> {
        let Pipeline { input, stdout, options } = self;
        input.map(|input| {
            stdout.write_title("::", Pass::TITLE, Color::White)?;
            stdout.set_color(ColorSpec::new()
                .set_fg(Some(Color::Black))
                .set_intense(true))?;
            let (elapsed, result) = elapsed::measure_time(|| Pass::apply(input, stdout, options));
            stdout.set_color(ColorSpec::new()
                .set_fg(Some(Color::White)))?;
            write!(stdout, "(")?;
            stdout.set_color(ColorSpec::new()
                .set_fg(Some(Color::Black))
                .set_intense(true))?;
            write!(stdout, "{}", elapsed)?;
            stdout.set_color(ColorSpec::new()
                .set_fg(Some(Color::White)))?;
            writeln!(stdout, ")")?;
            result
        })
        .transpose()
        .map(move |output| {
            let input = output.filter(|_| options.max_pass_name != Pass::NAME);
            Pipeline { input, stdout, options }
        })
    }
}

macro_rules! compiler_passes {
    ($($struct_name: ident),+) => {
        $(crate struct $struct_name;)*
        crate const COMPILER_PASS_NAMES: &[&'static str] = &[
            $($struct_name::NAME,)*
        ];
    }
}

compiler_passes!(Load, Parse, VerifySemantics, ReportRedundantBindings, SSA, Evaluate);

crate trait CompilerPass<Input, Output> {
    const NAME: &'static str;
    const TITLE: &'static str;
    fn apply(input: Input, stdout: &mut StandardStream, options: &PipelineOptions) -> Result<Output, Box<dyn Error>>;
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
    const NAME: &'static str = "load";
    const TITLE: &'static str = "Loading";

    fn apply(path: String, stdout: &mut StandardStream, _: &PipelineOptions) -> Result<SourceFileRef, Box<dyn Error>> {
        let source_file = SourceFile::load(&path)?;
        let line_count = source_file.lines().len();
        writeln!(stdout, "{}, {} lines", Self::file_size_pretty(source_file.size()), line_count)?;
        Ok(source_file)
    }
}

impl CompilerPass<SourceFileRef, ASTBlock> for Parse {
    const NAME: &'static str = "parse";
    const TITLE: &'static str = "Parsing";

    fn apply(source_file: SourceFileRef, stdout: &mut StandardStream, options: &PipelineOptions) -> Result<ASTBlock, Box<dyn Error>> {
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
    const NAME: &'static str = "sem";
    const TITLE: &'static str = "Verifying semantics";

    fn apply(ast: ASTBlock, stdout: &mut StandardStream, options: &PipelineOptions) -> Result<ExprRef, Box<dyn Error>> {
        let checker = SemanticsChecker::new();
        let hir = checker.check_module(&ast)?;
        if options.dump_intermediate {
            writeln!(stdout, "{:?}", hir)?;
        }
        Ok(hir)
    }
}

impl CompilerPass<ExprRef, ExprRef> for ReportRedundantBindings {
    const NAME: &'static str = "rb";
    const TITLE: &'static str = "Detecting redundant bindings";

    fn apply(hir: ExprRef, _stdout: &mut StandardStream, options: &PipelineOptions) -> Result<ExprRef, Box<dyn Error>> {
        // TODO pass stdout to report_redundant_bindings()
        report_redundant_bindings(&hir, Warnings(options.warnings));
        Ok(hir)
    }
}

impl CompilerPass<ExprRef, (ExprRef, Vec<SSAStatement>)> for SSA {
    const NAME: &'static str = "ssa";
    const TITLE: &'static str = "Generating SSA";

    fn apply(hir: ExprRef, stdout: &mut StandardStream, options: &PipelineOptions) -> Result<(ExprRef, Vec<SSAStatement>), Box<dyn Error>> {
        let ssa = Codegen::new().build(&hir);
        if options.dump_intermediate {
            writeln!(stdout, "{:?}", ssa.iter().format("\n"))?;
        }
        Ok((hir, ssa))
    }
}

impl CompilerPass<(ExprRef, Vec<SSAStatement>), Evalue> for Evaluate {
    const NAME: &'static str = "eval";
    const TITLE: &'static str = "Evaluating";

    fn apply(ir: (ExprRef, Vec<SSAStatement>), stdout: &mut StandardStream, _: &PipelineOptions) -> Result<Evalue, Box<dyn Error>> {
        let mut evaluator = Evaluator::new();
        let value = evaluator.eval(&ir.0)?;
        writeln!(stdout, "{:?}", value)?;
        Ok(value)
    }
}

crate trait StandardStreamUtils {
    fn write_title(&mut self, prefix: &str, title: &str, color: Color) -> std::io::Result<()>;
}

impl StandardStreamUtils for StandardStream {
    fn write_title(&mut self, prefix: &str, title: &str, color: Color) -> std::io::Result<()> {
        self.set_color(ColorSpec::new()
            .set_fg(Some(Color::Blue))
            .set_bold(true))?;
        write!(self, "{} ", prefix)?;
        self.set_color(ColorSpec::new()
            .set_fg(Some(color))
            .set_bold(true))?;
        writeln!(self, "{}", title)
    }
}
