use std::error::Error;
use std::fs::File;
use std::io::Write;

use itertools::Itertools;
use termcolor::Color;
use termcolor::ColorSpec;
use termcolor::StandardStream;
use termcolor::WriteColor;

use crate::eval::Evaluator;
use crate::eval::Evalue;
use crate::graphviz::Graphviz;
use crate::ir;
use crate::ir::eval::EvalContext;
use crate::lexer::Lexer;
use crate::parser::Block as ASTBlock;
use crate::parser::Parser;
use crate::semantics::ExprRef;
use crate::semantics::SemanticsChecker;
use crate::source::SourceFile;
use crate::source::SourceFileRef;
use crate::utils::stdout;

#[derive(Clone)]
pub(crate) struct PipelineOptions {
    pub(crate) enable_warnings: bool,
    pub(crate) dump_intermediate: bool,
    pub(crate) dump_cfg: bool,
    pub(crate) cfg_include_comments: bool,
    pub(crate) max_pass_name: String
}

pub(crate) struct Pipeline<'a, T> {
    input: Option<T>,
    options: &'a PipelineOptions
}

impl<'a, T> Pipeline<'a, T> {
    pub(crate) fn new(input: T, options: &'a PipelineOptions) -> Self {
        Self { input: Some(input), options }
    }

    pub(crate) fn map<U, Pass: CompilerPass<T, U>>(self, _: Pass) -> Result<Pipeline<'a, U>, Box<dyn Error>> {
        let Pipeline { input, options } = self;
        input.map(|input| {
            let stdout = &mut stdout();
            stdout.write_title("::", Pass::TITLE, Color::White)?;
            stdout.set_color(ColorSpec::new()
                .set_fg(Some(Color::Black))
                .set_intense(true))?;
            let (elapsed, result) = elapsed::measure_time(|| Pass::apply(input, options));
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
        .transpose_result()
        .map(move |output| {
            let input = output.filter(|_| options.max_pass_name != Pass::NAME);
            Pipeline { input, options }
        })
    }
}

// TODO use Iterator::transpose() when it's stabilized
trait TransposeResult<T, E> {
    fn transpose_result(self) -> Result<Option<T>, E>;
}

impl<T, E> TransposeResult<T, E> for Option<Result<T, E>> {
    fn transpose_result(self) -> Result<Option<T>, E> {
        match self {
            Some(Ok(x)) => Ok(Some(x)),
            Some(Err(e)) => Err(e),
            None => Ok(None),
        }
    }
}

macro_rules! compiler_passes {
    ($($struct_name: ident),+) => {
        $(pub(crate) struct $struct_name;)*
        pub(crate) const COMPILER_PASS_NAMES: &[&'static str] = &[
            $($struct_name::NAME,)*
        ];
    }
}

compiler_passes! {
    Load,
    Parse,
    VerifySemantics,
    IR,
    EvaluateIR,
    Evaluate
}

pub(crate) trait CompilerPass<Input, Output> {
    const NAME: &'static str;
    const TITLE: &'static str;
    fn apply(input: Input, options: &PipelineOptions) -> Result<Output, Box<dyn Error>>;
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

    fn apply(path: String, _: &PipelineOptions) -> Result<SourceFileRef, Box<dyn Error>> {
        let source_file = SourceFile::load(&path)?;
        let line_count = source_file.lines().len();
        let stdout = &mut stdout();
        writeln!(stdout, "{}, {} lines", Self::file_size_pretty(source_file.size()), line_count)?;
        Ok(source_file)
    }
}

impl CompilerPass<SourceFileRef, ASTBlock> for Parse {
    const NAME: &'static str = "parse";
    const TITLE: &'static str = "Parsing";

    fn apply(source_file: SourceFileRef, options: &PipelineOptions) -> Result<ASTBlock, Box<dyn Error>> {
        let lexer = Lexer::new(source_file);
        let mut parser = Parser::new(lexer);
        let ast = parser.parse()?;
        let stats = { parser.lexer_stats() };
        let stdout = &mut stdout();
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

    fn apply(ast: ASTBlock, options: &PipelineOptions) -> Result<ExprRef, Box<dyn Error>> {
        let checker = SemanticsChecker::new();
        let hir = checker.check_module(&ast)?;
        if options.dump_intermediate {
            writeln!(&mut stdout(), "{:?}", hir)?;
        }
        Ok(hir)
    }
}

impl CompilerPass<ExprRef, (ExprRef, ir::ControlFlowGraph)> for IR {
    const NAME: &'static str = "ir";
    const TITLE: &'static str = "Generating IR";

    fn apply(hir: ExprRef, options: &PipelineOptions) -> Result<(ExprRef, ir::ControlFlowGraph), Box<dyn Error>> {
        let frontend = ir::FrontEnd::new(ir::EnableWarnings(options.enable_warnings));
        let cfg = frontend.build(&hir);
        if options.dump_cfg {
            let mut file = File::create("ir_cfg.dot")?;
            Graphviz::new(&cfg)
                .include_comments(options.cfg_include_comments)
                .fmt(&mut file)?;
        }
        Ok((hir, cfg))
    }
}

impl CompilerPass<(ExprRef, ir::ControlFlowGraph), ExprRef> for EvaluateIR {
    const NAME: &'static str = "eval_ir";
    const TITLE: &'static str = "Evaluating IR";

    fn apply(input: (ExprRef, ir::ControlFlowGraph), _: &PipelineOptions) -> Result<ExprRef, Box<dyn Error>> {
        let value = EvalContext::new(&input.1).eval();
        writeln!(&mut stdout(), "{:?}", value)?;
        Ok(input.0)
    }
}

impl CompilerPass<ExprRef, Evalue> for Evaluate {
    const NAME: &'static str = "eval";
    const TITLE: &'static str = "Evaluating HIR";

    fn apply(hir: ExprRef, _: &PipelineOptions) -> Result<Evalue, Box<dyn Error>> {
        let mut evaluator = Evaluator::new();
        let value = evaluator.eval(&hir)?;
        writeln!(&mut stdout(), "{:?}", value)?;
        Ok(value)
    }
}

pub(crate) trait StandardStreamUtils {
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
