use crate::cfg::construct_cfg;
use crate::cfg::dump_graph;
use crate::constants::CFP;
use crate::eval::Evaluator;
use crate::eval::Evalue;
use crate::lexer::Lexer;
use crate::parser::Parser;
use crate::parser::Statement;
use crate::redundant_bindings::RedundantBindings;
use crate::redundant_bindings::Warnings;
use crate::semantics::check_module;
use crate::semantics::TypedStatement;
use crate::source::SourceFile;
use crate::utils::ByLine;
use std::error::Error;
use std::io::Write;
use termcolor::Color;
use termcolor::ColorSpec;
use termcolor::StandardStream;
use termcolor::WriteColor;

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
crate struct ConstructCFG;
crate struct RemoveRedundantBindings1;
crate struct PropagateConstants1;
crate struct RemoveRedundantBindings2;
crate struct PropagateConstants2;
crate struct ConstructCFGOptimized;
crate struct Evaluate;

#[derive(PartialOrd, PartialEq, Copy, Clone)]
crate enum PassId {
    Load,
    Parse,
    VerifySemantics,
    ConstructCFG,
    RemoveRedundantBindings1,
    PropagateConstants1,
    RemoveRedundantBindings2,
    PropagateConstants2,
    ConstructCFGOptimized,
    Evaluate
}

impl PassId {
    crate const fn all() -> &'static [PassId] {&[
        PassId::Load,
        PassId::Parse,
        PassId::VerifySemantics,
        PassId::ConstructCFG,
        PassId::RemoveRedundantBindings1,
        PassId::PropagateConstants1,
        PassId::RemoveRedundantBindings2,
        PassId::PropagateConstants2,
        PassId::ConstructCFGOptimized,
        PassId::Evaluate
    ]}

    crate fn name(&self) -> &str {
        match self {
            PassId::Load => "load",
            PassId::Parse => "parse",
            PassId::VerifySemantics => "sem",
            PassId::ConstructCFG => "cfg",
            PassId::RemoveRedundantBindings1 => "rb1",
            PassId::PropagateConstants1 => "cfp1",
            PassId::RemoveRedundantBindings2 => "rb2",
            PassId::PropagateConstants2 => "cfp2",
            PassId::ConstructCFGOptimized => "cfgopt",
            PassId::Evaluate => "eval"
        }
    }

    fn title(&self) -> &str {
        match self {
            PassId::Load => "Loading",
            PassId::Parse => "Parsing",
            PassId::VerifySemantics => "Verifying semantics",
            PassId::ConstructCFG => "Constructing CFG",
            PassId::RemoveRedundantBindings1 => "Removing redundant bindings (pass 1)",
            PassId::PropagateConstants1 => "Propagating constants (pass 1)",
            PassId::RemoveRedundantBindings2 => "Removing redundant bindings (pass 2)",
            PassId::PropagateConstants2 => "Propagating constants (pass 2)",
            PassId::ConstructCFGOptimized => "Constructing CFG (optimized)",
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

impl CompilerPass<String, SourceFile> for Load {
    const ID: PassId = PassId::Load;

    fn apply_impl(path: String, stdout: &mut StandardStream, _: &PipelineOptions) -> Result<SourceFile, Box<dyn Error>> {
        let source_code = SourceFile::read(&path)?;
        let source_code_length = source_code.len();
        let source_file = SourceFile::from(&path, &source_code)?;
        let line_count = source_file.lines.len();
        writeln!(stdout, "{}, {} lines", Self::file_size_pretty(source_code_length), line_count);
        Ok(source_file)
    }
}

impl CompilerPass<SourceFile, Vec<Statement>> for Parse {
    const ID: PassId = PassId::Parse;

    fn apply_impl(source_file: SourceFile, stdout: &mut StandardStream, options: &PipelineOptions) -> Result<Vec<Statement>, Box<dyn Error>> {
        let lexer = Lexer::new(source_file);
        let mut parser = Parser::new(lexer);
        let ast = parser.parse()?;
        let stats = { parser.lexer_stats() };
        writeln!(stdout, "{} lexemes", stats.lexeme_count);
        if options.dump_intermediate {
            writeln!(stdout, "-----------\n{}", ast.iter().join_as_strings("\n"));
        }
        Ok(ast)
    }
}

impl CompilerPass<Vec<Statement>, Vec<TypedStatement>> for VerifySemantics {
    const ID: PassId = PassId::VerifySemantics;

    fn apply_impl(ast: Vec<Statement>, stdout: &mut StandardStream, options: &PipelineOptions) -> Result<Vec<TypedStatement>, Box<dyn Error>> {
        let hir = check_module(ast.as_slice())?;
        if options.dump_intermediate {
            writeln!(stdout, "{}", hir.iter().join_as_strings("\n"));
        }
        Ok(hir)
    }
}

impl ConstructCFG {
    fn apply<'a>(hir: Vec<TypedStatement>, _: &'a mut StandardStream, options: &PipelineOptions, filename: &str) -> Result<Vec<TypedStatement>, Box<dyn Error>> {
        let cfg = construct_cfg(hir.as_slice());
        if options.dump_cfg {
            dump_graph(&cfg, filename);
        }
        Ok(hir)
    }
}

impl CompilerPass<Vec<TypedStatement>, Vec<TypedStatement>> for ConstructCFG {
    const ID: PassId = PassId::ConstructCFG;

    fn apply_impl(hir: Vec<TypedStatement>, stdout: &mut StandardStream, options: &PipelineOptions) -> Result<Vec<TypedStatement>, Box<dyn Error>> {
        Self::apply(hir, stdout, options, "cfg.dot")
    }
}

impl CompilerPass<Vec<TypedStatement>, Vec<TypedStatement>> for ConstructCFGOptimized {
    const ID: PassId = PassId::ConstructCFGOptimized;

    fn apply_impl(hir: Vec<TypedStatement>, stdout: &mut StandardStream, options: &PipelineOptions) -> Result<Vec<TypedStatement>, Box<dyn Error>> {
        ConstructCFG::apply(hir, stdout, options, "cfg-optimized.dot")
    }
}

impl RemoveRedundantBindings1 {
    fn apply(hir: Vec<TypedStatement>, stdout: &mut StandardStream, options: &PipelineOptions, warnings: Warnings) -> Result<Vec<TypedStatement>, Box<dyn Error>> {
        let result = RedundantBindings::remove(hir.as_slice(), warnings);
        drop(hir);
        if options.dump_intermediate {
            writeln!(stdout, "{}", result.iter().join_as_strings("\n"));
        }
        Ok(result)
    }
}

impl CompilerPass<Vec<TypedStatement>, Vec<TypedStatement>> for RemoveRedundantBindings1 {
    const ID: PassId = PassId::RemoveRedundantBindings1;

    fn apply_impl(hir: Vec<TypedStatement>, stdout: &mut StandardStream, options: &PipelineOptions) -> Result<Vec<TypedStatement>, Box<dyn Error>> {
        Self::apply(hir, stdout, options, if options.warnings { Warnings::On } else { Warnings::Off })
    }
}

impl CompilerPass<Vec<TypedStatement>, Vec<TypedStatement>> for RemoveRedundantBindings2 {
    const ID: PassId = PassId::RemoveRedundantBindings2;

    fn apply_impl(hir: Vec<TypedStatement>, stdout: &mut StandardStream, options: &PipelineOptions) -> Result<Vec<TypedStatement>, Box<dyn Error>> {
        RemoveRedundantBindings1::apply(hir, stdout, options, Warnings::Off)
    }
}

impl PropagateConstants1 {
    fn apply(hir: Vec<TypedStatement>, stdout: &mut StandardStream, options: &PipelineOptions) -> Result<Vec<TypedStatement>, Box<dyn Error>> {
        let mut cfp = CFP::new();
        let result = cfp.fold_statements(hir.as_slice());
        drop(hir);
        if options.dump_intermediate {
            writeln!(stdout, "{}", result.iter().join_as_strings("\n"));
        }
        Ok(result)
    }
}

impl CompilerPass<Vec<TypedStatement>, Vec<TypedStatement>> for PropagateConstants1 {
    const ID: PassId = PassId::PropagateConstants1;

    fn apply_impl(hir: Vec<TypedStatement>, stdout: &mut StandardStream, options: &PipelineOptions) -> Result<Vec<TypedStatement>, Box<dyn Error>> {
        Self::apply(hir, stdout, options)
    }
}

impl CompilerPass<Vec<TypedStatement>, Vec<TypedStatement>> for PropagateConstants2 {
    const ID: PassId = PassId::PropagateConstants2;

    fn apply_impl(hir: Vec<TypedStatement>, stdout: &mut StandardStream, options: &PipelineOptions) -> Result<Vec<TypedStatement>, Box<dyn Error>> {
        PropagateConstants1::apply(hir, stdout, options)
    }
}

impl CompilerPass<Vec<TypedStatement>, Evalue> for Evaluate {
    const ID: PassId = PassId::Evaluate;

    fn apply_impl(hir: Vec<TypedStatement>, stdout: &mut StandardStream, _: &PipelineOptions) -> Result<Evalue, Box<dyn Error>> {
        let mut evaluator = Evaluator::new();
        let value = evaluator.eval_module(hir.as_slice())?;
        writeln!(stdout, "{:?}", value);
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
        write!(self, " ");
        self.write_colored(title, color)?;
        writeln!(self);
        self.set_color(ColorSpec::new()
            .set_fg(Some(Color::Black))
            .set_intense(true)
        )
    }

    fn write_colored(&mut self, text: &str, color: Color) -> std::io::Result<()> {
        self.set_color(ColorSpec::new()
            .set_fg(Some(color))
            .set_bold(true)
        )?;
        write!(self, "{}", text)
    }
}