use core::borrow::Borrow;
use std::error::Error;
use std::ffi::OsStr;
use std::fs::File;
use std::io::BufWriter;
use std::io::Write;
use std::path::Path;

use itertools::Itertools;
use termcolor::Color;
use termcolor::ColorSpec;
use termcolor::StandardStream;
use termcolor::WriteColor;

use crate::{riscv_backend, riscv_runner};
use crate::frontend::FrontEnd;
use crate::graphviz::graphviz_dot_write;
use crate::ir::ControlFlowGraph;
use crate::ir::eval::EvalContext;
use crate::ir::fmt_statement;
use crate::ir::Value;
use crate::lexer::Lexer;
use crate::parser::Block;
use crate::parser::Parser;
use crate::riscv_backend::RICSVImage;
use crate::semantics::ExprRef;
use crate::semantics::SemanticsChecker;
use crate::source::SourceFile;
use crate::source::SourceFileRef;
use crate::utils::stdout;

#[derive(Clone)]
pub(crate) struct PipelineOptions {
    pub(crate) max_pass_name: String,
    pub(crate) print_passes: bool,
    pub(crate) dump_intermediate: bool,
    pub(crate) enable_warnings: bool,
    pub(crate) dump_cfg: bool,
    pub(crate) cfg_include_comments: bool,
    pub(crate) enable_cfp: bool,
    pub(crate) enable_dce: bool,
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
            if options.print_passes {
                stdout.write_title("::", Pass::TITLE, Color::White)?;
                stdout.set_color(ColorSpec::new()
                    .set_fg(Some(Color::Black))
                    .set_intense(true))?;
            }
                let (elapsed, result) = elapsed::measure_time(|| Pass::apply(input, options));
            if options.print_passes {
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
            }
            result
        })
        .transpose()
        .map(move |output| {
            let input = output.filter(|_| options.max_pass_name != Pass::NAME);
            Pipeline { input, options }
        })
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
    RISCVBackend,
    RISCVSimulator
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

    fn apply(path: String, options: &PipelineOptions) -> Result<SourceFileRef, Box<dyn Error>> {
        let source_file = SourceFile::load(&path)?;
        let line_count = source_file.lines().len();
        let stdout = &mut stdout();
        if options.print_passes {
            writeln!(stdout, "{}, {} lines", Self::file_size_pretty(source_file.size()), line_count)?;
        }
        Ok(source_file)
    }
}

impl CompilerPass<SourceFileRef, (Block, SourceFileRef)> for Parse {
    const NAME: &'static str = "parse";
    const TITLE: &'static str = "Parsing";

    fn apply(source_file: SourceFileRef, options: &PipelineOptions) -> Result<(Block, SourceFileRef), Box<dyn Error>> {
        let lexer = Lexer::new(source_file.clone());
        let mut parser = Parser::new(lexer);
        let ast = parser.parse()?;
        let stats = { parser.lexer_stats() };
        let stdout = &mut stdout();
        if options.print_passes {
            writeln!(stdout, "{} lexemes", stats.lexeme_count)?;
        }
        if options.dump_intermediate {
            writeln!(stdout, "{:?}", ast.statements.iter().format("\n"))?;
        }
        Ok((ast, source_file))
    }
}

impl CompilerPass<(Block, SourceFileRef), (ExprRef, SourceFileRef)> for VerifySemantics {
    const NAME: &'static str = "sem";
    const TITLE: &'static str = "Verifying semantics";

    fn apply((ast, source_file): (Block, SourceFileRef), options: &PipelineOptions)
        -> Result<(ExprRef, SourceFileRef), Box<dyn Error>>
    {
        let checker = SemanticsChecker::new();
        let hir = checker.check_module(&ast)?;
        if options.dump_intermediate {
            writeln!(&mut stdout(), "{:?}", hir)?;
        }
        Ok((hir, source_file))
    }
}

impl CompilerPass<(ExprRef, SourceFileRef), ControlFlowGraph> for IR {
    const NAME: &'static str = "ir";
    const TITLE: &'static str = "Generating IR";

    fn apply((hir, source_file): (ExprRef, SourceFileRef), options: &PipelineOptions) -> Result<ControlFlowGraph, Box<dyn Error>> {
        let frontend = FrontEnd::new("main")
            .enable_warnings(options.enable_warnings)
            .include_comments(options.cfg_include_comments)
            .enable_cfp(options.enable_cfp)
            .enable_dce(options.enable_dce);
        let cfg = frontend.build(&hir);
        let mut stdout = stdout();
        if options.print_passes {
            writeln!(&mut stdout, "{} statements", cfg_statements_count(&cfg.borrow()))?;
        }
        if options.dump_intermediate {
            dump_cfg(&cfg, &cfg.name, &mut stdout)?;
        }
        if options.dump_cfg {
            let src_path = Path::new(source_file.name());
            let src_file_name = src_path
                .file_name()
                .and_then(OsStr::to_str)
                .unwrap_or_else(|| panic!("failed to extract the file name from path: {}", src_path.display()));
            let file = File::create(&format!("{}_cfg.dot", src_file_name))?;
            graphviz_dot_write(&mut BufWriter::new(file), &cfg)?;
        }
        Ok(cfg)
    }
}

impl CompilerPass<ControlFlowGraph, ControlFlowGraph> for EvaluateIR {
    const NAME: &'static str = "eval_ir";
    const TITLE: &'static str = "Evaluating IR";

    fn apply(cfg: ControlFlowGraph, options: &PipelineOptions) -> Result<ControlFlowGraph, Box<dyn Error>> {
        let value = EvalContext::new(&cfg, &cfg.functions).eval();
        if options.print_passes {
            writeln!(&mut stdout(), "{:?}", value)?;
        }
        Ok(cfg)
    }
}

impl CompilerPass<ControlFlowGraph, RICSVImage> for RISCVBackend {
    const NAME: &'static str = "rvgen";
    const TITLE: &'static str = "Generating RISC-V code";

    fn apply(cfg: ControlFlowGraph, _options: &PipelineOptions) -> Result<RICSVImage, Box<dyn Error>> {
        let image = riscv_backend::generate(&cfg);
        image.verify()?;
//        if options.print_passes {
//            writeln!(&mut stdout(), "{:?}", &image)?;
//        }
        Ok(image)
    }
}

impl CompilerPass<RICSVImage, ()> for RISCVSimulator {
    const NAME: &'static str = "rvsim";
    const TITLE: &'static str = "Executing RISC-V code";

    fn apply(image: RICSVImage, _options: &PipelineOptions) -> Result<(), Box<dyn Error>> {
        riscv_runner::run(&image);
//        if options.print_passes {
//            ...
//        }
        Ok(())
    }
}

fn cfg_statements_count(cfg: &ControlFlowGraph) -> usize {
    let functions = cfg.values
        .iter()
        .filter_map(|(value, _)| match value {
            Value::Function(fn_id) => Some(&cfg.functions[fn_id]),
            _ => None,
        });
    cfg.graph
        .node_indices()
        .map(|block| cfg.graph[block].len())
        .chain(functions.map(|function_cfg| cfg_statements_count(&function_cfg.borrow())))
        .sum()
}

fn dump_cfg(cfg: &ControlFlowGraph, name: &str, stdout: &mut StandardStream) -> std::io::Result<()> {
    writeln!(stdout, "// {}", name)?;
    for block in cfg.graph.node_indices() {
        writeln!(stdout, "{}_{}:", name, block.index())?;
        for statement in cfg.graph[block].iter() {
            write!(stdout, "    ")?;
            fmt_statement(stdout, statement, &cfg.values)?;
            writeln!(stdout)?;
        }
    }
    let functions = cfg.values
        .iter()
        .filter_map(|(value, _)| match value {
            Value::Function(fn_id) => Some((fn_id, &cfg.functions[fn_id])),
            _ => None,
        })
        .collect_vec();
    if !functions.is_empty() {
        writeln!(stdout)?;
        for (id, cfg) in functions {
            writeln!(stdout, "// {}", cfg.name)?;
            let name = id.to_string();
            dump_cfg(cfg, &name, stdout)?;
        }
    }
    Ok(())
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
