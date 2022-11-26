use std::{
    error::Error,
    ffi::OsStr,
    fs::File,
    io::{
        BufWriter,
        Write,
    },
    path::Path,
};

use itertools::Itertools;
use number_prefix::NumberPrefix;
use riscv_backend::EnableComments;
use risky::abi::A0;
use termcolor::{
    Color,
    ColorSpec,
    StandardStream,
    WriteColor,
};

use crate::{
    frontend::{
        FrontEnd,
        FrontEndState,
    },
    graphviz::IrGraphvizFile,
    ir::{
        eval::eval,
        fmt_statement,
        IRModule,
        Value,
    },
    lexer::Lexer,
    parser::{
        Block,
        Parser,
    },
    riscv_backend,
    riscv_backend::{
        DumpCode,
        EnableImmediateIntegers,
    },
    riscv_exe::{
        run,
        DumpState,
        Executable,
    },
    semantics::{
        EnableWarnings,
        ExprRef,
        SemanticsChecker,
    },
    source::{
        SourceFile,
        SourceFileRef,
    },
    utils::stdout,
};

#[derive(Clone)]
#[allow(clippy::struct_excessive_bools)]
pub(crate) struct PipelineOptions {
    pub(crate) max_pass_name: String,
    pub(crate) enable_warnings: bool,
    pub(crate) dump_cfg: bool,
    pub(crate) enable_ir_comments: bool,
    pub(crate) enable_tc_comments: bool,
    pub(crate) enable_cfp: bool,
    pub(crate) enable_dce: bool,
    pub(crate) enable_immediate_integers: bool,
    pub(crate) eval_ir: bool,
    pub(crate) verbosity: u8,
    pub(crate) dump_ast: bool,
    pub(crate) dump_hir: bool,
    pub(crate) dump_ir: bool,
    pub(crate) dump_executable: bool,
    pub(crate) print_target_code: bool,
}

pub(crate) struct Pipeline<'a, T> {
    input: Option<T>,
    options: &'a PipelineOptions,
}

impl<'a, T> Pipeline<'a, T> {
    pub(crate) fn new(input: T, options: &'a PipelineOptions) -> Self {
        Self {
            input: Some(input),
            options,
        }
    }

    pub(crate) fn map<U, Pass: CompilerPass<T, U>>(
        self,
        _: Pass,
    ) -> Result<Pipeline<'a, U>, Box<dyn Error>> {
        let Pipeline { input, options } = self;
        input
            .map(|input| {
                let stdout = &mut stdout();
                if options.verbosity >= 1 {
                    stdout.write_title("::", Pass::TITLE, Color::White)?;
                    stdout.set_color(
                        ColorSpec::new()
                            .set_fg(Some(Color::Black))
                            .set_intense(true),
                    )?;
                    stdout.flush()?;
                }
                let (elapsed, result) = elapsed::measure_time(|| Pass::apply(input, options));
                if options.verbosity >= 1 {
                    stdout.set_color(ColorSpec::new().set_fg(Some(Color::White)))?;
                    write!(stdout, "(")?;
                    stdout.set_color(
                        ColorSpec::new()
                            .set_fg(Some(Color::Black))
                            .set_intense(true),
                    )?;
                    write!(stdout, "{}", elapsed)?;
                    stdout.set_color(ColorSpec::new().set_fg(Some(Color::White)))?;
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
    RISCVEmulator
}

pub(crate) trait CompilerPass<Input, Output> {
    const NAME: &'static str;
    const TITLE: &'static str;
    fn apply(input: Input, options: &PipelineOptions) -> Result<Output, Box<dyn Error>>;
}

impl Load {
    #[allow(clippy::cast_precision_loss)]
    fn file_size_pretty(size: usize) -> String {
        [
            (usize::pow(2, 30), "G"),
            (usize::pow(2, 20), "M"),
            (usize::pow(2, 10), "K"),
        ]
        .iter()
        .find_map(|(unit, name)| {
            if size >= *unit {
                Some(format!("{:.2} {}iB", size as f32 / *unit as f32, name))
            } else {
                None
            }
        })
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
        if options.verbosity >= 1 {
            writeln!(
                stdout,
                "{}, {} lines",
                Self::file_size_pretty(source_file.size()),
                line_count
            )?;
        }
        Ok(source_file)
    }
}

impl CompilerPass<SourceFileRef, (Block, SourceFileRef)> for Parse {
    const NAME: &'static str = "parse";
    const TITLE: &'static str = "Parsing";

    fn apply(
        source_file: SourceFileRef,
        options: &PipelineOptions,
    ) -> Result<(Block, SourceFileRef), Box<dyn Error>> {
        let lexer = Lexer::new(source_file.clone());
        let mut parser = Parser::new(lexer);
        let ast = parser.parse()?;
        let stats = { parser.lexer_stats() };
        let stdout = &mut stdout();
        if options.verbosity >= 1 {
            writeln!(stdout, "{} lexemes", stats.lexeme_count)?;
        }
        if options.dump_ast {
            writeln!(stdout, "{:?}", ast.statements.iter().format("\n"))?;
        }
        Ok((ast, source_file))
    }
}

impl CompilerPass<(Block, SourceFileRef), (ExprRef, SourceFileRef)> for VerifySemantics {
    const NAME: &'static str = "sem";
    const TITLE: &'static str = "Verifying semantics";

    fn apply(
        (ast, source_file): (Block, SourceFileRef),
        options: &PipelineOptions,
    ) -> Result<(ExprRef, SourceFileRef), Box<dyn Error>> {
        let checker = SemanticsChecker::new(EnableWarnings(options.enable_warnings));
        let hir = checker.check_module(&ast)?;
        if options.dump_hir {
            writeln!(&mut stdout(), "{hir:?}")?;
        }
        Ok((hir, source_file))
    }
}

impl CompilerPass<(ExprRef, SourceFileRef), IRModule> for IR {
    const NAME: &'static str = "ir";
    const TITLE: &'static str = "Generating IR";

    fn apply(
        (hir, source_file): (ExprRef, SourceFileRef),
        options: &PipelineOptions,
    ) -> Result<IRModule, Box<dyn Error>> {
        let mut state = FrontEndState::new();
        let frontend = FrontEnd::new(source_file.name(), &mut state)
            .include_comments(options.enable_ir_comments)
            .enable_cfp(options.enable_cfp)
            .enable_dce(options.enable_dce);
        let module = frontend.build(&hir);
        let mut stdout = stdout();
        if options.verbosity >= 1 {
            writeln!(&mut stdout, "{} statements", unit_statements_count(&module))?;
        }
        if options.dump_ir {
            writeln!(&mut stdout)?;
            dump_module(&module, &module.name, &mut stdout)?;
            writeln!(&mut stdout)?;
        }
        if options.dump_cfg {
            let src_path = Path::new(source_file.name());
            let src_file_name = src_path
                .file_name()
                .and_then(OsStr::to_str)
                .unwrap_or_else(|| {
                    panic!(
                        "failed to extract the file name from path: {}",
                        src_path.display()
                    )
                });
            let file = IrGraphvizFile::create(format!("{src_file_name}_cfg.dot"))?;
            file.write(&module)?;
        }
        Ok(module)
    }
}

impl CompilerPass<IRModule, IRModule> for EvaluateIR {
    const NAME: &'static str = "eval_ir";
    const TITLE: &'static str = "Evaluating IR";

    fn apply(module: IRModule, options: &PipelineOptions) -> Result<IRModule, Box<dyn Error>> {
        let value = eval(&module);
        if options.verbosity >= 1 {
            writeln!(&mut stdout(), "{value:?}")?;
        }
        Ok(module)
    }
}

impl CompilerPass<IRModule, Executable> for RISCVBackend {
    const NAME: &'static str = "rvgen";
    const TITLE: &'static str = "Generating RISC-V code";

    fn apply(module: IRModule, options: &PipelineOptions) -> Result<Executable, Box<dyn Error>> {
        let stdout = &mut stdout();
        let image = riscv_backend::generate(
            &module,
            if options.print_target_code {
                DumpCode::Yes(stdout)
            } else {
                DumpCode::No
            },
            EnableImmediateIntegers(options.enable_immediate_integers),
            EnableComments(options.enable_tc_comments),
        )?;

        if options.dump_executable {
            let file = File::create(module.name + "_exe.bin")?;
            bincode::encode_into_std_write(
                &image,
                &mut BufWriter::new(file),
                bincode::config::standard(),
            )?;
        }

        if options.verbosity >= 1 {
            // FIXME "image.code.len() / 4" is only correct for 32-bit instructions
            writeln!(
                stdout,
                "code: {} ({} instructions)",
                size_string(image.code.len()),
                image.code.len() / 4
            )?;
            writeln!(stdout, "data: {}", size_string(image.data.len()))?;
        }

        Ok(image)
    }
}

#[allow(clippy::cast_precision_loss)]
fn size_string(size: usize) -> String {
    match NumberPrefix::binary(size as f32) {
        NumberPrefix::Standalone(size) => format!("{size} bytes"),
        NumberPrefix::Prefixed(prefix, size) => format!("{size:.1} {prefix}B"),
    }
}

impl CompilerPass<Executable, ()> for RISCVEmulator {
    const NAME: &'static str = "rvsim";
    const TITLE: &'static str = "Executing RISC-V code";

    fn apply(image: Executable, options: &PipelineOptions) -> Result<(), Box<dyn Error>> {
        let stdout = &mut stdout();
        let dump_state = match options.verbosity {
            2 => DumpState::Instructions(stdout),
            3 => DumpState::Everything(stdout),
            _ => DumpState::None,
        };
        let cpu = run(&image, dump_state)?;
        if options.verbosity >= 1 {
            let result = cpu.read_register(A0.into());
            writeln!(stdout, "result at x{A0} = 0x{result:08x} ({result})",)?;
        }
        Ok(())
    }
}

fn unit_statements_count(module: &IRModule) -> usize {
    let functions = module.values.iter().filter_map(|(value, _)| match value {
        Value::Function(fn_id, _) => Some(&module.functions[fn_id]),
        _ => None,
    });
    module
        .cfg
        .node_indices()
        .map(|block| module.cfg[block].len())
        .chain(functions.map(unit_statements_count))
        .sum()
}

fn dump_module(module: &IRModule, name: &str, stdout: &mut StandardStream) -> std::io::Result<()> {
    writeln!(stdout, "// {name}")?;
    for block in module.cfg.node_indices() {
        writeln!(stdout, "{name}_{block}:")?;
        for statement in module.cfg[block].iter() {
            write!(stdout, "    ")?;
            fmt_statement(stdout, statement, &module.values)?;
            writeln!(stdout)?;
        }
    }
    let functions = module
        .values
        .iter()
        .filter_map(|(value, _)| match value {
            Value::Function(fn_id, name) => Some((name.as_ref(), &module.functions[fn_id])),
            _ => None,
        })
        .collect_vec();
    if !functions.is_empty() {
        for (name, module) in functions {
            writeln!(stdout)?;
            dump_module(module, name, stdout)?;
        }
    }
    Ok(())
}

pub(crate) trait StandardStreamUtils {
    fn write_title(&mut self, prefix: &str, title: &str, color: Color) -> std::io::Result<()>;
}

impl StandardStreamUtils for StandardStream {
    fn write_title(&mut self, prefix: &str, title: &str, color: Color) -> std::io::Result<()> {
        self.set_color(ColorSpec::new().set_fg(Some(Color::Blue)).set_bold(true))?;
        write!(self, "{prefix} ")?;
        self.set_color(ColorSpec::new().set_fg(Some(color)).set_bold(true))?;
        writeln!(self, "{title}")
    }
}
