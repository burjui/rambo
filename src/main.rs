#![warn(rust_2018_idioms)]

use crate::pipeline::EvaluateIR;
use crate::pipeline::Load;
use crate::pipeline::Parse;
use crate::pipeline::Pipeline;
use crate::pipeline::PipelineOptions;
use crate::pipeline::RISCVBackend;
use crate::pipeline::RISCVSimulator;
use crate::pipeline::StandardStreamUtils;
use crate::pipeline::VerifySemantics;
use crate::pipeline::COMPILER_PASS_NAMES;
use crate::pipeline::IR;
use crate::utils::{stderr, stdout};
use elapsed::measure_time;
use getopts::Options;
use number_prefix::NumberPrefix;
use std::alloc::System;
use std::env::args as program_args;
use std::error::Error;
use std::io::Write;
use termcolor::Color;
use termcolor::ColorSpec;
use termcolor::WriteColor;

#[macro_use]
mod utils;
#[macro_use]
mod ir;
#[macro_use]
mod riscv_backend;
#[macro_use]
mod riscv_simulator;

mod env;
mod frontend;
mod graphviz;
mod lexer;
mod parser;
mod pipeline;
mod riscv_base;
mod riscv_exe;
mod semantics;
mod source;
mod stable_vec;
mod tracking_allocator;
mod unique_rc;

#[cfg(test)]
mod test_config;

type TrackingAllocator = tracking_allocator::TrackingAllocator<System>;

#[global_allocator]
static ALLOCATOR: TrackingAllocator = TrackingAllocator::new(System);

fn main() -> Result<(), Box<dyn Error>> {
    let stderr = &mut stderr();
    let result = parse_command_line().and_then(|command_line| {
        if command_line.input_files.is_empty() {
            println!("{}: no input files", command_line.program_name);
            std::process::exit(1)
        } else {
            command_line
                .input_files
                .iter()
                .try_for_each(|path| process(path.clone(), &command_line.pipeline_options))
        }
    });
    match result {
        Ok(_) => {}
        Err(error) => {
            stderr.set_color(ColorSpec::new().set_fg(Some(Color::Red)).set_bold(true))?;
            write!(stderr, "error: ")?;
            stderr.reset()?;
            writeln!(stderr, "{}", error)?;
        }
    }
    stderr.reset()?;
    Ok(())
}

struct CommandLine {
    program_name: String,
    input_files: Vec<String>,
    pipeline_options: PipelineOptions,
}

fn parse_command_line() -> Result<CommandLine, Box<dyn Error>> {
    const MAXIMUM_VERBOSITY: usize = 3;

    static HELP_OPTION: &str = "h";
    static WARNINGS_OPTION: &str = "w";
    static DUMP_CFG_OPTION: &str = "dump-cfg";
    static IR_COMMENTS_OPTION: &str = "ir-comments";
    static TC_COMMENTS_OPTION: &str = "tc-comments";
    static PASS_OPTION: &str = "p";
    static NO_CFP_OPTION: &str = "no-cfp";
    static NO_DCE_OPTION: &str = "no-dce";
    static NO_IMMINT_OPTION: &str = "no-immint";
    static EVAL_IR: &str = "e";
    static VERBOSE_OPTION: &str = "v";
    static DUMP_AST_OPTION: &str = "dump-ast";
    static DUMP_HIR_OPTION: &str = "dump-hir";
    static DUMP_IR_OPTION: &str = "dump-ir";
    static DUMP_EXECUTABLE: &str = "dump-exe";
    static PRINT_TARGET_CODE_OPTION: &str = "print-tc";

    let args: Vec<String> = program_args().collect();
    let mut spec = Options::new();
    spec.optflag(HELP_OPTION, "help", "print this help menu");
    spec.optflag(WARNINGS_OPTION, "", "suppress warnings");
    spec.optflag(EVAL_IR, "", "evaluate the generated IR");
    spec.optflag("", DUMP_CFG_OPTION, "dump CFG");
    spec.optflag("", IR_COMMENTS_OPTION, "comment the generated IR");
    spec.optflag("", TC_COMMENTS_OPTION, "comment the generated target code");
    spec.optflag(
        "",
        NO_CFP_OPTION,
        "disable constant folding and propagation",
    );
    spec.optflag("", NO_DCE_OPTION, "disable dead code elimination");
    spec.optflag(
        "",
        NO_IMMINT_OPTION,
        "do not emit integers as immediates, load from memory instead",
    );
    spec.optflagmulti(
        VERBOSE_OPTION,
        "verbose",
        "print individual passes;\nuse twice to dump intermediate results",
    );
    let pass_name_list: String = COMPILER_PASS_NAMES.join(", ");
    spec.optopt(
        PASS_OPTION,
        "pass",
        &format!(
            "name of the last compiler pass in the pipeline;\nvalid names are: {}",
            pass_name_list
        ),
        "PASS",
    );
    spec.optflag("", DUMP_AST_OPTION, "dump AST");
    spec.optflag("", DUMP_HIR_OPTION, "dump HIR");
    spec.optflag("", DUMP_IR_OPTION, "dump IR");
    spec.optflag("", DUMP_EXECUTABLE, "dump executable");
    spec.optflag("", PRINT_TARGET_CODE_OPTION, "print target code");

    let matches = spec.parse(&args[1..])?;
    let program_name = args[0].clone();
    if matches.opt_present(HELP_OPTION) {
        let brief = format!("Usage: {} [options] file...", program_name);
        print!("{}", spec.usage(&brief));
        std::process::exit(0);
    }
    let verbosity = matches.opt_count(VERBOSE_OPTION);
    if matches.opt_count(VERBOSE_OPTION) > MAXIMUM_VERBOSITY {
        return Err(From::from(format!(
            "maximum verbosity level ({}) exceeded",
            MAXIMUM_VERBOSITY
        )));
    }

    let pipeline_options = PipelineOptions {
        max_pass_name: {
            let result: Result<String, Box<dyn Error>> = matches.opt_str(PASS_OPTION).map_or_else(
                || Ok((*COMPILER_PASS_NAMES.iter().last().unwrap()).to_string()),
                |max_pass_name| {
                    if COMPILER_PASS_NAMES.contains(&max_pass_name.as_str()) {
                        Ok(max_pass_name)
                    } else {
                        Err(From::from(format!(
                            "invalid compiler pass name: {}",
                            max_pass_name
                        )))
                    }
                },
            );
            result?
        },
        enable_warnings: !matches.opt_present(WARNINGS_OPTION),
        dump_cfg: matches.opt_present(DUMP_CFG_OPTION),
        enable_ir_comments: matches.opt_present(IR_COMMENTS_OPTION),
        enable_tc_comments: matches.opt_present(TC_COMMENTS_OPTION),
        enable_cfp: !matches.opt_present(NO_CFP_OPTION),
        enable_dce: !matches.opt_present(NO_DCE_OPTION),
        enable_immediate_integers: !matches.opt_present(NO_IMMINT_OPTION),
        eval_ir: matches.opt_present(EVAL_IR),
        verbosity: verbosity as u8,
        dump_ast: matches.opt_present(DUMP_AST_OPTION),
        dump_hir: matches.opt_present(DUMP_HIR_OPTION),
        dump_ir: matches.opt_present(DUMP_IR_OPTION),
        dump_executable: matches.opt_present(DUMP_EXECUTABLE),
        print_target_code: matches.opt_present(PRINT_TARGET_CODE_OPTION),
    };
    let input_files = matches.free;
    Ok(CommandLine {
        program_name,
        input_files,
        pipeline_options,
    })
}

fn process(path: String, options: &PipelineOptions) -> Result<(), Box<dyn Error>> {
    let stdout = &mut stdout();
    stdout.write_title("==>", &path, Color::Yellow)?;
    stdout.reset()?;
    stdout.flush()?;
    let (elapsed, result) = measure_time(|| {
        let mut pipeline = Pipeline::new(path, options)
            .map(Load)?
            .map(Parse)?
            .map(VerifySemantics)?
            .map(IR)?;
        if options.eval_ir {
            pipeline = pipeline.map(EvaluateIR)?;
        }
        pipeline.map(RISCVBackend)?.map(RISCVSimulator).map(|_| ())
    });
    println!("Execution time: {}", elapsed);
    println!(
        "Memory usage: {}",
        match NumberPrefix::binary(ALLOCATOR.max_usage() as f32) {
            NumberPrefix::Standalone(usage) => format!("{} bytes", usage),
            NumberPrefix::Prefixed(prefix, usage) => format!("{:.0} {}B", usage, prefix),
        }
    );
    result
}
