#![feature(crate_visibility_modifier)]
#![warn(rust_2018_idioms)]
#![feature(box_syntax)]
#![feature(box_patterns)]
#![feature(transpose_result)]
#![feature(self_struct_ctor)]

use crate::pipeline::ALL_PASS_IDS;
use crate::pipeline::ConstructCFG;
use crate::pipeline::ConstructCFGOptimized;
use crate::pipeline::Evaluate;
use crate::pipeline::Load;
use crate::pipeline::Parse;
use crate::pipeline::PassId;
use crate::pipeline::Pipeline;
use crate::pipeline::PipelineOptions;
use crate::pipeline::PropagateConstants1;
use crate::pipeline::PropagateConstants2;
use crate::pipeline::RemoveRedundantBindings1;
use crate::pipeline::RemoveRedundantBindings2;
use crate::pipeline::StandardStreamUtils;
use crate::pipeline::VerifySemantics;
use getopts::Options;
use itertools::join;
use std::env::args as program_args;
use std::error::Error;
use std::io::Write;
use termcolor::Color;
use termcolor::ColorChoice;
use termcolor::ColorSpec;
use termcolor::StandardStream;
use termcolor::WriteColor;

#[macro_use]
mod utils;
mod source;
mod lexer;
mod parser;
mod eval;
mod semantics;
mod constants;
mod env;
mod redundant_bindings;
mod typed_visitor;
mod cfg;
mod pipeline;

fn main() -> Result<(), std::io::Error> {
    let is_tty = unsafe { libc::isatty(libc::STDOUT_FILENO as i32) } != 0;
    let color_choice = if is_tty { ColorChoice::Always } else { ColorChoice::Never };
    let mut stdout = StandardStream::stdout(color_choice);

    let result = parse_command_line()
        .and_then(|command_line|
            if command_line.input_files.is_empty() {
                println!("{}: no input files", command_line.program_name);
                std::process::exit(1)
            } else {
                command_line.input_files.iter()
                    .map(|path| process(path.clone(), &mut stdout, &command_line.pipeline_options))
                    .collect::<Result<(), Box<dyn Error>>>()
            });
    match result {
        Ok(_) => {},
        Err(error) => {
            stdout.set_color(ColorSpec::new()
                .set_fg(Some(Color::Red))
                .set_bold(true))?;
            write!(&mut stdout, "error: ");
            stdout.reset()?;
            writeln!(&mut stdout, "{}", error)?;
        }
    }
    stdout.reset()
}

struct CommandLine {
    program_name: String,
    input_files: Vec<String>,
    pipeline_options: PipelineOptions
}

fn parse_command_line() -> Result<CommandLine, Box<dyn Error>> {
    const HELP_OPTION: &str = "h";
    const WARNINGS_OPTION: &str = "w";
    const DUMP_OPTION: &str = "d";
    const DUMP_CFG_OPTION: &str = "dump-cfg";
    const PASS_OPTION: &str = "p";

    let args: Vec<String> = program_args().collect();
    let mut spec = Options::new();
    spec.optflag(HELP_OPTION, "help", "print this help menu");
    spec.optflag(WARNINGS_OPTION, "", "suppress warnings");
    spec.optflag(DUMP_OPTION, "dump", "dump intermediate compilation results, e.g. AST");
    spec.optflag("", DUMP_CFG_OPTION, "dump CFGs");

    let pass_name_list: String = join(ALL_PASS_IDS.iter().map(PassId::name), ", ");
    spec.optopt(PASS_OPTION, "pass", &format!("last compiler pass to be executed;\nfollowing pass names are recognized: {}", pass_name_list), "PASS");

    let matches = spec.parse(&args[1..])?;
    let program_name = args[0].clone();
    if matches.opt_present(HELP_OPTION) {
        let brief = format!("Usage: {} [options] file...", program_name);
        print!("{}", spec.usage(&brief));
        std::process::exit(0);
    }
    let pipeline_options = PipelineOptions {
        warnings: !matches.opt_present(WARNINGS_OPTION),
        dump_intermediate: matches.opt_present(DUMP_OPTION),
        dump_cfg: matches.opt_present(DUMP_CFG_OPTION),
        max_pass: {
            *matches.opt_str(PASS_OPTION)
                .map(|max_pass_name|
                    ALL_PASS_IDS.iter()
                        .find(|id| id.name() == max_pass_name)
                        .ok_or_else(|| format!("invalid compiler pass name: {}", max_pass_name)))
                .unwrap_or(Ok(&PassId::Evaluate))?
        }
    };
    let input_files = matches.free;
    Ok(CommandLine { program_name, input_files, pipeline_options })
}

fn process(path: String, stdout: &mut StandardStream, options: &PipelineOptions) -> Result<(), Box<dyn Error>> {
    stdout.write_title("==>", &path, Color::Yellow)?;

    Pipeline::new(path, stdout, options)
        .map(Load)?
        .map(Parse)?
        .map(VerifySemantics)?
        .map(ConstructCFG)?
        .map(RemoveRedundantBindings1)?
        .map(PropagateConstants1)?
        .map(RemoveRedundantBindings2)?
        .map(PropagateConstants2)?
        .map(ConstructCFGOptimized)?
        .map(Evaluate)
        .map(|_| ())
}