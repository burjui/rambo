#![warn(rust_2018_idioms)]

use std::env::args as program_args;
use std::error::Error;
use std::io::Write;

use getopts::Options;
use termcolor::Color;
use termcolor::ColorSpec;
use termcolor::WriteColor;

use crate::pipeline::COMPILER_PASS_NAMES;
use crate::pipeline::Evaluate;
use crate::pipeline::EvaluateIR;
use crate::pipeline::IR;
use crate::pipeline::Load;
use crate::pipeline::Parse;
use crate::pipeline::Pipeline;
use crate::pipeline::PipelineOptions;
use crate::pipeline::StandardStreamUtils;
use crate::pipeline::VerifySemantics;
use crate::utils::stdout;

#[macro_use]
mod utils;

#[cfg(test)]
#[macro_use]
mod vm;

#[cfg(test)]
mod runtime;

mod source;
mod lexer;
mod parser;
mod eval;
mod semantics;
mod env;
mod pipeline;
mod unique_rc;
mod graphviz;
mod ir;

fn main() -> Result<(), Box<dyn Error>> {
    let stdout = &mut stdout();
    let result = parse_command_line()
        .and_then(|command_line|
            if command_line.input_files.is_empty() {
                println!("{}: no input files", command_line.program_name);
                std::process::exit(1)
            } else {
                command_line.input_files.iter()
                    .map(|path| process(path.clone(), &command_line.pipeline_options))
                    .collect::<Result<(), Box<dyn Error>>>()
            });
    match result {
        Ok(_) => {},
        Err(error) => {
            stdout.set_color(ColorSpec::new()
                .set_fg(Some(Color::Red))
                .set_bold(true))?;
            write!(stdout, "error: ")?;
            stdout.reset()?;
            writeln!(stdout, "{}", error)?;
        }
    }
    stdout.reset()?;
    Ok(())
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
    spec.optflagmulti("", DUMP_CFG_OPTION, "dump CFG; use twice to include comments");

    let pass_name_list: String = COMPILER_PASS_NAMES.join(", ");
    spec.optopt(PASS_OPTION, "pass", &format!("name of the last compiler pass in the pipeline;\nvalid names are: {}", pass_name_list), "PASS");

    let matches = spec.parse(&args[1..])?;
    let program_name = args[0].clone();
    if matches.opt_present(HELP_OPTION) {
        let brief = format!("Usage: {} [options] file...", program_name);
        print!("{}", spec.usage(&brief));
        std::process::exit(0);
    }
    let pipeline_options = PipelineOptions {
        enable_warnings: !matches.opt_present(WARNINGS_OPTION),
        dump_intermediate: matches.opt_present(DUMP_OPTION),
        dump_cfg: matches.opt_present(DUMP_CFG_OPTION),
        cfg_include_comments: matches.opt_count(DUMP_CFG_OPTION) > 1,
        max_pass_name: {
            let result: Result<String, Box<dyn Error>> = matches.opt_str(PASS_OPTION)
                .map(|max_pass_name|
                    if COMPILER_PASS_NAMES.contains(&max_pass_name.as_str()) {
                        Ok(max_pass_name)
                    } else {
                        Err(From::from(format!("invalid compiler pass name: {}", max_pass_name)))
                    })
                .unwrap_or_else(|| Ok(COMPILER_PASS_NAMES.iter().last().unwrap().to_string()));
            result?
        }
    };
    let input_files = matches.free;
    Ok(CommandLine { program_name, input_files, pipeline_options })
}

fn process(path: String, options: &PipelineOptions) -> Result<(), Box<dyn Error>> {
    let stdout = &mut stdout();
    stdout.write_title("==>", &path, Color::Yellow)?;
    Pipeline::new(path, options)
        .map(Load)?
        .map(Parse)?
        .map(VerifySemantics)?
        .map(IR)?
        .map(EvaluateIR)?
        .map(Evaluate)
        .map(|_| ())
}
