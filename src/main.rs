#![feature(crate_visibility_modifier)]
#![warn(rust_2018_idioms)]
#![feature(box_syntax)]
#![feature(box_patterns)]

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

use std::env::{args as program_args};
use std::error::Error;
use crate::source::*;
use crate::lexer::Lexer;
use crate::parser::*;
use crate::eval::*;
use crate::semantics::*;
use crate::constants::*;
use crate::utils::*;
use crate::redundant_bindings::*;
use crate::cfg::*;
use getopts::Options;

fn main() {
    let args: Vec<String> = program_args().collect();
    let mut opts = Options::new();
    opts.optflag("h", "help", "print this help menu");
    const WARNINGS_OPTION: &str = "w";
    const DUMP_OPTION: &str = "d";
    const DUMP_CFG_OPTION: &str = "dump-cfg";
    opts.optflag(WARNINGS_OPTION, "", "suppress warnings");
    opts.optflag(DUMP_OPTION, "dump", "dump intermediate compilation results, e.g. AST");
    opts.optflag("", DUMP_CFG_OPTION, "dump CFGs");

    let matches = opts.parse(&args[1..]).unwrap();
    if matches.opt_present("h") || matches.free.is_empty() {
        print_usage(&args[0], &opts);
    } else {
        for path in &matches.free {
            let options = &ProcessOptions {
                warnings: !matches.opt_present(WARNINGS_OPTION),
                dump_intermediate: matches.opt_present(DUMP_OPTION),
                dump_cfg: matches.opt_present(DUMP_CFG_OPTION)
            };
            match process(path, &options) {
                Ok(_) => {},
                Err(error) => println!("error: {}", error)
            }
        }
    }
}

struct ProcessOptions {
    warnings: bool,
    dump_intermediate: bool,
    dump_cfg: bool
}

fn process(path: &str, options: &ProcessOptions) -> Result<(), Box<dyn Error>> {
    println!(">> Processing {}...", path);
    let source_file = load_source_file(path)?;
    let ast = parse_source_file(source_file, options)?;
    println!(">> Semantic check");
    let hir0 = check_module(ast.as_slice())?;
    drop(ast);
    if options.dump_intermediate {
        println!("{}", hir0.iter().join_as_strings("\n"));
    }

    println!(">> CFG");
    let cfg = construct_cfg(hir0.as_slice());
    if options.dump_cfg {
        dump_graph(&cfg, "cfg.dot");
    }

    println!(">> Redundant bindings (pass 1)");
    let hir1 = RedundantBindings::remove(hir0.as_slice(), if options.warnings { Warnings::On } else { Warnings::Off });
    drop(hir0);
    if options.dump_intermediate {
        println!("{}", hir1.iter().join_as_strings("\n"));
    }

    println!(">> CFP (pass 1)");
    let mut cfp = CFP::new();
    let hir2 = cfp.fold_statements(hir1.as_slice());
    drop(hir1);
    if options.dump_intermediate {
        println!("{}", hir2.iter().join_as_strings("\n"));
    }

    println!(">> Redundant bindings (pass 2)");
    let hir3 = RedundantBindings::remove(hir2.as_slice(), Warnings::Off);
    drop(hir2);
    if options.dump_intermediate {
        println!("{}", hir3.iter().join_as_strings("\n"));
    }

    println!(">> CFP (pass 2)");
    let mut cfp = CFP::new();
    let hir4 = cfp.fold_statements(hir3.as_slice());
    drop(hir3);
    if options.dump_intermediate {
        println!("{}", hir4.iter().join_as_strings("\n"));
    }

    println!(">> CFG (optimized)");
    let cfg = construct_cfg(hir4.as_slice());
    if options.dump_cfg {
        dump_graph(&cfg, "cfg-optimized.dot");
    }

    let mut evaluator = Evaluator::new();
    let evalue = evaluator.eval_module(hir4.as_slice())?;
    println!(">> Evaluated: {:?}", evalue);

    Ok(())
}

fn load_source_file(path: &str) -> Result<SourceFile, Box<dyn Error>> {
    let source_code = SourceFile::read(&path)?;
    let source_code_length = source_code.len();
    let source_file = SourceFile::new(path, &source_code)?;
    let line_count = source_file.lines.len();
    println!("{}, {} lines", file_size_pretty(source_code_length), line_count);
    Ok(source_file)
}

fn parse_source_file(source_file: SourceFile, options: &ProcessOptions) -> Result<Vec<Statement>, Box<dyn Error>> {
    println!(">> Parsing");
    let lexer = Lexer::new(source_file);
    let mut parser = Parser::new(lexer);
    let ast = parser.parse()?;
    let stats = { parser.lexer_stats() };
    println!("{} lexemes", stats.lexeme_count);
    if options.dump_intermediate {
        println!(".. AST:\n{}", ast.iter().join_as_strings("\n"));
    }
    Ok(ast)
}

fn print_usage(program: &str, opts: &Options) {
    let brief = format!("Usage: {} FILES [options]", program);
    print!("{}", opts.usage(&brief));
}

fn file_size_pretty(size: usize) -> String {
    [(usize::pow(2, 30), "G"),
     (usize::pow(2, 20), "M"),
     (usize::pow(2, 10), "K")]
    .iter()
    .find(|(unit, _)| size >= *unit)
    .map(|&(unit, name)| format!("{:.2} {}iB", size as f32 / unit as f32, name))
    .unwrap_or_else(|| format!("{} bytes", size))
}
