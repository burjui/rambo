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
    let source_file = load_source_file_pass(path)?;
    let ast = parse_source_file_pass(source_file, options)?;
    let hir0 = semantic_check_pass(ast, options)?;
    let hir1 = construct_cfg_pass(hir0, "cfg.dot", "", options);
    let mut redundant_bindings_pass_count: u8 = 0;
    let hir2 = redundant_bindings_pass(hir1, &mut redundant_bindings_pass_count, options);
    let mut cfp_pass_count: u8 = 0;
    let hir3 = cfp_pass(hir2, &mut cfp_pass_count, options);
    let hir4 = redundant_bindings_pass(hir3, &mut redundant_bindings_pass_count, &ProcessOptions {
        warnings: false,
        ..*options
    });
    let hir5 = cfp_pass(hir4, &mut cfp_pass_count, options);
    let hir6 = construct_cfg_pass(hir5, "cfg-optimized.dot", " (optimized)", options);
    let _ = eval_pass(hir6)?;
    Ok(())
}

fn load_source_file_pass(path: &str) -> Result<SourceFile, Box<dyn Error>> {
    let source_code = SourceFile::read(&path)?;
    let source_code_length = source_code.len();
    let source_file = SourceFile::new(path, &source_code)?;
    let line_count = source_file.lines.len();
    println!("{}, {} lines", file_size_pretty(source_code_length), line_count);
    Ok(source_file)
}

fn parse_source_file_pass(source_file: SourceFile, options: &ProcessOptions) -> Result<Vec<Statement>, Box<dyn Error>> {
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

fn semantic_check_pass(ast: Vec<Statement>, options: &ProcessOptions) -> Result<Vec<TypedStatement>, Box<dyn Error>> {
    println!(">> Semantic check");
    let hir = check_module(ast.as_slice())?;
    if options.dump_intermediate {
        println!("{}", hir.iter().join_as_strings("\n"));
    }
    Ok(hir)
}

fn construct_cfg_pass(hir: Vec<TypedStatement>, filename: &str, postfix: &str, options: &ProcessOptions) -> Vec<TypedStatement> {
    println!(">> CFG{}", postfix);
    let cfg = construct_cfg(hir.as_slice());
    if options.dump_cfg {
        dump_graph(&cfg, filename);
    }
    hir
}

fn redundant_bindings_pass(hir: Vec<TypedStatement>, pass_count: &mut u8, options: &ProcessOptions) -> Vec<TypedStatement> {
    println!(">> Redundant bindings (pass {})", *pass_count + 1);
    *pass_count += 1;
    let hir = RedundantBindings::remove(hir.as_slice(), if options.warnings { Warnings::On } else { Warnings::Off });
    if options.dump_intermediate {
        println!("{}", hir.iter().join_as_strings("\n"));
    }
    hir
}

fn cfp_pass(hir: Vec<TypedStatement>, pass_count: &mut u8, options: &ProcessOptions) -> Vec<TypedStatement> {
    println!(">> CFP (pass {})", *pass_count + 1);
    *pass_count += 1;
    let mut cfp = CFP::new();
    let hir = cfp.fold_statements(hir.as_slice());
    if options.dump_intermediate {
        println!("{}", hir.iter().join_as_strings("\n"));
    }
    hir
}

fn eval_pass(hir: Vec<TypedStatement>) -> Result<Evalue, Box<dyn Error>> {
    println!(">> Evaluating");
    let mut evaluator = Evaluator::new();
    let evalue = evaluator.eval_module(hir.as_slice())?;
    println!("{:?}", evalue);
    Ok(evalue)
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
