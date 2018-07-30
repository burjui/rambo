#![feature(rust_2018_preview)]
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
mod dead_bindings;
mod constants;
mod env;

use getopts::Options;
use std::env::{args as program_args};
use std::error::Error;

use crate::source::*;
use crate::lexer::Lexer;
use crate::parser::*;
use crate::eval::*;
use crate::semantics::*;
use crate::dead_bindings::*;
use crate::constants::*;
use crate::utils::*;

fn main() {
    let args: Vec<String> = program_args().collect();
    let mut opts = Options::new();
    opts.optflag("h", "help", "print this help menu");

    let matches = opts.parse(&args[1..]).unwrap();
    if matches.opt_present("h") || matches.free.is_empty() {
        print_usage(&args[0], opts);
    } else {
        for path in matches.free {
            match process(path) {
                Ok(_) => {},
                Err(error) => println!("error: {}", error.description())
            }
        }
    }
}

fn process(path: String) -> Result<(), Box<dyn Error>> {
    println!(">> Processing {}...", path);
    let source_code = SourceFile::read(&path)?;
    let source_code_length = source_code.len();
    let file = SourceFile::new(source_code, path)?;
    let line_count = file.lines.len();
    let lexer = Lexer::new(file);
    println!("{:?}", lexer);

    let mut parser = Parser::new(lexer);
    let statements = parser.parse()?;
    let stats = { parser.lexer_stats() };
    println!(">> {}, {} lines, {} lexemes", file_size_pretty(source_code_length), line_count, stats.lexeme_count);
    println!(">> AST:\n{}", statements.iter().join_as_strings("\n"));

    let statements = check_module(statements.as_slice())?;
    println!(">> Semantic check:\n{}", statements.iter().join_as_strings("\n"));

    let statements = remove_dead_bindings(&statements, Warnings::On);
    println!(">> Removed unused bindings:\n{}", statements.iter().join_as_strings("\n"));

    let mut cfp = CFP::new();
    let statements = cfp.fold_and_propagate_constants(statements);
    println!(">> CFP:\n{}", statements.iter().join_as_strings("\n"));

    let mut evaluator = Evaluator::new();
    let evalue = evaluator.eval_module(statements.as_slice())?;
    println!(">> Evaluated: {:?}", evalue);

    Ok(())
}

fn print_usage(program: &str, opts: Options) {
    let brief = format!("Usage: {} FILES [options]", program);
    print!("{}", opts.usage(&brief));
}

fn file_size_pretty(size: usize) -> String {
    [(usize::pow(2, 30), "GiB"),
     (usize::pow(2, 20), "MiB"),
     (usize::pow(2, 10), "KiB")]
    .iter()
    .find(|(unit, _)| size >= *unit)
    .map(|&(unit, name)| format!("{:.2} {}", size as f32 / unit as f32, name))
    .unwrap_or_else(|| format!("{} bytes", size))
}
