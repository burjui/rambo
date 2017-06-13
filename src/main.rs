#![feature(associated_consts)]
#![feature(box_syntax)]
#![feature(box_patterns)]

extern crate getopts;
extern crate itertools;
extern crate num;

#[macro_use]
mod utils;
mod source;
mod lexer;
mod parser;
mod eval;
mod semantics;
mod dead_bindings;
mod constants;

use getopts::Options;
use std::env;
use std::path::Path;
use std::error::Error;
use itertools::Itertools;

use source::*;
use lexer::Lexer;
use parser::*;
use eval::*;
use semantics::*;
use dead_bindings::*;
use constants::*;

fn main() {
    let args: Vec<String> = env::args().collect();
    let mut opts = Options::new();
    opts.optflag("h", "help", "print this help menu");

    let matches = opts.parse(&args[1..]).unwrap();
    if matches.opt_present("h") {
        print_usage(&args[0], opts);
    } else if matches.free.is_empty() {
        print_usage(&args[0], opts);
    } else {
        for path in matches.free {
            match process(Path::new(&path)) {
                Ok(_) => {},
                Err(error) => println!("error: {}", error.description())
            }
        }
    }
}

fn process(path: &Path) -> Result<(), Box<Error>> {
    println!(">> Processing {}...", path.to_string_lossy());
    let source_code = SourceFile::read(path)?;
    let file = SourceFile::new(&source_code, path)?;
    let lexer = Lexer::new(&file);
    println!("{:?}", lexer);

    let mut parser = Parser::new(lexer);
    let statements = parser.parse()?;
    let stats = { parser.lexer_stats() };
    println!(">> {}, {} lines, {} lexemes", file_size_pretty(source_code.len()), file.lines.len(), stats.lexeme_count);
    println!(">> AST:\n{}", statements.iter().by_line());

    let statements = check_module(statements.as_slice())?;
    println!(">> Semantic check:\n{}", statements.iter().by_line());

    let statements = remove_dead_bindings(statements, Warnings::On);
    println!(">> Removed unused bindings:\n{}", statements.iter().by_line());

    let mut cfp = CFP::new();
    let statements = cfp.fold_and_propagate_constants(statements);
    println!(">> CFP:\n{}", statements.iter().by_line());

    let mut evaluator = Evaluator::new();
    let evalue = evaluator.eval_module(statements.as_slice())?;
    println!(">> Evaluated: {:?}", evalue);

    Ok(())
}

use std::fmt::Debug;
pub trait ByLine: Iterator {
    fn by_line(&mut self) -> String
        where Self: Sized, Self::Item: Debug {
        self.map(|x| format!("{:?}", x)).join("\n")
    }
}

impl<I> ByLine for I where I: Iterator {}

fn print_usage(program: &str, opts: Options) {
    let brief = format!("Usage: {} FILES [options]", program);
    print!("{}", opts.usage(&brief));
}

fn file_size_pretty(size: usize) -> String {
    for &(unit, name) in &[
        (usize::pow(2, 30), "GiB"),
        (usize::pow(2, 20), "MiB"),
        (usize::pow(2, 10), "KiB"),
    ] {
        if size >= unit {
            return format!("{:.2} {}", size as f32 / unit as f32, name)
        }
    }
    format!("{} bytes", size)
}
