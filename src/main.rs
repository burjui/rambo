#![feature(crate_visibility_modifier)]
#![warn(rust_2018_idioms)]
#![feature(box_syntax)]
#![feature(box_patterns)]
#![feature(min_const_fn)]
#![feature(transpose_result)]

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
use itertools::join;

#[derive(PartialOrd, PartialEq, Copy, Clone)]
// TODO separate types
enum Pass {
    Load, Parse, Semantics, CFG, RedundantBindings1, CFP1, RedundantBindings2, CFP2, CFGOptimized, Eval
}

impl Pass {
    const fn all() -> &'static [Pass] {&[
        Pass::Load,
        Pass::Parse,
        Pass::Semantics,
        Pass::CFG,
        Pass::RedundantBindings1,
        Pass::CFP1,
        Pass::RedundantBindings2,
        Pass::CFP2,
        Pass::CFGOptimized,
        Pass::Eval
    ]}

    fn name(&self) -> &str {
        match self {
            Pass::Load => "load",
            Pass::Parse => "parse",
            Pass::Semantics => "sem",
            Pass::CFG => "cfg",
            Pass::RedundantBindings1 => "rb1",
            Pass::CFP1 => "cfp1",
            Pass::RedundantBindings2 => "rb2",
            Pass::CFP2 => "cfp2",
            Pass::CFGOptimized => "cfgopt",
            Pass::Eval => "eval"
        }
    }
}

#[derive(Clone)]
struct ProcessOptions {
    program_name: String,
    warnings: bool,
    dump_intermediate: bool,
    dump_cfg: bool,
    max_pass: Pass
}

fn main() {
    let result = parse_command_line()
        .and_then(|(input_files, options)|
            if input_files.is_empty() {
                println!("{}: no input files", options.program_name);
                std::process::exit(1)
            } else {
                input_files.iter()
                    .map(|path| process(path, &options))
                    .collect::<Result<(), Box<dyn Error>>>()
            });
    match result {
        Ok(_) => {},
        Err(error) => println!("error: {}", error)
    }
}

fn parse_command_line() -> Result<(Vec<String>, ProcessOptions), Box<dyn Error>> {
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

    let pass_name_list: String = join(Pass::all().iter().map(Pass::name), ", ");
    spec.optopt(PASS_OPTION, "pass", &format!("last compiler pass to be executed;\nfollowing pass names are recognized: {}", pass_name_list), "PASS");

    let matches = spec.parse(&args[1..])?;
    let program_name = &args[0];
    if matches.opt_present(HELP_OPTION) {
        let brief = format!("Usage: {} [options] file...", program_name);
        print!("{}", spec.usage(&brief));
        std::process::exit(0);
    }
    let options = ProcessOptions {
        program_name: args[0].clone(),
        warnings: !matches.opt_present(WARNINGS_OPTION),
        dump_intermediate: matches.opt_present(DUMP_OPTION),
        dump_cfg: matches.opt_present(DUMP_CFG_OPTION),
        max_pass: {
            *matches.opt_str(PASS_OPTION)
                .map(|max_pass_name|
                    Pass::all().iter()
                        .find(|pass| pass.name() == max_pass_name)
                        .ok_or_else(|| format!("invalid pass name: {}", max_pass_name)))
                .unwrap_or(Ok(&Pass::Eval))?
        }
    };
    let input_files = matches.free;
    Ok((input_files, options))
}

struct Pipeline<T> {
    max_pass: Pass,
    data: Option<T>
}

impl<T> Pipeline<T> {
    fn new(max_pass: Pass, data: Option<T>) -> Self {
        Self {
            max_pass,
            data
        }
    }

    fn map<U, F: FnOnce(T) -> U>(self, pass: Pass, f: F) -> Pipeline<U> {
        let (max_pass, data) = self.map_internal(pass, f);
        Pipeline::new(max_pass, data)
    }

    fn map_result<U, F>(self, pass: Pass, f: F) -> Result<Pipeline<U>, Box<dyn Error>>
        where F: FnOnce(T) -> Result<U, Box<dyn Error>>
    {
        let (max_pass, data) = self.map_internal(pass, f);
        data.transpose().map(|data| Pipeline::new(max_pass, data))
    }

    fn map_internal<U, F: FnOnce(T) -> U>(self, pass: Pass, f: F) -> (Pass, Option<U>) {
        let max_pass = self.max_pass;
        (max_pass, self.data
            .filter(|_| max_pass >= pass)
            .map(|x| f(x)))
    }
}

impl<T> Pipeline<Result<T, Box<dyn Error>>> {
    fn result(self) -> Result<Option<T>, Box<dyn Error>> {
        self.data.transpose()
    }
}

fn process(path: &str, options: &ProcessOptions) -> Result<(), Box<dyn Error>> {
    println!(">> Processing {}...", path); // TODO colored output using termcolor
    let source_file = load_source_file_pass(path)?;
    let mut rb_pass_count: u8 = 0;
    let mut cfp_pass_count: u8 = 0;
    Pipeline::new(options.max_pass, Some(source_file))
        .map_result(Pass::Parse, |source_file| parse_source_file_pass(source_file, options))?
        .map_result(Pass::Semantics, |ast| semantic_check_pass(ast, options))?
        .map(Pass::CFG, |hir| construct_cfg_pass(hir, "cfg.dot", "", options))
        .map(Pass::RedundantBindings1, |hir| redundant_bindings_pass(hir, &mut rb_pass_count, options))
        .map(Pass::CFP1, |hir| cfp_pass(hir, &mut cfp_pass_count, options))
        .map(Pass::RedundantBindings2, |hir| redundant_bindings_pass(hir, &mut rb_pass_count, &ProcessOptions {
            warnings: false,
            ..options.clone()
        }))
        .map(Pass::CFP2, |hir| cfp_pass(hir, &mut cfp_pass_count, options))
        .map(Pass::CFGOptimized, |hir| construct_cfg_pass(hir, "cfg-optimized.dot", " (optimized)", options))
        .map(Pass::Eval, |hir| eval_pass(hir))
        .result()
        .map(|_| ())
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

fn file_size_pretty(size: usize) -> String {
    [(usize::pow(2, 30), "G"),
     (usize::pow(2, 20), "M"),
     (usize::pow(2, 10), "K")]
    .iter()
    .find(|(unit, _)| size >= *unit)
    .map(|&(unit, name)| format!("{:.2} {}iB", size as f32 / unit as f32, name))
    .unwrap_or_else(|| format!("{} bytes", size))
}
