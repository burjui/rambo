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
use std::io::Write;
use termcolor::{Color, ColorChoice, ColorSpec, StandardStream, WriteColor};

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

type PipelineData<'a, T> = (T, &'a mut StandardStream);

struct Pipeline<'a, T> {
    max_pass: Pass,
    data: Option<PipelineData<'a, T>>
}

impl<'a, T> Pipeline<'a, T> {
    fn new(max_pass: Pass, data: Option<(T, &'a mut StandardStream)>) -> Self {
        Self {
            max_pass,
            data
        }
    }

    fn map<U, F>(self, pass: Pass, f: F) -> Result<Pipeline<'a, U>, Box<dyn Error>>
        where F: FnOnce(T, &'a mut StandardStream) -> Result<PipelineData<'a, U>, Box<dyn Error>>
    {
        let max_pass = self.max_pass;
        self.data
            .filter(|_| max_pass >= pass)
            .map(|(data, stdout)| f(data, stdout))
            .transpose()
            .map(|data| Pipeline::new(max_pass, data))
    }
}

fn process(path: &str, options: &ProcessOptions) -> Result<(), Box<dyn Error>> {
    // TODO use isatty() to choose coloring scheme
    let mut stdout = StandardStream::stdout(ColorChoice::Auto);
    let mut rb_pass_count: u8 = 0;
    let mut cfp_pass_count: u8 = 0;
    Pipeline::new(options.max_pass, Some((path, &mut stdout)))
        .map(Pass::Load, |path, stdout| load_source_file_pass(path, stdout))?
        .map(Pass::Parse, |source_file, stdout| parse_source_file_pass(source_file, stdout, options))?
        .map(Pass::Semantics, |ast, stdout| semantic_check_pass(ast, stdout, options))?
        .map(Pass::CFG, |hir, stdout| construct_cfg_pass(hir, stdout, "cfg.dot", "", options))?
        .map(Pass::RedundantBindings1, |hir, stdout| redundant_bindings_pass(hir, stdout, &mut rb_pass_count, options))?
        .map(Pass::CFP1, |hir, stdout| cfp_pass(hir, stdout, &mut cfp_pass_count, options))?
        .map(Pass::RedundantBindings2, |hir, stdout| redundant_bindings_pass(hir, stdout, &mut rb_pass_count, &ProcessOptions {
            warnings: false,
            ..options.clone()
        }))?
        .map(Pass::CFP2, |hir, stdout| cfp_pass(hir, stdout, &mut cfp_pass_count, options))?
        .map(Pass::CFGOptimized, |hir, stdout| construct_cfg_pass(hir, stdout, "cfg-optimized.dot", " (optimized)", options))?
        .map(Pass::Eval, |hir, stdout| eval_pass(hir, stdout)).map(|_| ())
}

trait StandardStreamUtils {
    fn write_title(&mut self, title: &str) -> std::io::Result<()>;
    fn write_colored(&mut self, text: &str, color: Color) -> std::io::Result<()>;
}

impl StandardStreamUtils for StandardStream {
    fn write_title(&mut self, title: &str) -> std::io::Result<()> {
        self.write_colored(":: ", Color::Green)?;
        self.write_colored(title, Color::Yellow)?;
        writeln!(self);
        self.reset()
    }

    fn write_colored(&mut self, text: &str, color: Color) -> std::io::Result<()> {
        self.set_color(ColorSpec::new()
            .set_fg(Some(color))
            .set_bold(true)
        )?;
        write!(self, "{}", text)
    }
}

fn load_source_file_pass<'a>(path: &str, stdout: &'a mut StandardStream) -> Result<(SourceFile, &'a mut StandardStream), Box<dyn Error>> {
    stdout.write_title(&format!("Loading {}", path))?;
    let source_code = SourceFile::read(&path)?;
    let source_code_length = source_code.len();
    let source_file = SourceFile::new(path, &source_code)?;
    let line_count = source_file.lines.len();
    println!("{}, {} lines", file_size_pretty(source_code_length), line_count);
    Ok((source_file, stdout))
}

fn parse_source_file_pass<'a>(source_file: SourceFile, stdout: &'a mut StandardStream, options: &ProcessOptions) -> Result<(Vec<Statement>, &'a mut StandardStream), Box<dyn Error>> {
    stdout.write_title("Parsing")?;
    let lexer = Lexer::new(source_file);
    let mut parser = Parser::new(lexer);
    let ast = parser.parse()?;
    let stats = { parser.lexer_stats() };
    println!("{} lexemes", stats.lexeme_count);
    if options.dump_intermediate {
        println!("-----------\n{}", ast.iter().join_as_strings("\n"));
    }
    Ok((ast, stdout))
}

fn semantic_check_pass<'a>(ast: Vec<Statement>, stdout: &'a mut StandardStream, options: &ProcessOptions) -> Result<(Vec<TypedStatement>, &'a mut StandardStream), Box<dyn Error>> {
    stdout.write_title("Semantic check")?;
    let hir = check_module(ast.as_slice())?;
    if options.dump_intermediate {
        println!("{}", hir.iter().join_as_strings("\n"));
    }
    Ok((hir, stdout))
}

fn construct_cfg_pass<'a>(hir: Vec<TypedStatement>, stdout: &'a mut StandardStream, filename: &str, postfix: &str, options: &ProcessOptions) -> Result<(Vec<TypedStatement>, &'a mut StandardStream), Box<dyn Error>> {
    stdout.write_title(&format!("CFG{}", postfix))?;
    let cfg = construct_cfg(hir.as_slice());
    if options.dump_cfg {
        dump_graph(&cfg, filename);
    }
    Ok((hir, stdout))
}

fn redundant_bindings_pass<'a>(hir: Vec<TypedStatement>, stdout: &'a mut StandardStream, pass_count: &mut u8, options: &ProcessOptions) -> Result<(Vec<TypedStatement>, &'a mut StandardStream), Box<dyn Error>> {
    stdout.write_title(&format!("Redundant bindings (pass {})", *pass_count + 1))?;
    *pass_count += 1;
    let hir = RedundantBindings::remove(hir.as_slice(), if options.warnings { Warnings::On } else { Warnings::Off });
    if options.dump_intermediate {
        println!("{}", hir.iter().join_as_strings("\n"));
    }
    Ok((hir, stdout))
}

fn cfp_pass<'a>(hir: Vec<TypedStatement>, stdout: &'a mut StandardStream, pass_count: &mut u8, options: &ProcessOptions) -> Result<(Vec<TypedStatement>, &'a mut StandardStream), Box<dyn Error>> {
    stdout.write_title(&format!("CFP (pass {})", *pass_count + 1))?;
    *pass_count += 1;
    let mut cfp = CFP::new();
    let hir = cfp.fold_statements(hir.as_slice());
    if options.dump_intermediate {
        println!("{}", hir.iter().join_as_strings("\n"));
    }
    Ok((hir, stdout))
}

fn eval_pass<'a>(hir: Vec<TypedStatement>, stdout: &'a mut StandardStream) -> Result<(Evalue, &'a mut StandardStream), Box<dyn Error>> {
    stdout.write_title("Evaluating")?;
    let mut evaluator = Evaluator::new();
    let evalue = evaluator.eval_module(hir.as_slice())?;
    println!("{:?}", evalue);
    Ok((evalue, stdout))
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
