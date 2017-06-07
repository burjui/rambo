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

use getopts::Options;
use std::env;
use std::path::Path;
use std::fs::File;
use std::io::Read;
use std::error::Error;
use itertools::Itertools;

use source::*;
use lexer::Lexer;
use parser::*;
use eval::*;
use semantics::*;

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
    let text = read_file(path)?;
    let (source_file, bom_length) = SourceFile {
        path: &path,
        text: &text
    }.skip_byte_order_mark()?;
    let lexer = Lexer::new(&source_file);
    println!("{:?}", lexer);
    let mut parser = Parser::new(lexer);
    let entities = parser.parse()?;
    println!(">> AST:\n{}", entities.iter().map(|x| format!("{:?}", x)).join("\n"));
    let stats = { parser.lexer_stats() };
    println!(">> {}, {} lines, {} lexemes", file_size_pretty(bom_length + stats.byte_count), stats.line_count, stats.lexeme_count);
    let semantics = Semantics::new();
    let typed_entities = semantics.check_module(entities.as_slice())?;
    println!(">> Semantic check:\n{}", typed_entities.iter().map(|x| format!("# {:?}", x)).join("\n"));
    let mut evaluator = Evaluator::new();
    let evalue = evaluator.eval_module(typed_entities.as_slice())?;
    println!(">> Evaluated: {:?}", evalue);
    Ok(())
}

fn read_file(path: &Path) -> Result<String, Box<Error>> {
    let mut file = File::open(&path)?;
    let mut text = String::new();
    file.read_to_string(&mut text)?;
    Ok(text)
}

impl<'a> SourceFile<'a> {
    fn skip_byte_order_mark(&self) -> Result<(SourceFile<'a>, usize), String> {
        const UTF8_BOM: [u8; 3] = [0xEF, 0xBB, 0xBF];
        const BOMS: &[(&[u8], &str)] = &[
            (&[0x00, 0x00, 0xFE, 0xFF], "UTF-32BE"),
            (&[0xFF, 0xFE, 0x00, 0x00], "UTF-32LE"),
            (&[0xFF, 0xFE], "UTF-16LE"),
            (&[0xFE, 0xFF], "UTF-16BE"),
            (&UTF8_BOM, "UTF8")
        ];
        let text_bytes = self.text.as_bytes();
        let bom_length = BOMS.iter()
            .find(|&&(bom, _)| text_bytes.starts_with(bom))
            .map_or(Ok(0), |&(bom, bom_name)|
                if bom == UTF8_BOM {
                    Ok(bom.len())
                } else {
                    Err(format!("{:?}: this file has a {} byte order mark at the beginning, \
                                but only UTF-8 encoding is supported", self.path, bom_name))
                }
            )?;
        let result = SourceFile {
            path: self.path,
            text: &self.text[bom_length..]
        };
        Ok((result, bom_length))
    }
}

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
