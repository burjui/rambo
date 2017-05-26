#![feature(associated_consts)]
#![feature(box_syntax)]
#![feature(box_patterns)]

extern crate getopts;
extern crate itertools;
extern crate num;

use getopts::Options;
use std::env;
use std::path::Path;
use std::fs::File;
use std::io::Read;
use std::error::Error;
use itertools::Itertools;

mod source;
mod lexer;
mod parser;

use source::*;
use lexer::Lexer;
use parser::*;

fn main() {
    let args: Vec<String> = env::args().collect();
    let mut opts = Options::new();
    opts.optflag("h", "help", "print this help menu");

    let matches = match opts.parse(&args[1..]) {
        Ok(m) => { m }
        Err(f) => { panic!(f.to_string()) }
    };

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
        path: path,
        text: &text,
        line_offsets: vec![]
    }.skip_byte_order_mark()?;
    let lexer = Lexer::new(&source_file);
    println!("{:?}", lexer);
    let mut parser = Parser::new(lexer);
    let entities = parser.parse()?;
    println!(">> Parsed:\n{}", entities.iter().map(|x| format!("{:?}", x)).join("\n"));
    let node_count: usize = entities.iter().map(Entity::node_count).sum();
    println!(">> {} nodes", node_count);
    let stats = { parser.lexer_stats() };
    println!(">> {}, {} lines, {} lexemes", file_size_pretty(bom_length + stats.byte_count), stats.line_count, stats.lexeme_count);
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
            text: &self.text[bom_length..],
            line_offsets: self.line_offsets.clone()
        };
        Ok((result, bom_length))
    }
}

impl<'a> Entity<'a> {
    fn node_count(&self) -> usize {
        match self {
            &Entity::Expr(ref expr) => expr.node_count(),
            &Entity::Binding { ref value, .. } => value.node_count()
        }
    }
}

impl<'a> Expr<'a> {
    fn node_count(&self) -> usize {
        match self {
            &Expr::Int {..} | &Expr::String(_) | &Expr::Id(_) => 1,
            &Expr::Lambda { ref args, ref body } => sum_node_count(&mut args.iter()) + body.node_count(),
            &Expr::Binary { ref left, ref right, .. } => left.node_count() + right.node_count(),
            &Expr::Application { ref function, ref args } => function.node_count() + sum_node_count(&mut args.iter()),
        }
    }
}

fn sum_node_count(iterator: &mut Iterator<Item = &Expr>) -> usize {
    iterator.map(Expr::node_count).sum()
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
