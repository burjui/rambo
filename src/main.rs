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

mod source;
mod lexer;
mod parser;

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
        return;
    }

    if matches.free.is_empty() {
        print_usage(&args[0], opts);
    } else {
        for file in matches.free {
            match process(Path::new(&file)) {
                Ok(_) => {},
                Err(error) => println!("error: {}", error.description())
            }
        }
    }
}

fn process(path: &Path) -> Result<(), Box<Error>> {
    println!(">> Processing {}...", path.to_string_lossy());

    let mut file = File::open(&path)?;
    let mut content = String::new();
    file.read_to_string(&mut content)?;
    let path_str_owned = path.to_string_lossy();
    let path_str = path_str_owned.as_ref();
    let lexer = Lexer::new(path_str, &content)?;

    println!("{}", lexer);
    let mut parser = Parser::new(lexer);
    {
        let entities = parser.parse()?;
        println!(">> {:?}", entities);
        let node_count: usize = entities.iter().map(Entity::node_count).sum();
        println!(">> {} nodes", node_count);
    }

    let stats = { parser.lexer_stats() };
    println!(">> {}, {} lines, {} tokens", file_size_pretty(stats.byte_count), stats.line_count, stats.token_count);

    Ok(())
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
            &Expr::Int(_) | &Expr::String(_) | &Expr::Id(_) => 1,
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
