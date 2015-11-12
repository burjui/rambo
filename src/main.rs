#![feature(associated_consts)]
#![feature(box_syntax)]
#![feature(box_patterns)]

extern crate getopts;

use getopts::Options;
use std::env;
use std::path::Path;
use std::fs::File;
use std::io::Read;

mod source;
mod lexer;
mod parser;
mod class;
mod report;
mod ast;

use lexer::Lexer;
use parser::Parser;

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
    }
    
    for file in matches.free {
        process(Path::new(&file));
    }
}

fn process(path: &Path) {
    println!(">> Processing {}...", path.to_string_lossy());
    
    let mut file = match File::open(&path) {
        Ok(f) => f,
        Err(err) => {
            println!("{}", err);
            return
        }
    };
    
    let mut content = String::new();
    match file.read_to_string(&mut content) {
        Ok(_) => {},
        Err(message) => {
            println!("{}", message);
            return
        }
    };
    
    let path_str_owned = path.to_string_lossy();
    let path_str = path_str_owned.as_ref();
    let mut lexer = match Lexer::new(path_str, &content) {
    	Ok(lexer) => lexer,
    	Err(message) => {
    		println!("{}: {}", path_str, message);
    		return;
    	}
    };
    
    println!("{}", lexer);
//    let mut token_count = 0;
//    let mut last_line_index = 0;
//    while let Some(token) = lexer.read() {
//        token_count += 1;
//        last_line_index = token.location.line_index;
////        println!("{}", token);
//    }
//    
//    println!(">> {} tokens, {} lines", token_count, last_line_index + 1);
    
    let mut parser = Parser::new(lexer);
    let ast = parser.parse();
    match ast {
        Ok(expr) => println!(">> {}", expr),
        Err(error) => println!("error: {}: {}", error.location, error.message)
    };
}

fn print_usage(program: &str, opts: Options) {
    let brief = format!("Usage: {} FILES [options]", program);
    print!("{}", opts.usage(&brief));
}

