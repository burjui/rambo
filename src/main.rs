#![feature(associated_consts)]

extern crate getopts;

use std::fmt::Formatter;
use std::fmt::Display;
use std::str::Chars;
use std::str::Lines;
use getopts::Options;
use std::env;
use std::path::Path;
use std::fs::File;
use std::io::Read;


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
        Err(err) => {
            println!("{}", err);
            return
        }
    };
    
    let path_str = path.to_string_lossy();
    let mut lexer = Lexer::new(path_str.as_ref(), &content);
    
//    let start = Location::new("xxxxx");
//    xxxx(&start);
    
    println!("{}", lexer);
    let mut token_count = 0;
    while let Some(token) = lexer.read() {
        println!("{}", token);
        token_count += 1;
    }
    println!("{}", lexer.location);
    println!("{} tokens", token_count);
}

fn print_usage(program: &str, opts: Options) {
    let brief = format!("Usage: {} FILES [options]", program);
    print!("{}", opts.usage(&brief));
}

#[derive(Debug)]
enum Kind {
    Id, Number
}

#[derive(Copy, Clone)]
struct Location<'a> {
    source_name: &'a str,
    offset: usize,
    line: usize,
    column: usize
}

impl<'a> Location<'a> {
    fn new(source_name: &'a str) -> Location<'a> {
        Location { source_name: source_name, offset: 0, line: 1, column: 0 }
    }
}

impl<'a> Display for Location<'a> {
    fn fmt(&self, formatter: &mut Formatter) -> std::fmt::Result {
        formatter.write_str(&format!("{}({}:{})", self.source_name, self.line, self.column))
    }
}

struct Token<'a> {
    kind: Kind,
    text: &'a str,
    location: Location<'a>
}

impl<'a> Token<'a> {
    fn new<'x>(kind: Kind, text: &'a str, location: &'x Location<'a>) -> Token<'a> {
        Token { kind: kind, text: text, location: *location }
    }
}

impl<'a> Display for Token<'a> {
    fn fmt(&self, formatter: &mut Formatter) -> std::fmt::Result {
        formatter.write_str(&format!("Token{{ {} ({:?}) at {} }}",
                self.text, self.kind, self.location))
    }
}

struct Lexer<'a> {
    source: &'a str,
    source_lines: Vec<&'a str>,
    source_iterator: Chars<'a>,
    location: Location<'a>,
    last_char: Option<char>,
    is_first_char: bool
}

impl<'a> Lexer<'a> {
//    #[inline]
//    fn byte_order_marks() -> Vec<Vec<u8>> {
//        vec!(
//            vec!(0x00, 0x00, 0xFE, 0xFF), // UTF-32BE
//            vec!(0xFF, 0xFE, 0x00, 0x00), // UTF-32LE
//            vec!(0xFF, 0xFE),             // UTF-16LE
//            vec!(0xFE, 0xFF),             // UTF-16BE
//            ,       // UTF-8
//        )
//    }
    
    #[inline]
    fn utf8_byte_order_mark() -> Vec<u8> {
        vec!(0xEF, 0xBB, 0xBF)
    }
    
    fn new(source_name: &'a str, source: &'a str) -> Lexer<'a> {
        let mut bytes_to_skip: usize = 0;
        let bom = Lexer::utf8_byte_order_mark();
        if source.as_bytes().starts_with(&bom) {
            bytes_to_skip = bom.len();
        }
        
        let source = &source[bytes_to_skip..];
        let mut lexer = Lexer {
            source: source,
            source_lines: source.lines().collect(),
            source_iterator: source.chars(),
            location: Location::new(source_name),
            last_char: None,
            is_first_char: true
        };
        
        lexer.next();
        lexer
    }
    
    fn read<'x>(&'x mut self) -> Option<Token> {
        self.skip_whitepace();
        let start = self.location;
        
        let id = self.read_id(&start);
        id
//        if id.is_some() {
//            return id
//        }
//        
//        while let Some(c) = self.last_char {
//            if c.is_whitespace() {
//                break
//            } else {
//                self.next()
//            }
//        }
//        
//        if self.location.offset > start.offset {
//            Some(Token::new(Kind::Id, self.source_from(start.offset), &start))
//        } else {
//            None
//        }
    }
    
    fn read_id<'x, 'b>(&'b mut self, start: &'x Location<'b>) -> Option<Token> {
        match self.last_char {
            Some(c) => {
                if c.is_alphabetic() {
                    self.next();
                    while let Some(c) = self.last_char {
                        if c.is_alphanumeric() || c == '_' {
                            self.next()
                        } else {
                            break
                        }
                    }
                    Some(Token::new(Kind::Id, self.source_from(start.offset), start))
                } else {
                    None
                }
            },
            None => None
        }
    }
    
    fn source_from(&self, start: usize) -> &str {
        &self.source[start..self.location.offset]
    }
    
    fn skip_whitepace(&mut self) {
        while let Some(c) = self.last_char {
            if c.is_whitespace() {
                self.next()
            } else {
                break
            }
        }
    }
    
    fn next(&mut self) {
        self.last_char = self.source_iterator.next();
        if let Some(c) = self.last_char {
            if !self.is_first_char {
                self.location.offset += 1
            }
            self.is_first_char = false;            

            if c == '\n' {
                self.location.line += 1;
                self.location.column = 1;
            } else {
                self.location.column += 1;
            }
        }
    }
}

impl<'a> Display for Lexer<'a> {
    fn fmt(&self, formatter: &mut Formatter) -> std::fmt::Result {
        formatter.write_str(
            &format!("Lexer{{ source: {}, location: {}}}", self.source.len(), self.location))
    }
}