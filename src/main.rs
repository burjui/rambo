#![feature(associated_consts)]

extern crate getopts;

use std::fmt::Formatter;
use std::fmt::Display;
use std::str::Chars;
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
    let mut token_count = 0;
    while let Some(token) = lexer.read() {
        println!("{}", token);
        token_count += 1;
    }
    
    println!(">> done: {}", lexer.location.source_name);
    println!(">> {} tokens", token_count);
}

fn print_usage(program: &str, opts: Options) {
    let brief = format!("Usage: {} FILES [options]", program);
    print!("{}", opts.usage(&brief));
}

#[derive(Debug, PartialEq)]
enum Kind {
    Id, Number, String, Eq, EqEq, Lt, LtEq, Gt, GtEq, Tilde, Minus, Plus, MinusGt, Star, Slash
}

#[derive(Copy, Clone)]
struct Location<'a> {
    source_name: &'a str,
    offset: usize,
    line: usize,
    column: usize
}

impl<'a> Location<'a> {
    fn new(source_name: &str) -> Location {
        Location { source_name: source_name, offset: 0, line: 0, column: 0 }
    }
}

impl<'a> Display for Location<'a> {
    fn fmt(&self, formatter: &mut Formatter) -> std::fmt::Result {
        formatter.write_str(&format!("{}({}:{})", self.source_name, self.line + 1, self.column + 1))
    }
}

struct Token<'a> {
    kind: Kind,
    text: &'a str,
    location: Location<'a>
}

impl<'a> Token<'a> {
    fn new<'x>(kind: Kind, text: &'a str, location: &Location<'a>) -> Token<'a> {
        Token { kind: kind, text: text, location: location.clone() }
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
    is_first_char: bool,
    next_line_to_print: usize
}

struct LexerError<'a> {
    location: Location<'a>,
    message: &'a str
}

type LexerResult<'a> = Result<Lexer<'a>, String>;

impl<'a> Lexer<'a> {
    #[inline]
    fn byte_order_marks() -> Vec<Vec<u8>> {
        vec!(
            vec!(0x00, 0x00, 0xFE, 0xFF),  // UTF-32BE
            vec!(0xFF, 0xFE, 0x00, 0x00),  // UTF-32LE
            vec!(0xFF, 0xFE),              // UTF-16LE
            vec!(0xFE, 0xFF),              // UTF-16BE
            Lexer::utf8_byte_order_mark(), // UTF-8
        )
    }
    
    #[inline]
    fn utf8_byte_order_mark() -> Vec<u8> {
        vec!(0xEF, 0xBB, 0xBF)
    }
    
    fn new(source_name: &'a str, source: &'a str) -> LexerResult<'a> {
        let mut bytes_to_skip: usize = 0;
        for bom in Lexer::byte_order_marks() {
	        if source.as_bytes().starts_with(&bom) {
	        	if bom != Lexer::utf8_byte_order_mark() {
	        		// TODO: return LexerError instead
	        		return Err(format!("error: {}: only UTF-8 encoding is supported", source_name))
	        	}
	            bytes_to_skip = bom.len();
	            break;
	        }
        }
        
        let source = &source[bytes_to_skip..];
        let mut lexer = Lexer {
            source: source,
            source_lines: source.lines().collect(),
            source_iterator: source.chars(),
            location: Location::new(source_name),
            last_char: None,
            is_first_char: true,
            next_line_to_print: 0
        };
        
        lexer.next();
        Ok(lexer)
    }
    
    fn read(&mut self) -> Option<Token> {
    	let current_line = self.location.line;
        if current_line >= self.next_line_to_print && current_line < self.source_lines.len() {
            self.next_line_to_print += 1;
            println!("# {}", self.source_lines[self.location.line]);
        }
        
        self.skip_whitepace();
        let start = self.location;
        
        match self.last_char {
            Some(c) => {
//                let parsed =
                    None
                    .or(self.read_id(&start, c))
                    .or(self.read_number(&start, c))
                    .or(self.read_operator(&start, c))
                    .or(self.read_string(&start, c))
//                match parsed {
//                    Some(token) => Ok(parsed),
//                    None => LexerError {
//                        location: self.location,
//                        message: format!("unknown token starting with '{}'", c)
//                    }
//                }
            },
            None => None
        }
    }
    
    fn read_id(&mut self, start: &Location<'a>, first_char: char) -> Option<Token<'a>> {
        if first_char.is_alphabetic() {
            self.next();
            while let Some(c) = self.last_char {
                if c.is_alphanumeric() || c == '_' {
                    self.next()
                } else {
                    break
                }
            }
            Some(self.new_token(Kind::Id, start))
        } else {
            None
        }
    }
    
    fn read_number(&mut self, start: &Location<'a>, first_char: char) -> Option<Token<'a>> {
        if first_char.is_numeric() {
            loop { match self.last_char {
                Some(c) if c.is_numeric() => self.next(),
                _ => break
            }}
            Some(self.new_token(Kind::Number, start))
        } else {
            None
        }
    }
    
    fn read_string(&mut self, start: &Location<'a>, first_char: char) -> Option<Token<'a>> {
        if first_char == '"' {
        	self.next();
        	
            loop { match self.last_char {
                Some('"') => break,
                Some(_) => self.next(),
                None => break
            }}
            
            match self.last_char {
                Some('"') => {
                    self.next();
                    Some(self.new_token(Kind::String, start))
                },
                _ => {
                    println!("error: {}: unclosed string starting at {}", self.location, start);
                    None
                }
            }
        } else {
            None
        }
    }
    
    fn read_operator(&mut self, start: &Location<'a>, first_char: char) -> Option<Token<'a>> {
        match first_char {
            '=' | '~' | '<' | '>' | '-' | '+' | '*' | '/' => {
                self.next();
                
                let kind = match first_char {
                    '=' => match self.last_char {
                        Some('=') => {
                            self.next();
                            Kind::EqEq
                        },
                        
                        _ => Kind::Eq
                    },
                    
                    '~' => Kind::Tilde,
                    
                    '<' | '>' => {
                        let one_char_kind = if first_char == '<' { Kind::Lt } else { Kind::Gt };
                        match self.last_char {
                            Some('=') => {
                                self.next();
                                if one_char_kind == Kind::Lt { Kind::LtEq } else { Kind::GtEq }
                            },
                            
                            _ => one_char_kind
                        }
                    },
                    
                    '-' => match self.last_char {
                        Some('>') => {
                            self.next();
                            Kind::MinusGt
                        },
                        
                        _ => Kind::Minus
                    },
                    
                    '+' => Kind::Plus,
                    '*' => Kind::Star,
                    '/' => Kind::Slash,
                    _ => return None
                };
                
                Some(self.new_token(kind, start))
            },
            _ => None
        }
    }
    
    fn new_token(&self, kind: Kind, start: &Location<'a>) -> Token<'a> {
        Token::new(kind, self.source_from(start), start)
    }
    
    fn source_from(&self, start: &Location) -> &'a str {
        &self.source[start.offset..self.location.offset]
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
                self.location.column = 0;
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