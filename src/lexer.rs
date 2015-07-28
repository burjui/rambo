use source::Location;
use std::fmt::Display;
use std::fmt::Formatter;
use std::fmt::Result as FmtResult;
use std::str::CharIndices;

#[derive(Debug, PartialEq, Copy, Clone)]
pub enum Kind {
    Id, Number, String, Eq, EqEq, Lt, LtEq, Gt, GtEq, Tilde, Minus, MinusGt, Plus, Star, Slash
}

pub struct Token<'a> {
    pub kind: Kind,
    pub text: &'a str,
    pub location: Location<'a>
}

impl<'a> Token<'a> {
    fn new<'x>(kind: Kind, text: &'a str, location: &Location<'a>) -> Token<'a> {
        Token { kind: kind, text: text, location: location.clone() }
    }
}

impl<'a> Display for Token<'a> {
    fn fmt(&self, formatter: &mut Formatter) -> FmtResult {
        formatter.write_str(&format!("Token{{ {} ({:?}) at {} }}", self.text, self.kind, self.location))
    }
}

pub struct Lexer<'a> {
    source: &'a str,
    source_lines: Vec<&'a str>,
    source_iterator: CharIndices<'a>,
    location: Location<'a>,
    last_char: Option<char>,
    last_char_index: usize,
    is_first_char: bool,
    next_line_to_print: usize
}

pub type LexerResult<'a> = Result<Lexer<'a>, String>;

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
    
    pub fn new(source_name: &'a str, source: &'a str) -> LexerResult<'a> {
        let mut bytes_to_skip: usize = 0;
        for bom in Lexer::byte_order_marks() {
	        if source.as_bytes().starts_with(&bom) {
	        	if bom != Lexer::utf8_byte_order_mark() {
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
            source_iterator: source.char_indices(),
            location: Location::new(source_name),
            last_char: None,
            last_char_index: 0,
            is_first_char: true,
            next_line_to_print: 0
        };
        
        lexer.next();
        Ok(lexer)
    }
    
    pub fn read(&mut self) -> Option<Token> {
    	let current_line = self.location.line_index;
        if current_line >= self.next_line_to_print && current_line < self.source_lines.len() {
            self.next_line_to_print += 1;
//            println!("# {}", self.source_lines[self.location.line]);
        }
        
        self.skip_whitepace();
        let start = self.location;
        
        match self.last_char {
            Some(_) => {
                None
                .or(self.read_id(&start))
                .or(self.read_number(&start))
                .or(self.read_operator(&start))
                .or(self.read_string(&start))
            },
            None => None
        }
    }
    
    fn read_id(&mut self, start: &Location<'a>) -> Option<Token<'a>> {
        match self.last_char {
            Some(c) if c.is_alphabetic() => {
                self.next();
                while let Some(c) = self.last_char {
                    if c.is_alphanumeric() || c == '_' {
                        self.next()
                    } else {
                        break
                    }
                }
                Some(self.new_token(Kind::Id, start))
            },
            _ => None
        }
    }
    
    fn read_number(&mut self, start: &Location<'a>) -> Option<Token<'a>> {
        match self.last_char {
            Some(c) if c.is_numeric() => {
                loop {
                    match self.last_char {
                        Some(c) if c.is_numeric() => self.next(),
                        _ => break
                    }
                }
                Some(self.new_token(Kind::Number, start))
            },
            _ => None
        }
    }
    
    fn read_string(&mut self, start: &Location<'a>) -> Option<Token<'a>> {
        match self.last_char {
            Some('"') => {
            	self.next();
            	
                loop {
                    match self.last_char {
                        Some('"') => break,
                        Some(_) => self.next(),
                        None => break
                    }
                }
                
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
            },
            _ => None
        }
    }
    
    fn read_operator(&mut self, start: &Location<'a>) -> Option<Token<'a>> {
        macro_rules! on {
            ($char: expr, $kind: expr, $handler: expr) => {
                match self.last_char {
                    Some($char) => {
                        self.next();
                        ($handler).or(Some($kind))
                    },
                    _ => None
                }
            };
            
            ($char: expr, $kind: expr) => { on!($char, $kind, None) };
        }
        
        match None
            .or(on!('~', Kind::Tilde))
            .or(on!('+', Kind::Plus))
            .or(on!('*', Kind::Star))
            .or(on!('/', Kind::Slash))
            .or(on!('=', Kind::Eq, on!('=', Kind::EqEq)))
            .or(on!('<', Kind::Lt, on!('=', Kind::LtEq)))
            .or(on!('>', Kind::Gt, on!('=', Kind::GtEq)))
            .or(on!('-', Kind::Minus, on!('>', Kind::MinusGt)))
        {
            Some(kind) => Some(self.new_token(kind, start)),
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
        let last_char = self.source_iterator.next();
        self.last_char = last_char.map(|(_, c)| c);
        self.last_char_index = last_char.map(|(l, _)| l).unwrap_or_default();
        if let Some(c) = self.last_char {
            self.location.offset = self.last_char_index;
            self.is_first_char = false;            

            if c == '\n' {
                self.location.line_index += 1;
                self.location.column_index = 0;
            } else {
                self.location.column_index += 1;
            }
        }
    }
}

impl<'a> Display for Lexer<'a> {
    fn fmt(&self, formatter: &mut Formatter) -> FmtResult {
        formatter.write_str(
            &format!("Lexer{{ source: {}, location: {}}}", self.source.len(), self.location))
    }
}