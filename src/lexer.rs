use source::Location;
use std::fmt::Display;
use std::fmt::Formatter;
use std::fmt::Result as FmtResult;
use std::str::CharIndices;

#[derive(Debug, PartialEq)]
pub enum Kind {
    Id, Number, String, Eq, EqEq, Lt, LtEq, Gt, GtEq, Tilde, Minus, Plus, MinusGt, Star, Slash
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
    last_char: Option<(usize, char)>,
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
            Some((_, c)) => {
                None
                .or(self.read_id(&start, c))
                .or(self.read_number(&start, c))
                .or(self.read_operator(&start, c))
                .or(self.read_string(&start, c))
            },
            None => None
        }
    }
    
    fn read_id(&mut self, start: &Location<'a>, first_char: char) -> Option<Token<'a>> {
        if first_char.is_alphabetic() {
            self.next();
            while let Some((_, c)) = self.last_char {
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
                Some((_, c)) if c.is_numeric() => self.next(),
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
                Some((_, '"')) => break,
                Some(_) => self.next(),
                None => break
            }}
            
            match self.last_char {
                Some((_, '"')) => {
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
                        Some((_, '=')) => {
                            self.next();
                            Kind::EqEq
                        },
                        
                        _ => Kind::Eq
                    },
                    
                    '~' => Kind::Tilde,
                    
                    '<' | '>' => {
                        let one_char_kind = if first_char == '<' { Kind::Lt } else { Kind::Gt };
                        match self.last_char {
                            Some((_, '=')) => {
                                self.next();
                                if one_char_kind == Kind::Lt { Kind::LtEq } else { Kind::GtEq }
                            },
                            
                            _ => one_char_kind
                        }
                    },
                    
                    '-' => match self.last_char {
                        Some((_, '>')) => {
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
        while let Some((_, c)) = self.last_char {
            if c.is_whitespace() {
                self.next()
            } else {
                break
            }
        }
    }
    
    fn next(&mut self) {
        self.last_char = self.source_iterator.next();
        if let Some((index, c)) = self.last_char {
            self.location.offset = index;
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