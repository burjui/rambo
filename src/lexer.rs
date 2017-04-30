use source::Location;
use std::fmt::Display;
use std::fmt::Formatter;
use std::fmt::Result as FmtResult;
use std::str::CharIndices;
use std::cmp::Ordering;

#[derive(Debug, PartialEq, Copy, Clone)]
pub enum Kind {
    Id, Number, String, Eq, EqEq, Lt, LtEq, Gt, GtEq, Lambda, Minus, Arrow, Plus, Star, Slash
}

pub struct Token<'a> {
    pub kind: Kind,
    pub text: &'a str,
    pub location: Location<'a>
}

impl<'a> Token<'a> {
    fn new(kind: Kind, text: &'a str, location: &Location<'a>) -> Token<'a> {
        Token { kind: kind, text: text, location: location.clone() }
    }
}

impl<'a> Display for Token<'a> {
    fn fmt(&self, formatter: &mut Formatter) -> FmtResult {
        formatter.write_str(&format!("{} ({:?}), {}", self.text, self.kind, self.location))
    }
}

pub struct LexerStats {
    pub byte_count: usize,
    pub line_count: usize,
    pub token_count: usize,
}

pub struct Lexer<'a> {
    source: &'a str,
    source_iterator: CharIndices<'a>,
    location: Location<'a>,
    token_start: Location<'a>,
    last_char: Option<char>,
    is_first_char: bool,
    stats: LexerStats
}

pub type LexerResult<'a> = Result<Lexer<'a>, String>;

impl<'a> Lexer<'a> {
    const UTF8_BOM: [u8; 3] = [0xEF, 0xBB, 0xBF];
    const BYTE_ORDER_MARKS: &'static[&'static[u8]] = &[
        &[0x00, 0x00, 0xFE, 0xFF],  // UTF-32BE
        &[0xFF, 0xFE, 0x00, 0x00],  // UTF-32LE
        &[0xFF, 0xFE],              // UTF-16LE
        &[0xFE, 0xFF],              // UTF-16BE
        &Lexer::UTF8_BOM,
    ];

    pub fn new(source_name: &'a str, source: &'a str) -> LexerResult<'a> {
        let mut bom_length: usize = 0;
        for bom in Lexer::BYTE_ORDER_MARKS {
	        if source.as_bytes().starts_with(&bom) {
	        	if (&bom[..]).cmp(&Lexer::UTF8_BOM[..]) != Ordering::Equal {
	        		return Err(format!("error: {}: only UTF-8 encoding is supported", source_name))
	        	} else {
                    bom_length = bom.len();
                    break;
                }
	        }
        }

        let mut lexer = Lexer::make_lexer(source_name, &source[bom_length..], bom_length);
        lexer.next();
        Ok(lexer)
    }

    pub fn read(&mut self) -> Result<Option<Token<'a>>, String> {
        self.skip_whitepace();
        self.token_start = self.location;
        match self.last_char {
            Some(c) => {
                None
                .or(self.read_number())
                .or(self.read_operator())
                .or(self.read_string())
                .or(self.read_id())
                .map_or_else(
                    || Err(format!("unrecognized character: {}", c)),
                    |token| {
                        self.stats.token_count += 1;
                        Ok(Some(token))
                    })
            },
            None => Ok(None)
        }
    }

    pub fn stats(&self) -> &LexerStats {
        &self.stats
    }

    fn make_lexer(source_name: &'a str, source: &'a str, bom_length: usize) -> Lexer<'a> {
        let starting_location = Location::new(source_name);
        Lexer {
            source: source,
            source_iterator: source.char_indices(),
            location: starting_location,
            token_start: starting_location,
            last_char: None,
            is_first_char: true,
            stats: LexerStats {
                byte_count: bom_length,
                line_count: 1,
                token_count: 0
            }
        }
    }

    fn read_id(&mut self) -> Option<Token<'a>> {
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
                Some(self.new_token(Kind::Id))
            },
            _ => None
        }
    }

    fn read_number(&mut self) -> Option<Token<'a>> {
        match self.last_char {
            Some(c) if c.is_numeric() => {
                loop {
                    match self.last_char {
                        Some(c) if c.is_numeric() => self.next(),
                        _ => break
                    }
                }
                Some(self.new_token(Kind::Number))
            },
            _ => None
        }
    }

    fn read_string(&mut self) -> Option<Token<'a>> {
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
                        Some(self.new_token(Kind::String))
                    },
                    _ => {
                        println!("error: {}: unclosed string starting at {}", self.location, self.token_start);
                        None
                    }
                }
            },
            _ => None
        }
    }

    fn read_operator(&mut self) -> Option<Token<'a>> {
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
            .or(on!('λ', Kind::Lambda))
            .or(on!('\\', Kind::Lambda))
            .or(on!('+', Kind::Plus))
            .or(on!('-', Kind::Minus, on!('>', Kind::Arrow)))
            .or(on!('*', Kind::Star))
            .or(on!('/', Kind::Slash))
            .or(on!('=', Kind::Eq, on!('=', Kind::EqEq)))
            .or(on!('<', Kind::Lt, on!('=', Kind::LtEq)))
            .or(on!('>', Kind::Gt, on!('=', Kind::GtEq)))
            .or(on!('→', Kind::Arrow))
        {
            Some(kind) => Some(self.new_token(kind)),
            _ => None
        }
    }

    fn new_token(&self, kind: Kind) -> Token<'a> {
        Token::new(kind, self.source_from(&self.token_start), &self.token_start)
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
        if let Some((offset, c)) = last_char {
            self.location.offset = offset;
            self.stats.byte_count += c.len_utf8();

            if c == '\n' {
                self.location.line_index += 1;
                self.location.column_index = 0;
                self.stats.line_count += 1;
            } else if !self.is_first_char {
                self.location.column_index += 1;
            }

            self.is_first_char = false;
        }
        self.last_char = last_char.map(|(_, c)| c);
    }
}

impl<'a> Display for Lexer<'a> {
    fn fmt(&self, formatter: &mut Formatter) -> FmtResult {
        formatter.write_str(
            &format!("Lexer{{ source: {}, location: {}}}", self.source.len(), self.location))
    }
}