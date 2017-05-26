use source::*;
use std::fmt::{Display, Debug, Formatter, Result as FmtResult};
use std::str::CharIndices;

#[derive(Debug, PartialEq, Copy, Clone)]
pub enum Token {
    EOF, Id, Number, String, LParen, RParen, Eq, EqEq, Lt, LtEq, Gt, GtEq, Lambda, Minus, Arrow, Plus, Star, Slash
}

#[derive(Copy, Clone)]
pub struct Lexeme<'a> {
    pub token: Token,
    pub source: Source<'a>
}

impl<'a> Lexeme<'a> {
    pub fn text(&self) -> &'a str {
        self.source.text()
    }

    fn new(token: Token, source: Source<'a>) -> Lexeme<'a> {
        Lexeme { token, source }
    }
}

impl<'a> Debug for Lexeme<'a> {
    fn fmt(&self, formatter: &mut Formatter) -> FmtResult {
        formatter.write_str(&format!("{} ({:?}) | {}", self.text(), self.token, self.source.segment.start))
    }
}

impl<'a> Display for Lexeme<'a> {
    fn fmt(&self, formatter: &mut Formatter) -> FmtResult {
        formatter.write_str(if self.token == Token::EOF { "end of file" } else { self.text() })
    }
}

#[derive(Copy, Clone)]
pub struct LexerStats {
    pub byte_count: usize,
    pub line_count: usize,
    pub lexeme_count: usize,
}

pub struct Lexer<'a> {
    source_file: &'a SourceFile<'a>,
    source_iterator: CharIndices<'a>,
    location: Location,
    lexeme_start: Location,
    last_char: Option<char>,
    is_first_char: bool,
    stats: LexerStats
}

pub type LexerResult<'a, T> = Result<T, String>;

impl<'a> Lexer<'a> {
    pub fn new(source_file: &'a SourceFile<'a>) -> Lexer<'a> {
        let starting_location = Location::new();
        let mut lexer = Lexer {
            source_file,
            source_iterator: source_file.text.char_indices(),
            location: starting_location,
            lexeme_start: starting_location,
            last_char: None,
            is_first_char: true,
            stats: LexerStats {
                byte_count: 0,
                line_count: 1,
                lexeme_count: 0
            }
        };
        lexer.read_char();
        lexer
    }

    pub fn read(&mut self) -> LexerResult<'a, Lexeme<'a>> {
        self.skip_whitepace();
        self.lexeme_start = self.location;
        match self.last_char {
            Some(c) => {
                for matcher in &[ Lexer::read_number, Lexer::read_operator, Lexer::read_string, Lexer::read_id ] {
                    if let Ok(Some(lexeme)) = matcher(self) {
                        self.stats.lexeme_count += 1;
                        return Ok(lexeme)
                    }
                }
                Err(format!("unexpected character: {}", c))
            },
            None => Ok(self.new_lexeme(Token::EOF))
        }
    }

    pub fn stats(&self) -> &LexerStats {
        &self.stats
    }

    pub fn make_source(&self, segment: Segment) -> Source<'a> {
        Source {
            file: self.source_file,
            segment
        }
    }

    fn read_id(&mut self) -> LexerResult<'a, Option<Lexeme<'a>>> {
        Ok(match self.last_char {
            Some(c) if c.is_alphabetic() => {
                self.read_char();
                while let Some(c) = self.last_char {
                    if c.is_alphanumeric() || c == '_' {
                        self.read_char()
                    } else {
                        break
                    }
                }
                Some(self.new_lexeme(Token::Id))
            },
            _ => None
        })
    }

    fn read_number(&mut self) -> LexerResult<'a, Option<Lexeme<'a>>> {
        Ok(match self.last_char {
            Some(c) if c.is_numeric() => {
                loop {
                    match self.last_char {
                        Some(c) if c.is_numeric() => self.read_char(),
                        _ => break
                    }
                }
                Some(self.new_lexeme(Token::Number))
            },
            _ => None
        })
    }

    fn read_string(&mut self) -> LexerResult<'a, Option<Lexeme<'a>>> {
        match self.last_char {
            Some('"') => {
            	self.read_char();

                loop {
                    match self.last_char {
                        Some('"') => break,
                        Some(_) => self.read_char(),
                        None => break
                    }
                }

                match self.last_char {
                    Some('"') => {
                        self.read_char();
                        Ok(Some(self.new_lexeme(Token::String)))
                    },
                    _ => {
                        Err(format!("{}: unclosed string starting at {}", self.location, self.lexeme_start))
                    }
                }
            },
            _ => Ok(None)
        }
    }

    fn read_operator(&mut self) -> LexerResult<'a, Option<Lexeme<'a>>> {
        macro_rules! on {
            ($char: tt, $token: expr, $handler: expr) => (
                match self.last_char {
                    Some($char) => {
                        self.read_char();
                        $handler.or(Some($token))
                    },
                    _ => None
                }
            );

            ($char: expr, $token: expr) => { on!($char, $token, None) };
        }

        Ok(None
            .or_else(|| on!('(', Token::LParen))
            .or_else(|| on!(')', Token::RParen))
            .or_else(|| on!('λ', Token::Lambda))
            .or_else(|| on!('\\', Token::Lambda))
            .or_else(|| on!('+', Token::Plus))
            .or_else(|| on!('-', Token::Minus, on!('>', Token::Arrow)))
            .or_else(|| on!('*', Token::Star))
            .or_else(|| on!('/', Token::Slash))
            .or_else(|| on!('=', Token::Eq, on!('=', Token::EqEq)))
            .or_else(|| on!('<', Token::Lt, on!('=', Token::LtEq)))
            .or_else(|| on!('>', Token::Gt, on!('=', Token::GtEq)))
            .or_else(|| on!('→', Token::Arrow))
            .map(|token| self.new_lexeme(token)))
    }

    fn new_lexeme(&self, token: Token) -> Lexeme<'a> {
        let segment = Segment {
            start: self.lexeme_start,
            end: self.location
        };
        Lexeme::new(token, self.make_source(segment))
    }

    fn skip_whitepace(&mut self) {
        while let Some(c) = self.last_char {
            if c.is_whitespace() {
                self.read_char()
            } else {
                break
            }
        }
    }

    fn read_char(&mut self) {
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

impl<'a> Debug for Lexer<'a> {
    fn fmt(&self, formatter: &mut Formatter) -> FmtResult {
        formatter.write_str(
            &format!("Lexer{{ source: {{ {:?}, {} bytes }}, location: {}}}", self.source_file.path, self.source_file.text.len(), self.location))
    }
}
