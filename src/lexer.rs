use source::*;
use std::fmt::{Display, Debug, Formatter, Result as FmtResult};
use std::str::CharIndices;
use std::error::Error;

#[derive(Debug, PartialEq, Copy, Clone)]
pub enum Token {
    EOF, Id, Int, String, LParen, RParen, Eq, EqEq, Lt, LtEq, Gt, GtEq, Lambda, Minus, Arrow, Plus, Star, Slash, Colon
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
}

impl<'a> Debug for Lexeme<'a> {
    fn fmt(&self, formatter: &mut Formatter) -> FmtResult {
        formatter.write_str(&format!("{} | {:?} | [{:?}:{:?}]",
             self.text(), self.token, self.source.start.line, self.source.file.column(&self.source.start)))
    }
}

impl<'a> Display for Lexeme<'a> {
    fn fmt(&self, formatter: &mut Formatter) -> FmtResult {
        formatter.write_str(if self.token == Token::EOF { "end of file" } else { self.text() })
    }
}

#[derive(Copy, Clone)]
pub struct LexerStats {
    pub lexeme_count: usize,
}

pub struct Lexer<'a> {
    file: &'a SourceFile<'a>,
    source_iterator: CharIndices<'a>,
    lexeme_position: Position,
    current_position: Position,
    last_char: Option<char>,
    stats: LexerStats,
    end_position: Position,
    pub eof_lexeme: Lexeme<'a>
}

pub type LexerResult<'a, T> = Result<T, Box<Error>>;

impl<'a> Lexer<'a> {
    pub fn new(file: &'a SourceFile) -> Lexer<'a> {
        let start_position = Position {
            offset: 0,
            line: 0
        };
        let end_position = Position {
            offset: file.text.len(),
            line: file.lines.len() - 1
        };
        let eof_lexeme = Lexeme {
            token: Token::EOF,
            source: Source {
                file,
                start: end_position,
                end: end_position
            }
        };
        let mut lexer = Lexer {
            file,
            source_iterator: file.text.char_indices(),
            lexeme_position: start_position,
            current_position: start_position,
            last_char: None,
            stats: LexerStats {
                lexeme_count: 0
            },
            end_position,
            eof_lexeme
        };
        lexer.read_char();
        lexer
    }

    pub fn read(&mut self) -> LexerResult<'a, Lexeme<'a>> {
        self.skip_whitepace();
        self.lexeme_position = self.current_position;
        match self.last_char {
            Some(c) => {
                for matcher in &[ Lexer::read_int, Lexer::read_operator, Lexer::read_string, Lexer::read_id ] {
                    if let Ok(Some(lexeme)) = matcher(self) {
                        self.stats.lexeme_count += 1;
                        return Ok(lexeme)
                    }
                }
                error!("unexpected character: {}", c)
            },
            None => Ok(self.eof_lexeme)
        }
    }

    pub fn stats(&self) -> &LexerStats {
        &self.stats
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

    fn read_int(&mut self) -> LexerResult<'a, Option<Lexeme<'a>>> {
        Ok(match self.last_char {
            Some(c) if c.is_numeric() => {
                loop {
                    match self.last_char {
                        Some(c) if c.is_numeric() => self.read_char(),
                        _ => break
                    }
                }
                Some(self.new_lexeme(Token::Int))
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
                        // TODO include column
                        error!("{:?}({}): unclosed string starting at {}", self.file.path, self.current_position.line, self.lexeme_position.line)
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
            .or_else(|| on!(':', Token::Colon))
            .map(|token| self.new_lexeme(token)))
    }

    fn new_lexeme(&self, token: Token) -> Lexeme<'a> {
        let source = Source {
            file: self.file,
            start: self.lexeme_position,
            end: self.current_position
        };
        Lexeme { token, source }
    }

    // TODO comments
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
        self.last_char = if let Some((offset, c)) = self.source_iterator.next() {
            self.current_position.offset = offset;
            if offset > self.file.lines[self.current_position.line].end {
                self.current_position.line += 1;
            }
            Some(c)
        } else {
            self.current_position = self.end_position;
            None
        };
    }
}

impl<'a> Debug for Lexer<'a> {
    fn fmt(&self, formatter: &mut Formatter) -> FmtResult {
        formatter.write_str(
            &format!("Lexer{{ source: {{ {:?}, {} bytes }}, position: {:?}}}", self.file.path, self.file.text.len(), self.current_position))
    }
}
