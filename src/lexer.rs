use source::*;
use std::fmt::{Display, Debug, Formatter, Result as FmtResult};
use std::error::Error;
use std::iter::once;
use itertools::Itertools;

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
        formatter.write_str(&format!("{} | {:?} | {:?}",
             self.text(), self.token, self.source.file.position(self.source.range.start).unwrap()))
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
    pub eof_lexeme: Lexeme<'a>,
    file: &'a SourceFile<'a>,
    source_iterator: Box<Iterator<Item = ((usize, char), (usize, char))> + 'a>,
    lexeme_offset: usize,
    lexeme_line: usize,
    current_offset: usize,
    current_line: usize,
    current_character: Option<char>,
    next_character: Option<char>,
    stats: LexerStats
}

pub type LexerResult<'a, T> = Result<T, Box<Error>>;

impl<'a> Lexer<'a> {
    pub fn new(file: &'a SourceFile<'a>) -> Lexer<'a> {
        let eof_offset = file.text.len();
        let eof_lexeme = Lexeme {
            token: Token::EOF,
            source: Source {
                file,
                range: Range {
                    start: eof_offset,
                    end: eof_offset
                }
            }
        };
        let source_iterator = box(file.text
            .char_indices()
            .chain(once((eof_offset, '\0')))
            .tuple_windows::<(_, _)>());
        let mut lexer = Lexer {
            file,
            source_iterator,
            lexeme_offset: 0,
            lexeme_line: 0,
            current_offset: 0,
            current_line: 0,
            current_character: None,
            next_character: None,
            stats: LexerStats {
                lexeme_count: 0
            },
            eof_lexeme
        };
        lexer.read_char();
        lexer
    }

    /// Returns (Lexeme, line) pair
    pub fn read(&mut self) -> LexerResult<'a, (Lexeme<'a>, usize)> {
        self.skip_whitepace()?;
        self.lexeme_offset = self.current_offset;
        self.lexeme_line = self.current_line;
        match self.current_character {
            Some(c) => {
                for matcher in &[ Lexer::read_int, Lexer::read_operator, Lexer::read_string, Lexer::read_id ] {
                    if let Some(lexeme) = matcher(self)? {
                        self.stats.lexeme_count += 1;
                        return Ok((lexeme, self.lexeme_line))
                    }
                }
                error!("{:?}({:?}): unexpected character: {}",
                    self.file.path.to_string_lossy(), self.file.position(self.current_offset)?, c)
            },
            None => Ok((self.eof_lexeme, self.file.lines.len()))
        }
    }

    pub fn stats(&self) -> &LexerStats {
        &self.stats
    }

    fn read_id(&mut self) -> LexerResult<'a, Option<Lexeme<'a>>> {
        Ok(match self.current_character {
            Some(c) if c.is_alphabetic() => {
                self.read_char();
                while let Some(c) = self.current_character {
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
        Ok(match self.current_character {
            Some(c) if c.is_numeric() => {
                loop {
                    match self.current_character {
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
        match self.current_character {
            Some('"') => {
                self.read_char();

                loop {
                    match self.current_character {
                        Some('"') => break,
                        Some(_) => self.read_char(),
                        None => break
                    }
                }

                match self.current_character {
                    Some('"') => {
                        self.read_char();
                        Ok(Some(self.new_lexeme(Token::String)))
                    },
                    _ => {
                        let start_position = self.file.position(self.lexeme_offset)?;
                        let end_position = self.file.position(self.current_offset)?;
                        error!("{:?}({:?}): unclosed string starting at {:?}",
                            self.file.path.to_string_lossy(), end_position, start_position)
                    }
                }
            },
            _ => Ok(None)
        }
    }

    fn read_operator(&mut self) -> LexerResult<'a, Option<Lexeme<'a>>> {
        macro_rules! on {
            ($char: tt, $token: expr, $handler: expr) => (
                match self.current_character {
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
            range: Range {
                start: self.lexeme_offset,
                end: self.current_offset
            }
        };
        Lexeme { token, source }
    }

    // TODO comments
    fn skip_whitepace(&mut self) -> LexerResult<()> {
        while let Some(c) = self.current_character {
            if c.is_whitespace() {
                self.read_char()
            } else if let (Some('/'), Some('*')) = (self.current_character, self.next_character) {
                self.skip_comment()?
            } else {
                break
            }
        }
        Ok(())
    }

    fn skip_comment(&mut self) -> LexerResult<()> {
        let comment_start = self.current_offset;
        self.read_char();
        self.read_char();
        const TERMINATOR: (char, char) = ('*', '/');
        while let Some(chars) = self.current_chars() {
            if chars == TERMINATOR {
                break
            } else {
                self.read_char();
            }
        }
        let x = self.current_chars();
        if self.current_chars() == Some(TERMINATOR) {
            self.read_char();
            self.read_char();
        } else {
            return error!("{}({:?}): unterminated comment starting at {:?}",
            self.file.path.to_string_lossy(),
            self.file.position(self.current_offset)?,
            self.file.position(comment_start)?);
        }
        Ok(())
    }

    fn current_chars(&self) -> Option<(char, char)> {
        self.current_character.into_iter().zip(self.next_character.into_iter()).next()
    }

    fn read_char(&mut self) {
        if let Some(((offset, character), (_, next_character))) = self.source_iterator.next() {
            self.current_offset = offset;
            if offset >= self.file.lines[self.current_line].end {
                self.current_line += 1;
            }
            self.current_character = Some(character);
            self.next_character = Some(next_character);
        } else {
            self.current_offset = self.file.text.len();
            self.current_character = None;
            self.next_character = None;
        }
    }
}

impl<'a> Debug for Lexer<'a> {
    fn fmt(&self, formatter: &mut Formatter) -> FmtResult {
        formatter.write_str(
            &format!("Lexer {{ {}({:?}) }}",
                     self.file.path.to_string_lossy(), self.file.position(self.current_offset).unwrap()))
    }
}
