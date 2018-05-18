use source::*;
use std::fmt::{Display, Debug, Formatter, Result as FmtResult};
use std::error::Error;
use std::iter::once;
use itertools::Itertools;
use std::rc::Rc;

#[derive(Debug, PartialEq, Copy, Clone)]
pub enum Token {
    EOF, Id, Int, String, LParen, RParen, LBrace, RBrace, Eq, EqEq, Lt, LtEq, Gt, GtEq, Lambda, Minus, Arrow, Plus, Star, Slash, Colon
}

#[derive(Clone)]
pub struct Lexeme {
    pub token: Token,
    pub source: Source
}

impl Lexeme {
    pub fn text(&self) -> &str {
        self.source.text()
    }
}

impl Debug for Lexeme {
    fn fmt(&self, formatter: &mut Formatter) -> FmtResult {
        formatter.write_str(&format!("{} | {:?} | {:?}",
             self.text(), self.token, self.source.file.position(self.source.range.start).unwrap()))
    }
}

impl Display for Lexeme {
    fn fmt(&self, formatter: &mut Formatter) -> FmtResult {
        formatter.write_str(if self.token == Token::EOF { "end of file" } else { self.text() })
    }
}

#[derive(Copy, Clone)]
pub struct LexerStats {
    pub lexeme_count: usize,
}

pub struct Lexer {
    pub eof_lexeme: Lexeme,
    eof_offset: usize,
    file: Rc<SourceFile>,
    lexeme_offset: usize,
    lexeme_line: usize,
    current_offset: usize,
    current_line: usize,
    current_character: Option<char>,
    next_character_offset: usize,
    next_character: Option<char>,
    stats: LexerStats
}

pub type LexerResult<T> = Result<T, Box<Error>>;

impl Lexer {
    pub fn new(file: SourceFile) -> Lexer {
        let eof_offset = file.text.len();
        let file = Rc::new(file);
        let eof_lexeme = Lexeme {
            token: Token::EOF,
            source: Source {
                file: file.clone(),
                range: Range {
                    start: eof_offset,
                    end: eof_offset
                }
            }
        };
        let initial_offset = file.bom_length;
        let mut lexer = Lexer {
            eof_lexeme,
            eof_offset,
            file: file.clone(),
            lexeme_offset: initial_offset,
            lexeme_line: 0,
            current_offset: initial_offset,
            current_line: 0,
            current_character: None,
            next_character_offset: initial_offset,
            next_character: None,
            stats: LexerStats {
                lexeme_count: 0
            },
        };
        lexer.read_char();
        lexer
    }

    /// Returns (Lexeme, line) pair
    pub fn read(&mut self) -> LexerResult<(Lexeme, usize)> {
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
                error!("{:?}({:?}): unexpected character: {} ({})",
                    self.file.path, self.file.position(self.current_offset)?, c, c as u32)
            },
            None => Ok((self.eof_lexeme.clone(), self.file.lines.len()))
        }
    }

    pub fn stats(&self) -> &LexerStats {
        &self.stats
    }

    fn read_id(&mut self) -> LexerResult<Option<Lexeme>> {
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
                Some(self.new_lexeme(Token::Id, None))
            },
            _ => None
        })
    }

    fn read_int(&mut self) -> LexerResult<Option<Lexeme>> {
        Ok(match self.current_character {
            Some(c) if c.is_numeric() => {
                loop {
                    match self.current_character {
                        Some(c) if c.is_numeric() => self.read_char(),
                        _ => break
                    }
                }
                Some(self.new_lexeme(Token::Int, None))
            },
            _ => None
        })
    }

    fn read_string(&mut self) -> LexerResult<Option<Lexeme>> {
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
                        let range = Range {
                            start: self.lexeme_offset,
                            end: self.current_offset
                        };
                        Ok(Some(self.new_lexeme(Token::String, Some(range))))
                    },
                    _ => {
                        let start_position = self.file.position(self.lexeme_offset)?;
                        let end_position = self.file.position(self.current_offset)?;
                        error!("{:?}({:?}): unclosed string starting at {:?}",
                            self.file.path, end_position, start_position)
                    }
                }
            },
            _ => Ok(None)
        }
    }

    fn read_operator(&mut self) -> LexerResult<Option<Lexeme>> {
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
            .or_else(|| on!('{', Token::LBrace))
            .or_else(|| on!('}', Token::RBrace))
            .map(|token| self.new_lexeme(token, None)))
    }

    fn new_lexeme(&self, token: Token, range: Option<Range>) -> Lexeme {
        let source = Source {
            file: self.file.clone(),
            range: range.unwrap_or_else(|| Range {
                start: self.lexeme_offset,
                end: self.current_offset
            })
        };
        Lexeme { token, source }
    }

    fn skip_whitepace(&mut self) -> LexerResult<()> {
        while let Some(c) = self.current_character {
            if c.is_whitespace() {
                self.read_char();
            } else if let Some(('/', '/')) = self.current_chars() {
                self.skip_line_comment();
            } else if let Some(('/', '*')) = self.current_chars() {
                self.skip_multiline_comment()?;
            } else {
                break
            }
        }
        Ok(())
    }

    fn skip_line_comment(&mut self) {
        self.read_char();
        self.read_char();
        while let Some(c) = self.current_character {
            self.read_char();
            if c == '\n' {
                break;
            }
        }
    }

    fn skip_multiline_comment(&mut self) -> LexerResult<()> {
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
        if self.current_chars() == Some(TERMINATOR) {
            self.read_char();
            self.read_char();
        } else {
            return error!("{}({:?}): unterminated comment starting at {:?}",
            self.file.path,
            self.file.position(self.current_offset)?,
            self.file.position(comment_start)?);
        }
        Ok(())
    }

    fn current_chars(&self) -> Option<(char, char)> {
        self.current_character.into_iter().zip(self.next_character.into_iter()).next()
    }

    fn read_char(&mut self) {
        self.current_offset = self.next_character_offset;
        let mut source_iterator = (&self.file.text[self.current_offset..])
            .char_indices()
            .chain(once((self.eof_offset - self.current_offset, '\0')))
            .tuple_windows::<(_, _)>();
        if let Some(((_, character), (next_offset, next_character))) = source_iterator.next() {
            self.next_character_offset += next_offset;
            if self.current_offset >= self.file.lines[self.current_line].end {
                self.current_line += 1;
            }
            self.current_character = Some(character);
            self.next_character = Some(next_character);
        } else {
            self.current_offset = self.eof_offset;
            self.current_character = None;
            self.next_character = None;
        }
    }
}

impl Debug for Lexer {
    fn fmt(&self, formatter: &mut Formatter) -> FmtResult {
        formatter.write_str(
            &format!("Lexer {{ {}({:?}) }}", self.file.path, self.file.position(self.current_offset).unwrap()))
    }
}
