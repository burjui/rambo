use std::{
    error::Error,
    fmt::{Debug, Display, Formatter},
    iter::once,
    ops::Range,
};

use itertools::Itertools;

use crate::source::{Source, SourceFileRef};

#[derive(Debug, PartialEq, Copy, Clone)]
pub(crate) enum Token {
    Eof,
    Id,
    Int,
    String,
    LParen,
    RParen,
    LBrace,
    RBrace,
    Eq,
    EqEq,
    Lt,
    LtEq,
    Gt,
    GtEq,
    Lambda,
    Minus,
    Arrow,
    Plus,
    Star,
    Slash,
    Colon,
    Comma,
}

#[derive(Clone)]
pub(crate) struct Lexeme {
    pub(crate) token: Token,
    pub(crate) source: Source,
}

impl Lexeme {
    pub(crate) fn text(&self) -> &str {
        self.source.text()
    }
}

impl Debug for Lexeme {
    fn fmt(&self, formatter: &mut Formatter<'_>) -> std::fmt::Result {
        formatter.write_str(&format!(
            "{} | {:?} | {:?}",
            self.text(),
            self.token,
            self.source.file().position(&self.source)
        ))
    }
}

impl Display for Lexeme {
    fn fmt(&self, formatter: &mut Formatter<'_>) -> std::fmt::Result {
        formatter.write_str(if self.token == Token::Eof {
            "end of file"
        } else {
            self.text()
        })
    }
}

#[derive(Copy, Clone)]
pub(crate) struct LexerStats {
    pub(crate) lexeme_count: usize,
}

pub(crate) struct Lexer {
    pub(crate) eof_lexeme: Lexeme,
    eof_offset: usize,
    file: SourceFileRef,
    lexeme_offset: usize,
    lexeme_line: usize,
    current_offset: usize,
    current_line: usize,
    current_character: Option<char>,
    next_character_offset: usize,
    next_character: Option<char>,
    stats: LexerStats,
}

pub(crate) type LexerResult<T> = Result<T, Box<dyn Error>>;

impl Lexer {
    pub(crate) fn new(file: SourceFileRef) -> Self {
        let eof_offset = file.text().len();
        let eof_lexeme = Lexeme {
            token: Token::Eof,
            source: Source::new(file.clone(), eof_offset..eof_offset),
        };
        let mut lexer = Self {
            eof_lexeme,
            eof_offset,
            file,
            lexeme_offset: 0,
            lexeme_line: 0,
            current_offset: 0,
            current_line: 0,
            current_character: None,
            next_character_offset: 0,
            next_character: None,
            stats: LexerStats { lexeme_count: 0 },
        };
        lexer.read_char();
        lexer
    }

    /// Returns (Lexeme, line) pair
    pub(crate) fn read(&mut self) -> LexerResult<(Lexeme, usize)> {
        self.skip_whitespace()?;
        self.lexeme_offset = self.current_offset;
        self.lexeme_line = self.current_line;
        match self.current_character {
            Some(c) => {
                for matcher in &[
                    Self::read_int,
                    Self::read_operator,
                    Self::read_string,
                    Self::read_id,
                ] {
                    if let Some(lexeme) = matcher(self)? {
                        self.stats.lexeme_count += 1;
                        return Ok((lexeme, self.lexeme_line));
                    }
                }
                error!(
                    "{:?}({:?}): unexpected character: {} ({})",
                    self.file.name(),
                    self.file.position(&self.current_source()),
                    c,
                    c as u32
                )
            }
            None => Ok((self.eof_lexeme.clone(), self.file.lines().len())),
        }
    }

    pub(crate) const fn stats(&self) -> &LexerStats {
        &self.stats
    }

    #[allow(clippy::unnecessary_wraps)]
    fn read_id(&mut self) -> LexerResult<Option<Lexeme>> {
        Ok(match self.current_character {
            Some(c) if c.is_alphabetic() => {
                self.read_char();
                while let Some(c) = self.current_character {
                    if c.is_alphanumeric() || c == '_' {
                        self.read_char();
                    } else {
                        break;
                    }
                }
                Some(self.new_lexeme(Token::Id, None))
            }
            _ => None,
        })
    }

    #[allow(clippy::unnecessary_wraps)]
    fn read_int(&mut self) -> LexerResult<Option<Lexeme>> {
        Ok(match self.current_character {
            Some(c) if c.is_numeric() => {
                loop {
                    match self.current_character {
                        Some(c) if c.is_numeric() => self.read_char(),
                        _ => break,
                    }
                }
                Some(self.new_lexeme(Token::Int, None))
            }
            _ => None,
        })
    }

    fn read_string(&mut self) -> LexerResult<Option<Lexeme>> {
        match self.current_character {
            Some('"') => {
                self.read_char();

                loop {
                    match self.current_character {
                        Some('"') | None => break,
                        Some(_) => self.read_char(),
                    }
                }

                if let Some('"') = self.current_character {
                    self.read_char();
                    let range = self.lexeme_offset..self.current_offset;
                    Ok(Some(self.new_lexeme(Token::String, Some(range))))
                } else {
                    let start_position = self.file.position(&self.lexeme_source());
                    let end_position = self.file.position(&self.current_source());
                    error!(
                        "{:?}({:?}): unclosed string starting at {:?}",
                        self.file.name(),
                        end_position,
                        start_position
                    )
                }
            }
            _ => Ok(None),
        }
    }

    #[allow(clippy::unnecessary_wraps)]
    fn read_operator(&mut self) -> LexerResult<Option<Lexeme>> {
        macro_rules! on {
            ($char: tt, $token: expr, $handler: expr) => {{
                match self.current_character {
                    Some($char) => {
                        self.read_char();
                        $handler.or(Some($token))
                    }
                    _ => None,
                }
            }};

            ($char: expr, $token: expr) => {{
                on!($char, $token, None)
            }};
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
            .or_else(|| on!(',', Token::Comma))
            .or_else(|| on!('{', Token::LBrace))
            .or_else(|| on!('}', Token::RBrace))
            .map(|token| self.new_lexeme(token, None)))
    }

    fn new_lexeme<R>(&self, token: Token, range: R) -> Lexeme
    where
        R: Into<Option<Range<usize>>>,
    {
        let source = range.into().map_or_else(
            || self.lexeme_source(),
            |range| Source::new(self.file.clone(), range),
        );
        Lexeme { token, source }
    }

    fn current_source(&self) -> Source {
        Source::new(self.file.clone(), self.current_offset..self.current_offset)
    }

    fn lexeme_source(&self) -> Source {
        Source::new(self.file.clone(), self.lexeme_offset..self.current_offset)
    }

    fn skip_whitespace(&mut self) -> LexerResult<()> {
        while let Some(c) = self.current_character {
            if c.is_whitespace() {
                self.read_char();
            } else if let Some(('/', '/')) = self.current_chars() {
                self.skip_line_comment();
            } else if let Some(('/', '*')) = self.current_chars() {
                self.skip_multiline_comment()?;
            } else {
                break;
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
        const TERMINATOR: (char, char) = ('*', '/');

        let comment_start = self.current_source();
        self.read_char();
        self.read_char();
        while let Some(chars) = self.current_chars() {
            if chars == TERMINATOR {
                break;
            }
            self.read_char();
        }
        if self.current_chars() == Some(TERMINATOR) {
            self.read_char();
            self.read_char();
        } else {
            return error!(
                "{}({:?}): unterminated comment starting at {:?}",
                self.file.name(),
                self.file.position(&self.current_source()),
                self.file.position(&comment_start)
            );
        }
        Ok(())
    }

    fn current_chars(&self) -> Option<(char, char)> {
        self.current_character
            .into_iter()
            .zip(self.next_character)
            .next()
    }

    fn read_char(&mut self) {
        self.current_offset = self.next_character_offset;
        let mut source_iterator = self.file.text()[self.current_offset..]
            .char_indices()
            .chain(once((self.eof_offset - self.current_offset, '\0')))
            .tuple_windows::<(_, _)>();
        if let Some(((_, character), (next_offset, next_character))) = source_iterator.next() {
            self.next_character_offset += next_offset;
            if self.current_offset >= self.file.lines()[self.current_line].end {
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
    fn fmt(&self, formatter: &mut Formatter<'_>) -> std::fmt::Result {
        formatter.write_str(&format!(
            "Lexer {{ {}({:?}) }}",
            self.file.name(),
            self.file.position(&self.current_source())
        ))
    }
}
