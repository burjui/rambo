use std::fmt::{Debug, Formatter, Result as FmtResult};
use lexer::*;
use itertools::Itertools;

pub enum Expr<'a> {
    Int(i32),
    String(&'a str),
    Id(&'a str),
    Sequence(Vec<Expr<'a>>)
}

impl<'a> Debug for Expr<'a> {
    fn fmt(&self, formatter: &mut Formatter) -> FmtResult {
        match self {
            &Expr::Int(value) => write!(formatter, "{}", value),
            &Expr::String(value) => write!(formatter, "\"{}\"", value),
            &Expr::Id(name) => write!(formatter, "{}", name),
            &Expr::Sequence(ref exprs) => {
                formatter.write_str("{ ")?;
                let exprs_result = exprs
                    .iter()
                    .enumerate()
                    .fold(Ok(()), |result, (i, expr)| {
                        if i > 0 {
                            result.and(formatter.write_str("; "))?
                        }
                        result.and(expr.fmt(formatter))
                    });
                exprs_result.and(formatter.write_str(" }"))
            }
        }
    }
}

type ParseResult<'a> = Result<Expr<'a>, String>;

pub struct Parser<'a> {
    buffer: TokenBuffer<'a>
}

impl<'a> Parser<'a> {
    pub fn new(lexer: Lexer<'a>) -> Parser<'a> {
        Parser { buffer: TokenBuffer::new(lexer) }
    }

    pub fn parse(&mut self) -> ParseResult {
        self.buffer.init()?;
        self.parse_expression_sequence()
    }

    pub fn lexer_stats(&self) -> &LexerStats {
        self.buffer.lexer.stats()
    }

    fn parse_expression_sequence(&mut self) -> ParseResult<'a> {
        let mut exprs: Vec<Expr> = vec!();
        while self.token().is_some() {
            exprs.push(self.parse_expr()?);
            self.read_token()?;
        }
        Ok(Expr::Sequence(exprs))
    }

    fn parse_expr(&mut self) -> ParseResult<'a> {
        let token = self.token().clone();
        self.read_token()?;
        token.map_or_else(
            || Err("unexpected end of file".to_owned()),
            |token| Ok(Expr::String(token.text)))
    }

    fn read_token(&mut self) -> Result<(), String> {
        self.buffer.read_token()
    }

    fn token(&self) -> &Option<Token<'a>> {
        self.buffer.token()
    }

    fn next_token(&self) -> &Option<Token<'a>> {
        self.buffer.next_token()
    }
}

struct TokenBuffer<'a> {
    lexer: Lexer<'a>,
    tokens: [Option<Token<'a>>; 2],
    token_index: usize,
    next_token_index: usize
}

impl<'a> TokenBuffer<'a> {
    fn new(lexer: Lexer<'a>) -> TokenBuffer<'a> {
        TokenBuffer {
            lexer: lexer,
            tokens: [ None, None ],
            token_index: 0,
            next_token_index: 1
        }
    }

    fn init(&mut self) -> Result<(), String> {
        self.read_token()?;
        self.read_token()
    }

    fn read_token(&mut self) -> Result<(), String> {
        use std::mem::swap;
        swap(&mut self.token_index, &mut self.next_token_index);
        self.tokens[self.next_token_index] = self.lexer.read()?;
        Ok(())
    }

    fn token(&self) -> &Option<Token<'a>> {
        &self.tokens[self.token_index]
    }

    fn next_token(&self) -> &Option<Token<'a>> {
        &self.tokens[self.next_token_index]
    }
}