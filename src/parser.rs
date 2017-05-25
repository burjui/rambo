use std::fmt::{Debug, Formatter, Write, Result as FmtResult};
use lexer::*;
use source::*;
use num::bigint::BigInt;
use std::str::FromStr;
use std::error::Error;

pub enum BinaryOperation {
    Assign, Add, Subtract, Multiply, Divide
}

impl Debug for BinaryOperation {
    fn fmt(&self, formatter: &mut Formatter) -> FmtResult {
        formatter.write_char(match self {
            &BinaryOperation::Assign => '=',
            &BinaryOperation::Add => '+',
            &BinaryOperation::Subtract => '-',
            &BinaryOperation::Multiply => '*',
            &BinaryOperation::Divide => '/',
        })
    }
}

pub enum Expr<'a> {
    Int(BigInt),
    String(&'a str),
    Id(&'a str),
    Lambda {
        args: Vec<Expr<'a>>,
        body: Box<Expr<'a>>
    },
    Binary {
        operation: BinaryOperation,
        left: Box<Expr<'a>>,
        right: Box<Expr<'a>>
    },
    Application {
        function: Box<Expr<'a>>,
        args: Box<Vec<Expr<'a>>>
    }
}

#[derive(Debug)]
pub enum Entity<'a> {
    Expr(Expr<'a>),
    Binding {
        name: &'a str,
        value: Box<Expr<'a>>
    }
}

impl<'a> Debug for Expr<'a> {
    fn fmt(&self, formatter: &mut Formatter) -> FmtResult {
        match self {
            &Expr::Int(ref value) => write!(formatter, "{}", value),
            &Expr::String(value) => write!(formatter, "\"{}\"", value),
            &Expr::Id(name) => write!(formatter, "{}", name),
            &Expr::Lambda { ref args, ref body } => write!(formatter, "'λ {:?} → {:?}", args, body),
            &Expr::Binary { ref operation, ref left, ref right } => write!(formatter, "({:?} {:?} {:?})", left, operation, right),
            &Expr::Application { ref function, ref args } => write!(formatter, "({:?} {:?})", function, args),
        }
    }
}

type ParseResult<'a, T> = Result<T, Box<Error>>;

pub struct Parser<'a> {
    lexer: Lexer<'a>,
    token: Token<'a>
}

impl<'a> Parser<'a> {
    pub fn new(lexer: Lexer<'a>) -> Parser<'a> {
        let dummy_token = Token {
            kind: Kind::EOF,
            text: "",
            location: Location::new("")
        };
        Parser {
            lexer,
            token: dummy_token
        }
    }

    pub fn parse(&mut self) -> ParseResult<'a, Box<Vec<Entity<'a>>>> {
        self.read_token()?;
        let mut entities = box(vec!());
        while self.token.kind != Kind::EOF {
            entities.push(
                match self.token {
                    Token { kind: Kind::Id, text: "let", .. } => {
                        self.read_token()?;
                        self.parse_binding()?
                    },
                    _ => Entity::Expr(self.parse_expr()?),
                }
            )
        }
        Ok(entities)
    }

    pub fn lexer_stats(&self) -> &LexerStats {
        self.lexer.stats()
    }

    fn parse_expr(&mut self) -> ParseResult<'a, Expr<'a>> {
        self.parse_binary(Parser::MIN_PRECEDENCE)
    }

    fn parse_binary(&mut self, precedence: u8) -> ParseResult<'a, Expr<'a>> {
        if precedence > Parser::MAX_PRECEDENCE {
            return self.parse_function_application()
        }

        let mut result = self.parse_binary(precedence + 1)?;
        let first_token_kind = self.token.kind;
        if !first_token_kind.is_binary_operator() {
            return Ok(result)
        }

        let first_operator_precedence = first_token_kind.precedence();
        if first_operator_precedence >= precedence {
            loop {
                let operator = self.token.kind;
                if !operator.is_binary_operator() || operator.precedence() < first_operator_precedence {
                    break
                }
                self.read_token()?;
                let operator_associativity_adjustment = if operator.is_left_associative() { 1 } else { 0 };
                let right_operand = self.parse_binary(first_operator_precedence + operator_associativity_adjustment)?;
                result = Expr::Binary {
                    operation: operator.binary_operation(),
                    left: box(result),
                    right: box(right_operand)
                }
            }
        }

        Ok(result)
    }

    fn parse_function_application(&mut self) -> ParseResult<'a, Expr<'a>> {
        let first_expr_line_index = self.token.location.line_index;
        let first_expr = self.parse_primary()?;
        let mut args = box(vec!());
        while self.token.kind.can_be_expression_start() && self.token.location.line_index == first_expr_line_index {
            args.push(self.parse_primary()?)
        }

        let result =
            if args.is_empty() {
                first_expr
            } else {
                Expr::Application {
                    function: box(first_expr),
                    args
                }
            };
        Ok(result)
    }

    fn parse_primary(&mut self) -> ParseResult<'a, Expr<'a>> {
        let primary = self.token;
        self.read_token()?;
        match primary.kind {
            Kind::Id => Ok(Expr::Id(primary.text)),
            Kind::Number => BigInt::from_str(primary.text).map(Expr::Int).map_err(From::from),
            Kind::String => Ok(Expr::String(primary.text)),
            Kind::Lambda => self.parse_lambda(),
            Kind::LParen => {
                let expr = self.parse_expr();
                match self.token.kind {
                    Kind::RParen => {
                        self.read_token()?;
                        expr
                    },
                    _ => self.error(&format!("expected `)', found: {}", self.token), &self.token.location)
                }
            }
            _ => self.error(&format!("expected an identifier, a literal or '(', not: {}", primary), &primary.location)
        }
    }

    fn parse_binding(&mut self) -> ParseResult<'a, Entity<'a>> {
        let name = self.token;
        if name.kind != Kind::Id {
            return self.error(&format!("expected an identifier, found: {}", name), &name.location);
        }
        self.read_token()?;

        let equals = self.token;
        if equals.kind != Kind::Eq {
            return self.error(&format!("expected `=', found: {}", equals), &equals.location);
        }
        self.read_token()?;

        let value = self.parse_expr()?;
        Ok(Entity::Binding {
            name: name.text,
            value: box(value)
        })
    }

    fn parse_lambda(&mut self) -> ParseResult<'a, Expr<'a>> {
        let mut args = vec!();
        while let Token { kind: Kind::Id, text: arg_name, .. } = self.token {
            args.push(Expr::Id(arg_name));
            self.read_token()?;
        }

        let arrow = self.token;
        if arrow.kind != Kind::Arrow {
            return self.error(&format!("expected an arrow, found: {}", arrow), &arrow.location);
        }
        self.read_token()?;

        let body = self.parse_expr()?;
        Ok(Expr::Lambda {
            args,
            body: box(body)
        })
    }

    fn read_token(&mut self) -> Result<(), String> {
        self.token = self.lexer.read()?;
        Ok(())
    }

    fn error<T>(&self, text: &str, location: &Location) -> ParseResult<'a, T> {
        Err(self.format_error(text, location)).map_err(From::from)
    }

    fn format_error(&self, text: &str, location: &Location) -> String {
        format!("{}: {}", location, text)
    }

    const MIN_PRECEDENCE: u8 = 0;
    const MAX_PRECEDENCE: u8 = 2;
}

impl Kind {
    fn is_binary_operator(self) -> bool {
        match self {
            Kind::Eq | Kind::Plus | Kind::Minus | Kind::Star | Kind::Slash => true,
            _ => false
        }
    }

    fn precedence(self) -> u8 {
        match self {
            Kind::Eq => 0,
            Kind::Plus | Kind::Minus => 1,
            Kind::Star | Kind::Slash => 2,
            _ => unreachable!()
        }
    }

    fn is_left_associative(self) -> bool {
        match self {
            Kind::Eq => false,
            Kind::Plus | Kind::Minus | Kind::Star | Kind::Slash => true,
            _ => unreachable!()
        }
    }

    fn binary_operation(self) -> BinaryOperation {
        match self {
            Kind::Eq => BinaryOperation::Assign,
            Kind::Plus => BinaryOperation::Add,
            Kind::Minus => BinaryOperation::Subtract,
            Kind::Star => BinaryOperation::Multiply,
            Kind::Slash => BinaryOperation::Divide,
            _ => unreachable!()
        }
    }

    fn can_be_expression_start(self) -> bool {
        match self {
            Kind::Id | Kind::Number | Kind::String | Kind::LParen | Kind::Lambda => true,
            _ => false
        }
    }
}