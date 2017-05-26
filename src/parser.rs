use std::fmt::{Debug, Formatter, Write, Result as FmtResult};
use lexer::*;
use source::*;
use std::error::Error;
use itertools::Itertools;

pub enum BinaryOperation {
    Assign, Add, Subtract, Multiply, Divide
}

impl BinaryOperation {
    fn precedence(&self) -> Precedence {
        match self {
            &BinaryOperation::Assign => Precedence::Assignment,
            &BinaryOperation::Add | &BinaryOperation::Subtract => Precedence::Additive,
            &BinaryOperation::Multiply | &BinaryOperation::Divide => Precedence::Multiplicative,
        }
    }
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
    Int(Source<'a>),
    String(Source<'a>),
    Id(Source<'a>),
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

impl<'a> Expr<'a> {
    fn precedence(&self) -> Precedence {
        match self {
            &Expr::Int(_) | &Expr::String(_) | &Expr::Id(_) => Precedence::Primitive,
            &Expr::Lambda { .. } => Precedence::Lambda,
            &Expr::Binary { ref operation, .. } => operation.precedence(),
            &Expr::Application { .. } => Precedence::Application,
        }
    }

    fn format(&self, subexpr: &Expr) -> String {
        let need_parenthesis = self.precedence() > subexpr.precedence();
        let opening_parenthesis = if need_parenthesis { "(" } else { "" };
        let closing_parenthesis = if need_parenthesis { ")" } else { "" };
        format!("{}{:?}{}", opening_parenthesis, subexpr, closing_parenthesis)
    }

    fn format_list(&self, list: &[Expr]) -> String {
        list.iter().map(|x| self.format(x)).join(" ")
    }
}

impl<'a> Debug for Expr<'a> {
    fn fmt(&self, formatter: &mut Formatter) -> FmtResult {
        match self {
            &Expr::Int(source) | &Expr::String(source) | &Expr::Id(source) =>
                write!(formatter, "{:?}", source),
            &Expr::Lambda { ref args, ref body } =>
                write!(formatter, "λ {} → {}", self.format_list(args.as_slice()), self.format(body)),
            &Expr::Binary { ref operation, ref left, ref right } =>
                write!(formatter, "{} {:?} {}", self.format(left), operation, self.format(right)),
            &Expr::Application { ref function, ref args } =>
                write!(formatter, "{} {}", self.format(function), self.format_list(args.as_slice())),
        }
    }
}

pub enum Entity<'a> {
    Expr(Expr<'a>),
    Binding {
        name: Source<'a>,
        value: Box<Expr<'a>>
    }
}

impl<'a> Debug for Entity<'a> {
    fn fmt(&self, formatter: &mut Formatter) -> FmtResult {
        match self {
            &Entity::Expr(ref expr) => expr.fmt(formatter),
            &Entity::Binding { ref name, ref value } => write!(formatter, "let {:?} = {:?}", name, value)
        }
    }
}

type ParseResult<'a, T> = Result<T, Box<Error>>;

pub struct Parser<'a> {
    lexer: Lexer<'a>,
    lexeme: Lexeme<'a>
}

impl<'a> Parser<'a> {
    pub fn new(lexer: Lexer<'a>) -> Parser<'a> {
        let dummy_lexeme = Lexeme {
            token: Token::EOF,
            source: lexer.make_source(
                Segment {
                    start: Location::new(),
                    end: Location::new()
                }
            )
        };
        Parser {
            lexer,
            lexeme: dummy_lexeme
        }
    }

    pub fn parse(&mut self) -> ParseResult<'a, Box<Vec<Entity<'a>>>> {
        self.read_lexeme()?;
        let mut entities = box(vec!());
        while self.lexeme.token != Token::EOF {
            entities.push(
                if self.lexeme.token == Token::Id && self.lexeme.text() == "let" {
                    self.read_lexeme()?;
                    self.parse_binding()?
                } else {
                    Entity::Expr(self.parse_expr()?)
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
        let first_lexeme_token = self.lexeme.token;
        if !first_lexeme_token.is_binary_operator() {
            return Ok(result)
        }

        let first_operator_precedence = first_lexeme_token.precedence();
        if first_operator_precedence >= precedence {
            loop {
                let operator = self.lexeme.token;
                if !operator.is_binary_operator() || operator.precedence() < first_operator_precedence {
                    break
                }
                self.read_lexeme()?;
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
        let first_expr_line_index = self.lexeme.source.segment.start.line_index;
        let first_expr = self.parse_primary()?;
        let mut args = box(vec!());
        while self.lexeme.token.can_be_expression_start() && self.lexeme.source.segment.start.line_index == first_expr_line_index {
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
        let primary = self.lexeme;
        self.read_lexeme()?;
        match primary.token {
            Token::Id => Ok(Expr::Id(primary.source)),
            Token::Number => Ok(Expr::Int(primary.source)),
            Token::String => Ok(Expr::String(primary.source)),
            Token::Lambda => self.parse_lambda(),
            Token::LParen => {
                let expr = self.parse_expr();
                println!("## {:?} {}", expr, self.lexeme.source.segment.start);
                match self.lexeme.token {
                    Token::RParen => {
                        self.read_lexeme()?;
                        expr
                    },
                    _ => self.error(&format!("expected `)', found: {}", self.lexeme), &self.lexeme.source)
                }
            }
            _ => self.error(&format!("expected an identifier, a literal or '(', not: {}", primary), &primary.source)
        }
    }

    fn parse_binding(&mut self) -> ParseResult<'a, Entity<'a>> {
        let name = self.lexeme;
        if name.token != Token::Id {
            return self.error(&format!("expected an identifier, found: {}", name), &name.source);
        }
        self.read_lexeme()?;

        let equals = self.lexeme;
        if equals.token != Token::Eq {
            return self.error(&format!("expected `=', found: {}", equals), &equals.source);
        }
        self.read_lexeme()?;

        let value = self.parse_expr()?;
        Ok(Entity::Binding {
            name: name.source,
            value: box(value)
        })
    }

    fn parse_lambda(&mut self) -> ParseResult<'a, Expr<'a>> {
        let mut args = vec!();
        while let Lexeme { token: Token::Id, source } = self.lexeme {
            args.push(Expr::Id(source));
            self.read_lexeme()?;
        }

        let arrow = self.lexeme;
        if arrow.token != Token::Arrow {
            return self.error(&format!("expected an arrow, found: {}", arrow), &arrow.source);
        }
        self.read_lexeme()?;

        let body = self.parse_expr()?;
        Ok(Expr::Lambda {
            args,
            body: box(body)
        })
    }

    fn read_lexeme(&mut self) -> Result<(), String> {
        self.lexeme = self.lexer.read()?;
        Ok(())
    }

    fn error<T>(&self, text: &str, source: &Source) -> ParseResult<'a, T> {
        Err(self.format_error(text, source)).map_err(From::from)
    }

    fn format_error(&self, text: &str, source: &Source) -> String {
        format!("{}: {}", source, text)
    }

    const MIN_PRECEDENCE: u8 = 0;
    const MAX_PRECEDENCE: u8 = 2;
}

#[derive(PartialOrd, PartialEq)]
enum Precedence {
    Assignment,
    Lambda,
    Additive,
    Multiplicative,
    Application,
    Primitive
}

impl Token {
    fn is_binary_operator(self) -> bool {
        match self {
            Token::Eq | Token::Plus | Token::Minus | Token::Star | Token::Slash => true,
            _ => false
        }
    }

    fn precedence(self) -> u8 {
        match self {
            Token::Eq => 0,
            Token::Plus | Token::Minus => 1,
            Token::Star | Token::Slash => 2,
            _ => unreachable!()
        }
    }

    fn is_left_associative(self) -> bool {
        match self {
            Token::Eq => false,
            Token::Plus | Token::Minus | Token::Star | Token::Slash => true,
            _ => unreachable!()
        }
    }

    fn binary_operation(self) -> BinaryOperation {
        match self {
            Token::Eq => BinaryOperation::Assign,
            Token::Plus => BinaryOperation::Add,
            Token::Minus => BinaryOperation::Subtract,
            Token::Star => BinaryOperation::Multiply,
            Token::Slash => BinaryOperation::Divide,
            _ => unreachable!()
        }
    }

    fn can_be_expression_start(self) -> bool {
        match self {
            Token::Id | Token::Number | Token::String | Token::LParen | Token::Lambda => true,
            _ => false
        }
    }
}