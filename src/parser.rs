use std::fmt::{Debug, Formatter, Write, Result as FmtResult};
use std::error::Error;
use itertools::Itertools;

use lexer::*;
use source::*;
use semantics::*;

#[derive(Clone)]
pub struct Parameter<'a> {
    pub name: Source<'a>,
    pub type_: Type
}

impl<'a> Debug for Parameter<'a> {
    fn fmt(&self, formatter: &mut Formatter) -> FmtResult {
        write!(formatter, "({:?}: {:?})", self.name, self.type_)
    }
}

#[derive(Copy, Clone)]
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

#[derive(Clone)]
//#[derive(Debug)]
pub enum Expr<'a> {
    Int(Source<'a>),
    String(Source<'a>),
    Id(Source<'a>),
    Lambda {
        parameters: Vec<Parameter<'a>>,
        body: Box<Expr<'a>>
    },
    Binary {
        operation: BinaryOperation,
        left: Box<Expr<'a>>,
        right: Box<Expr<'a>>
    },
    Application {
        function: Box<Expr<'a>>,
        arguments: Vec<Expr<'a>>
    }
}

impl<'a> Debug for Expr<'a> {
    fn fmt(&self, formatter: &mut Formatter) -> FmtResult {
        match self {
            &Expr::Int(source) | &Expr::String(source) | &Expr::Id(source) =>
                write!(formatter, "{:?}", source),
            &Expr::Lambda { ref parameters, ref body } =>
                write!(formatter, "λ {} → {}", format_parameters(parameters.as_slice()), self.format(body)),
            &Expr::Binary { ref operation, ref left, ref right } =>
                write!(formatter, "{} {:?} {}", self.format(left), operation, self.format(right)),
            &Expr::Application { ref function, ref arguments } =>
                write!(formatter, "{} {}", self.format(function), self.format_exprs(arguments.as_slice())),
        }
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

    fn format_exprs(&self, list: &[Expr]) -> String {
        list.iter().map(|x| self.format(x)).join(" ")
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
    lexeme: Lexeme<'a>,
    lexeme_line: usize
}

macro_rules! error {
    ($self_: ident, $text: expr, $location: expr) => ($self_.error(line!(), $text, $location));
}

impl<'a> Parser<'a> {
    pub fn new(lexer: Lexer<'a>) -> Parser<'a> {
        let eof_lexeme = lexer.eof_lexeme;
        Parser {
            lexer,
            lexeme: eof_lexeme,
            lexeme_line: 0
        }
    }

    pub fn parse(&mut self) -> ParseResult<'a, Vec<Entity<'a>>> {
        self.read_lexeme()?;
        let mut entities = vec![];
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
        self.parse_binary(Precedence::Assignment)
    }

    fn parse_binary(&mut self, precedence: Precedence) -> ParseResult<'a, Expr<'a>> {
        let parse_next = |self_: &mut Parser<'a>, precedence: Option<Precedence>| {
            if let Some(precedence) = precedence {
                self_.parse_binary(precedence)
            } else {
                self_.parse_function_application()
            }
        };

        let mut result = parse_next(self, precedence.next_binary_precedence())?;
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
                let right_precedence = Some(first_operator_precedence)
                    .and_then(|p| if operator.is_left_associative() { p.next_binary_precedence() } else { Some(p) });
                let right_operand = parse_next(self, right_precedence)?;
                result = Expr::Binary {
                    operation: operator.binary_operation(),
                    left: box(result),
                    right: box(right_operand)
                };
            }
        }

        Ok(result)
    }

    fn parse_function_application(&mut self) -> ParseResult<'a, Expr<'a>> {
        let first_expr_line_index = self.lexeme_line;
        let first_expr = self.parse_primary()?;
        let mut arguments = vec![];
        while self.lexeme.token.can_be_expression_start() && self.lexeme_line == first_expr_line_index {
            arguments.push(self.parse_primary()?)
        }

        let result =
            if arguments.is_empty() {
                first_expr
            } else {
                Expr::Application {
                    function: box(first_expr),
                    arguments: arguments
                }
            };
        Ok(result)
    }

    fn parse_primary(&mut self) -> ParseResult<'a, Expr<'a>> {
        let primary = self.lexeme;
        self.read_lexeme()?;
        match primary.token {
            Token::Id => Ok(Expr::Id(primary.source)),
            Token::Int => Ok(Expr::Int(primary.source)),
            Token::String => Ok(Expr::String(primary.source)),
            Token::Lambda => self.parse_lambda(),
            Token::LParen => {
                let expr = self.parse_expr()?;
                self.expect(Token::RParen, ")")?;
                Ok(expr)
            }
            _ => error!(self, &format!("expected an identifier, a literal or '(', not: {}", primary), &primary.source)
        }
    }

    fn parse_binding(&mut self) -> ParseResult<'a, Entity<'a>> {
        let name = self.expect(Token::Id, "identifier")?;
        self.expect(Token::Eq, "=")?;
        let value = self.parse_expr()?;
        Ok(Entity::Binding {
            name: name.source,
            value: box(value)
        })
    }

    fn parse_lambda(&mut self) -> ParseResult<'a, Expr<'a>> {
        let mut parameters = vec![];
        while self.lexeme.token == Token::LParen {
            let parameter = self.parse_parameter()?;
            parameters.push(parameter);
        }
        if parameters.is_empty() {
            error!(self, &format!("expected an argument list, found: {}", self.lexeme), &self.lexeme.source)
        } else {
            self.expect(Token::Arrow, "arrow")?;
            let body = self.parse_expr()?;
            Ok(Expr::Lambda {
                parameters,
                body: box(body)
            })
        }
    }

    fn parse_parameter(&mut self) -> ParseResult<'a, Parameter<'a>> {
        self.expect(Token::LParen, "(")?;
        let Lexeme { source: name, .. } = self.expect(Token::Id, "identifier")?;
        self.expect(Token::Colon, ":")?;
        let type_ = self.parse_type()?;
        self.expect(Token::RParen, ")")?;
        Ok(Parameter {
            name,
            type_
        })
    }

    fn parse_type(&mut self) -> ParseResult<'a, Type> {
        Ok(self.lexeme)
            .and_then(|type_name| match (type_name.token, type_name.text()) {
                (Token::Id, "num") => Ok(Type::Int),
                (Token::Id, "str") => Ok(Type::String),
                _ => Err((type_name))
            })
            .or_else(|type_name| error!(self, &format!("expected a type, found: {}", type_name), &type_name.source))
            .and_then(|type_| self.read_lexeme().and(Ok(type_)))
    }

    fn expect(&mut self, token: Token, name: &str) -> ParseResult<'a, Lexeme<'a>> {
        let lexeme = self.lexeme;
        if lexeme.token != token {
            let starts_with_a_letter = name.chars().next().into_iter().all(char::is_alphanumeric);
            let (opening_quote, closing_quote) = if starts_with_a_letter { ("`", "'") } else { ("", "") };
            error!(self, &format!("expected {} {}{}{}, found: {}",
                                  article(name), opening_quote, name, closing_quote, lexeme), &lexeme.source)
        } else {
            self.read_lexeme()?;
            Ok(lexeme)
        }
    }

    fn read_lexeme(&mut self) -> ParseResult<'a, ()> {
        let (lexeme, line) = self.lexer.read()?;
        self.lexeme = lexeme;
        self.lexeme_line = line;
        Ok(())
    }

    fn error<T>(&self, line: u32, text: &str, source: &Source) -> ParseResult<'a, T> {
        Err(self.format_error(line, text, source)).map_err(From::from)
    }

    fn format_error(&self, line: u32, text: &str, source: &Source) -> String {
        format!("[{}] {}: {}", line, source, text)
    }
}

#[derive(Copy, Clone, PartialOrd, PartialEq, Debug)]
enum Precedence {
    Assignment,
    Lambda,
    Additive,
    Multiplicative,
    Application,
    Primitive
}

impl Precedence {
    fn next_binary_precedence(&self) -> Option<Precedence> {
        let next_precedence = match *self {
            Precedence::Assignment => Some(Precedence::Additive),
            Precedence::Additive => Some(Precedence::Multiplicative),
            _ => None
        };
        if let Some(next_precedence) = next_precedence {
            assert!(next_precedence > *self)
        }
        next_precedence
    }
}

impl Token {
    fn is_binary_operator(self) -> bool {
        match self {
            Token::Eq | Token::Plus | Token::Minus | Token::Star | Token::Slash => true,
            _ => false
        }
    }

    fn precedence(self) -> Precedence {
        match self {
            Token::Eq => Precedence::Assignment,
            Token::Plus | Token::Minus => Precedence::Additive,
            Token::Star | Token::Slash => Precedence::Multiplicative,
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
            Token::Id | Token::Int | Token::String | Token::LParen | Token::Lambda => true,
            _ => false
        }
    }
}

fn format_parameters(list: &[Parameter]) -> String {
    list.iter().map(|x| format_parameter(x)).join(" ")
}

fn format_parameter(parameter: &Parameter) -> String {
    format!("({:?}: {:?})", parameter.name, parameter.type_)
}

fn article(word: &str) -> &str {
    "aouie".chars()
        .find(|&c| word.starts_with(c))
        .map_or("a", |_| "an")
}
