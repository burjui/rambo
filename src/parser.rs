use std::fmt::{Debug, Formatter, Write, Result as FmtResult};
use std::error::Error;

use lexer::*;
use source::*;
use semantics::*;

#[derive(Clone)]
pub struct Parameter<'a> {
    pub name: String,
    pub source: Source<'a>,
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
    Unit(Source<'a>),
    Int(Source<'a>),
    String(Source<'a>),
    Id(Source<'a>),
    Lambda {
        source: Source<'a>,
        parameters: Vec<Parameter<'a>>,
        body: Box<Expr<'a>>
    },
    Binary {
        source: Source<'a>,
        operation: BinaryOperation,
        left: Box<Expr<'a>>,
        right: Box<Expr<'a>>
    },
    Application {
        source: Source<'a>,
        function: Box<Expr<'a>>,
        arguments: Vec<Expr<'a>>
    },
    Conditional {
        source: Source<'a>,
        condition: Box<Expr<'a>>,
        positive: Box<Expr<'a>>,
        negative: Option<Box<Expr<'a>>>
    },
    Block {
        source: Source<'a>,
        statements: Vec<Statement<'a>>
    }
}

impl<'a> Debug for Expr<'a> {
    fn fmt(&self, formatter: &mut Formatter) -> FmtResult {
        write!(formatter, "{:?}", self.source())
    }
}

impl<'a> Expr<'a> {
    pub fn source(&self) -> &Source<'a> {
        match self {
            &Expr::Unit(ref source) |
            &Expr::String(ref source) |
            &Expr::Int(ref source) |
            &Expr::Id(ref source) |
            &Expr::Lambda { ref source, .. } |
            &Expr::Binary { ref source, .. } |
            &Expr::Application { ref source, .. } |
            &Expr::Conditional { ref source, .. } |
            &Expr::Block { ref source, .. }
            => source
        }
    }
}

#[derive(Clone)]
pub enum Statement<'a> {
    Expr(Expr<'a>),
    Binding {
        // TODO maybe introduce Source for the whole binding
        name: Source<'a>,
        value: Box<Expr<'a>>
    }
}

impl<'a> Debug for Statement<'a> {
    fn fmt(&self, formatter: &mut Formatter) -> FmtResult {
        match self {
            &Statement::Expr(ref expr) => expr.fmt(formatter),
            &Statement::Binding { ref name, ref value } => write!(formatter, "let {:?} = {:?}", name, value)
        }
    }
}

type ParseResult<T> = Result<T, Box<Error>>;

pub struct Parser<'a> {
    lexer: Lexer<'a>,
    lexeme: Lexeme<'a>,
    lexeme_line: usize,
    previous_lexeme_line: usize,
    previous_lexeme_end: usize
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
            lexeme_line: 0,
            previous_lexeme_line: 0,
            previous_lexeme_end: 0,
        }
    }

    pub fn parse(&mut self) -> ParseResult<Vec<Statement<'a>>> {
        self.read_lexeme()?;
        let mut statements = vec![];
        while self.lexeme.token != Token::EOF {
            statements.push(self.parse_statement()?)
        }
        Ok(statements)
    }

    pub fn lexer_stats(&self) -> &LexerStats {
        self.lexer.stats()
    }

    fn parse_statement(&mut self) -> ParseResult<Statement<'a>> {
        if self.lexeme.token == Token::Id && self.lexeme.text() == "let" {
            self.read_lexeme()?;
            self.parse_binding()
        } else {
            Ok(Statement::Expr(self.parse_expression()?))
        }
    }

    fn parse_expression(&mut self) -> ParseResult<Expr<'a>> {
        if self.lexeme.token == Token::Id && self.lexeme.text() == "if" {
            self.parse_conditional()
        } else {
            self.parse_binary(Precedence::Assignment)
        }
    }

    fn parse_conditional(&mut self) -> ParseResult<Expr<'a>> {
        let start = self.lexeme.source;
        self.read_lexeme()?;
        let condition_is_parenthesized = self.lexeme.token == Token::LParen;
        let condition = {
            if condition_is_parenthesized {
                self.read_lexeme()?;
            }
            let condition = box self.parse_expression()?;
            if condition_is_parenthesized {
                self.expect(Token::RParen, ")")?;
            }
            condition
        };
        let positive = if condition_is_parenthesized {
            self.parse_block_or_expr()?
        } else {
            self.parse_block()?
        };
        let negative = if self.lexeme.token == Token::Id && self.lexeme.text() == "else" {
            self.read_lexeme()?;
            Some(self.parse_block_or_expr()?)
        } else {
            None
        };
        let source = start.extend(self.previous_lexeme_end);
        Ok(Expr::Conditional {
            source,
            condition,
            positive: box positive,
            negative: negative.map(Box::new)
        })
    }

    fn parse_block_or_expr(&mut self) -> ParseResult<Expr<'a>> {
        if self.lexeme.token == Token::LBrace {
            self.parse_block()
        } else {
            self.parse_expr_as_block()
        }
    }

    fn parse_expr_as_block(&mut self) -> ParseResult<Expr<'a>> {
        let expr = self.parse_expression()?;
        let source = expr.source().clone();
        let statements = vec![Statement::Expr(expr)];
        Ok(Expr::Block {
            source,
            statements
        })
    }

    fn parse_block(&mut self) -> ParseResult<Expr<'a>> {
        let mut statements = vec![];
        let start = self.expect(Token::LBrace, "{")?.source;
        while self.lexeme.token != Token::EOF && self.lexeme.token != Token::RBrace {
            statements.push(self.parse_statement()?)
        }
        let end = self.expect(Token::RBrace, "}")?.source.range.end;
        let source = start.extend(end);
        Ok(Expr::Block {
            source,
            statements
        })
    }

    fn parse_binary(&mut self, precedence: Precedence) -> ParseResult<Expr<'a>> {
        let parse_next = |self_: &mut Parser<'a>, precedence: Option<Precedence>| {
            if let Some(precedence) = precedence {
                self_.parse_binary(precedence)
            } else {
                self_.parse_function_application()
            }
        };

        let mut start = self.lexeme.source;
        let mut result = parse_next(self, precedence.next_binary_precedence())?;
        let first_lexeme_token = self.lexeme.token;
        if self.previous_lexeme_line != self.lexeme_line || !first_lexeme_token.is_binary_operator() {
            return Ok(result)
        }

        let first_operator_precedence = first_lexeme_token.precedence();
        if first_operator_precedence >= precedence {
            loop {
                let operator = self.lexeme.token;
                if self.previous_lexeme_line != self.lexeme_line || !operator.is_binary_operator() ||
                    operator.precedence() < first_operator_precedence {
                    break
                }
                self.read_lexeme()?;
                let right_precedence = Some(first_operator_precedence)
                    .and_then(|p| if operator.is_left_associative() { p.next_binary_precedence() } else { Some(p) });
                let right_operand = parse_next(self, right_precedence)?;
                result = Expr::Binary {
                    source: start.extend(self.previous_lexeme_end),
                    operation: operator.binary_operation(),
                    left: box(result),
                    right: box(right_operand)
                };
                start = self.lexeme.source;
            }
        }

        Ok(result)
    }

    fn parse_function_application(&mut self) -> ParseResult<Expr<'a>> {
        let start = self.lexeme.source;
        let first_expr_line_index = self.lexeme_line;
        let first_expr = self.parse_primary()?;
        let mut arguments = vec![];
        while self.lexeme.can_be_expression_start() && self.lexeme_line == first_expr_line_index {
            arguments.push(self.parse_primary()?);
        }

        let result =
            if arguments.is_empty() {
                first_expr
            } else {
                Expr::Application {
                    source: start.extend(self.previous_lexeme_end),
                    function: box(first_expr),
                    arguments: arguments
                }
            };
        Ok(result)
    }

    fn parse_primary(&mut self) -> ParseResult<Expr<'a>> {
        let primary = self.lexeme;
        self.read_lexeme()?;
        match primary.token {
            Token::Id => Ok(Expr::Id(primary.source)),
            Token::Int => Ok(Expr::Int(primary.source)),
            Token::String => Ok(Expr::String(primary.source)),
            Token::Lambda => self.parse_lambda(&primary.source),
            Token::LParen => {
                if self.lexeme.token == Token::RParen {
                    self.read_lexeme()?;
                    let source = primary.source.extend(self.previous_lexeme_end);
                    Ok(Expr::Unit(source))
                } else {
                    let expr = self.parse_expression()?;
                    self.expect(Token::RParen, ")")?;
                    Ok(expr)
                }
            }
            _ => error!(self, &format!("expected an expression, found: {}", primary), &primary.source)
        }
    }

    fn parse_binding(&mut self) -> ParseResult<Statement<'a>> {
        let name = self.expect(Token::Id, "identifier")?;
        self.expect(Token::Eq, "=")?;
        let value = self.parse_expression()?;
        Ok(Statement::Binding {
            name: name.source,
            value: box(value)
        })
    }

    fn parse_lambda(&mut self, start: &Source<'a>) -> ParseResult<Expr<'a>> {
        let mut parameters = vec![];
        while self.lexeme.token == Token::LParen {
            self.read_lexeme()?;
            if self.lexeme.token == Token::RParen {
                self.read_lexeme()?;
                parameters.push(Parameter {
                    name: "_".to_string(),
                    source: self.lexeme.source.file.empty_source(),
                    type_: Type::Unit
                });
                break;
            } else {
                let parameter = self.parse_parameter()?;
                parameters.push(parameter);
            }
        }
        if parameters.is_empty() {
            error!(self, &format!("expected an argument list, found: {}", self.lexeme), &self.lexeme.source)
        } else {
            self.expect(Token::Arrow, "arrow")?;
            let body = self.parse_expression()?;
            Ok(Expr::Lambda {
                source: start.extend(self.previous_lexeme_end),
                parameters,
                body: box(body)
            })
        }
    }

    fn parse_parameter(&mut self) -> ParseResult<Parameter<'a>> {
        let Lexeme { source, .. } = self.expect(Token::Id, "identifier")?;
        self.expect(Token::Colon, ":")?;
        let type_ = self.parse_type()?;
        self.expect(Token::RParen, ")")?;
        Ok(Parameter {
            name: source.text().to_string(),
            source,
            type_
        })
    }

    fn parse_type(&mut self) -> ParseResult<Type> {
        Ok(self.lexeme)
            .and_then(|type_name| match (type_name.token, type_name.text()) {
                (Token::Id, "num") => Ok(Type::Int),
                (Token::Id, "str") => Ok(Type::String),
                _ => Err(type_name)
            })
            .or_else(|type_name| error!(self, &format!("expected a type, found: {}", type_name), &type_name.source))
            .and_then(|type_| self.read_lexeme().and(Ok(type_)))
    }

    fn expect(&mut self, token: Token, name: &str) -> ParseResult<Lexeme<'a>> {
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

    fn read_lexeme(&mut self) -> ParseResult<()> {
        self.previous_lexeme_line = self.lexeme_line;
        self.previous_lexeme_end = self.lexeme.source.range.end;
        let (lexeme, line) = self.lexer.read()?;
        self.lexeme = lexeme;
        self.lexeme_line = line;
        Ok(())
    }

    fn error<T>(&self, line: u32, text: &str, source: &Source) -> ParseResult<T> {
        Err(self.format_error(line, text, source)).map_err(From::from)
    }

    fn format_error(&self, line: u32, text: &str, source: &Source) -> String {
        format!("[{}] {}: {}", line, source, text)
    }
}

#[derive(Copy, Clone, PartialOrd, PartialEq, Debug)]
enum Precedence {
    Assignment,
    Additive,
    Multiplicative,
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
}

impl<'a> Lexeme<'a> {
    fn can_be_expression_start(&self) -> bool {
        match self.token {
            Token::Id => self.text() != "else",
            Token::Int | Token::String | Token::LParen | Token::Lambda => true,
            _ => false
        }
    }
}

fn article(word: &str) -> &str {
    "aouie".chars()
        .find(|&c| word.starts_with(c))
        .map_or("a", |_| "an")
}
