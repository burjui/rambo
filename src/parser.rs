use std::fmt::{Debug, Formatter, Write, Result as FmtResult};
use std::error::Error;

use crate::lexer::*;
use crate::source::*;
use crate::semantics::*;

#[derive(Clone)]
crate struct Parameter {
    crate name: String,
    crate source: Source,
    crate type_: Type
}

impl Debug for Parameter {
    fn fmt(&self, formatter: &mut Formatter<'_>) -> FmtResult {
        write!(formatter, "({:?}: {:?})", self.name, self.type_)
    }
}

#[derive(Copy, Clone)]
crate enum BinaryOperation {
    Assign, Add, Subtract, Multiply, Divide
}

impl Debug for BinaryOperation {
    fn fmt(&self, formatter: &mut Formatter<'_>) -> FmtResult {
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
crate enum Expr {
    Unit(Source),
    Int(Source),
    String(Source),
    Id(Source),
    Lambda {
        source: Source,
        parameters: Vec<Parameter>,
        body: Box<Expr>
    },
    Binary {
        source: Source,
        operation: BinaryOperation,
        left: Box<Expr>,
        right: Box<Expr>
    },
    Application {
        source: Source,
        function: Box<Expr>,
        arguments: Vec<Expr>
    },
    Conditional {
        source: Source,
        condition: Box<Expr>,
        positive: Box<Expr>,
        negative: Option<Box<Expr>>
    },
    Block {
        source: Source,
        statements: Vec<Statement>
    }
}

impl Debug for Expr {
    fn fmt(&self, formatter: &mut Formatter<'_>) -> FmtResult {
        write!(formatter, "{:?}", self.source())
    }
}

impl Expr {
    crate fn source(&self) -> &Source {
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
crate enum Statement {
    Expr(Expr),
    Binding {
        // TODO maybe introduce Source for the whole binding
        name: Source,
        value: Box<Expr>
    }
}

impl Debug for Statement {
    fn fmt(&self, formatter: &mut Formatter<'_>) -> FmtResult {
        match self {
            &Statement::Expr(ref expr) => expr.fmt(formatter),
            &Statement::Binding { ref name, ref value } => write!(formatter, "let {:?} = {:?}", name, value)
        }
    }
}

type ParseResult<T> = Result<T, Box<dyn Error>>;

crate struct Parser {
    lexer: Lexer,
    lexeme: Lexeme,
    lexeme_line: usize,
    previous_lexeme_line: usize,
    previous_lexeme_end: usize
}

macro_rules! error {
    ($self_: ident, $text: expr, $location: expr) => ($self_.error(line!(), $text, $location));
}

impl Parser {
    crate fn new(lexer: Lexer) -> Parser {
        let eof_lexeme = lexer.eof_lexeme.clone();
        Parser {
            lexer,
            lexeme: eof_lexeme,
            lexeme_line: 0,
            previous_lexeme_line: 0,
            previous_lexeme_end: 0,
        }
    }

    crate fn parse(&mut self) -> ParseResult<Vec<Statement>> {
        self.read_lexeme()?;
        let mut statements = vec![];
        while self.lexeme.token != Token::EOF {
            statements.push(self.parse_statement()?)
        }
        Ok(statements)
    }

    crate fn lexer_stats(&self) -> &LexerStats {
        self.lexer.stats()
    }

    fn parse_statement(&mut self) -> ParseResult<Statement> {
        if self.lexeme.token == Token::Id && self.lexeme.text() == "let" {
            self.read_lexeme()?;
            self.parse_binding()
        } else {
            Ok(Statement::Expr(self.parse_expression()?))
        }
    }

    fn parse_expression(&mut self) -> ParseResult<Expr> {
        if self.lexeme.token == Token::Id && self.lexeme.text() == "if" {
            self.parse_conditional()
        } else {
            self.parse_binary(Precedence::Assignment)
        }
    }

    fn parse_conditional(&mut self) -> ParseResult<Expr> {
        let start = self.lexeme.source.clone();
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

    fn parse_block_or_expr(&mut self) -> ParseResult<Expr> {
        if self.lexeme.token == Token::LBrace {
            self.parse_block()
        } else {
            self.parse_expr_as_block()
        }
    }

    fn parse_expr_as_block(&mut self) -> ParseResult<Expr> {
        let expr = self.parse_expression()?;
        let source = expr.source().clone();
        let statements = vec![Statement::Expr(expr)];
        Ok(Expr::Block {
            source,
            statements
        })
    }

    fn parse_block(&mut self) -> ParseResult<Expr> {
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

    fn parse_binary(&mut self, precedence: Precedence) -> ParseResult<Expr> {
        let parse_next = |self_: &mut Parser, precedence: Option<Precedence>| {
            if let Some(precedence) = precedence {
                self_.parse_binary(precedence)
            } else {
                self_.parse_function_application()
            }
        };

        let mut start = self.lexeme.source.clone();
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
                start = self.lexeme.source.clone();
            }
        }

        Ok(result)
    }

    fn parse_function_application(&mut self) -> ParseResult<Expr> {
        let start = self.lexeme.source.clone();
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
                    arguments
                }
            };
        Ok(result)
    }

    fn parse_primary(&mut self) -> ParseResult<Expr> {
        let primary = self.lexeme.clone();
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

    fn parse_binding(&mut self) -> ParseResult<Statement> {
        let name = self.expect(Token::Id, "identifier")?;
        self.expect(Token::Eq, "=")?;
        let value = self.parse_expression()?;
        Ok(Statement::Binding {
            name: name.source,
            value: box(value)
        })
    }

    fn parse_lambda(&mut self, start: &Source) -> ParseResult<Expr> {
        let mut parameters = vec![];
        while self.lexeme.token == Token::LParen {
            let lparen_source = self.lexeme.source.clone();
            self.read_lexeme()?;
            if self.lexeme.token == Token::RParen {
                let rparen_source_end = self.lexeme.source.range.end;
                self.read_lexeme()?;
                parameters.push(Parameter {
                    name: "_".to_string(),
                    source: lparen_source.extend(rparen_source_end),
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

    fn parse_parameter(&mut self) -> ParseResult<Parameter> {
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
        Ok(self.lexeme.clone())
            .and_then(|type_name| match (type_name.token, type_name.text()) {
                (Token::Id, "num") => Ok(Type::Int),
                (Token::Id, "str") => Ok(Type::String),
                _ => Err(type_name)
            })
            .or_else(|type_name| error!(self, &format!("expected a type, found: {}", type_name), &type_name.source))
            .and_then(|type_| self.read_lexeme().and(Ok(type_)))
    }

    fn expect(&mut self, token: Token, name: &str) -> ParseResult<Lexeme> {
        let lexeme = self.lexeme.clone();
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

impl Lexeme {
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
