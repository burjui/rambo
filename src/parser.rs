use std::{
    error::Error,
    fmt::{
        Debug,
        Formatter,
        Write,
    },
};

use rustc_hash::FxHashMap;

use crate::{
    lexer::{
        Lexeme,
        Lexer,
        LexerStats,
        Token,
    },
    semantics::{
        FunctionType,
        FunctionTypeRef,
        Type,
    },
    source::Source,
};

#[derive(Clone)]
pub(crate) struct Parameter {
    pub(crate) name: Source,
    pub(crate) type_: Type,
    pub(crate) source: Source,
}

impl PartialEq for Parameter {
    fn eq(&self, other: &Self) -> bool {
        self.type_ == other.type_
    }
}

impl Debug for Parameter {
    fn fmt(&self, formatter: &mut Formatter<'_>) -> std::fmt::Result {
        write!(formatter, "({}: {:?})", self.name.text(), self.type_)
    }
}

#[derive(Copy, Clone)]
pub(crate) enum BinaryOperation {
    Assign,
    Add,
    Subtract,
    Multiply,
    Divide,
}

impl Debug for BinaryOperation {
    fn fmt(&self, formatter: &mut Formatter<'_>) -> std::fmt::Result {
        formatter.write_char(match self {
            Self::Assign => '=',
            Self::Add => '+',
            Self::Subtract => '-',
            Self::Multiply => '*',
            Self::Divide => '/',
        })
    }
}

#[derive(Clone)]
pub(crate) struct Block {
    pub(crate) statements: Vec<Statement>,
    pub(crate) source: Source,
}

#[derive(Clone)]
pub(crate) enum Expr {
    Unit(Source),
    Int(Source),
    String(Source),
    Id(Source),
    Function {
        name: Option<Source>,
        source: Source,
        parameters: Vec<Parameter>,
        body: Box<Expr>,
    },
    Binary {
        source: Source,
        operation: BinaryOperation,
        left: Box<Expr>,
        right: Box<Expr>,
    },
    Application {
        source: Source,
        function: Box<Expr>,
        arguments: Vec<Expr>,
    },
    Conditional {
        source: Source,
        condition: Box<Expr>,
        then_branch: Box<Expr>,
        else_branch: Option<Box<Expr>>,
    },
    Block(Block),
}

impl Debug for Expr {
    fn fmt(&self, formatter: &mut Formatter<'_>) -> std::fmt::Result {
        write!(formatter, "{:?}", self.source())
    }
}

impl Expr {
    pub(crate) fn source(&self) -> &Source {
        match self {
            Self::Unit(source)
            | Self::String(source)
            | Self::Int(source)
            | Self::Id(source)
            | Self::Function { source, .. }
            | Self::Binary { source, .. }
            | Self::Application { source, .. }
            | Self::Conditional { source, .. }
            | Self::Block(Block { source, .. }) => source,
        }
    }
}

#[derive(Clone)]
pub(crate) enum Statement {
    Expr(Expr),
    Binding {
        name: Source,
        value: Expr,
        source: Source,
    },
}

impl Debug for Statement {
    fn fmt(&self, formatter: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Expr(expr) => expr.fmt(formatter),
            Self::Binding { name, value, .. } => write!(formatter, "let {name:?} = {value:?}"),
        }
    }
}

type ParseResult<T> = Result<T, Box<dyn Error>>;

pub(crate) struct Parser {
    lexer: Lexer,
    lexeme: Lexeme,
    lexeme_line: usize,
    previous_lexeme_line: usize,
    previous_lexeme_source: Source,
}

macro error($text: expr, $location: expr) {
    Self::error(line!(), $text, $location)
}

impl Parser {
    pub(crate) fn new(lexer: Lexer) -> Self {
        let eof_lexeme = lexer.eof_lexeme.clone();
        let dummy_source = eof_lexeme.source.clone();
        Self {
            lexer,
            lexeme: eof_lexeme,
            lexeme_line: 0,
            previous_lexeme_line: 0,
            previous_lexeme_source: dummy_source,
        }
    }

    pub(crate) fn parse(&mut self) -> ParseResult<Block> {
        self.read_lexeme()?;
        let start = self.lexeme.source.clone();
        let mut statements = vec![];
        while self.lexeme.token != Token::Eof {
            statements.push(self.parse_statement()?);
        }
        Ok(Block {
            statements,
            source: start.extend(&self.lexeme.source),
        })
    }

    pub(crate) const fn lexer_stats(&self) -> &LexerStats {
        self.lexer.stats()
    }

    fn parse_statement(&mut self) -> ParseResult<Statement> {
        if self.lexeme.token == Token::Id && self.lexeme.text() == "let" {
            let start = self.lexeme.source.clone();
            self.read_lexeme()?;
            self.parse_binding(&start)
        } else if self.lexeme.token == Token::Id && self.lexeme.text() == "fn" {
            let start = self.lexeme.source.clone();
            self.read_lexeme()?;
            self.parse_function(&start)
        } else {
            Ok(Statement::Expr(self.parse_expression()?))
        }
    }

    fn parse_function(&mut self, start: &Source) -> ParseResult<Statement> {
        let name = self.expect(Token::Id, "identifier")?;
        let parameters = self.parse_function_parameters()?;
        let body = self.parse_block_or_expr()?;
        let source = start.extend(&self.previous_lexeme_source);
        let function = Expr::Function {
            name: Some(name.source.clone()),
            source: source.clone(),
            parameters,
            body: Box::new(body),
        };
        Ok(Statement::Binding {
            name: name.source,
            value: function,
            source,
        })
    }

    fn parse_expression(&mut self) -> ParseResult<Expr> {
        self.parse_binary(Precedence::Assignment)
    }

    fn parse_conditional(&mut self, start: &Source) -> ParseResult<Expr> {
        let condition_is_parenthesized = self.lexeme.token == Token::LParen;
        let condition = {
            if condition_is_parenthesized {
                self.read_lexeme()?;
            }
            let condition = Box::new(self.parse_expression()?);
            if condition_is_parenthesized {
                self.expect(Token::RParen, ")")?;
            }
            condition
        };
        let then_branch = if condition_is_parenthesized {
            self.parse_block_or_expr()?
        } else {
            self.parse_block()?
        };
        let else_branch = if self.lexeme.token == Token::Id && self.lexeme.text() == "else" {
            self.read_lexeme()?;
            Some(self.parse_block_or_expr()?)
        } else {
            None
        };
        Ok(Expr::Conditional {
            source: start.extend(&self.previous_lexeme_source),
            condition,
            then_branch: Box::new(then_branch),
            else_branch: else_branch.map(Box::new),
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
        Ok(Expr::Block(Block { statements, source }))
    }

    fn parse_block(&mut self) -> ParseResult<Expr> {
        let mut statements = vec![];
        let start = self.expect(Token::LBrace, "{")?.source;
        while self.lexeme.token != Token::Eof && self.lexeme.token != Token::RBrace {
            statements.push(self.parse_statement()?);
        }
        let end = self.expect(Token::RBrace, "}")?.source;
        let source = start.extend(&end);
        Ok(Expr::Block(Block { statements, source }))
    }

    fn parse_binary(&mut self, precedence: Precedence) -> ParseResult<Expr> {
        let parse_next = |self_: &mut Self, precedence: Option<Precedence>| {
            if let Some(precedence) = precedence {
                self_.parse_binary(precedence)
            } else {
                self_.parse_function_application()
            }
        };

        let start = self.lexeme.source.clone();
        let mut result = parse_next(self, precedence.next_binary_precedence())?;
        let first_lexeme_token = self.lexeme.token;
        if self.previous_lexeme_line != self.lexeme_line || !first_lexeme_token.is_binary_operator()
        {
            return Ok(result);
        }

        let first_operator_precedence = first_lexeme_token.precedence();
        if first_operator_precedence >= precedence {
            loop {
                let operator = self.lexeme.token;
                if self.previous_lexeme_line != self.lexeme_line
                    || !operator.is_binary_operator()
                    || operator.precedence() < first_operator_precedence
                {
                    break;
                }
                self.read_lexeme()?;
                let right_precedence = Some(first_operator_precedence).and_then(|p| {
                    if operator.is_left_associative() {
                        p.next_binary_precedence()
                    } else {
                        Some(p)
                    }
                });
                let right_operand = parse_next(self, right_precedence)?;
                result = Expr::Binary {
                    source: start.extend(&self.previous_lexeme_source),
                    operation: operator.binary_operation(),
                    left: Box::new(result),
                    right: Box::new(right_operand),
                };
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

        let result = if arguments.is_empty() {
            first_expr
        } else {
            Expr::Application {
                source: start.extend(&self.previous_lexeme_source),
                function: Box::new(first_expr),
                arguments,
            }
        };
        Ok(result)
    }

    fn parse_primary(&mut self) -> ParseResult<Expr> {
        let primary = self.lexeme.clone();
        if self.lexeme.token == Token::LBrace {
            return self.parse_block();
        }
        self.read_lexeme()?;
        match primary.token {
            Token::Id => {
                if primary.text() == "if" {
                    self.parse_conditional(&primary.source)
                } else {
                    Ok(Expr::Id(primary.source))
                }
            }
            Token::Int => Ok(Expr::Int(primary.source)),
            Token::String => Ok(Expr::String(primary.source)),
            Token::Lambda => self.parse_lambda(&primary.source),
            Token::LParen => {
                if self.lexeme.token == Token::RParen {
                    self.read_lexeme()?;
                    let source = primary.source.extend(&self.previous_lexeme_source);
                    Ok(Expr::Unit(source))
                } else {
                    let expr = self.parse_expression()?;
                    self.expect(Token::RParen, ")")?;
                    Ok(expr)
                }
            }
            _ => error!(
                &format!("expected an expression, found: {primary}"),
                &primary.source
            ),
        }
    }

    fn parse_binding(&mut self, start: &Source) -> ParseResult<Statement> {
        let name = self.expect(Token::Id, "identifier")?;
        self.expect(Token::Eq, "=")?;
        let value = self.parse_expression()?;
        let source = start.extend(&self.previous_lexeme_source);
        Ok(Statement::Binding {
            name: name.source,
            value,
            source,
        })
    }

    fn parse_lambda(&mut self, start: &Source) -> ParseResult<Expr> {
        let parameters = self.parse_function_parameters()?;
        self.expect(Token::Arrow, "arrow")?;
        let body = self.parse_block_or_expr()?;
        Ok(Expr::Function {
            name: None,
            source: start.extend(&self.previous_lexeme_source),
            parameters,
            body: Box::new(body),
        })
    }

    fn parse_function_parameters(&mut self) -> ParseResult<Vec<Parameter>> {
        let mut parameters = vec![];
        let lparen = self.expect(Token::LParen, "`('")?;
        if self.lexeme.token == Token::RParen {
            let source = lparen.source.extend(&self.lexeme.source);
            self.read_lexeme()?;
            parameters.push(Parameter {
                name: source.clone(),
                source,
                type_: Type::Unit,
            });
        } else {
            let mut first = true;
            let mut already_declared = FxHashMap::<String, Source>::default();
            while self.lexeme.token != Token::RParen {
                if !first {
                    self.expect(Token::Comma, "`,'")?;
                }
                let parameter = self.parse_parameter()?;
                let parameter_name_str = parameter.name.text();
                if let Some(previous_definition) = already_declared.get(parameter_name_str) {
                    let message = format!(
                        "redefinition of parameter `{parameter_name_str}', previous definition is at {previous_definition}"
                    );
                    return error!(&message, &parameter.name);
                }
                already_declared.insert(parameter_name_str.to_owned(), parameter.name.clone());
                parameters.push(parameter);
                first = false;
            }
            self.read_lexeme()?;
        }

        if parameters.is_empty() {
            error!(
                &format!("expected an argument list, found: {}", self.lexeme),
                &self.lexeme.source
            )
        } else {
            Ok(parameters)
        }
    }

    fn parse_parameter(&mut self) -> ParseResult<Parameter> {
        let id = self.expect(Token::Id, "identifier")?;
        self.expect(Token::Colon, ":")?;
        let type_ = self.parse_type()?;
        Ok(Parameter {
            name: id.source.clone(),
            source: id.source.extend(&self.previous_lexeme_source),
            type_,
        })
    }

    fn parse_type(&mut self) -> ParseResult<Type> {
        let (token, source) = (self.lexeme.token, self.lexeme.source.clone());
        self.read_lexeme()?;
        match (token, source.text()) {
            (Token::Id, "num") => Ok(Type::Int),
            (Token::Id, "str") => Ok(Type::String),
            (Token::Lambda, _) => self.parse_function_type(),
            _ => {
                error!(
                    &format!("expected a type, found: {}", source.text()),
                    &source
                )
            }
        }
    }

    fn parse_function_type(&mut self) -> ParseResult<Type> {
        let parameters = self.parse_function_parameters()?;
        self.expect(Token::Arrow, "arrow")?;
        let result = self.parse_type()?;
        Ok(Type::Function(FunctionTypeRef::new(FunctionType {
            parameters,
            result,
        })))
    }

    fn expect(&mut self, token: Token, name: &str) -> ParseResult<Lexeme> {
        let lexeme = self.lexeme.clone();
        if lexeme.token == token {
            self.read_lexeme()?;
            Ok(lexeme)
        } else {
            let starts_with_a_letter = name.chars().next().into_iter().all(char::is_alphanumeric);
            let (opening_quote, closing_quote) = if starts_with_a_letter {
                ("`", "'")
            } else {
                ("", "")
            };
            error!(
                &format!(
                    "expected {} {}{}{}, found: {}",
                    article(name),
                    opening_quote,
                    name,
                    closing_quote,
                    lexeme
                ),
                &lexeme.source
            )
        }
    }

    fn read_lexeme(&mut self) -> ParseResult<()> {
        self.previous_lexeme_line = self.lexeme_line;
        self.previous_lexeme_source = self.lexeme.source.clone();
        let (lexeme, line) = self.lexer.read()?;
        self.lexeme = lexeme;
        self.lexeme_line = line;
        Ok(())
    }

    fn error<T>(line: u32, text: &str, source: &Source) -> ParseResult<T> {
        Err(Self::format_error(line, text, source)).map_err(From::from)
    }

    fn format_error(line: u32, text: &str, source: &Source) -> String {
        format!("[{line}] {source}: {text}")
    }
}

#[derive(Copy, Clone, PartialOrd, PartialEq, Debug)]
enum Precedence {
    Assignment,
    Additive,
    Multiplicative,
}

impl Precedence {
    fn next_binary_precedence(self) -> Option<Self> {
        let next_precedence = match self {
            Self::Assignment => Some(Self::Additive),
            Self::Additive => Some(Self::Multiplicative),
            Self::Multiplicative => None,
        };
        if let Some(next_precedence) = next_precedence {
            assert!(next_precedence > self);
        }
        next_precedence
    }
}

impl Token {
    fn is_binary_operator(self) -> bool {
        matches!(
            self,
            Self::Eq | Self::Plus | Self::Minus | Self::Star | Self::Slash
        )
    }

    fn precedence(self) -> Precedence {
        match self {
            Self::Eq => Precedence::Assignment,
            Self::Plus | Self::Minus => Precedence::Additive,
            Self::Star | Self::Slash => Precedence::Multiplicative,
            _ => unreachable!(),
        }
    }

    fn is_left_associative(self) -> bool {
        matches!(self, Self::Plus | Self::Minus | Self::Star | Self::Slash)
    }

    fn binary_operation(self) -> BinaryOperation {
        match self {
            Self::Eq => BinaryOperation::Assign,
            Self::Plus => BinaryOperation::Add,
            Self::Minus => BinaryOperation::Subtract,
            Self::Star => BinaryOperation::Multiply,
            Self::Slash => BinaryOperation::Divide,
            _ => unreachable!(),
        }
    }
}

impl Lexeme {
    fn can_be_expression_start(&self) -> bool {
        matches!(self.token,
            Token::Int | Token::String | Token::LParen | Token::Lambda | Token::Id if self.text() != "else")
    }
}

fn article(word: &str) -> &str {
    "aouie"
        .chars()
        .find(|&c| word.starts_with(c))
        .map_or("a", |_| "an")
}
