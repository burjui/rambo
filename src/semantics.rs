use crate::parser::BinaryOperation;
use crate::parser::Block as ASTBlock;
use crate::parser::Expr;
use crate::parser::Parameter as ParsedParameter;
use crate::parser::Parameter;
use crate::parser::Statement;
use crate::source::Source;
use itertools::Itertools;
use num_bigint::BigInt;
use std::cell::RefCell;
use std::collections::HashMap;
use std::error::Error;
use std::fmt::Debug;
use std::fmt::Formatter;
use std::hash::Hash;
use std::hash::Hasher;
use std::ops::Deref;
use std::rc::Rc;

crate type FunctionTypeRef = Rc<FunctionType>;

#[derive(Clone, PartialEq)]
crate struct FunctionType {
    crate parameters: Vec<Parameter>,
    crate result: Type
}

impl Debug for FunctionType {
    fn fmt(&self, formatter: &mut Formatter<'_>) -> std::fmt::Result {
        let parameter_list = self.parameters.iter().map(|x| format!("{:?}", x)).join(" ");
        write!(formatter, "({} -> {:?})", parameter_list, self.result)
    }
}

// TODO impl Debug with parenthesis
#[derive(Clone, PartialEq)]
crate enum Type {
    Unit,
    Int,
    String,
    Function(FunctionTypeRef),
}

impl Debug for Type {
    fn fmt(&self, formatter: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Unit => write!(formatter, "()"),
            Type::Int => write!(formatter, "num"),
            Type::String => write!(formatter, "str"),
            Type::Function(type_) => type_.fmt(formatter)
        }
    }
}

#[derive(Clone)]
crate struct ExprRef(Rc<TypedExpr>);

impl From<TypedExpr> for ExprRef {
    fn from(expr: TypedExpr) -> Self {
        Self(Rc::new(expr))
    }
}

impl Deref for ExprRef {
    type Target = TypedExpr;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl Debug for ExprRef {
    fn fmt(&self, formatter: &mut Formatter<'_>) -> std::fmt::Result {
        self.0.fmt(formatter)
    }
}

#[derive(Clone)]
crate struct Lambda {
    crate type_: FunctionTypeRef,
    crate parameters: Vec<Parameter>,
    crate body: ExprRef
}

impl Debug for Lambda {
    fn fmt(&self, formatter: &mut Formatter<'_>) -> std::fmt::Result {
        write!(formatter, "(λ {:?} = {:?})", self.type_, &self.body)
    }
}

crate struct Block {
    crate statements: Vec<TypedStatement>,
    crate source: Source
}

impl Debug for Block {
    fn fmt(&self, formatter: &mut Formatter<'_>) -> std::fmt::Result {
        writeln!(formatter, "{{")?;
        for statement in &self.statements {
            writeln!(formatter, "{:?}", statement)?;
        }
        write!(formatter, "}}")
    }
}

crate enum TypedExpr {
    Phantom(Type), // TODO rename to PseudoArg
    Unit(Source),
    Int(BigInt, Source),
    String(String, Source),
    Deref(String, Type, Source),
    Lambda(Rc<Lambda>, Source),
    Application {
        type_: Type,
        function: ExprRef,
        arguments: Vec<ExprRef>,
        source: Source
    },
    AddInt(ExprRef, ExprRef, Source),
    SubInt(ExprRef, ExprRef, Source),
    MulInt(ExprRef, ExprRef, Source),
    DivInt(ExprRef, ExprRef, Source),
    AddStr(ExprRef, ExprRef, Source),
    Assign(String, ExprRef, Source),
    Conditional {
        condition: ExprRef,
        positive: ExprRef,
        negative: Option<ExprRef>,
        source: Source
    },
    Block(Block)
}

impl Debug for TypedExpr {
    fn fmt(&self, formatter: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            TypedExpr::Phantom(_) => write!(formatter, "@"),
            TypedExpr::Unit(_) => write!(formatter, "()"),
            TypedExpr::Int(value, _) => write!(formatter, "{}", value),
            TypedExpr::String(value, _) => write!(formatter, "\"{}\"", value),
            TypedExpr::Deref(name, _, _) => write!(formatter, "#{}", name),
            TypedExpr::AddInt(left, right, _) => write!(formatter, "({:?} + {:?})", left, right),
            TypedExpr::SubInt(left, right, _) => write!(formatter, "({:?} - {:?})", left, right),
            TypedExpr::MulInt(left, right, _) => write!(formatter, "({:?} * {:?})", left, right),
            TypedExpr::DivInt(left, right, _) => write!(formatter, "({:?} / {:?})", left, right),
            TypedExpr::AddStr(left, right, _) => write!(formatter, "({:?} + {:?})", left, right),
            TypedExpr::Assign(name, value, _) => write!(formatter, "({} = {:?})", name, value),
            TypedExpr::Lambda(lambda, _) => lambda.fmt(formatter),
            TypedExpr::Application { function, arguments, .. } => write!(formatter, "({:?} @ {:?})", function, arguments),
            TypedExpr::Conditional { condition, positive, negative, .. } => {
                let negative = match negative {
                    Some(negative) => format!(" else {:?})", negative),
                    _ => "".to_owned()
                };
                write!(formatter, "(if {:?} {:?}{})", condition, positive, negative)
            },
            TypedExpr::Block(block) => block.fmt(formatter)
        }
    }
}

impl TypedExpr {
    crate fn type_(&self) -> Type {
        match self {
            TypedExpr::Phantom(type_) => type_.clone(),
            TypedExpr::Unit(_) => Type::Unit,

            TypedExpr::Int(_, _) |
            TypedExpr::AddInt(_, _, _) |
            TypedExpr::SubInt(_, _, _) |
            TypedExpr::MulInt(_, _, _) |
            TypedExpr::DivInt(_, _, _) => Type::Int,

            TypedExpr::String(_, _) |
            TypedExpr::AddStr(_, _, _) => Type::String,

            TypedExpr::Deref(_, type_, _) => type_.clone(),
            TypedExpr::Assign(_, value, _) => value.type_(),
            TypedExpr::Lambda(lambda, _) => Type::Function(lambda.type_.clone()),
            TypedExpr::Application { type_, .. } => type_.clone(),
            TypedExpr::Conditional { positive, negative, .. } => {
                if negative.is_none() {
                    Type::Unit
                } else {
                    positive.type_()
                }
            },
            TypedExpr::Block(Block { statements, .. }) => statements.last().map(TypedStatement::type_).unwrap_or_else(|| Type::Unit)
        }
    }

    crate fn clone_at(&self, source: Source) -> ExprRef {
        ExprRef::from(match self {
            TypedExpr::Phantom(type_) => TypedExpr::Phantom(type_.clone()),
            TypedExpr::Unit(_) => TypedExpr::Unit(source),
            TypedExpr::Int(value, _) => TypedExpr::Int(value.clone(), source),
            TypedExpr::AddInt(left, right, _) => TypedExpr::AddInt(left.clone(), right.clone(), source),
            TypedExpr::SubInt(left, right, _) => TypedExpr::SubInt(left.clone(), right.clone(), source),
            TypedExpr::MulInt(left, right, _) => TypedExpr::MulInt(left.clone(), right.clone(), source),
            TypedExpr::DivInt(left, right, _) => TypedExpr::DivInt(left.clone(), right.clone(), source),
            TypedExpr::String(value, _) => TypedExpr::String(value.clone(), source),
            TypedExpr::AddStr(left, right, _) => TypedExpr::AddStr(left.clone(), right.clone(), source),
            TypedExpr::Deref(name, type_, _) => TypedExpr::Deref(name.clone(), type_.clone(), source),
            TypedExpr::Assign(left, right, _) => TypedExpr::Assign(left.clone(), right.clone(), source),
            TypedExpr::Lambda(lambda, _) => TypedExpr::Lambda(lambda.clone(), source),
            TypedExpr::Application { type_, function, arguments, .. } => TypedExpr::Application {
                type_: type_.clone(),
                function: function.clone(),
                arguments: arguments.clone(),
                source
            },
            TypedExpr::Conditional { condition, positive, negative, .. } => TypedExpr::Conditional {
                condition: condition.clone(),
                positive: positive.clone(),
                negative: negative.clone(),
                source
            },
            TypedExpr::Block(Block { statements, .. }) => TypedExpr::Block(Block{
                statements: statements.clone(),
                source
            }),
        })
    }
}

crate type BindingCell = RefCell<Binding>;

#[derive(Clone)]
crate struct BindingRef(Rc<BindingCell>);

impl From<Binding> for BindingRef {
    fn from(binding: Binding) -> Self {
        Self(Rc::new(BindingCell::new(binding)))
    }
}

impl Deref for BindingRef {
    type Target = Rc<BindingCell>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl PartialEq<BindingRef> for BindingRef {
    fn eq(&self, other: &BindingRef) -> bool {
        Rc::ptr_eq(self, other)
    }
}

impl Eq for BindingRef {}

impl Hash for BindingRef {
    fn hash<H: Hasher>(&self, state: &mut H) {
        Rc::into_raw(self.0.clone()).hash(state)
    }
}

impl Debug for BindingRef {
    fn fmt(&self, formatter: &mut Formatter<'_>) -> std::fmt::Result {
        self.0.borrow().fmt(formatter)
    }
}

crate struct Binding {
    crate name: String,
    crate data: ExprRef,
    crate source: Source,
}

impl Binding {
    crate fn new(name: String, data: ExprRef, source: Source) -> Binding {
        Binding { name, data, source }
    }
}

impl Debug for Binding {
    fn fmt(&self, formatter: &mut Formatter<'_>) -> std::fmt::Result {
        write!(formatter, "let {} = {:?}", self.name, self.data)
    }
}

#[derive(Clone)]
crate enum TypedStatement {
    Expr(ExprRef),
    Binding(BindingRef) // TODO replace with Binding(name, type, source...)
}

impl Debug for TypedStatement {
    fn fmt(&self, formatter: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            TypedStatement::Expr(expr) => expr.fmt(formatter),
            TypedStatement::Binding(binding) => binding.borrow().fmt(formatter)
        }
    }
}

impl TypedStatement {
    crate fn type_(&self) -> Type {
        match self {
            TypedStatement::Binding(_) => Type::Unit,
            TypedStatement::Expr(expr) => expr.type_()
        }
    }
}

type CheckResult<T> = Result<T, Box<dyn Error>>;

crate fn check_module(code: &ASTBlock) -> CheckResult<ExprRef> {
    let mut statements = vec![];
    let mut env = Environment::new();
    for statement in &code.statements {
        statements.push(check_statement(&mut env, statement)?);
    }
    Ok(ExprRef::from(TypedExpr::Block(Block {
        statements,
        source: code.source.clone()
    })))
}

fn check_statement(env: &mut Environment, statement: &Statement) -> CheckResult<TypedStatement> {
    match statement {
        Statement::Expr(expr) => {
            let expr = check_expr(env, expr)?;
            Ok(TypedStatement::Expr(expr))
        },
        Statement::Binding { name, value, source } => {
            let value = check_expr(env, &value)?;
            let binding = BindingRef::from(Binding {
                name: name.text().to_owned(),
                data: value,
                source: source.clone(),
            });
            env.bind(binding.clone());
            Ok(TypedStatement::Binding(binding))
        }
    }
}

fn check_expr(env: &mut Environment, expr: &Expr) -> CheckResult<ExprRef> {
    match expr {
        Expr::Unit(source) => Ok(ExprRef::from(TypedExpr::Unit(source.clone()))),
        Expr::Int(source) => {
            let value = source.text().parse::<BigInt>()?;
            Ok(ExprRef::from(TypedExpr::Int(value, source.clone())))
        },
        Expr::String(source) => {
            let text = source.text();
            let value = text[1..text.len() - 1].to_owned();
            Ok(ExprRef::from(TypedExpr::String(value, source.clone())))
        },
        Expr::Id(name) => env.resolve(name),
        Expr::Binary { operation, left, right, .. } => {
            match operation {
                BinaryOperation::Assign => {
                    if let Expr::Id(name) = left as &Expr {
                        env.resolve(name)?;
                        let right_checked = check_expr(env, right)?;
                        Ok(ExprRef::from(TypedExpr::Assign(name.text().to_owned(), right_checked, expr.source().clone())))
                    } else {
                        error!("a variable expected at the left side of assignment, but found: {:?}", left)
                    }
                },
                _ => {
                    let left_checked = check_expr(env, left)?;
                    let right_checked = check_expr(env, right)?;
                    let left_type = left_checked.type_();
                    let right_type = right_checked.type_();
                    if left_type != right_type {
                        return error!("operands of `{:?}' have incompatible types:\n  {:?}: {:?}\n  {:?}: {:?}",
                                      operation, left, left_type, right, right_type);
                    }

                    match operation {
                        BinaryOperation::Add => match left_type {
                            Type::Int => Ok(ExprRef::from(TypedExpr::AddInt(left_checked, right_checked, expr.source().clone()))),
                            Type::String => Ok(ExprRef::from(TypedExpr::AddStr(left_checked, right_checked, expr.source().clone()))),
                            _ => error!("operation `{:?}' is not implemented for type `{:?}'", operation, left_type)
                        },
                        operation => {
                            if let Type::Int = left_type {
                                let constructor =
                                    match operation {
                                        BinaryOperation::Subtract => TypedExpr::SubInt,
                                        BinaryOperation::Multiply => TypedExpr::MulInt,
                                        BinaryOperation::Divide => TypedExpr::DivInt,
                                        _ => unreachable!()
                                    };
                                Ok(ExprRef::from(constructor(left_checked, right_checked, expr.source().clone())))
                            } else {
                                error!("operation `{:?}' is not implemented for type `{:?}'", operation, left_type)
                            }
                        }
                    }
                }
            }
        },
        Expr::Lambda { source, parameters, body } => Ok(ExprRef::from(TypedExpr::Lambda(
            Rc::new(check_function(env, parameters.as_slice(), &body)?), source.clone()))),
        Expr::Application { source, function, arguments } => {
            let function = check_expr(env, &function)?;
            let function_type = function.type_();
            let function_type_result: CheckResult<FunctionTypeRef> = match function_type {
                Type::Function(type_) => Ok(type_.clone()),
                _ => error!("2 expected a function, found `{:?}' of type `{:?}'", expr, function_type)
            };
            let function_type = function_type_result?;

            let arguments_checked = {
                let function_parameters = &function_type.parameters;
                let parameter_count = function_parameters.len();
                let argument_count = arguments.len();
                if argument_count != parameter_count {
                    return error!("invalid number of arguments: expected {}, found {}:\n  {:?}",
                        parameter_count, argument_count, expr);
                }

                let mut arguments_checked = vec![];
                for (parameter, argument) in function_parameters.iter().zip(arguments.iter()) {
                    let argument_checked = check_expr(env, argument)?;
                    let argument_type = argument_checked.type_();
                    if argument_type != parameter.type_ {
                        return error!("argument type mismatch for {}: expected `{:?}', found `{:?}'\n  {:?}",
                            parameter.name, parameter.type_, argument_type, argument);
                    }
                    arguments_checked.push(argument_checked);
                }

                arguments_checked
            };

            Ok(ExprRef::from(TypedExpr::Application {
                type_: function_type.result.clone(),
                function,
                arguments: arguments_checked,
                source: source.clone()
            }))
        },
        Expr::Conditional { source, condition, positive, negative } => {
            let condition_typed = check_expr(env, condition)?;
            let condition_type = condition_typed.type_();
            if condition_type != Type::Int && condition_type != Type::String {
                return error!("a condition can only be of type `num' or `str': {:?}", condition.source())
            }

            let (positive, positive_source) = match positive {
                box Expr::Block(ASTBlock { source, statements }) => (statements, source),
                _ => unreachable!()
            };
            if positive.is_empty() {
                warning!("empty positive conditional clause: {:?}", positive_source);
            }
            let positive = check_block(env, positive.iter(), positive_source.clone())?;
            let positive_type = positive.type_();

            let negative = match negative {
                Some(box Expr::Block(ASTBlock { source, statements, .. })) => {
                    if statements.is_empty() {
                        warning!("empty negative conditional clause: {:?}", source);
                    }
                    Some(check_block(env, statements.iter(), source.clone())?)
                },
                _ => None
            };
            let negative_type = negative.as_ref().map(|expr| expr.type_());

            if let Some(negative_type) = negative_type {
                if positive_type != negative_type {
                    return error!("types of positive and negative clauses of a conditional don't match: `{:?}' and `{:?}'",
                        positive_type, negative_type);
                }
            }

            Ok(ExprRef::from(TypedExpr::Conditional {
                condition: condition_typed,
                positive,
                negative,
                source: source.clone()
            }))
        },
        Expr::Block(ASTBlock { source, statements }) => {
            let statements = statements.iter()
                .map(|statement| check_statement(env, statement))
                .collect::<CheckResult<Vec<_>>>()?;
            Ok(ExprRef::from(TypedExpr::Block(Block {
                statements,
                source: source.clone()
            })))
        }
    }
}

fn check_function(env: &mut Environment, parameters: &[ParsedParameter], body: &Expr) -> CheckResult<Lambda> {
    env.push();
    for parameter in parameters {
        env.bind(BindingRef::from(Binding {
            name: parameter.name.text().to_owned(),
            data: ExprRef::from(TypedExpr::Phantom(parameter.type_.clone())),
            source: parameter.source.clone(),
        }));
    }
    let body = check_expr(env, body)?;
    env.pop();

    let parameters = parameters.to_vec();
    let function_type = Rc::new(FunctionType {
        parameters: parameters.clone(),
        result: body.type_()
    });
    Ok(Lambda{
        type_: function_type,
        parameters,
        body
    })
}

fn check_block<'a, BlockIterator>(env: &mut Environment, block: BlockIterator, source: Source) -> CheckResult<ExprRef>
    where BlockIterator: Iterator<Item = &'a Statement>
{
    let statements = block
        .map(|statement| check_statement(env, statement))
        .collect::<Result<_, _>>()?;
    Ok(ExprRef::from(TypedExpr::Block(Block {
        statements,
        source
    })))
}

struct Environment {
    scopes: Vec<HashMap<String, BindingRef>>
}

impl Environment {
    crate fn new() -> Environment {
        Environment {
            scopes: vec![HashMap::new()]
        }
    }

    crate fn bind(&mut self, binding: BindingRef) {
        let name = binding.borrow().name.to_owned();
        self.scopes.last_mut().unwrap().insert(name, binding);
    }

    crate fn resolve(&self, name: &Source) -> Result<ExprRef, Box<dyn Error>> {
        for scope in self.scopes.iter().rev() {
            if let Some(binding) = scope.get(name.text()) {
                let value = &binding.borrow().data;
                return Ok(ExprRef::from(TypedExpr::Deref(name.text().to_owned(), value.type_(), name.clone())));
            }
        }
        error!("`{:?}' is undefined", name.text())
    }

    fn push(&mut self) {
        self.scopes.push(HashMap::new())
    }

    fn pop(&mut self) {
        self.scopes.pop();
    }
}
