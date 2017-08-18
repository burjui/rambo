use std::fmt::{Debug, Formatter, Result as FmtResult};
use std::error::Error;
use std::rc::Rc;
use std::cell::RefCell;
use num::BigInt;
use std::collections::HashMap;
use itertools::Itertools;
use utils::*;

use parser::*;

pub type FunctionTypeRef = Rc<FunctionType>;

#[derive(Clone, PartialEq)]
pub struct Parameter {
    pub name: String,
    pub type_: Type
}

impl Debug for Parameter {
    fn fmt(&self, formatter: &mut Formatter) -> FmtResult {
        write!(formatter, "({}: {:?})", self.name, self.type_)
    }
}

#[derive(Clone, PartialEq)]
pub struct FunctionType {
    pub parameters: Vec<Parameter>,
    pub result: Type
}

impl Debug for FunctionType {
    fn fmt(&self, formatter: &mut Formatter) -> FmtResult {
        let parameter_list = self.parameters.iter().map(|x| format!("{:?}", x)).join(" ");
        write!(formatter, "({} -> {:?})", parameter_list, self.result)
    }
}

// TODO impl Debug with parenthesis
#[derive(Clone, PartialEq)]
pub enum Type {
    Unit,
    Int,
    String,
    Function(FunctionTypeRef),
}

impl Debug for Type {
    fn fmt(&self, formatter: &mut Formatter) -> FmtResult {
        match self {
            &Type::Unit => write!(formatter, "()"),
            &Type::Int => write!(formatter, "num"),
            &Type::String => write!(formatter, "str"),
            &Type::Function(ref type_) => type_.fmt(formatter)
        }
    }
}

pub type ExprRef = Rc<TypedExpr>;

pub enum TypedExpr {
    Phantom,
    Unit,
    Int(BigInt),
    String(String),
    Deref(BindingRef),
    Lambda {
        type_: FunctionTypeRef,
        parameter_bindings: Vec<BindingRef>,
        body: ExprRef
    },
    Application {
        type_: Type,
        function: ExprRef,
        arguments: Vec<ExprRef>
    },
    AddInt(ExprRef, ExprRef),
    SubInt(ExprRef, ExprRef),
    MulInt(ExprRef, ExprRef),
    DivInt(ExprRef, ExprRef),
    AddStr(ExprRef, ExprRef),
    Assign(ExprRef, ExprRef),
    Conditional {
        condition: ExprRef,
        positive: ExprRef,
        negative: Option<ExprRef>
    },
    Block(Vec<TypedStatement>)
}

impl Debug for TypedExpr {
    fn fmt(&self, formatter: &mut Formatter) -> FmtResult {
        match self {
            &TypedExpr::Phantom => write!(formatter, "@"),
            &TypedExpr::Unit => write!(formatter, "()"),
            &TypedExpr::Int(ref value) => write!(formatter, "{}", value),
            &TypedExpr::String(ref value) => write!(formatter, "\"{}\"", value),
            &TypedExpr::Deref(ref binding) => {
                let suffix = match &binding.borrow().value {
                    &BindingValue::Var(_) => format!("[{}]", binding.borrow().index),
                    _ => "".to_string()
                };
                write!(formatter, "(*{}{})", binding.borrow().name, suffix)
            },
            &TypedExpr::AddInt(ref left, ref right) => write!(formatter, "({:?} + {:?})", left, right),
            &TypedExpr::SubInt(ref left, ref right) => write!(formatter, "({:?} - {:?})", left, right),
            &TypedExpr::MulInt(ref left, ref right) => write!(formatter, "({:?} * {:?})", left, right),
            &TypedExpr::DivInt(ref left, ref right) => write!(formatter, "({:?} / {:?})", left, right),
            &TypedExpr::AddStr(ref left, ref right) => write!(formatter, "({:?} + {:?})", left, right),
            &TypedExpr::Assign(ref left, ref right) => write!(formatter, "({:?} = {:?})", left, right),
            &TypedExpr::Lambda { ref type_, ref body, .. } => write!(formatter, "(λ {:?} = {:?})", type_, body),
            &TypedExpr::Application { ref function, ref arguments, .. } => write!(formatter, "({:?} @ {:?})", function, arguments),
            &TypedExpr::Conditional { ref condition, ref positive, ref negative } => {
                let negative = match negative {
                    &Some(ref negative) => format!(" else {:?})", negative),
                    _ => "".to_string()
                };
                write!(formatter, "(if {:?} {:?}{})", condition, positive, negative)
            },
            &TypedExpr::Block(ref statements) => write!(formatter, "{{ {} }})", statements.iter().to_string("; "))
        }
    }
}

impl TypedExpr {
    pub fn type_(&self) -> Type {
        match self {
            &TypedExpr::Phantom => unreachable!(),
            &TypedExpr::Unit => Type::Unit,

            &TypedExpr::Int(_) |
            &TypedExpr::AddInt(_, _) |
            &TypedExpr::SubInt(_, _) |
            &TypedExpr::MulInt(_, _) |
            &TypedExpr::DivInt(_, _) => Type::Int,

            &TypedExpr::String(_) |
            &TypedExpr::AddStr(_, _) => Type::String,

            &TypedExpr::Deref(ref binding) => binding.borrow().type_(),
            &TypedExpr::Assign(ref left, _) => left.type_(),
            &TypedExpr::Lambda { ref type_, .. } => Type::Function(type_.clone()),
            &TypedExpr::Application { ref type_, .. } => type_.clone(),
            &TypedExpr::Conditional { ref positive, ref negative, .. } => {
                if negative.is_none() {
                    Type::Unit
                } else {
                    positive.type_()
                }
            },
            &TypedExpr::Block(ref statements) => statements.last().map(TypedStatement::type_).unwrap_or_else(|| Type::Unit)
        }
    }
}

#[derive(Debug, Clone)]
pub enum BindingValue {
    Var(ExprRef),
    Arg(Type)
}

impl BindingValue {
    pub fn type_(&self) -> Type {
        match self {
            &BindingValue::Var(ref expr) => expr.type_(),
            &BindingValue::Arg(ref type_) => type_.clone()
        }
    }
}

pub type BindingRef = Rc<RefCell<Binding>>;

pub struct Binding {
    pub name: String,
    pub value: BindingValue,
    pub index: usize,
    pub assigned: bool,
    pub dirty: bool
}

impl Binding {
    pub fn type_(&self) -> Type {
        self.value.type_()
    }
}

impl Debug for Binding {
    fn fmt(&self, formatter: &mut Formatter) -> FmtResult {
        write!(formatter, "let {}[{}] = {:?}", self.name, self.index, self.value)
    }
}

#[derive(Clone)]
pub enum TypedStatement {
    Expr(ExprRef),
    Binding(BindingRef)
}

impl Debug for TypedStatement {
    fn fmt(&self, formatter: &mut Formatter) -> FmtResult {
        match self {
            &TypedStatement::Expr(ref expr) => expr.fmt(formatter),
            &TypedStatement::Binding(ref binding) => binding.borrow().fmt(formatter)
        }
    }
}

impl TypedStatement {
    pub fn type_(&self) -> Type {
        match self {
            &TypedStatement::Binding(_) => Type::Unit,
            &TypedStatement::Expr(ref expr) => expr.type_()
        }
    }
}

type CheckResult<T> = Result<T, Box<Error>>;

pub fn check_module(code: &[Statement]) -> CheckResult<Vec<TypedStatement>> {
    let mut typed_module = vec![];
    let mut env = Environment::new();
    for statement in code {
        typed_module.push(check_statement(&mut env, &statement)?);
    }
    Ok(typed_module)
}

fn check_statement(env: &mut Environment, statement: &Statement) -> CheckResult<TypedStatement> {
    match statement {
        &Statement::Expr(ref expr) => {
            let expr = check_expr(env, expr)?;
            Ok(TypedStatement::Expr(expr))
        },
        &Statement::Binding { ref name, ref value } => {
            let value = check_expr(env, value)?;
            let binding = Rc::new(RefCell::new(Binding {
                name: name.text().to_string(),
                value: BindingValue::Var(value),
                index: env.current_scope_len(),
                assigned: false,
                dirty: false
            }));
            env.bind(binding.clone())?;
            Ok(TypedStatement::Binding(binding))
        }
    }
}

fn check_expr(env: &mut Environment, expr: &Expr) -> CheckResult<ExprRef> {
    match expr {
        &Expr::Unit(_) => Ok(ExprRef::new(TypedExpr::Unit)),
        &Expr::Int(ref source) => {
            let value = source.text().parse::<BigInt>()?;
            Ok(ExprRef::new(TypedExpr::Int(value)))
        },
        &Expr::String(ref source) => {
            let text = source.text();
            let value = text[1..text.len() - 1].to_string();
            Ok(ExprRef::new(TypedExpr::String(value)))
        },
        &Expr::Id(ref name) => env.resolve(name.text()),
        &Expr::Binary { ref operation, ref left, ref right, .. } => {
            let left_checked = check_expr(env, left)?;
            let right_checked = check_expr(env, right)?;
            let left_type = left_checked.type_();
            let right_type = right_checked.type_();
            if left_type != right_type {
                return error!("operands of `{:?}' have incompatible types:\n  {:?}: {:?}\n  {:?}: {:?}",
                              operation, left, left_type, right, right_type);
            }

            match operation {
                &BinaryOperation::Assign => {
                    if let TypedExpr::Deref(_) = *left_checked {
                        Ok(ExprRef::new(TypedExpr::Assign(left_checked, right_checked)))
                    } else {
                        error!("a variable expected at the left side of assignment, but found: {:?}", left_checked)
                    }
                },
                &BinaryOperation::Add => match &left_type {
                    &Type::Int => Ok(ExprRef::new(TypedExpr::AddInt(left_checked, right_checked))),
                    &Type::String => Ok(ExprRef::new(TypedExpr::AddStr(left_checked, right_checked))),
                    _ => error!("operation `{:?}' is not implemented for type `{:?}'", operation, left_type)
                },
                operation => {
                    if let &Type::Int = &left_type {
                        let constructor =
                            match operation {
                                &BinaryOperation::Subtract => TypedExpr::SubInt,
                                &BinaryOperation::Multiply => TypedExpr::MulInt,
                                &BinaryOperation::Divide => TypedExpr::DivInt,
                                _ => unreachable!()
                            };
                        Ok(ExprRef::new(constructor(left_checked, right_checked)))
                    } else {
                        error!("operation `{:?}' is not implemented for type `{:?}'", operation, left_type)
                    }
                }
            }
        },
        &Expr::Lambda { ref parameters, ref body, .. } => {
            env.push();
            let mut parameter_bindings = vec![];
            for (parameter_index, parameter) in parameters.iter().enumerate() {
                let binding = Rc::new(RefCell::new(Binding {
                    name: parameter.name.to_string(),
                    value: BindingValue::Arg(parameter.type_.clone()),
                    index: parameter_index,
                    assigned: false,
                    dirty: false
                }));
                parameter_bindings.push(binding.clone());
                env.bind(binding)?;
            }
            let body = check_expr(env, body)?;
            env.pop();
            let parameters = parameters.iter()
                .map(|parameter| Parameter {
                    name: parameter.name.to_string(),
                    type_: parameter.type_.clone()
                })
                .collect();
            let function_type = Rc::new(FunctionType {
                parameters,
                result: body.type_()
            });
            Ok(ExprRef::new(TypedExpr::Lambda{
                type_: function_type,
                parameter_bindings,
                body
            }))
        },
        &Expr::Application { ref function, ref arguments, .. } => {
            let function_checked = check_expr(env, function)?;
            let function_checked_type = function_checked.type_();
            let function_type;
            if let &Type::Function(ref type_) = &function_checked_type {
                function_type = type_.clone()
            } else {
                return error!("expected a function, found `{:?}' of type `{:?}'", function, function_checked_type);
            };

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
                if &argument_type != &parameter.type_ {
                    return error!("argument type mismatch for {}: expected `{:?}', found `{:?}'\n  {:?}",
                        parameter.name, parameter.type_, argument_type, argument);
                }
                arguments_checked.push(argument_checked);
            }

            Ok(ExprRef::new(TypedExpr::Application {
                type_: function_type.result.clone(),
                function: function_checked,
                arguments: arguments_checked
            }))
        },
        &Expr::Conditional { ref condition, ref positive, ref negative, .. } => {
            let condition_typed = check_expr(env, condition)?;
            let condition_type = condition_typed.type_();
            if condition_type != Type::Int && condition_type != Type::String {
                return error!("a condition can only be of type `num' or `str': {:?}", condition.source())
            }

            let (positive, positive_source) = match positive {
                &box Expr::Block { ref source, ref statements } => (statements, source),
                _ => unreachable!()
            };
            if positive.is_empty() {
                warning!("empty positive conditional clause: {:?}", positive_source);
            }
            let positive = check_block(env, positive.iter())?;
            let positive_type = positive.type_();

            let negative = match negative {
                &Some(box Expr::Block { ref source, ref statements, .. }) => {
                    if statements.is_empty() {
                        warning!("empty negative conditional clause: {:?}", source);
                    }
                    Some(check_block(env, statements.iter())?)
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

            Ok(ExprRef::new(TypedExpr::Conditional {
                condition: condition_typed,
                positive,
                negative
            }))
        },
        &Expr::Block { ref source, ref statements } => {
            let statements = statements.iter()
                .map(|statement| check_statement(env, statement))
                .collect::<CheckResult<Vec<_>>>()?;
            Ok(ExprRef::new(TypedExpr::Block(statements)))
        }
    }
}

fn check_block<'a, BlockIterator>(env: &mut Environment, block: BlockIterator) -> CheckResult<ExprRef>
    where BlockIterator: Iterator<Item = &'a Statement<'a>>
{
    let statements: CheckResult<Vec<_>> = block.map(|statement| check_statement(env, statement)).collect();
    Ok(Rc::new(TypedExpr::Block(statements?)))
}

pub struct Environment {
    scopes: Vec<RefCell<HashMap<String, BindingRef>>>
}

impl Environment {
    pub fn new() -> Environment {
        Environment {
            scopes: vec![RefCell::new(HashMap::new())]
        }
    }

    pub fn bind(&self, binding: BindingRef) -> Result<(), Box<Error>> {
        let name = &binding.borrow().name;
        if let &BindingValue::Arg(_) = &binding.borrow().value {
            if self.last().borrow().contains_key(name) {
                return error!("redefinition of parameter {}", name)
            }
        }
        self.last().borrow_mut().insert(name.to_string(), binding.clone());
        Ok(())
    }

    pub fn resolve(&self, name: &str) -> Result<ExprRef, Box<Error>> {
        for scope in self.scopes.iter().rev() {
            if let Some(binding) = scope.borrow().get(name) {
                return Ok(ExprRef::new(TypedExpr::Deref(binding.clone())));
            }
        }
        Err(From::from(format!("`{}' is undefined", name)))
    }

    pub fn current_scope_len(&self) -> usize {
        self.last().borrow().len()
    }

    fn push(&mut self) {
        self.scopes.push(RefCell::new(HashMap::new()))
    }

    fn pop(&mut self) {
        if self.scopes.len() == 1 {
            panic!("Dropping the base scope")
        }
        self.scopes.pop();
    }

    fn last(&self) -> &RefCell<HashMap<String, BindingRef>> {
        self.scopes.last().unwrap()
    }
}
