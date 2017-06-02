use std::fmt::{Debug, Formatter, Result as FmtResult};
use std::collections::HashMap;
use std::cell::RefCell;
use std::rc::Rc;
use std::error::Error;
use num::BigInt;

use parser::*;

macro_rules! error {
    ($format_string: expr $(, $argument: expr)*) => { Err(From::from(format!($format_string $(, $argument)*))) };
}

pub struct Evaluator {}

impl<'a> Evaluator {
    pub fn new() -> Evaluator {
        Evaluator {}
    }

    pub fn eval_module(&mut self, code: &[Entity<'a>]) -> Result<Evalue<'a>, Box<Error>> {
        let scope = Scope::new(None);
        let mut result = Evalue::String("doh!".to_string());
        for entity in code {
            result = self.eval_entity(scope.clone(), entity)?;
        }
        Ok(result)
    }

    fn eval_entity(&self, scope: ScopeRef<'a>, entity: &Entity<'a>) -> Result<Evalue<'a>, Box<Error>> {
        match entity {
            &Entity::Expr(ref expr) => self.eval_expr(scope, expr),
            &Entity::Binding { ref name, ref value } => {
                let name = name.text();
                let result = self.eval_expr(scope.clone(), value)?;
                scope.borrow().bind(name, &result)?;
                Ok(result)
            }
        }
    }

    fn eval_expr(&self, scope: ScopeRef<'a>, expr: &Expr<'a>) -> Result<Evalue<'a>, Box<Error>> {
        match expr {
            &Expr::Int(source) => Ok(Evalue::Number(source.text().parse::<BigInt>()?)),
            &Expr::Binary { ref operation, ref left, ref right } => {
                let left = self.eval_expr(scope.clone(), left)?;
                let right = self.eval_expr(scope.clone(), right)?;
                let (left, right) = match *operation {
                    BinaryOperation::Assign => (left, right),
                    _ => (Self::deref(scope.clone(), &left)?, Self::deref(scope.clone(), &right)?)
                };
                match *operation {
                    BinaryOperation::Add => Self::numeric_binary_operation(&left, &right, "+", |a, b| a + b),
                    BinaryOperation::Subtract => Self::numeric_binary_operation(&left, &right, "-", |a, b| a - b),
                    BinaryOperation::Multiply => Self::numeric_binary_operation(&left, &right, "*", |a, b| a * b),
                    BinaryOperation::Divide => Self::numeric_binary_operation(&left, &right, "/", |a, b| a / b),
                    BinaryOperation::Assign => {
                        match (&left, &right) {
                            (&Evalue::Ref(ref name), _) => {
                                scope.borrow().assign(name, &right)?;
                                Ok(left.clone())
                            },
                            _ => error!("cannot assign to {:?}", left)
                        }
                    },
                }
            },
            &Expr::Lambda { ref parameters, ref body } => {
                Ok(Evalue::Lambda {
                    parameters: parameters.clone(),
                    body: body.clone()
                })
            },
            &Expr::Id(source) => {
                let name = source.text();
                scope.borrow().resolve(name)?;
                Ok(Evalue::Ref(name.to_string()))
            },
            &Expr::String(source) => {
                Ok(Evalue::String(source.text().to_string()))
            },
            &Expr::Application { ref function, ref arguments } => {
                let function = self.eval_expr(scope.clone(), &function)?;
                let function = Self::deref(scope.clone(), &function)?;
                match &function {
                    &Evalue::Lambda { ref parameters, ref body } => {
                        let lambda_scope = Scope::new(Some(scope.clone()));
                        for (i, parameter) in parameters.iter().enumerate() {
                            let argument = self.eval_expr(scope.clone(), &arguments[i])?;
                            let argument = Self::deref(scope.clone(), &argument)?;
                            let parameter_name = parameter.name.text();
                            lambda_scope.borrow().bind(&parameter_name, &argument)?;
                        }
                        self.eval_expr(lambda_scope.clone(), body)
                    },
                    _ => error!("not a function: {:?}", function)
                }
            }
        }
    }

    fn numeric_binary_operation<Eval>(left: &Evalue, right: &Evalue, operation_name: &str, eval: Eval)
                                      -> Result<Evalue<'a>, Box<Error>>
        where Eval: FnOnce(&BigInt, &BigInt) -> BigInt
    {
        if let (&Evalue::Number(ref left), &Evalue::Number(ref right)) = (left, right) {
            Ok(Evalue::Number(eval(left, right)))
        } else {
            error!("invalid operands for `{}': `{:?}' and `{:?}'", operation_name, left, right)
        }
    }

    fn deref(scope: ScopeRef<'a>, value: &Evalue<'a>) -> Result<Evalue<'a>, Box<Error>> {
        if let &Evalue::Ref(ref name) = value {
            Ok(scope.borrow().resolve(&name)?)
        } else {
            Ok(value.clone())
        }
    }
}

type ScopeRef<'a> = Rc<RefCell<Scope<'a>>>;

struct Scope<'a> {
    bindings: RefCell<HashMap<String, Evalue<'a>>>,
    outer_scope: Option<ScopeRef<'a>>
}

impl<'a> Scope<'a> {
    fn new(outer_scope: Option<ScopeRef<'a>>) -> ScopeRef<'a> {
        Rc::new(RefCell::new(Scope {
            bindings: RefCell::new(HashMap::new()),
            outer_scope
        }))
    }

    fn bind(&self, name: &str, value: &Evalue<'a>) -> Result<(), Box<Error>> {
        if self.bindings.borrow().contains_key(name) {
            error!("redefinition of {}", name)
        } else {
            self.bindings.borrow_mut().insert(name.to_string(), value.clone());
            Ok(())
        }
    }

    fn assign(&self, name: &str, value: &Evalue<'a>) -> Result<(), Box<Error>> {
        self.resolve(name)?;
        self.bindings.borrow_mut().insert(name.to_string(), value.clone());
        Ok(())
    }

    fn resolve(&self, name: &str) -> Result<Evalue<'a>, Box<Error>> {
        self.bindings.borrow().get(name)
            .map(Clone::clone)
            .map_or_else(
                || self.outer_scope.clone().map_or_else(
                    || Err(From::from(format!("`{}' is undefined", name))),
                    |scope| scope.borrow().resolve(name)),
                Ok)
    }
}

#[derive(Clone)]
pub enum Evalue<'a> {
    Number(BigInt),
    String(String),
    Lambda {
        parameters: Vec<Parameter<'a>>,
        body: Box<Expr<'a>>
    },
    Ref(String)
}

impl<'a> Debug for Evalue<'a> {
    fn fmt(&self, formatter: &mut Formatter) -> FmtResult {
        match self {
            &Evalue::Number(ref value) => write!(formatter, "{}", value),
            &Evalue::String(ref value) => write!(formatter, "{}", value),
            &Evalue::Lambda { ref parameters, ref body } =>
                write!(formatter, "(\\ {:?} -> {:?})", parameters, body),
            &Evalue::Ref(ref name) => write!(formatter, "{}", name),
        }
    }
}
