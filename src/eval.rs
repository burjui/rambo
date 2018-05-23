use std::fmt::{Debug, Formatter, Result as FmtResult};
use std::error::Error;
use num::BigInt;
use num::Zero;

use semantics::*;
use env::Environment;

type Env = Environment<BindingPtr, Evalue>;

pub struct Evaluator {
    env: Env,
}

impl<'a> Evaluator {
    pub fn new() -> Evaluator {
        Evaluator {
            env: Env::new()
        }
    }

    pub fn eval_module(&mut self, code: &[TypedStatement]) -> Result<Evalue, Box<Error>> {
        let mut result = Evalue::Unit;
        for statement in code {
            result = self.eval_statement(statement)?;
        }
        Ok(result)
    }

    fn eval_statement(&mut self, statement: &TypedStatement) -> Result<Evalue, Box<Error>> {
        match statement {
            &TypedStatement::Expr(ref expr) => Ok(self.eval_expr(expr)?),
            &TypedStatement::Binding(ref binding) => {
                let value = match &binding.borrow().value {
                    &BindingValue::Var(ref expr) => self.eval_expr(expr)?,
                    &BindingValue::Arg(_) => panic!("{:?}", binding.borrow().value)
                };
                self.env.bind(binding.ptr(), value).unwrap();
                Ok(Evalue::Unit)
            }
        }
    }

    fn eval_expr(&mut self, expr: &TypedExpr) -> Result<Evalue, Box<Error>> {
        match expr {
            &TypedExpr::Phantom => unreachable!(),
            &TypedExpr::Unit => Ok(Evalue::Unit),
            &TypedExpr::Int(ref value) => Ok(Evalue::Int(value.clone())),
            &TypedExpr::String(ref value) => Ok(Evalue::String(value.clone())),
            &TypedExpr::AddInt(ref left, ref right) => {
                let left = self.eval_expr(left)?;
                let right = self.eval_expr(right)?;
                Self::numeric_binary_operation(&left, &right, "+", |a, b| a + b)
            },
            &TypedExpr::SubInt(ref left, ref right) => {
                let left = self.eval_expr(left)?;
                let right = self.eval_expr(right)?;
                Self::numeric_binary_operation(&left, &right, "-", |a, b| a - b)
            },
            &TypedExpr::MulInt(ref left, ref right) => {
                let left = self.eval_expr(left)?;
                let right = self.eval_expr(right)?;
                Self::numeric_binary_operation(&left, &right, "*", |a, b| a * b)
            },
            &TypedExpr::DivInt(ref left, ref right) => {
                let left = self.eval_expr(left)?;
                let right = self.eval_expr(right)?;
                Self::numeric_binary_operation(&left, &right, "/", |a, b| a / b)
            },
            &TypedExpr::AddStr(ref left, ref right) => {
                let left = self.eval_expr(left)?;
                let right = self.eval_expr(right)?;
                if let (&Evalue::String(ref left), &Evalue::String(ref right)) = (&left, &right) {
                    Ok(Evalue::String(left.to_string() + right))
                } else {
                    unreachable!()
                }
            },
            &TypedExpr::Assign(ref left, ref right) => {
                let left_binding;
                if let &TypedExpr::Deref(ref binding) = &left as &TypedExpr {
                    left_binding = binding
                } else {
                    unreachable!()
                };
                let value = self.eval_expr(right)?;
                self.env.bind_force(left_binding.ptr(), value.clone());
                Ok(value)
            },
            &TypedExpr::Deref(ref binding) => Ok(self.env.resolve(&binding.ptr()).unwrap()),
            &TypedExpr::Application { ref function, ref arguments, .. } => {
                let arguments: Result<Vec<Evalue>, Box<Error>> = arguments.iter().rev()
                    .map(|argument| self.eval_expr(argument)).collect();
                let arguments = arguments?;
                self.env.push();
                    {
                        let parameters = match self.eval_expr(function).unwrap() {
                            Evalue::Lambda(ref lambda) => match lambda as &TypedExpr {
                                &TypedExpr::Lambda(Lambda { ref parameters, .. }) => parameters.clone(),
                                _ => {
                                    unreachable!()
                                }
                            } ,
                            _ => unreachable!()
                        };
                        for (parameter, argument) in parameters.into_iter().zip(arguments.into_iter()) {
                            self.env.bind(parameter.ptr(), argument).unwrap();
                        }
                    }
                    let function = self.eval_expr(function)?;
                    let function_body;
                    if let &Evalue::Lambda(ref body) = &function {
                        function_body = body
                    } else {
                        unreachable!()
                    };
                    let result = self.eval_expr(function_body);
                self.env.pop();
                result
            },
            &TypedExpr::Lambda(Lambda { ref body, .. }) => Ok(Evalue::Lambda(body.clone())),
            &TypedExpr::Conditional { ref condition, ref positive, ref negative } => {
                let condition = match self.eval_expr(condition)? {
                    Evalue::Int(value) => !value.is_zero(),
                    Evalue::String(value) => value.len() > 0,
                    _ => unreachable!()
                };
                let clause = if condition {
                    Some(positive)
                } else {
                    negative.as_ref()
                };
                clause.map(|expr| self.eval_expr(expr)).unwrap_or_else(|| Ok(Evalue::Unit))
            },
            &TypedExpr::Block(ref statements) => {
                statements.iter()
                    .map(|statement| self.eval_statement(statement))
                    .last()
                    .unwrap_or(Ok(Evalue::Unit))
            }
        }
    }

    fn numeric_binary_operation<Eval>(left: &Evalue, right: &Evalue, operation_name: &str, eval: Eval)
                                      -> Result<Evalue, Box<Error>>
        where Eval: FnOnce(&BigInt, &BigInt) -> BigInt
    {
        if let (&Evalue::Int(ref left), &Evalue::Int(ref right)) = (left, right) {
            Ok(Evalue::Int(eval(left, right)))
        } else {
            error!("invalid operands for `{}': `{:?}' and `{:?}'", operation_name, left, right)
        }
    }
}

#[derive(Clone)]
pub enum Evalue {
    Unit,
    Int(BigInt),
    String(String),
    Lambda(ExprRef)
}

impl Debug for Evalue {
    fn fmt(&self, formatter: &mut Formatter) -> FmtResult {
        match self {
            &Evalue::Unit => write!(formatter, "()"),
            &Evalue::Int(ref value) => write!(formatter, "{}", value),
            &Evalue::String(ref value) => write!(formatter, "\"{}\"", value),
            &Evalue::Lambda(ref body) => write!(formatter, "(\\ ... -> {:?})", body),
        }
    }
}
