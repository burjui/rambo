use std::fmt::{Debug, Formatter, Result as FmtResult};
use std::error::Error;
use num::BigInt;
use std::ops::Deref;
use num::Zero;
use std::cell::RefCell;
use std::collections::HashMap;

use semantics::*;

pub struct Evaluator {
    stack: Vec<BindingMap>,
    bindings: BindingMap,
}

type BindingCell = RefCell<Binding>;
type BindingPtr = *const BindingCell;

trait Ptr<T> {
    fn ptr(self) -> *const T;
}

impl<'a> Ptr<BindingCell> for &'a BindingRef {
    fn ptr(self) -> *const BindingCell {
        self as &BindingCell as *const BindingCell
    }
}

type BindingMap = HashMap<BindingPtr, Evalue>;

impl<'a> Evaluator {
    pub fn new() -> Evaluator {
        Evaluator {
            stack: vec![],
            bindings: HashMap::new()
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
                self.bindings.insert(binding.ptr(), value);
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
                if let &TypedExpr::Deref(ref binding) = left.deref() {
                    left_binding = binding
                } else {
                    unreachable!()
                };
                let value = self.eval_expr(right)?;
                *self.bindings.get_mut(&left_binding.ptr()).unwrap() = value.clone();
                Ok(value)
            },
            &TypedExpr::Deref(ref binding) => {
                match &binding.borrow().value {
                    &BindingValue::Var(_) => {
                        let value = self.bindings.get(&binding.ptr()).unwrap().clone();
                        Ok(value)
                    },
                    &BindingValue::Arg(_) => {
                        let value = self.stack.last().unwrap().get(&binding.ptr()).unwrap().clone();
                        Ok(value)
                    }
                }
            },
            &TypedExpr::Application { ref function, ref arguments, .. } => {
                let arguments: Result<Vec<Evalue>, Box<Error>> = arguments.iter().rev()
                    .map(|argument| self.eval_expr(argument)).collect();
                let arguments = arguments?;
                let stack_size = self.stack.len();
                self.stack.push(HashMap::new());
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
                        let map = self.stack.last_mut().unwrap();
                        for (parameter, argument) in parameters.into_iter().zip(arguments.into_iter()) {
                            map.insert(parameter.ptr(), argument.clone());
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
                self.stack.resize(stack_size, HashMap::new());
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
