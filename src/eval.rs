use std::fmt::{Debug, Formatter, Result as FmtResult};
use std::error::Error;
use num::BigInt;
use std::ops::Deref;

use semantics::*;

pub struct Evaluator {
    stack: Vec<Evalue>,
    bindings: Vec<Evalue>,
}

impl<'a> Evaluator {
    pub fn new() -> Evaluator {
        Evaluator {
            stack: vec![],
            bindings: vec![]
        }
    }

    pub fn eval_module(&mut self, code: &[TypedStatement]) -> Result<Option<Evalue>, Box<Error>> {
        let mut result = None;
        for statement in code {
            match statement {
                &TypedStatement::Expr(ref expr) => result = Some(self.eval_expr(expr)?),
                &TypedStatement::Binding(ref binding) => {
                    let value = match &binding.borrow().value {
                        &BindingValue::Var(ref expr) => self.eval_expr(expr)?,
                        &BindingValue::Arg(_) => panic!("{:?}", binding.borrow().value)
                    };
                    self.bindings.push(value);
                }
            }
        }
        Ok(result)
    }

    fn eval_expr(&mut self, expr: &TypedExpr) -> Result<Evalue, Box<Error>> {
        match expr {
            &TypedExpr::Phantom(_) => unreachable!(),
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
                self.bindings[left_binding.borrow().index] = value.clone();
                Ok(value)
            },
            &TypedExpr::Deref(ref binding) => {
                match &binding.borrow().value {
                    &BindingValue::Var(_) => {
                        let value = self.bindings[binding.borrow().index].clone();
                        Ok(value)
                    },
                    &BindingValue::Arg(_) => {
                        let value = self.stack[self.stack.len() - 1 - binding.borrow().index].clone();
                        Ok(value)
                    }
                }
            },
            &TypedExpr::Application { ref function, ref arguments, .. } => {
                let arguments: Result<Vec<Evalue>, Box<Error>> = arguments.iter().rev()
                    .map(|argument| self.eval_expr(argument)).collect();
                let arguments = arguments?;
                for argument in arguments.as_slice() {
                    self.stack.push(argument.clone())
                }
                let function = self.eval_expr(function)?;
                let function_body;
                if let &Evalue::Lambda(ref body) = &function {
                    function_body = body
                } else {
                    unreachable!()
                };
                let result = self.eval_expr(function_body);
                let stack_size = self.stack.len();
                self.stack.resize(stack_size - arguments.len(), Evalue::Int(From::from(0)));
                result
            },
            &TypedExpr::Lambda { ref body, .. } => Ok(Evalue::Lambda(body.clone())),
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
    Int(BigInt),
    String(String),
    Lambda(ExprRef)
}

impl Debug for Evalue {
    fn fmt(&self, formatter: &mut Formatter) -> FmtResult {
        match self {
            &Evalue::Int(ref value) => write!(formatter, "{}", value),
            &Evalue::String(ref value) => write!(formatter, "\"{}\"", value),
            &Evalue::Lambda(ref body) => write!(formatter, "(\\ ... -> {:?})", body),
        }
    }
}
