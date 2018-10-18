use crate::env::Environment;
use crate::semantics::*;
use crate::semantics::Block;
use num_bigint::BigInt;
use num_traits::Zero;
use std::error::Error;
use std::rc::Rc;

type Env = Environment<BindingPtr, Evalue>;

crate struct Evaluator {
    env: Env,
}

impl<'a> Evaluator {
    crate fn new() -> Evaluator {
        Evaluator {
            env: Env::new()
        }
    }

    crate fn eval_module(&mut self, code: &[TypedStatement]) -> Result<Evalue, Box<dyn Error>> {
        let mut result = Evalue::Unit;
        for statement in code {
            result = self.eval_statement(statement)?;
        }
        Ok(result)
    }

    fn eval_statement(&mut self, statement: &TypedStatement) -> Result<Evalue, Box<dyn Error>> {
        match statement {
            TypedStatement::Expr(expr) => Ok(self.eval_expr(expr)?),
            TypedStatement::Binding(binding) => {
                let value = match &binding.borrow().data {
                    BindingValue::Var(expr) => self.eval_expr(expr)?,
                    BindingValue::Arg(_) => panic!("{:?}", binding.borrow().data)
                };
                self.env.bind_force(binding.ptr(), value);
                Ok(Evalue::Unit)
            }
        }
    }

    fn eval_expr(&mut self, expr: &TypedExpr) -> Result<Evalue, Box<dyn Error>> {
        match expr {
            TypedExpr::Phantom => unreachable!(),
            TypedExpr::Unit(_) => Ok(Evalue::Unit),
            TypedExpr::Int(value, _) => Ok(Evalue::Int(value.clone())),
            TypedExpr::String(value, _) => Ok(Evalue::String(value.clone())),
            TypedExpr::AddInt(left, right, _) => {
                let left = self.eval_expr(left)?;
                let right = self.eval_expr(right)?;
                Self::numeric_binary_operation(&left, &right, "+", |a, b| a + b)
            },
            TypedExpr::SubInt(left, right, _) => {
                let left = self.eval_expr(left)?;
                let right = self.eval_expr(right)?;
                Self::numeric_binary_operation(&left, &right, "-", |a, b| a - b)
            },
            TypedExpr::MulInt(left, right, _) => {
                let left = self.eval_expr(left)?;
                let right = self.eval_expr(right)?;
                Self::numeric_binary_operation(&left, &right, "*", |a, b| a * b)
            },
            TypedExpr::DivInt(left, right, _) => {
                let left = self.eval_expr(left)?;
                let right = self.eval_expr(right)?;
                Self::numeric_binary_operation(&left, &right, "/", |a, b| a / b)
            },
            TypedExpr::AddStr(left, right, _) => {
                let left = self.eval_expr(left)?;
                let right = self.eval_expr(right)?;
                if let (Evalue::String(left), Evalue::String(right)) = (left, right) {
                    Ok(Evalue::String(left.to_string() + &right))
                } else {
                    unreachable!()
                }
            },
            TypedExpr::Assign(binding, value, _) => {
                let value = self.eval_expr(value)?;
                self.env.bind_force(binding.ptr(), value.clone());
                Ok(value)
            },
            TypedExpr::Deref(binding, _) => Ok(self.env.resolve(&binding.ptr()).unwrap()),
            TypedExpr::Application { function, arguments, .. } => {
                let arguments: Result<Vec<Evalue>, Box<dyn Error>> = arguments.iter()
                    .map(|argument| self.eval_expr(argument)).collect();
                let arguments = arguments?;
                let lambda = match self.eval_expr(function).unwrap() {
                    Evalue::Lambda(lambda) => lambda.clone(),
                    _ => unreachable!()
                };
                self.env.push();
                for (parameter, argument) in lambda.parameters.iter().zip(arguments.into_iter()) {
                    self.env.bind(parameter.ptr(), argument).unwrap();
                }
                let result = self.eval_expr(&lambda.body);
                self.env.pop();
                result
            },
            TypedExpr::Lambda(lambda, _) => Ok(Evalue::Lambda(lambda.clone())),
            TypedExpr::Conditional { condition, positive, negative, .. } => {
                let condition = match self.eval_expr(condition)? {
                    Evalue::Int(value) => !value.is_zero(),
                    Evalue::String(value) => value.is_empty(),
                    _ => unreachable!()
                };
                let clause = if condition {
                    Some(positive)
                } else {
                    negative.as_ref()
                };
                clause.map(|expr| self.eval_expr(expr)).unwrap_or_else(|| Ok(Evalue::Unit))
            },
            TypedExpr::Block(Block { statements, .. }) => {
                self.env.push();
                let result = statements.iter()
                    .map(|statement| self.eval_statement(statement))
                    .last()
                    .unwrap_or(Ok(Evalue::Unit));
                self.env.pop();
                result
            }
        }
    }

    fn numeric_binary_operation<Eval>(left: &Evalue, right: &Evalue, operation_name: &str, eval: Eval)
                                      -> Result<Evalue, Box<dyn Error>>
        where Eval: FnOnce(&BigInt, &BigInt) -> BigInt
    {
        if let (Evalue::Int(left), Evalue::Int(right)) = (left, right) {
            Ok(Evalue::Int(eval(left, right)))
        } else {
            error!("invalid operands for `{}': `{:?}' and `{:?}'", operation_name, left, right)
        }
    }
}

#[derive(Clone)]
crate enum Evalue {
    Unit,
    Int(BigInt),
    String(String),
    Lambda(Rc<Lambda>)
}

impl std::fmt::Debug for Evalue {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Evalue::Unit => write!(formatter, "()"),
            Evalue::Int(value) => write!(formatter, "{}", value),
            Evalue::String(value) => write!(formatter, "\"{}\"", value),
            Evalue::Lambda(body) => write!(formatter, "(\\ ... -> {:?})", body),
        }
    }
}
