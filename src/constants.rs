use num_bigint::BigInt;
use num_traits::Zero;
use std::ops::{Add, Sub, Mul, Div};

use crate::semantics::*;
use crate::env::Environment;
use crate::source::Source;

// TODO implement operation-specific optimizations, such as "x*1 = x", "x+0 = x" and so on
// TODO get rid of unreachable()

crate struct CFP {
    env: Environment<BindingPtr, ExprRef>
}

impl CFP {
    crate fn new() -> CFP {
        CFP {
            env: Environment::new()
        }
    }

    crate fn fold_statements(&mut self, code: &[TypedStatement]) -> Vec<TypedStatement> {
        code.into_iter()
            .map(|statement| match statement {
                TypedStatement::Binding(binding) => {
                    self.process_binding(binding);
                    TypedStatement::Binding(binding.clone())
                },
                TypedStatement::Expr(expr) => TypedStatement::Expr(self.fold(&expr))
            })
            .collect::<Vec<_>>()
    }

    #[must_use]
    fn fold(&mut self, expr: &ExprRef) -> ExprRef {
        match expr as &TypedExpr {
            TypedExpr::Deref(binding, source) => {
                match &binding.borrow().data {
                    BindingValue::Var(value) => {
                        if is_primitive_constant(&value) {
                            value.clone_at(source.clone())
                        } else {
                            expr.clone()
                        }
                    },
                    BindingValue::Arg(_) => self.env.resolve(&binding.ptr()).unwrap().clone()
                }
            }
            TypedExpr::AddInt(left, right, source) => self.try_fold_numeric(expr, left, right, Add::add, source),
            TypedExpr::SubInt(left, right, source) => self.try_fold_numeric(expr, left, right, Sub::sub, source),
            TypedExpr::MulInt(left, right, source) => self.try_fold_numeric(expr, left, right, Mul::mul, source),
            TypedExpr::DivInt(left, right, source) => self.try_fold_numeric(expr, left, right, Div::div, source),
            TypedExpr::AddStr(left, right, source) => {
                match (&self.fold(left) as &TypedExpr, &self.fold(right) as &TypedExpr) {
                    (TypedExpr::String(left, _), TypedExpr::String(right, _)) =>
                        ExprRef::new(TypedExpr::String(left.to_string() + right, source.clone())),
                    _ => expr.clone()
                }
            },
            TypedExpr::Application { function, arguments, source, .. } => {
                let original_function = function;
                let mut function = self.fold(original_function);
                if is_primitive_constant(&function) {
                    function
                } else {
                    let arguments = arguments.iter().map(|argument| self.fold(argument)).collect::<Vec<_>>();
                    if arguments.iter().all(is_constant) {
                        while let TypedExpr::Deref(binding, _) = &function.clone() as &TypedExpr {
                            function = match &binding.borrow().data {
                                BindingValue::Var(value) => value.clone(),
                                _ => unreachable!()
                            };
                        }
                        if let TypedExpr::Lambda(lambda, _) = &function as &TypedExpr {
                            self.env.push();
                            for (parameter, argument) in lambda.parameters.iter().zip(arguments.into_iter()) {
                                self.env.bind(parameter.ptr(), argument).unwrap();
                            }
                            let result = self.fold(&lambda.body);
                            self.env.pop();
                            return result.clone_at(source.clone())
                        }
                    }
                    expr.clone()
                }
            },
            TypedExpr::Assign(binding, value, source) => {
                binding.borrow_mut().assigned = true;
                ExprRef::new(TypedExpr::Assign(binding.clone(), self.fold(value), source.clone()))
            },
            TypedExpr::Lambda(lambda, source) => {
                self.env.push();
                for parameter in &lambda.parameters {
                    self.env.bind(parameter.ptr(), ExprRef::new(TypedExpr::Phantom)).unwrap();
                }
                let body = self.fold(&lambda.body);
                self.env.pop();
                if is_primitive_constant(&body) {
                    body.clone_at(source.clone())
                } else {
                    expr.clone()
                }
            },
            TypedExpr::Conditional { condition, positive, negative, source } => {
                let condition = self.fold(condition);
                if let TypedExpr::Int(n, _) = &condition as &TypedExpr {
                    return if n == &BigInt::zero() {
                        negative.as_ref()
                            .map(|clause| self.fold(clause))
                            .unwrap_or_else(|| ExprRef::new(TypedExpr::Unit(source.clone())))
                    } else {
                        self.fold(positive).clone_at(source.clone())
                    }
                }
                expr.clone()
            },
            TypedExpr::Block(statements, source) => {
                self.env.push();
                let statements = self.fold_statements(statements);
                self.env.pop();
                if statements.iter().all(|statement| match statement {
                    TypedStatement::Expr(expr) => {
                        is_primitive_constant(expr)
                    },
                    _ => false
                }) {
                    if let TypedStatement::Expr(expr) = &statements[statements.len() - 1] {
                        return expr.clone_at(source.clone())
                    }
                }
                ExprRef::new(TypedExpr::Block(statements, source.clone()))
            },
            TypedExpr::Phantom |
            TypedExpr::Unit(_) |
            TypedExpr::Int(_, _) |
            TypedExpr::String(_, _) => expr.clone()
        }
    }

    fn process_binding(&mut self, binding: &BindingRef) {
        let value = binding.borrow().data.clone();
        if let BindingValue::Var(value) = value {
            if !binding.borrow().assigned {
                let mut value = value.clone();
                if !binding.borrow().dirty {
                    let mut binding = binding.borrow_mut();
                    binding.dirty = true;
                    value = self.fold(&value);
                    binding.data = BindingValue::Var(value.clone());
                }
            }
        }
    }

    #[must_use]
    fn try_fold_numeric<FoldFn>(&mut self, original_expr: &ExprRef,
        left: &ExprRef, right: &ExprRef, fold_impl: FoldFn, source: &Source) -> ExprRef
        where FoldFn: FnOnce(BigInt, BigInt) -> BigInt
    {
        let (left, right) = (self.fold(left), self.fold(right));
        match (&left as &TypedExpr, &right as &TypedExpr) {
            (TypedExpr::Int(left, _), TypedExpr::Int(right, _)) => {
                let result = fold_impl(left.clone(), right.clone());
                ExprRef::new(TypedExpr::Int(result, source.clone()))
            },
            _ => original_expr.clone()
        }
    }
}

fn is_constant(expr: &ExprRef) -> bool {
    match expr as &TypedExpr {
        &TypedExpr::Unit(_) | TypedExpr::Int(_, _) | &TypedExpr::String(_, _) | &TypedExpr::Lambda {..} => true,
        _ => false
    }
}

fn is_primitive_constant(expr: &ExprRef) -> bool {
    match expr as &TypedExpr {
        &TypedExpr::Unit(_) | TypedExpr::Int(_, _) | &TypedExpr::String(_, _) => true,
        _ => false
    }
}
