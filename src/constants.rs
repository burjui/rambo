use num::{BigInt, Zero};
use std::ops::{Add, Sub, Mul, Div};
use std::rc::Rc;

use crate::semantics::*;
use crate::dead_bindings::*;
use crate::env::Environment;

// TODO implement operation-specific optimizations, such as "x*1 = x", "x+0 = x" and so on

pub struct CFP {
    env: Environment<BindingPtr, ExprRef>
}

impl CFP {
    pub fn new() -> CFP {
        CFP {
            env: Environment::new()
        }
    }

    pub fn fold_and_propagate_constants(&mut self, code: Vec<TypedStatement>) -> Vec<TypedStatement> {
        let code = code.into_iter()
            .map(|statement| match statement {
                TypedStatement::Binding(_) => statement,
                TypedStatement::Expr(expr) => TypedStatement::Expr(self.fold(&expr))
            })
            .collect::<Vec<_>>();
        remove_dead_bindings(&code, Warnings::Off)
    }

    #[must_use]
    fn fold(&mut self, expr: &ExprRef) -> ExprRef {
        match &expr as &TypedExpr {
            &TypedExpr::Deref(ref binding) => {
                let value = binding.borrow().value.clone();
                match &value {
                    &BindingValue::Var(ref value) => {
                        if !binding.borrow().assigned {
                            let mut value = value.clone();
                            if !binding.borrow().dirty {
                                let mut binding = binding.borrow_mut();
                                binding.dirty = true;
                                value = self.fold(&value);
                                binding.value = BindingValue::Var(value.clone());
                            }
                            if is_primitive_constant(&value) {
                                return value;
                            }
                        }
                        ExprRef::new(TypedExpr::Deref(binding.clone()))
                    },
                    &BindingValue::Arg(_) => self.env.resolve(&binding.ptr()).unwrap().clone()
                }
            }
            &TypedExpr::AddInt(ref left, ref right) => self.try_fold_numeric(expr, left, right, Add::add),
            &TypedExpr::SubInt(ref left, ref right) => self.try_fold_numeric(expr, left, right, Sub::sub),
            &TypedExpr::MulInt(ref left, ref right) => self.try_fold_numeric(expr, left, right, Mul::mul),
            &TypedExpr::DivInt(ref left, ref right) => self.try_fold_numeric(expr, left, right, Div::div),
            &TypedExpr::AddStr(ref left, ref right) => {
                match (&self.fold(left) as &TypedExpr, &self.fold(right) as &TypedExpr) {
                    (&TypedExpr::String(ref left), &TypedExpr::String(ref right)) =>
                        ExprRef::new(TypedExpr::String(left.to_string() + right)),
                    _ => expr.clone()
                }
            },
            &TypedExpr::Application { ref function, ref arguments, .. } => {
                let original_function = function;
                let mut function = self.fold(original_function);
                if is_primitive_constant(&function) {
                    function
                } else {
                    let arguments = arguments.iter().map(|argument| self.fold(argument)).collect::<Vec<_>>();
                    if arguments.iter().all(is_constant) {
                        while let &TypedExpr::Deref(ref binding) = &function.clone() as &TypedExpr {
                            function = match &binding.borrow().value {
                                &BindingValue::Var(ref value) => value.clone(),
                                _ => unreachable!()
                            };
                        }
                        if let &TypedExpr::Lambda(ref lambda) = &function as &TypedExpr {
                            self.env.push();
                            for (parameter, argument) in lambda.parameters.iter().zip(arguments.into_iter()) {
                                self.env.bind(parameter.ptr(), argument).unwrap();
                            }
                            let result = self.fold(&lambda.body);
                            self.env.pop();
                            return result
                        }
                    }
                    expr.clone()
                }
            },
            &TypedExpr::Assign(ref left, ref right) => {
                let binding = match &left as &TypedExpr {
                    &TypedExpr::Deref(ref binding) => binding,
                    _ => unreachable!()
                };
                binding.borrow_mut().assigned = true;
                ExprRef::new(TypedExpr::Assign(left.clone(), self.fold(right)))
            },
            &TypedExpr::Lambda(ref lambda) => self.fold_function(lambda),
            &TypedExpr::Conditional { ref condition, ref positive, ref negative } => {
                if let &TypedExpr::Int(ref n) = &self.fold(condition) as &TypedExpr {
                    return if n == &BigInt::zero() {
                        negative.as_ref().map(|clause| self.fold(clause)).unwrap_or_else(|| ExprRef::new(TypedExpr::Unit))
                    } else {
                        self.fold(positive)
                    }
                }
                expr.clone()
            },
            _ => expr.clone()
        }
    }

    #[must_use]
    fn fold_function(&mut self, lambda: &Rc<Lambda>) -> ExprRef {
        self.env.push();
        for parameter in &lambda.parameters {
            self.env.bind(parameter.ptr(), ExprRef::new(TypedExpr::Phantom)).unwrap();
        }
        let body = self.fold(&lambda.body);
        self.env.pop();
        if is_primitive_constant(&body) {
            body
        } else {
            ExprRef::new(TypedExpr::Lambda(lambda.clone()))
        }
    }

    #[must_use]
    fn try_fold_numeric<FoldFn>(&mut self, original_expr: &ExprRef, left: &ExprRef, right: &ExprRef, fold_impl: FoldFn) -> ExprRef
        where FoldFn: FnOnce(BigInt, BigInt) -> BigInt
    {
        let (left, right) = (self.fold(left), self.fold(right));
        match (&left as &TypedExpr, &right as &TypedExpr) {
            (&TypedExpr::Int(ref left), &TypedExpr::Int(ref right)) => {
                let result = fold_impl(left.clone(), right.clone());
                ExprRef::new(TypedExpr::Int(result))
            },
            _ => original_expr.clone()
        }
    }
}

fn is_constant(expr: &ExprRef) -> bool {
    match expr as &TypedExpr {
        &TypedExpr::Int(_) | &TypedExpr::String(_) | &TypedExpr::Lambda {..} => true,
        _ => false
    }
}

fn is_primitive_constant(expr: &ExprRef) -> bool {
    match expr as &TypedExpr {
        &TypedExpr::Int(_) | &TypedExpr::String(_) => true,
        _ => false
    }
}
