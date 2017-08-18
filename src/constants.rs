use semantics::*;
use std::ops::Deref;
use num::BigInt;
use std::ops::{Add, Sub, Mul, Div};

use dead_bindings::*;

// TODO implement operation-specific optimizations, such as "x*1 = x", "x+0 = x" and so on

pub struct CFP {
    stack: Vec<ExprRef>
}

impl CFP {
    pub fn new() -> CFP {
        CFP {
            stack: vec![]
        }
    }

    pub fn fold_and_propagate_constants(&mut self, code: Vec<TypedStatement>) -> Vec<TypedStatement> {
        let mut env = Environment::new();
        let code = code.into_iter()
            .map(|statement| match &statement {
                &TypedStatement::Binding(ref binding) => {
                    env.bind(binding.clone()).unwrap();
                    statement.clone()
                },
                &TypedStatement::Expr(ref expr) => TypedStatement::Expr(self.fold(&mut env, &expr))
            })
            .collect::<Vec<_>>();
        drop(env);
        remove_dead_bindings(code, Warnings::Off)
    }

    fn fold(&mut self, env: &mut Environment, expr: &ExprRef) -> ExprRef {
        match expr.deref() {
            &TypedExpr::Deref(ref binding) => {
                let value = binding.borrow().value.clone();
                match &value {
                    &BindingValue::Var(ref value) => {
                        if !binding.borrow().assigned {
                            let mut value = value.clone();
                            if !binding.borrow().dirty {
                                let mut binding = binding.borrow_mut();
                                binding.dirty = true;
                                value = self.fold(env, &value);
                                binding.value = BindingValue::Var(value.clone());
                            }
                            if is_primitive_constant(&value) {
                                return value;
                            }
                        }
                        ExprRef::new(TypedExpr::Deref(binding.clone()))
                    },
                    &BindingValue::Arg(_) => self.stack[self.stack.len() - 1 - &binding.borrow().index].clone()
                }
            }
            &TypedExpr::AddInt(ref left, ref right) => self.try_fold_numeric(env, expr, left, right, Add::add),
            &TypedExpr::SubInt(ref left, ref right) => self.try_fold_numeric(env, expr, left, right, Sub::sub),
            &TypedExpr::MulInt(ref left, ref right) => self.try_fold_numeric(env, expr, left, right, Mul::mul),
            &TypedExpr::DivInt(ref left, ref right) => self.try_fold_numeric(env, expr, left, right, Div::div),
            &TypedExpr::AddStr(ref left, ref right) => {
                match (self.fold(env, left).deref(), self.fold(env, right).deref()) {
                    (&TypedExpr::String(ref left), &TypedExpr::String(ref right)) =>
                        ExprRef::new(TypedExpr::String(left.to_string() + right)),
                    _ => expr.clone()
                }
            },
            &TypedExpr::Application { ref type_, ref function, ref arguments } => {
                let mut function = self.fold(env, function);
                if is_primitive_constant(&function) {
                    function
                } else {
                    let arguments = arguments.iter().map(|argument| self.fold(env, argument)).collect::<Vec<_>>();
                    if arguments.iter().all(is_constant) {
                        while let &TypedExpr::Deref(ref binding) = function.clone().deref() {
                            function = match &binding.borrow().value {
                                &BindingValue::Var(ref value) => value.clone(),
                                _ => unreachable!()
                            };
                        }
                        if let &TypedExpr::Lambda { ref body, .. } = function.deref() {
                            let stack_size = self.stack.len();
                            for argument in arguments.into_iter().rev() {
                                self.stack.push(argument)
                            }
                            let result = self.fold(env, body);
                            self.stack.resize(stack_size, ExprRef::new(TypedExpr::Phantom));
                            return result
                        }
                    }
                    ExprRef::new(TypedExpr::Application {
                        type_: type_.clone(),
                        function,
                        arguments
                    })
                }
            },
            &TypedExpr::Assign(ref left, ref right) => {
                self.fold(env, left);
                let binding = match left.deref() {
                    &TypedExpr::Deref(ref binding) => binding,
                    _ => unreachable!()
                };
                binding.borrow_mut().assigned = true;
                ExprRef::new(TypedExpr::Assign(left.clone(), self.fold(env, right)))
            },
            &TypedExpr::Lambda { ref type_, ref body, .. } => {
                let stack_size = self.stack.len();
                let padding = || ExprRef::new(TypedExpr::Phantom);
                self.stack.resize(stack_size + type_.parameters.len(), padding());
                let body = self.fold(env, body);
                self.stack.resize(stack_size, padding());
                if is_primitive_constant(&body) {
                    body
                } else {
                    expr.clone()
                }
            },
            // TODO fold conditionals
            _ => expr.clone()
        }
    }

    fn try_fold_numeric<FoldFn>(&mut self, env: &mut Environment, original_expr: &ExprRef, left: &ExprRef, right: &ExprRef, fold_impl: FoldFn) -> ExprRef
        where FoldFn: FnOnce(BigInt, BigInt) -> BigInt
    {
        let (left, right) = (self.fold(env, left), self.fold(env, right));
        match (left.deref(), right.deref()) {
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
