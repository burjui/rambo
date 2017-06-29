use semantics::*;
use std::ops::Deref;
use num::BigInt;

use dead_bindings::*;

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
        let scope = Scope::new(None);
        let code = code.into_iter()
            .map(|statement| match &statement {
                &TypedStatement::Binding(ref binding) => {
                    scope.bind(binding.clone()).unwrap();
                    statement.clone()
                },
                &TypedStatement::Expr(ref expr) => TypedStatement::Expr(self.fold(&scope, &expr))
            })
            .collect::<Vec<_>>();
        drop(scope);
        remove_dead_bindings(code, Warnings::Off)
    }

    fn fold(&mut self, scope: &ScopeRef, expr: &ExprRef) -> ExprRef {
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
                                value = self.fold(scope, &value);
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
            &TypedExpr::AddInt(ref left, ref right) => self.fold_numeric(
                scope, left, right, TypedExpr::AddInt, |left, right| left + right),
            &TypedExpr::SubInt(ref left, ref right) => self.fold_numeric(
                scope, left, right, TypedExpr::SubInt, |left, right| left - right),
            &TypedExpr::MulInt(ref left, ref right) => self.fold_numeric(
                scope, left, right, TypedExpr::MulInt, |left, right| left * right),
            &TypedExpr::DivInt(ref left, ref right) => self.fold_numeric(
                scope, left, right, TypedExpr::DivInt, |left, right| left / right),
            &TypedExpr::AddStr(ref left, ref right) => {
                match (self.fold(&scope, left).deref(), self.fold(&scope, right).deref()) {
                    (&TypedExpr::String(ref left), &TypedExpr::String(ref right)) =>
                        ExprRef::new(TypedExpr::String(left.to_string() + right)),
                    _ => expr.clone()
                }
            },
            &TypedExpr::Application { ref type_, ref function, ref arguments } => {
                let mut function = self.fold(scope, function);
                if is_primitive_constant(&function) {
                    function
                } else {
                    let arguments = arguments.iter().map(|argument| self.fold(scope, argument)).collect::<Vec<_>>();
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
                            let result = self.fold(&scope, body);
                            self.stack.resize(stack_size, ExprRef::new(TypedExpr::Phantom(Type::Int)));
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
                self.fold(scope, left);
                let binding = match left.deref() {
                    &TypedExpr::Deref(ref binding) => binding,
                    _ => unreachable!()
                };
                binding.borrow_mut().assigned = true;
                ExprRef::new(TypedExpr::Assign(left.clone(), self.fold(scope, right)))
            },
            &TypedExpr::Lambda { ref type_, ref body, .. } => {
                let stack_size = self.stack.len();
                for parameter in type_.parameters.iter().rev() {
                    self.stack.push(ExprRef::new(TypedExpr::Phantom(parameter.type_.clone())))
                }
                let body = self.fold(&scope, body);
                self.stack.resize(stack_size, ExprRef::new(TypedExpr::Phantom(Type::Int)));
                if is_primitive_constant(&body) {
                    body
                } else {
                    expr.clone()
                }
            },
            _ => expr.clone()
        }
    }

    fn fold_numeric<Constructor, FoldFn>(&mut self,
        scope: &ScopeRef, left: &ExprRef, right: &ExprRef,
        constructor: Constructor, fold_impl: FoldFn) -> ExprRef
        where
            Constructor: FnOnce(ExprRef, ExprRef) -> TypedExpr,
            FoldFn: FnOnce(&BigInt, &BigInt) -> BigInt {

        let (left, right) = (self.fold(&scope, left), self.fold(&scope, right));
        match (left.deref(), right.deref()) {
            (&TypedExpr::Int(ref left), &TypedExpr::Int(ref right)) => ExprRef::new(TypedExpr::Int(fold_impl(left, right))),
            _ => ExprRef::new(constructor(left.clone(), right.clone()))
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
