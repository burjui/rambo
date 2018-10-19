use crate::env::Environment;
use crate::semantics::BindingRef;
use crate::semantics::BindingValue;
use crate::semantics::Block;
use crate::semantics::ExprRef;
use crate::semantics::TypedExpr;
use crate::semantics::TypedStatement;
use crate::source::Source;
use num_bigint::BigInt;
use num_traits::Zero;
use std::ops::Add;
use std::ops::Div;
use std::ops::Mul;
use std::ops::Sub;

// TODO implement operation-specific optimizations, such as "x*1 = x", "x+0 = x" and so on
// TODO get rid of unreachable()

crate struct CFP {
    env: Environment<BindingRef, ExprRef>,
}


impl CFP {
    crate fn new() -> CFP {
        CFP {
            env: Environment::new(),
        }
    }

    #[must_use]
    crate fn fold(&mut self, expr: &ExprRef) -> ExprRef {
        match expr as &TypedExpr {
            TypedExpr::Deref(binding, source) => {
                match &binding.borrow().data {
                    BindingValue::Var(value) => {
                        let value = self.fold(value);
                        if is_primitive_constant(&value) {
                            value.clone_at(source.clone())
                        } else {
                            expr.clone()
                        }
                    },
                    BindingValue::Arg(_) => {
                        let value = self.env.resolve(binding).unwrap().clone();
                        self.fold(&value)
                    }
                }
            }
            TypedExpr::AddInt(left, right, source) => self.try_fold_numeric(expr, left, right, Add::add, source),
            TypedExpr::SubInt(left, right, source) => self.try_fold_numeric(expr, left, right, Sub::sub, source),
            TypedExpr::MulInt(left, right, source) => self.try_fold_numeric(expr, left, right, Mul::mul, source),
            TypedExpr::DivInt(left, right, source) => self.try_fold_numeric(expr, left, right, Div::div, source),
            TypedExpr::AddStr(left, right, source) => {
                match (&self.fold(left) as &TypedExpr, &self.fold(right) as &TypedExpr) {
                    (TypedExpr::String(left, _), TypedExpr::String(right, _)) =>
                        ExprRef::from(TypedExpr::String(left.to_string() + right, source.clone())),
                    _ => expr.clone()
                }
            },
            TypedExpr::Assign(binding, value, source) => {
                binding.borrow_mut().assigned = true;
                ExprRef::from(TypedExpr::Assign(binding.clone(), self.fold(value), source.clone()))
            },
            TypedExpr::Application { function, arguments, source, .. } => {
                let mut function = self.fold(function);
                if is_primitive_constant(&function) {
                    function
                } else {
                    let arguments = arguments.iter()
                        .map(|argument| self.fold(argument))
                        .collect::<Vec<_>>();
                    if arguments.iter().all(|argument| is_constant(argument)) {
                        while let TypedExpr::Deref(binding, _) = &function.clone() as &TypedExpr {
                            function = match &binding.borrow().data {
                                BindingValue::Var(value) => value.clone(),
                                _ => unreachable!()
                            };
                        }
                        if let TypedExpr::Lambda(lambda, _) = &function as &TypedExpr {
                            self.env.push();
                            for (parameter, argument) in lambda.parameters.iter().zip(arguments.into_iter()) {
                                self.env.bind(parameter.clone(), argument.clone()).unwrap();
                            }
                            let result = self.fold(&lambda.body);
                            self.env.pop();
                            if is_primitive_constant(&result) {
                                return result.clone_at(source.clone())
                            }
                        }
                    }
                    expr.clone()
                }
            },
            TypedExpr::Lambda(lambda, source) => {
                self.env.push();
                for parameter in &lambda.parameters {
                    self.env.bind(parameter.clone(), ExprRef::from(TypedExpr::Phantom)).unwrap();
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
                            .unwrap_or_else(|| ExprRef::from(TypedExpr::Unit(source.clone())))
                    } else {
                        self.fold(positive).clone_at(source.clone())
                    }
                }
                expr.clone()
            },
            TypedExpr::Block(block) => self.fold_block(block),
            TypedExpr::Phantom |
            TypedExpr::Unit(_) |
            TypedExpr::Int(_, _) |
            TypedExpr::String(_, _) => expr.clone()
        }
    }

    fn fold_block(&mut self, block: &Block) -> ExprRef {
        self.env.push();
        let statements = block.statements.iter()
            .map(|statement| self.fold_statement(statement))
            .collect::<Vec<_>>();
        self.env.pop();

        let statements = statements.iter()
            .fold(Vec::new(), |mut result, statement| {
                if let Some(TypedStatement::Expr(e1)) = result.last() {
                    if is_primitive_constant(e1) {
                        (*result.last_mut().unwrap()) = statement.clone();
                        return result;
                    }
                }
                result.push(statement.clone());
                result
            });

        let source = block.source.clone();
        if statements.is_empty() {
            return ExprRef::from(TypedExpr::Unit(source));
        } else if statements.len() == 1 {
            if let Some(TypedStatement::Expr(expr)) = statements.first() {
                return expr.clone_at(source);
            }
        }

        ExprRef::from(TypedExpr::Block(Block {
            statements,
            source
        }))
    }

    fn fold_statement(&mut self, statement: &TypedStatement) -> TypedStatement {
        match statement {
            TypedStatement::Binding(binding) => {
                self.process_binding(binding);
                TypedStatement::Binding(binding.clone())
            },
            TypedStatement::Expr(expr) => TypedStatement::Expr(self.fold(&expr))
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
                ExprRef::from(TypedExpr::Int(result, source.clone()))
            },
            _ => original_expr.clone()
        }
    }
}

fn is_constant(expr: &ExprRef) -> bool {
    match expr as &TypedExpr {
        TypedExpr::Unit(_) | TypedExpr::Int(_, _) | TypedExpr::String(_, _) | TypedExpr::Lambda(_, _) => true,
        _ => false
    }
}

fn is_primitive_constant(expr: &ExprRef) -> bool {
    match expr as &TypedExpr {
        TypedExpr::Unit(_) | TypedExpr::Int(_, _) | TypedExpr::String(_, _) => true,
        _ => false
    }
}
