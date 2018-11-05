use crate::env::Environment;
use crate::semantics::Binding;
use crate::semantics::BindingRef;
use crate::semantics::Block;
use crate::semantics::ExprRef;
use crate::semantics::TypedExpr;
use crate::semantics::TypedStatement;
use crate::source::Source;
use num_bigint::BigInt;
use num_traits::Zero;
use std::collections::HashMap;
use std::ops::Add;
use std::ops::Div;
use std::ops::Mul;
use std::ops::Sub;
use std::rc::Rc;

// TODO implement operation-specific optimizations, such as "x*1 = x", "x+0 = x" and so on

struct BindingStats {
    usages: usize,
    assigned: bool
}

crate struct CFP {
    env: Environment<Rc<String>, BindingRef>,
    binding_stats: HashMap<BindingRef, BindingStats>,
}


impl CFP {
    crate fn new() -> CFP {
        CFP {
            env: Environment::new(),
            binding_stats: HashMap::new()
        }
    }

    #[must_use]
    crate fn fold(&mut self, expr: &ExprRef) -> ExprRef {
        self.fold_impl(expr)
    }

    fn fold_impl(&mut self, expr: &ExprRef) -> ExprRef {
        match expr as &TypedExpr {
            TypedExpr::Deref(name, _, source) => {
                let binding = self.env.resolve(name).unwrap();
                let stats = self.binding_stats(&binding);
                if stats.assigned || !is_primitive_constant(&binding.data) {
                    stats.usages += 1;
                }
                binding.data.clone_at(source.clone())
            },
            TypedExpr::AddInt(left, right, source) => self.try_fold_numeric(&left, &right, Add::add, TypedExpr::AddInt, source),
            TypedExpr::SubInt(left, right, source) => self.try_fold_numeric(&left, &right, Sub::sub, TypedExpr::SubInt, source),
            TypedExpr::MulInt(left, right, source) => self.try_fold_numeric(&left, &right, Mul::mul, TypedExpr::MulInt, source),
            TypedExpr::DivInt(left, right, source) => self.try_fold_numeric(&left, &right, Div::div, TypedExpr::DivInt, source),
            TypedExpr::AddStr(left, right, source) => {
                let left = self.fold(left);
                let right = self.fold(right);
                match (&left as &TypedExpr, &right as &TypedExpr) {
                    (TypedExpr::String(left, _), TypedExpr::String(right, _)) => {
                        let mut result = String::with_capacity(left.len() + right.len());
                        result.push_str(left);
                        result.push_str(right);
                        ExprRef::from(TypedExpr::String(Rc::new(result), source.clone()))
                    },
                    _ => ExprRef::from(TypedExpr::AddStr(left.clone(), right.clone(), source.clone()))
                }
            },
            TypedExpr::Assign(name, value, source) => {
                let binding = self.env.resolve(name).unwrap();
                let stats = self.binding_stats(&binding);
                stats.assigned = true;
                stats.usages += 1;
                ExprRef::from(TypedExpr::Assign(name.clone(), self.fold(value), source.clone()))
            },
            TypedExpr::Application { function, arguments, source, .. } => {
                let function_binding = match function as &TypedExpr {
                    TypedExpr::Deref(name, _, _) => Some(self.env.resolve(name).unwrap()),
                    _ => None
                };
                let function_ = function;
                let mut function = self.fold(function_);
                if is_primitive_constant(&function) {
                    function
                } else {
                    let arguments = arguments.iter()
                        .map(|argument| self.fold(argument))
                        .collect::<Vec<_>>();
                    if arguments.iter().all(|argument| is_constant(argument)) {
                        while let TypedExpr::Deref(name, _, source) = &function.clone() as &TypedExpr {
                            let binding = self.env.resolve(name).unwrap();
                            function = binding.data.clone_at(source.clone());
                        }
                        if let TypedExpr::Lambda(lambda, _) = &function as &TypedExpr {
                            self.env.push();
                            for (parameter, argument) in lambda.parameters.iter().zip(arguments.into_iter()) {
                                let parameter_name = Rc::new(parameter.name.text().to_owned());
                                let binding = BindingRef::from(Binding::new(parameter_name, argument, parameter.name.clone()));
                                self.register_binding(binding);
                                // TODO factor out functions for less indentation
                            }
                            let result = self.fold(&lambda.body);
                            self.env.pop();

                            if is_primitive_constant(&result) {
                                if let Some(binding) = function_binding {
                                    self.binding_stats(&binding).usages -= 1;
                                }
                                return result.clone_at(source.clone())
                            }
                        }
                    }
                    expr.clone()
                }
            },
            TypedExpr::Lambda(_, _) => expr.clone(),
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
            TypedExpr::Phantom(_) |
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
        let statements = statements.iter()
            .map(|statement| match statement {
                TypedStatement::Binding(binding) =>
                    if self.binding_stats(&binding).usages > 0 {
                        statement.clone()
                    } else {
                        TypedStatement::Expr(ExprRef::from(TypedExpr::Unit(binding.source.clone())))
                    },
                TypedStatement::Expr(_) => statement.clone()
            })
            .fold(Vec::new(), |mut result, statement| {
                // TODO eliminate subsequent assignments to the same var
                if let Some(TypedStatement::Expr(e1)) = result.last() {
                    if is_primitive_constant(e1) {
                        (*result.last_mut().unwrap()) = statement.clone();
                        return result;
                    }
                }
                result.push(statement.clone());
                result
            });
        self.env.pop();

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

    fn binding_stats(&mut self, binding: &BindingRef) -> &mut BindingStats {
        self.binding_stats.get_mut(&binding).unwrap()
    }

    fn fold_statement(&mut self, statement: &TypedStatement) -> TypedStatement {
        match statement {
            TypedStatement::Binding(binding) => {
                let binding = BindingRef::from(Binding {
                    name: binding.name.clone(),
                    data: self.fold(&binding.data),
                    source: binding.source.clone()
                });
                self.register_binding(binding.clone());
                TypedStatement::Binding(binding)
            },
            TypedStatement::Expr(expr) => TypedStatement::Expr(self.fold(&expr))
        }
    }

    fn register_binding(&mut self, binding: BindingRef) {
        self.env.bind(binding.name.clone(), binding.clone());
        self.binding_stats.insert(binding, BindingStats {
            usages: 0,
            assigned: false
        });
    }

    #[must_use]
    fn try_fold_numeric<FoldFn, Constructor>(
        &mut self, left: &ExprRef, right: &ExprRef,
        fold_impl: FoldFn, constructor: Constructor, source: &Source) -> ExprRef
    where FoldFn: FnOnce(BigInt, BigInt) -> BigInt,
          Constructor: FnOnce(ExprRef, ExprRef, Source) -> TypedExpr
    {
        let (left, right) = (self.fold(left), self.fold(right));
        match (&left as &TypedExpr, &right as &TypedExpr) {
            (TypedExpr::Int(left, _), TypedExpr::Int(right, _)) => {
                let result = fold_impl(left.clone(), right.clone());
                ExprRef::from(TypedExpr::Int(result, source.clone()))
            },
            _ => ExprRef::from(constructor(left.clone(), right.clone(), source.clone()))
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
