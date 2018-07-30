use std::usize;

use crate::semantics::*;
use crate::env::Environment;

// TODO use Position and the source code instead of {:?} for warnings
// TODO a better way of reporting warnings than mere println!()

crate enum Warnings { On, Off }

type BindingUsage = Environment<BindingPtr, usize>;

crate fn remove_dead_bindings(code: &Vec<TypedStatement>, warnings: Warnings) -> Vec<TypedStatement> {
    let usages = {
        let mut usages: BindingUsage = BindingUsage::new();
        let mut bindings = vec![];
        for statement in code.iter() {
            if let TypedStatement::Binding(binding) = statement {
                usages.bind(binding.ptr(), 1).unwrap();
                bindings.push(binding);
                process_binding(&binding, &mut usages);

                if let Warnings::On = warnings {
                    let usage = usages.resolve(&binding.ptr()).unwrap();
                    if usage == 0 {
                        warning!("unused binding: {:?}", binding.borrow());
                    }
                }

                let usage = usages.resolve(&binding.ptr()).unwrap_or(0);
                if usage > 0 {
                    if let Warnings::On = warnings {
                        let binding = binding.borrow();
                        let value = match &binding.value {
                            BindingValue::Var(expr) => expr,
                            _ => unreachable!()
                        };
                        if let TypedExpr::Deref(referenced_binding) = value as &TypedExpr {
                            if binding.name == referenced_binding.borrow().name {
                                warning!("redundant binding: {:?}", binding);
                            }
                        }
                    }
                } else {
                    usages.bind_force(binding.ptr(), 0);
                }
            }
        }

        usages
    };

    code.into_iter()
        .filter_map(|statement| match &statement {
            TypedStatement::Binding(binding) => {
                if usages.resolve(&binding.ptr()).unwrap() > 0 {
                    Some(statement.clone())
                } else {
                    None
                }
            },
            TypedStatement::Expr(expr) => {
                if let TypedExpr::Assign(left, _) = expr as &TypedExpr {
                    let binding = match left as &TypedExpr {
                        TypedExpr::Deref(binding) => binding,
                        _ => unreachable!()
                    };
                    if usages.resolve(&binding.ptr()).unwrap() == 0 {
                        return None
                    }
                }

                Some(statement.clone())
            }
        })
        .collect::<Vec<TypedStatement>>()
}

fn process_binding(binding: &BindingRef, usages: &mut BindingUsage) {
    if usages.resolve(&binding.ptr()).is_ok() {
        if let BindingValue::Var(expr) = &binding.borrow().value {
            process_expr(expr, usages);
        }
    }
}

fn process_expr(expr: &ExprRef, usages: &mut BindingUsage) {
    match expr as &TypedExpr {
        TypedExpr::Phantom |
        TypedExpr::Unit |
        TypedExpr::Int(_) |
        TypedExpr::String(_) => {}, // nothing to analyze in either of these

        TypedExpr::Deref(binding) => {
            if let BindingValue::Var(_) = &binding.borrow().value {
                if let Ok(usage) = usages.resolve(&binding.ptr()) {
                    usages.bind_force(binding.ptr(), usage - 1);
                }
                process_binding(binding, usages);
            }
        },
        TypedExpr::Lambda(lambda) => {
            usages.push();
            for binding in &lambda.parameters {
                usages.bind(binding.ptr(), 1).unwrap();
            }
            process_expr(&lambda.body, usages);
            for binding in &lambda.parameters {
                if usages.resolve(&binding.ptr()).unwrap() == 0 {
                    warning!("unused parameter `{}'", binding.borrow().name);
                }
            }
            usages.pop();
        },
        TypedExpr::Application { function, arguments, .. } => {
            process_expr(function, usages);
            for argument in arguments.iter() {
                process_expr(argument, usages);
            }
        },
        TypedExpr::AddInt(left, right) |
        TypedExpr::SubInt(left, right) |
        TypedExpr::MulInt(left, right) |
        TypedExpr::DivInt(left, right) |
        TypedExpr::AddStr(left, right) => {
            process_expr(left, usages);
            process_expr(right, usages);
        },
        TypedExpr::Assign(_, right) => process_expr(right, usages),
        TypedExpr::Conditional { condition, positive, negative } => {
            process_expr(condition, usages);
            process_expr(positive, usages);
            negative.as_ref().map(|expr| process_expr(expr, usages));
        },
        TypedExpr::Block(_) => {
        }
    }
}
