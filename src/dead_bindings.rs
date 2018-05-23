use std::usize;

use semantics::*;
use env::Environment;

// TODO use Position and the source code instead of {:?} for warnings
// TODO a better way of reporting warnings than mere println!()

pub enum Warnings { On, Off }

type BindingUsage = Environment<BindingPtr, usize>;

pub fn remove_dead_bindings(code: &Vec<TypedStatement>, warnings: Warnings) -> Vec<TypedStatement> {
    let usages = {
        let mut usages: BindingUsage = BindingUsage::new();
        let mut bindings = vec![];
        for statement in code.iter() {
            if let &TypedStatement::Binding(ref binding) = statement {
                usages.bind(binding.ptr(), 1).unwrap();
                bindings.push(binding);
            }
        }

        for binding in bindings.iter() {
            process_binding(&binding, &mut usages);
        }

        if let Warnings::On = warnings {
            for binding in bindings.iter() {
                let usage = usages.resolve(&binding.ptr()).unwrap();
                if usage == 0 {
                    warning!("unused binding: {:?}", binding.borrow());
                }
            }
        }

        for binding in bindings.iter() {
            let usage = usages.resolve(&binding.ptr()).unwrap_or(0);
            if usage > 0 {
                if let Warnings::On = warnings {
                    let binding = binding.borrow();
                    let value = match &binding.value {
                        &BindingValue::Var(ref expr) => expr,
                        _ => unreachable!()
                    };
                    if let &TypedExpr::Deref(ref referenced_binding) = &value as &TypedExpr {
                        if binding.name == referenced_binding.borrow().name {
                            warning!("redundant binding: {:?}", binding);
                        }
                    }
                }
            } else {
                usages.bind_force(binding.ptr(), 0);
            }
        }

        usages
    };

    code.into_iter()
        .filter_map(|statement| match &statement {
            &TypedStatement::Binding(ref binding) => {
                if usages.resolve(&binding.ptr()).unwrap() > 0 {
                    Some(statement.clone())
                } else {
                    None
                }
            },
            &TypedStatement::Expr(ref expr) => {
                if let &TypedExpr::Assign(ref left, _) = &expr as &TypedExpr {
                    let binding = match &left as &TypedExpr {
                        &TypedExpr::Deref(ref binding) => binding,
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
        if let &BindingValue::Var(ref expr) = &binding.borrow().value {
            process_expr(expr, usages);
        }
    }
}

fn process_expr(expr: &ExprRef, usages: &mut BindingUsage) {
    match &expr as &TypedExpr {
        &TypedExpr::Phantom | &TypedExpr::Unit | &TypedExpr::Int(_) | &TypedExpr::String(_) => {}, // nothing to analyze in either of these
        &TypedExpr::Deref(ref binding) => {
            if let &BindingValue::Var(_) = &binding.borrow().value {
                if let Ok(usage) = usages.resolve(&binding.ptr()) {
                    usages.bind_force(binding.ptr(), usage - 1);
                }
                process_binding(binding, usages);
            }
        },
        &TypedExpr::Lambda(ref lambda) => {
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
        &TypedExpr::Application { ref function, ref arguments, .. } => {
            process_expr(function, usages);
            for argument in arguments.iter() {
                process_expr(argument, usages);
            }
        },
        &TypedExpr::AddInt(ref left, ref right) |
        &TypedExpr::SubInt(ref left, ref right) |
        &TypedExpr::MulInt(ref left, ref right) |
        &TypedExpr::DivInt(ref left, ref right) |
        &TypedExpr::AddStr(ref left, ref right) => {
            process_expr(left, usages);
            process_expr(right, usages);
        },
        &TypedExpr::Assign(_, ref right) => process_expr(right, usages),
        &TypedExpr::Conditional { ref condition, ref positive, ref negative } => {
            process_expr(condition, usages);
            process_expr(positive, usages);
            negative.as_ref().map(|expr| process_expr(expr, usages));
        },
        &TypedExpr::Block(_) => {
        }
    }
}
