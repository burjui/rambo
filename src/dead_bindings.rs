use std::rc::Rc;
use std::ops::Deref;
use std::usize;

use semantics::*;

// TODO use Position and the source code instead of TypedStatement::Debug for warnings
// TODO a better way of reporting warnings than mere println!()

pub enum Warnings { On, Off }

pub fn remove_dead_bindings(code: Vec<TypedStatement>, warnings: Warnings) -> Vec<TypedStatement> {
    let usages = {
        let mut bindings = vec![];
        for statement in code.iter() {
            if let &TypedStatement::Binding(ref binding) = statement {
                bindings.push(binding);
            }
        }

        let binding_count = bindings.len();
        let mut usages: Vec<usize> = Vec::with_capacity(binding_count);
        usages.resize(binding_count, 0);
        for (binding, usage) in bindings.iter().zip(usages.iter_mut()) {
            *usage = Rc::strong_count(binding) - 1;
        }

        for binding in bindings.iter() {
            process_binding(&binding.borrow(), &mut usages);
        }

        for statement in code.iter() {
            if let &TypedStatement::Expr(ref expr) = statement {
                if let &TypedExpr::Assign(ref left, _) = expr.deref() {
                    let binding = match left.deref() {
                        &TypedExpr::Deref(ref binding) => binding.borrow(),
                        _ => unreachable!()
                    };
                    if let &BindingValue::Var(_) = &binding.value {
                        usages[binding.index] -= 1;
                    }
                }
            }
        }

        if let Warnings::On = warnings {
            for (binding, usage) in bindings.iter().zip(usages.iter()) {
                if *usage == 0 {
                    println!("warning: unused binding: {:?}", binding.borrow());
                }
            }
        }

        let mut new_binding_index = 0;
        for (index, binding) in bindings.iter().enumerate() {
            if usages[index] > 0 {
                binding.borrow_mut().index = new_binding_index;
                new_binding_index += 1;

                if let Warnings::On = warnings {
                    let binding = binding.borrow();
                    let value = match &binding.value {
                        &BindingValue::Var(ref expr) => expr,
                        _ => unreachable!()
                    };
                    if let &TypedExpr::Deref(ref referenced_binding) = value.deref() {
                        if binding.name == referenced_binding.borrow().name {
                            println!("warning: redundant binding: {:?}", binding);
                        }
                    }
                }
            } else {
                binding.borrow_mut().index = usize::MAX;
            }
        }

        let used_binding_count = new_binding_index;
        for index in 0..binding_count {
            usages[index] = if index < used_binding_count { 1 } else { 0 };
        }

        usages
    };

    code.into_iter()
        .filter_map(|statement| match &statement {
            &TypedStatement::Binding(ref binding) => {
                let binding_index = binding.borrow().index;
                if binding_index != usize::MAX && usages[binding_index] > 0 {
                    Some(statement.clone())
                } else {
                    None
                }
            },
            &TypedStatement::Expr(ref expr) => {
                if let &TypedExpr::Assign(ref left, _) = expr.deref() {
                    let binding = match left.deref() {
                        &TypedExpr::Deref(ref binding) => binding.borrow(),
                        _ => unreachable!()
                    };
                    if binding.index == usize::MAX {
                        return None
                    }
                }

                Some(statement.clone())
            }
        })
        .collect::<Vec<TypedStatement>>()
}

fn process_binding(binding: &Binding, usages: &mut Vec<usize>) {
    if usages[binding.index] == 0 {
        if let &BindingValue::Var(ref expr) = &binding.value {
            process_expr(expr, usages);
        }
    }
}

fn process_expr(expr: &ExprRef, usages: &mut Vec<usize>) {
    match expr.deref() {
        &TypedExpr::Phantom | &TypedExpr::Unit | &TypedExpr::Int(_) | &TypedExpr::String(_) => {}, // nothing to analyze in either of these
        &TypedExpr::Deref(ref binding) => {
            let binding = &binding.borrow();
            if let &BindingValue::Var(_) = &binding.value {
                usages[binding.index] -= 1;
                process_binding(binding, usages);
            }
        },
        &TypedExpr::Lambda { ref body, ref parameter_bindings, .. } => {
            process_expr(body, usages);
            for binding in parameter_bindings {
                if Rc::strong_count(binding) < 2 {
                    println!("warning: unused parameter `{}'", binding.borrow().name);
                }
            }
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
        &TypedExpr::Assign(_, ref right) => process_expr(right, usages)
    }
}
