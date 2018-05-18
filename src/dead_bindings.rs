use std::ops::Deref;
use std::usize;

use semantics::*;
use std::collections::HashMap;
use std::cell::RefCell;

// TODO use Position and the source code instead of {:?} for warnings
// TODO a better way of reporting warnings than mere println!()

pub enum Warnings { On, Off }

type BindingCell = RefCell<Binding>;
type BindingPtr = *const BindingCell;

trait Ptr<T> {
    fn ptr(self) -> *const T;
}

impl<'a> Ptr<BindingCell> for &'a BindingRef {
    fn ptr(self) -> *const BindingCell {
        self as &BindingCell as *const BindingCell
    }
}

type BindingUsageMap = HashMap<BindingPtr, usize>;

pub fn remove_dead_bindings(code: &Vec<TypedStatement>, warnings: Warnings) -> Vec<TypedStatement> {
    let usages = {
        let mut usages: BindingUsageMap = HashMap::new();
        let mut bindings = vec![];
        for statement in code.iter() {
            if let &TypedStatement::Binding(ref binding) = statement {
                usages.insert(binding.ptr(), 1);
                bindings.push(binding);
            }
        }

        for binding in bindings.iter() {
            process_binding(&binding, &mut usages);
        }

        if let Warnings::On = warnings {
            for binding in bindings.iter() {
                let usage = *usages.get(&binding.ptr()).unwrap();
                if usage == 0 {
                    warning!("unused binding: {:?}", binding.borrow());
                }
            }
        }

        for binding in bindings.iter() {
            let usage = *usages.get(&binding.ptr()).unwrap_or(&0);
            if usage > 0 {
                if let Warnings::On = warnings {
                    let binding = binding.borrow();
                    let value = match &binding.value {
                        &BindingValue::Var(ref expr) => expr,
                        _ => unreachable!()
                    };
                    if let &TypedExpr::Deref(ref referenced_binding) = value.deref() {
                        if binding.name == referenced_binding.borrow().name {
                            warning!("redundant binding: {:?}", binding);
                        }
                    }
                }
            } else {
                *usages.get_mut(&binding.ptr()).unwrap() = 0;
            }
        }

        usages
    };

    code.into_iter()
        .filter_map(|statement| match &statement {
            &TypedStatement::Binding(ref binding) => {
                if *usages.get(&binding.ptr()).unwrap() > 0 {
                    Some(statement.clone())
                } else {
                    None
                }
            },
            &TypedStatement::Expr(ref expr) => {
                if let &TypedExpr::Assign(ref left, _) = expr.deref() {
                    let binding = match left.deref() {
                        &TypedExpr::Deref(ref binding) => binding,
                        _ => unreachable!()
                    };
                    if *usages.get(&binding.ptr()).unwrap() == 0 {
                        return None
                    }
                }

                Some(statement.clone())
            }
        })
        .collect::<Vec<TypedStatement>>()
}

fn process_binding(binding: &BindingRef, usages: &mut BindingUsageMap) {
    if usages.contains_key(&binding.ptr()) {
        if let &BindingValue::Var(ref expr) = &binding.borrow().value {
            process_expr(expr, usages);
        }
    }
}

fn process_expr(expr: &ExprRef, usages: &mut BindingUsageMap) {
    match expr.deref() {
        &TypedExpr::Phantom | &TypedExpr::Unit | &TypedExpr::Int(_) | &TypedExpr::String(_) => {}, // nothing to analyze in either of these
        &TypedExpr::Deref(ref binding) => {
            if let &BindingValue::Var(_) = &binding.borrow().value {
                if let Some(usage) = usages.get_mut(&binding.ptr()) {
                    *usage -= 1;
                }
                process_binding(binding, usages);
            }
        },
        &TypedExpr::Lambda(Lambda { ref body, ref parameters, .. }) => {
            for binding in parameters {
                usages.insert(binding.ptr(), 1);
            }
            process_expr(&body, usages);
            for binding in parameters {
                if *usages.get(&binding.ptr()).unwrap() == 0 {
                    warning!("unused parameter `{}'", binding.borrow().name);
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
