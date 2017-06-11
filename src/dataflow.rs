use std::rc::Rc;
use std::ops::Deref;
use std::usize;

use semantics::*;

// TODO use Position and the source code instead of TypedEntity::Debug for warnings

pub fn remove_unused_bindings(code: Vec<TypedEntity>) -> Vec<TypedEntity> {
    let usages = {
        let mut bindings = vec![];
        for entity in code.iter() {
            if let &TypedEntity::Binding(ref binding) = entity {
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
            process_binding(binding, &mut usages);
        }

        let mut new_binding_index = 0;
        for (index, binding) in bindings.iter().enumerate() {
            if usages[index] > 0 {
                binding.borrow_mut().index = new_binding_index;
                new_binding_index += 1;

                if let &BindingValue::Var(ref expr) = &binding.borrow().value {
                    if let &TypedExpr::Deref(_) = expr.deref() {
                        println!("warning: redundant binding: {:?}", binding.borrow());
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
        .filter(|entity| match entity {
            &TypedEntity::Binding(ref binding) => {
                let binding_index = binding.borrow().index;
                binding_index != usize::MAX && usages[binding_index] > 0
            },
            _ => true
        })
        .collect::<Vec<TypedEntity>>()
}

fn process_binding(binding: &BindingRef, usages: &mut Vec<usize>) {
    if usages[binding.borrow().index] == 0 {
        println!("warning: unused binding: {:?}", binding.borrow());
        if let &BindingValue::Var(ref expr) = &binding.borrow().value {
            process_expr(expr, usages);
        }
    }
}

fn process_expr(expr: &ExprRef, usages: &mut Vec<usize>) {
    match expr.deref() {
        &TypedExpr::Int(_) | &TypedExpr::String(_) => {}, // nothing to analyze in literals
        &TypedExpr::Deref(ref referenced_binding) => {
            usages[referenced_binding.borrow().index] -= 1;
            process_binding(referenced_binding, usages);
        },
        &TypedExpr::Lambda { ref body, .. } => process_expr(body, usages),
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
        &TypedExpr::AddStr(ref left, ref right) |
        &TypedExpr::Assign(ref left, ref right) => {
            process_expr(left, usages);
            process_expr(right, usages);
        }
    }
}