use std::rc::Rc;

use semantics::*;

pub fn remove_unused_bindings(code: Vec<TypedEntity>) -> Vec<TypedEntity> {
    let mut code = code;
    loop {
        let used = code.iter()
            .map(|entity|
                match entity {
                    &TypedEntity::Binding(ref binding) => Rc::strong_count(binding) >= 2,
                    _ => true
                })
            .collect::<Vec<bool>>();
        code = code.into_iter().enumerate()
            .filter_map(|(i, entity)| if used[i] {
                Some(entity)
            } else {
                println!("warning: unused binding: {:?}", entity);
                None
            })
            .collect::<Vec<TypedEntity>>();
        if used.iter().all(|&id| id) {
            break
        }
    }
    let mut binding_index = 0;
    for entity in code.iter() {
        if let &TypedEntity::Binding(ref binding) = entity {
            binding.borrow_mut().index = binding_index;
            binding_index += 1;
        }
    }
    return code;
}