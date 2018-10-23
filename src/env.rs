use std::cmp::Eq;
use std::collections::HashMap;
use std::error::Error;
use std::fmt::Debug;
use std::hash::Hash;

// TODO implement as RefCell<Vec<(Key, Value)>> and pop() as setting length
crate struct Environment<Key, Value> {
    scopes: Vec<HashMap<Key, Value>>,
    current_scope_log: Vec<(Key, Value)>
}

impl<Key, Value> Environment<Key, Value>
where Key: Eq + Debug + Hash + Clone, Value: Clone + Debug {
    crate fn new() -> Environment<Key, Value> {
        Environment {
            scopes: vec![HashMap::new()],
            current_scope_log: vec![]
        }
    }

    crate fn bind(&mut self, key: Key, value: Value) {
        self.current_scope_log.push((key.clone(), value.clone()));
        self.scopes.last_mut().unwrap().insert(key, value);
    }

    crate fn resolve(&self, key: &Key) -> Result<Value, Box<dyn Error>> {
        for scope in self.scopes.iter().rev() {
            if let Some(value) = scope.get(key) {
                return Ok(value.clone());
            }
        }
        Err(From::from(format!("`{:?}' is undefined", key)))
    }

    crate fn push(&mut self) {
        self.scopes.push(HashMap::new())
    }

    crate fn pop(&mut self) -> Vec<(Key, Value)> {
        self.scopes.pop();
        std::mem::replace(&mut self.current_scope_log, vec![])
    }
}