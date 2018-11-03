use std::cmp::Eq;
use std::collections::HashMap;
use std::error::Error;
use std::fmt::Debug;
use std::hash::Hash;

// TODO implement as RefCell<Vec<(Key, Value)>> and pop() as setting length
crate struct Environment<Key, Value> {
    scopes: Vec<HashMap<Key, Value>>,
    scope_logs: Vec<Vec<Value>>
}

impl<Key, Value> Environment<Key, Value>
where Key: Eq + Debug + Hash + Clone, Value: Clone + Debug {
    crate fn new() -> Environment<Key, Value> {
        Environment {
            scopes: vec![HashMap::new()],
            scope_logs: vec![]
        }
    }

    crate fn bind(&mut self, key: Key, value: Value) {
        self.scopes.last_mut().unwrap().insert(key, value.clone());
        self.scope_logs.last_mut().unwrap().push(value);
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
        self.scopes.push(HashMap::new());
        self.scope_logs.push(vec![]);
    }

    crate fn pop(&mut self) -> Vec<Value> {
        self.scopes.pop();
        self.scope_logs.pop().unwrap()
    }
}
