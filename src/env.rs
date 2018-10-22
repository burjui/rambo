use std::cell::RefCell;
use std::cmp::Eq;
use std::collections::HashMap;
use std::error::Error;
use std::fmt::Debug;
use std::hash::Hash;

// TODO implement as RefCell<Vec<(Key, Value)>> and pop() as setting length
crate struct Environment<Key, Value> {
    scopes: Vec<RefCell<HashMap<Key, Value>>>
}

impl<Key, Value> Environment<Key, Value>
where Key: Eq + Debug + Hash, Value: Clone + Debug {
    crate fn new() -> Environment<Key, Value> {
        Environment {
            scopes: vec![RefCell::new(HashMap::new())]
        }
    }

    crate fn bind(&self, key: Key, value: Value) {
        self.last().borrow_mut().insert(key, value);
    }

    crate fn resolve(&self, key: &Key) -> Result<Value, Box<dyn Error>> {
        for scope in self.scopes.iter().rev() {
            if let Some(value) = scope.borrow().get(key) {
                return Ok(value.clone());
            }
        }
        Err(From::from(format!("`{:?}' is undefined", key)))
    }

    crate fn push(&mut self) {
        self.scopes.push(RefCell::new(HashMap::new()))
    }

    crate fn pop(&mut self) -> HashMap<Key, Value> {
        self.scopes.pop().unwrap().into_inner()
    }

    fn last(&self) -> &RefCell<HashMap<Key, Value>> {
        self.scopes.last().unwrap()
    }
}