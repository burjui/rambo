use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt::Debug;
use std::error::Error;
use std::cmp::Eq;
use std::hash::Hash;

pub struct Environment<Key, Value> {
    scopes: Vec<RefCell<HashMap<Key, Value>>>
}

impl<Key, Value> Environment<Key, Value>
where Key: Eq + Debug + Hash, Value: Clone {
    pub fn new() -> Environment<Key, Value> {
        Environment {
            scopes: vec![RefCell::new(HashMap::new())]
        }
    }

    pub fn bind(&self, key: Key, value: &Value) -> Result<(), Box<Error>> {
        if self.last().borrow().contains_key(&key) {
            error!("redefinition of key {:?}", key)
        } else {
            self.last().borrow_mut().insert(key, value.clone());
            Ok(())
        }
    }

    pub fn resolve(&self, key: &Key) -> Result<Value, Box<Error>> {
        for scope in self.scopes.iter().rev() {
            if let Some(value) = scope.borrow().get(key) {
                return Ok(value.clone());
            }
        }
        Err(From::from(format!("`{:?}' is undefined", key)))
    }

    pub fn push(&mut self) {
        self.scopes.push(RefCell::new(HashMap::new()))
    }

    pub fn pop(&mut self) {
        if self.scopes.len() == 1 {
            panic!("Dropping the base scope")
        }
        self.scopes.pop();
    }

    fn last(&self) -> &RefCell<HashMap<Key, Value>> {
        self.scopes.last().unwrap()
    }
}