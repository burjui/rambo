use std::cmp::Eq;
use std::collections::HashMap;
use std::fmt::Debug;
use std::hash::Hash;

pub(crate) struct Environment<Key, Value> {
    scopes: Vec<HashMap<Key, Value>>,
    current_scope_count: usize,
}

impl<Key, Value> Environment<Key, Value>
where
    Key: Eq + Debug + Hash + Clone,
    Value: Debug,
{
    pub(crate) fn new() -> Environment<Key, Value> {
        Environment {
            scopes: Vec::new(),
            current_scope_count: 0,
        }
    }

    pub(crate) fn bind(&mut self, key: Key, value: Value) {
        self.scopes[self.current_scope_count - 1].insert(key, value);
    }

    pub(crate) fn resolve(&self, key: &Key) -> Result<&Value, String> {
        for scope in self.scopes[0..self.current_scope_count].iter().rev() {
            if let Some(value) = scope.get(key) {
                return Ok(value);
            }
        }
        Err(format!("`{:?}' is undefined", key))
    }

    pub(crate) fn push(&mut self) {
        if self.current_scope_count == self.scopes.len() {
            self.scopes.push(HashMap::new());
        }
        self.current_scope_count += 1;
    }

    pub(crate) fn pop(&mut self) {
        debug_assert_ne!(self.current_scope_count, 0);
        self.scopes[self.current_scope_count - 1].clear();
        self.current_scope_count -= 1;
    }
}
