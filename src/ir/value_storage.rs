use std::collections::HashMap;
use std::fmt;
use std::ops::Index;
use std::ops::IndexMut;

use crate::ir::Value;

#[derive(Deref, DerefMut)]
pub(crate) struct Reused(bool);

pub(crate) struct ValueStorage {
    by_value: HashMap<Value, ValueIndex>,
    by_index: Vec<Value>,
}

impl ValueStorage {
    pub(crate) fn new() -> Self {
        Self {
            by_value: HashMap::new(),
            by_index: Vec::new(),
        }
    }

    pub(crate) fn insert(&mut self, value: Value) -> (ValueIndex, Reused) {
        let value_count = self.by_index.len();
        let index = *self.by_value
            .entry(value.clone())
            .or_insert_with(|| ValueIndex(value_count));
        let reused = index.0 < value_count;
        if !reused {
            self.by_index.push(value);
        }
        (index, Reused(reused))
    }
}

impl Index<ValueIndex> for ValueStorage {
    type Output = Value;

    fn index(&self, index: ValueIndex) -> &Self::Output {
        &self.by_index[index.0]
    }
}

impl IndexMut<ValueIndex> for ValueStorage {
    fn index_mut(&mut self, index: ValueIndex) -> &mut Self::Output {
        &mut self.by_index[index.0]
    }
}

#[derive(Debug, Eq, PartialEq, Hash, Copy, Clone)]
pub(crate) struct ValueIndex(usize);

impl fmt::Display for ValueIndex {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Debug::fmt(self, f)
    }
}
