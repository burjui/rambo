use std::collections::HashMap;
use std::fmt;
use std::ops::Deref;
use std::ops::Index;
use std::ops::IndexMut;

use stable_vec::StableVec;

use crate::ir::Value;

pub(crate) struct Reused(bool);

impl Deref for Reused {
    type Target = bool;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

pub(crate) struct ValueStorage {
    by_value: HashMap<Value, ValueIndex>,
    by_index: StableVec<Value>,
}

impl ValueStorage {
    pub(crate) fn new() -> Self {
        Self {
            by_value: HashMap::new(),
            by_index: StableVec::new(),
        }
    }

    pub(crate) fn insert(&mut self, value: Value) -> (ValueIndex, Reused) {
        match self.by_value.get(&value) {
            Some(index) => (*index, Reused(true)),
            None => {
                let index = ValueIndex(self.by_index.push(value.clone()));
                self.by_value.insert(value, index);
                (index, Reused(false))
            }
        }
    }

    pub(crate) fn iter(&self) -> impl Iterator<Item = &Value> {
        self.by_index.iter()
    }

    pub(crate) fn retain(&mut self, predicate: impl Fn(ValueIndex) -> bool) {
        self.by_value.retain(|_, index| predicate(*index));
        self.by_index.retain_indices(|index| predicate(ValueIndex(index)));
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
pub(crate) struct ValueIndex(pub(crate) usize);

impl fmt::Display for ValueIndex {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Debug::fmt(self, f)
    }
}
