use std::collections::HashMap;
use std::fmt;
use std::ops::Deref;
use std::ops::Index;
use std::ops::IndexMut;

use crate::ir::Value;
use crate::utils::RetainIndex;

pub(crate) struct Reused(bool);

impl Deref for Reused {
    type Target = bool;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

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

    pub(crate) fn iter(&self) -> impl Iterator<Item = &Value> {
        self.by_index.iter()
    }

    pub(crate) fn retain(
        &mut self,
        predicate: impl Fn(ValueIndex) -> bool,
        mut remap: impl FnMut(ValueIndex, ValueIndex))
    {
        self.by_value.retain(|_, index| predicate(*index));
        self.by_index.retain_index(|index| predicate(ValueIndex(index)), |old_index, new_index|
            remap(ValueIndex(old_index), ValueIndex(new_index)));
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
