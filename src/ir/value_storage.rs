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

#[derive(Clone)]
pub(crate) struct ValueStorage {
    by_value: HashMap<Value, ValueId>,
    by_index: StableVec<Value>,
}

impl ValueStorage {
    pub(crate) fn new() -> Self {
        Self {
            by_value: HashMap::new(),
            by_index: StableVec::new(),
        }
    }

    pub(crate) fn insert(&mut self, value: Value) -> (ValueId, Reused) {
        match self.by_value.get(&value) {
            Some(index) => (*index, Reused(true)),
            None => {
                let index = ValueId(self.by_index.push(value.clone()));
                self.by_value.insert(value, index);
                (index, Reused(false))
            }
        }
    }

    pub(crate) fn iter(&self) -> impl Iterator<Item = &Value> {
        self.by_index.iter()
    }

    pub(crate) fn indices(&self) -> impl Iterator<Item = &ValueId> {
        self.by_value.values()
    }

    pub(crate) fn retain(&mut self, mut predicate: impl FnMut(ValueId) -> bool) {
        self.by_value.retain(|_, index| predicate(*index));
        self.by_index.retain_indices(|index| predicate(ValueId(index)));
    }
}

impl Index<ValueId> for ValueStorage {
    type Output = Value;

    fn index(&self, index: ValueId) -> &Self::Output {
        &self.by_index[index.0]
    }
}

impl IndexMut<ValueId> for ValueStorage {
    fn index_mut(&mut self, index: ValueId) -> &mut Self::Output {
        &mut self.by_index[index.0]
    }
}

#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash, Ord, PartialOrd)]
pub(crate) struct ValueId(pub(crate) usize);

pub(crate) static UNDEFINED_VALUE: ValueId = ValueId(usize::max_value());

impl fmt::Display for ValueId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Debug::fmt(self, f)
    }
}
