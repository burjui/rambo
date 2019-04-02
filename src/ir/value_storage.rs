use std::collections::HashMap;
use std::fmt;
use std::ops::Deref;
use std::ops::Index;
use std::ops::IndexMut;

use crate::ir::Value;
use crate::utils::RetainIndices;

pub(crate) struct Reused(bool);

impl Deref for Reused {
    type Target = bool;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

#[derive(Clone)]
pub(crate) struct ValueStorage {
    value_to_id: HashMap<Value, ValueId>,
    values: Vec<Value>,
}

impl ValueStorage {
    pub(crate) fn new() -> Self {
        Self {
            value_to_id: HashMap::new(),
            values: Vec::new(),
        }
    }

    pub(crate) fn insert(&mut self, value: Value) -> (ValueId, Reused) {
        match self.value_to_id.get(&value) {
            Some(index) => (*index, Reused(true)),
            None => {
                let index = self.values.len();
                self.values.push(value.clone());
                self.value_to_id.insert(value, ValueId(index));
                (ValueId(index), Reused(false))
            }
        }
    }

    pub(crate) fn iter(&self) -> impl Iterator<Item = (&Value, ValueId)> {
        self.values
            .iter()
            .enumerate()
            .map(|(index, value)| (value, ValueId(index)))
    }

    pub(crate) fn iter_mut(&mut self) -> impl Iterator<Item = (&mut Value, ValueId)> {
        self.values
            .iter_mut()
            .enumerate()
            .map(|(index, value)| (value, ValueId(index)))
    }

    pub(crate) fn retain(&mut self, mut predicate: impl FnMut(ValueId) -> bool, mut remap: impl FnMut(ValueId, ValueId)) {
        self.value_to_id.retain(|_, index| predicate(*index));
        self.values.retain_indices(|_, index| predicate(ValueId(index)), |_, from, to| remap(ValueId(from), ValueId(to)));
        self.values.shrink_to_fit();
    }
}

impl Index<ValueId> for ValueStorage {
    type Output = Value;

    fn index(&self, value_id: ValueId) -> &Self::Output {
        &self.values[value_id.0]
    }
}

impl IndexMut<ValueId> for ValueStorage {
    fn index_mut(&mut self, value_id: ValueId) -> &mut Self::Output {
        &mut self.values[value_id.0]
    }
}

#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash, Ord, PartialOrd)]
pub(crate) struct ValueId(pub(crate) usize);

pub(crate) static UNDEFINED_VALUE: ValueId = ValueId(usize::max_value());

impl fmt::Display for ValueId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "v{}", self.0)
    }
}
