use std::convert::identity;
use std::ops::Index;

#[derive(Clone)]
pub(crate) struct StableVec<T>(Vec<Option<T>>);

impl<T> StableVec<T> {
    pub(crate) fn new() -> Self {
        Self(Vec::new())
    }

    pub(crate) fn count(&self) -> usize {
        self.0.iter().filter(|item| item.is_some()).count()
    }

    pub(crate) fn len(&self) -> usize {
        self.0.len()
    }

    pub(crate) fn get(&self, index: usize) -> Option<&T> {
        self.0.get(index).and_then(|item| item.as_ref())
    }

    pub(crate) fn push(&mut self, item: T) -> usize {
        self.0.push(Some(item));
        self.0.len() - 1
    }

    pub(crate) fn pop(&mut self) {
        if let Some(index) = self
            .0
            .iter()
            .enumerate()
            .rev()
            .filter_map(|(index, item)| if item.is_some() { Some(index) } else { None })
            .next()
        {
            self.remove(index);
        }
    }

    pub(crate) fn remove(&mut self, index: usize) {
        self.0[index] = None;
    }

    pub(crate) fn find_last(&self) -> Option<&T> {
        self.0.iter().rev().filter_map(|item| item.as_ref()).next()
    }

    pub(crate) fn iter(&self) -> impl Iterator<Item = &T> {
        self.0.iter().filter_map(|item| item.as_ref())
    }

    pub(crate) fn iter_mut(&mut self) -> impl Iterator<Item = &mut T> {
        self.0.iter_mut().filter_map(|item| item.as_mut())
    }

    pub(crate) fn into_iter(self) -> impl IntoIterator<Item = T> {
        self.0.into_iter().filter_map(identity)
    }
}

impl<T> Index<usize> for StableVec<T> {
    type Output = T;

    fn index(&self, index: usize) -> &Self::Output {
        self.0[index].as_ref().unwrap()
    }
}
