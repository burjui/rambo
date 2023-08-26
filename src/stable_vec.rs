use std::fmt::Debug;
use std::ops::{Index, IndexMut};

#[derive(Clone)]
pub(crate) struct StableVec<T> {
    items: Vec<Option<T>>,
    length: usize,
}

impl<T> StableVec<T>
where
    T: Debug,
{
    pub(crate) const fn new() -> Self {
        Self {
            items: Vec::new(),
            length: 0,
        }
    }

    pub(crate) fn len(&self) -> usize {
        self.length
    }

    pub(crate) fn get(&self, index: usize) -> Option<&T> {
        self.items.get(index).and_then(Option::as_ref)
    }

    pub(crate) fn push(&mut self, item: T) -> usize {
        self.items.push(Some(item));
        self.length += 1;
        self.items.len() - 1
    }

    pub(crate) fn pop(&mut self) {
        let last_occupied_index = self
            .items
            .iter()
            .enumerate()
            .rev()
            .find_map(
                |(index, item)| {
                    if item.is_some() {
                        Some(index)
                    } else {
                        None
                    }
                },
            );
        if let Some(index) = last_occupied_index {
            self.remove(index);
        }
    }

    pub(crate) fn remove(&mut self, index: usize) {
        self.items[index] = None;
        self.length -= 1;
    }

    pub(crate) fn find_last(&self) -> Option<&T> {
        self.items.iter().rev().find_map(Option::as_ref)
    }

    pub(crate) fn iter(&self) -> impl Iterator<Item = &T> {
        self.items.iter().filter_map(Option::as_ref)
    }

    pub(crate) fn enumerate(&self) -> impl Iterator<Item = (usize, &T)> {
        self.items
            .iter()
            .enumerate()
            .filter_map(|(index, item)| item.as_ref().map(|item| (index, item)))
    }

    pub(crate) fn iter_mut(&mut self) -> impl Iterator<Item = &mut T> {
        self.items.iter_mut().filter_map(Option::as_mut)
    }

    pub(crate) fn into_iter(self) -> impl Iterator<Item = T> {
        self.items.into_iter().flatten()
    }

    pub(crate) fn next_index(&self, index: usize) -> Option<usize> {
        (index + 1..self.items.len()).find(|index| self.get(*index).is_some())
    }

    pub(crate) fn indices(&self) -> impl Iterator<Item = usize> + '_ {
        self.enumerate().map(|(index, _)| index)
    }
}

impl<T> Index<usize> for StableVec<T> {
    type Output = T;

    fn index(&self, index: usize) -> &Self::Output {
        self.items[index].as_ref().unwrap()
    }
}

impl<T> IndexMut<usize> for StableVec<T> {
    fn index_mut(&mut self, index: usize) -> &mut Self::Output {
        self.items[index].as_mut().unwrap()
    }
}
#[test]
fn test() {
    let mut v = StableVec::<i32>::new();
    assert_eq!(v.len(), 0);
    assert_eq!(v.iter().next(), None);
    assert_eq!(v.iter_mut().next(), None);
    assert_eq!(v.clone().into_iter().next(), None);
    assert_eq!(v.find_last(), None);
    assert_eq!(v.get(0), None);
    assert_eq!(v.indices().next(), None);
    assert_eq!(v.next_index(0), None);

    v.push(-10);
    assert_eq!(v.len(), 1);
    assert_eq!(v.iter().next(), Some(&-10));
    assert_eq!(v.iter_mut().next(), Some(&mut -10));
    assert_eq!(v.clone().into_iter().next(), Some(-10));
    assert_eq!(v.find_last(), Some(&-10));
    assert_eq!(v.get(0), Some(&-10));
    assert_eq!(v.indices().next(), Some(0));
    assert_eq!(v.next_index(0), None);

    v.push(27);
    assert_eq!(v.len(), 2);
    assert_eq!(v.iter().next(), Some(&-10));
    assert_eq!(v.iter().nth(1), Some(&27));
    assert_eq!(v.iter_mut().next(), Some(&mut -10));
    assert_eq!(v.iter_mut().nth(1), Some(&mut 27));
    assert_eq!(v.clone().into_iter().next(), Some(-10));
    assert_eq!(v.clone().into_iter().nth(1), Some(27));
    assert_eq!(v.find_last(), Some(&27));
    assert_eq!(v.get(0), Some(&-10));
    assert_eq!(v.get(1), Some(&27));
    assert_eq!(v.indices().next(), Some(0));
    assert_eq!(v.indices().nth(1), Some(1));
    assert_eq!(v.next_index(0), Some(1));
    assert_eq!(v.next_index(1), None);

    v.push(0);
    assert_eq!(v.len(), 3);
    assert_eq!(v.iter().next(), Some(&-10));
    assert_eq!(v.iter().nth(1), Some(&27));
    assert_eq!(v.iter().nth(2), Some(&0));
    assert_eq!(v.iter_mut().next(), Some(&mut -10));
    assert_eq!(v.iter_mut().nth(1), Some(&mut 27));
    assert_eq!(v.iter_mut().nth(2), Some(&mut 0));
    assert_eq!(v.clone().into_iter().next(), Some(-10));
    assert_eq!(v.clone().into_iter().nth(1), Some(27));
    assert_eq!(v.clone().into_iter().nth(2), Some(0));
    assert_eq!(v.find_last(), Some(&0));
    assert_eq!(v.get(0), Some(&-10));
    assert_eq!(v.get(1), Some(&27));
    assert_eq!(v.get(2), Some(&0));
    assert_eq!(v.indices().next(), Some(0));
    assert_eq!(v.indices().nth(1), Some(1));
    assert_eq!(v.indices().nth(2), Some(2));
    assert_eq!(v.next_index(0), Some(1));
    assert_eq!(v.next_index(1), Some(2));
    assert_eq!(v.next_index(2), None);

    v.remove(1);
    assert_eq!(v.len(), 2);
    assert_eq!(v.iter().next(), Some(&-10));
    assert_eq!(v.iter().nth(1), Some(&0));
    assert_eq!(v.iter().nth(2), None);
    assert_eq!(v.iter_mut().next(), Some(&mut -10));
    assert_eq!(v.iter_mut().nth(1), Some(&mut 0));
    assert_eq!(v.iter_mut().nth(2), None);
    assert_eq!(v.clone().into_iter().next(), Some(-10));
    assert_eq!(v.clone().into_iter().nth(1), Some(0));
    assert_eq!(v.clone().into_iter().nth(2), None);
    assert_eq!(v.find_last(), Some(&0));
    assert_eq!(v.get(0), Some(&-10));
    assert_eq!(v.get(1), None);
    assert_eq!(v.get(2), Some(&0));
    assert_eq!(v.indices().next(), Some(0));
    assert_eq!(v.indices().nth(1), Some(2));
    assert_eq!(v.indices().nth(2), None);
    assert_eq!(v.next_index(0), Some(2));
    assert_eq!(v.next_index(1), Some(2));
    assert_eq!(v.next_index(2), None);

    v.remove(0);
    assert_eq!(v.len(), 1);
    assert_eq!(v.iter().next(), Some(&0));
    assert_eq!(v.iter().nth(1), None);
    assert_eq!(v.iter().nth(2), None);
    assert_eq!(v.iter_mut().next(), Some(&mut 0));
    assert_eq!(v.iter_mut().nth(1), None);
    assert_eq!(v.iter_mut().nth(2), None);
    assert_eq!(v.clone().into_iter().next(), Some(0));
    assert_eq!(v.clone().into_iter().nth(1), None);
    assert_eq!(v.clone().into_iter().nth(2), None);
    assert_eq!(v.find_last(), Some(&0));
    assert_eq!(v.get(0), None);
    assert_eq!(v.get(1), None);
    assert_eq!(v.get(2), Some(&0));
    assert_eq!(v.indices().next(), Some(2));
    assert_eq!(v.indices().nth(1), None);
    assert_eq!(v.indices().nth(2), None);
    assert_eq!(v.next_index(0), Some(2));
    assert_eq!(v.next_index(1), Some(2));
    assert_eq!(v.next_index(2), None);
}
