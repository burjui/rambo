use std::{
    fmt::Display,
    ops::{Index, IndexMut},
};

#[derive(Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord, Debug)]
pub(crate) struct SlabIndex(usize);

impl Display for SlabIndex {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Display::fmt(&self.0, f)
    }
}

impl Default for SlabIndex {
    fn default() -> Self {
        Self(usize::MAX)
    }
}

#[derive(Clone)]
enum SlabEntry<T> {
    Occupied(T),
    Empty(Option<usize>),
}

#[derive(Clone)]
pub(crate) struct Slab<T> {
    entries: Vec<SlabEntry<T>>,
    vacant_index: Option<usize>,
}

impl<T> Slab<T> {
    pub(crate) fn new() -> Self {
        Self {
            entries: Vec::new(),
            vacant_index: None,
        }
    }

    pub(crate) fn insert(&mut self, item: T) -> SlabIndex {
        match self.vacant_index {
            Some(index) => {
                let entry = &mut self.entries[index];
                match entry {
                    SlabEntry::Empty(optional_next) => self.vacant_index = *optional_next,
                    SlabEntry::Occupied(_) => unreachable!(),
                }
                *entry = SlabEntry::Occupied(item);
                SlabIndex(index)
            }

            None => {
                let index = self.entries.len();
                self.entries.push(SlabEntry::Occupied(item));
                SlabIndex(index)
            }
        }
    }

    pub(crate) fn remove(&mut self, index: SlabIndex) {
        match self.entries[index.0] {
            SlabEntry::Occupied(_) => {
                self.entries[index.0] = SlabEntry::Empty(self.vacant_index);
                self.vacant_index = Some(index.0);
            }

            SlabEntry::Empty(_) => panic!("entry at index {} is already empty", index.0),
        }
    }

    pub(crate) fn iter(&self) -> impl Iterator<Item = (SlabIndex, &T)> {
        self.entries
            .iter()
            .enumerate()
            .filter_map(|(index, entry)| match entry {
                SlabEntry::Occupied(item) => Some((SlabIndex(index), item)),
                SlabEntry::Empty(_) => None,
            })
    }
}

impl<T> Index<SlabIndex> for Slab<T> {
    type Output = T;

    fn index(&self, index: SlabIndex) -> &Self::Output {
        match &self.entries[index.0] {
            SlabEntry::Occupied(item) => item,
            SlabEntry::Empty(_) => unreachable!(),
        }
    }
}

impl<T> IndexMut<SlabIndex> for Slab<T> {
    fn index_mut(&mut self, index: SlabIndex) -> &mut Self::Output {
        match &mut self.entries[index.0] {
            SlabEntry::Occupied(item) => item,
            SlabEntry::Empty(_) => unreachable!(),
        }
    }
}
