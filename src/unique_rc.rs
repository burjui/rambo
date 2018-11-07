use std::rc::Rc;
use std::ops::Deref;
use std::hash::Hash;
use std::hash::Hasher;

crate struct UniqueRc<T>(Rc<T>);

impl<T> From<T> for UniqueRc<T> {
    fn from(value: T) -> Self {
        Self(Rc::new(value))
    }
}

impl<T> Clone for UniqueRc<T> {
    fn clone(&self) -> Self {
        Self(self.0.clone())
    }
}

impl<T> Deref for UniqueRc<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<T> PartialEq<UniqueRc<T>> for UniqueRc<T> {
    fn eq(&self, other: &UniqueRc<T>) -> bool {
        Rc::ptr_eq(&self.0, &other.0)
    }
}

impl<T> Eq for UniqueRc<T> {}

impl<T> Hash for UniqueRc<T> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        Rc::into_raw(self.0.clone()).hash(state)
    }
}
