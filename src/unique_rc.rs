use std::fmt;
use std::hash::Hash;
use std::hash::Hasher;
use std::ops::Deref;
use std::rc::Rc;

crate struct UniqueRc<T: ?Sized>(Rc<T>);

impl<T> From<T> for UniqueRc<T> {
    fn from(value: T) -> Self {
        UniqueRc(Rc::new(value))
    }
}

impl<T: ?Sized> Clone for UniqueRc<T> {
    fn clone(&self) -> Self {
        UniqueRc(self.0.clone())
    }
}

impl<T: ?Sized> Deref for UniqueRc<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<T: ?Sized> PartialEq for UniqueRc<T> {
    fn eq(&self, other: &UniqueRc<T>) -> bool {
        Rc::ptr_eq(&self.0, &other.0)
    }
}

impl<T: ?Sized> Eq for UniqueRc<T> {}

impl<T: ?Sized> Hash for UniqueRc<T> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        Rc::into_raw(self.0.clone()).hash(state)
    }
}

impl<T: ?Sized + fmt::Debug> fmt::Debug for UniqueRc<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Debug::fmt(&**self, f)
    }
}

impl<T: ?Sized + fmt::Display> fmt::Display for UniqueRc<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(&**self, f)
    }
}
