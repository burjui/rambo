use std::{
    fmt,
    hash::{
        Hash,
        Hasher,
    },
    ops::Deref,
    ptr,
};

pub(crate) struct StaticRef<T: ?Sized + 'static>(&'static T);

impl<T> From<T> for StaticRef<T> {
    // TODO replace with new()
    fn from(value: T) -> Self {
        Self(Box::leak(Box::new(value)))
    }
}

impl<T: ?Sized> Clone for StaticRef<T> {
    fn clone(&self) -> Self {
        Self(self.0)
    }
}

impl<T: ?Sized> Deref for StaticRef<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        self.0
    }
}

impl<T: ?Sized> PartialEq for StaticRef<T> {
    fn eq(&self, other: &Self) -> bool {
        ptr::eq(self.0, other.0)
    }
}

impl<T: ?Sized> Eq for StaticRef<T> {}

impl<T: ?Sized> Hash for StaticRef<T> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        (self.0 as *const T).hash(state);
    }
}

impl<T: ?Sized + fmt::Debug> fmt::Debug for StaticRef<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Debug::fmt(&**self, f)
    }
}

impl<T: ?Sized + fmt::Display> fmt::Display for StaticRef<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(&**self, f)
    }
}
