use crate::ReadableKey;

/// A wrapper that provides only readable methods on a key.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct ReadOnly<T: ReadableKey> {
    key: T,
}

impl<T: ReadableKey> ReadOnly<T> {
    /// Returns the inner key
    pub fn key(&self) -> &T {
        &self.key
    }
}

impl<T: ReadableKey> ReadableKey for ReadOnly<T> {
    type GetValue = T::GetValue;

    unsafe fn from_ptr(ptr: *mut elektra_sys::Key) -> Self {
        ReadOnly {
            key: T::from_ptr(ptr),
        }
    }

    fn value(&self) -> Self::GetValue {
        self.key.value()
    }
}

impl<T: ReadableKey> AsRef<elektra_sys::Key> for ReadOnly<T> {
    fn as_ref(&self) -> &elektra_sys::Key {
        self.key.as_ref()
    }
}
