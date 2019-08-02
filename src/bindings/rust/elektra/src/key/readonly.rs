use crate::{ReadableKey, WriteableKey};

#[derive(Debug)]
pub struct ReadOnly<T: WriteableKey> {
    key: T,
}

impl<T: WriteableKey> ReadableKey for ReadOnly<T> {
    fn as_ref(&self) -> &elektra_sys::Key {
        self.key.as_ref()
    }

    fn from_ptr(ptr: *mut elektra_sys::Key) -> Self {
        ReadOnly {
            key: T::from_ptr(ptr),
        }
    }

    type Value = T::Value;
    fn get_value(&self) -> Self::Value {
        self.key.get_value()
    }
}