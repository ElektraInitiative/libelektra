use crate::ReadableKey;

#[derive(Debug)]
pub struct ReadOnly<T: ReadableKey> {
    key: T,
}

impl<T: ReadableKey> ReadableKey for ReadOnly<T> {
    type Value = T::Value;
    type Duplicate = T::Duplicate;

    fn as_ref(&self) -> &elektra_sys::Key {
        self.key.as_ref()
    }

    fn from_ptr(ptr: *mut elektra_sys::Key) -> Self {
        ReadOnly {
            key: T::from_ptr(ptr),
        }
    }

    fn get_value(&self) -> Self::Value {
        self.key.get_value()
    }

    // TODO: This should return ReadOnly<T: ReadableKey> or similar
    fn duplicate(&self) -> Self::Duplicate
    where
        Self::Duplicate: Sized,
    {
        self.key.duplicate()
        // ReadOnly {
        // dup
        // }
    }
}
