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

impl<T: WriteableKey> Drop for ReadOnly<T> {
    fn drop(&mut self) {
        // The wrapped key holds a pointer to a key that we should not modify (const Key*)
        // But since we created a full T object with that pointer,
        // it's drop impl would call keyDel on that pointer
        // So we replace the memory with a new empty key, that can be safely deleted
        let old_val = std::mem::replace(&mut self.key, T::new_empty());
        // Then forget about the const Key*, s.t. it isnt dropped
        std::mem::forget(old_val);
    }
}