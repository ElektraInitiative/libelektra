extern crate elektra_sys;
use crate::{KeyBuilder, KeySetError, StringKey, WriteableKey};
use std::convert::TryInto;

#[derive(Debug)]
pub struct KeySet {
    ptr: std::ptr::NonNull<elektra_sys::KeySet>,
}

impl Drop for KeySet {
    fn drop(&mut self) {
        println!("Drop {:?}", self);
        unsafe {
            elektra_sys::ksDel(self.as_ptr());
        }
    }
}

//   impl Iterator for KeySet {
//             type Item = ReadOnly<StringKey>;
//             fn next(&mut self) -> Option<Self::Item> {
//                 let key_ptr = unsafe { elektra_sys::keyNextMeta(self.as_ptr()) };
//                 if key_ptr == std::ptr::null() {
//                     None
//                 } else {
//                     Some(ReadOnly::from_ptr(key_ptr as *mut elektra_sys::Key))
//                 }
//             }
//         }

impl KeySet {
    fn as_ptr(&mut self) -> *mut elektra_sys::KeySet {
        self.ptr.as_ptr()
    }

    fn as_ref(&self) -> &elektra_sys::KeySet {
        unsafe { self.ptr.as_ref() }
    }

    /// Create a new empty KeySet
    pub fn new() -> KeySet {
        let ks_ptr = unsafe { elektra_sys::ksNew(0, elektra_sys::KEY_END) };
        KeySet::from_ptr(ks_ptr)
    }

    /// Create a new KeySet that allocates enough space for the
    /// given capacity of keys
    pub fn with_capacity(capacity: usize) -> Self {
        let ks_ptr = unsafe { elektra_sys::ksNew(capacity, elektra_sys::KEY_END) };
        Self::from_ptr(ks_ptr)
    }

    fn from_ptr(keyset_ptr: *mut elektra_sys::KeySet) -> KeySet {
        KeySet {
            ptr: std::ptr::NonNull::new(keyset_ptr).unwrap(),
        }
    }

    /// Append a key to the keyset
    /// May return an InsertionFailure error, if the key was already added
    pub fn append_key<T: WriteableKey>(&mut self, mut key: T) -> Result<(), KeySetError> {
        let ret_val = unsafe { elektra_sys::ksAppendKey(self.as_ptr(), key.as_ptr()) };
        if ret_val == -1 {
            Err(KeySetError::InsertionFailure)
        } else {
            Ok(())
        }
    }

    /// Return a duplicate of a keyset.
    pub fn duplicate(&self) -> Self {
        let ks_ptr = unsafe { elektra_sys::ksDup(self.as_ref()) };
        Self::from_ptr(ks_ptr)
    }

    /// Replace the content of a keyset with another one.
    /// Copies the contents of source into self
    pub fn copy(&mut self, source: &Self) {
        unsafe { elektra_sys::ksCopy(self.as_ptr(), source.as_ref()) };
    }

    /// Return the number of keys that ks contains.
    pub fn get_size(&self) -> usize {
        unsafe { elektra_sys::ksGetSize(self.as_ref()).try_into().unwrap() }
    }

    /// Rewinds the KeySet internal cursor. 
    pub fn rewind(&mut self) {
        unsafe {
            elektra_sys::ksRewind(self.as_ptr());
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn can_build_simple_keyset() {
        let mut ks = KeySet::new();
        ks.append_key(
            KeyBuilder::<StringKey>::new("user/sw/org/app/bool")
                .value("true")
                .build(),
        )
        .unwrap();
    }
}
