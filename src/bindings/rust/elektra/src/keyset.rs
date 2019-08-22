extern crate elektra_sys;

use crate::{
    KeyError, ReadOnlyStringKeyIter, ReadableKey, StringKey, StringKeyIter,
    WriteableKey,
};
use bitflags::bitflags;
use std::convert::TryInto;

#[derive(Debug)]
pub struct KeySet {
    ptr: std::ptr::NonNull<elektra_sys::KeySet>,
    phantom: std::marker::PhantomData<elektra_sys::KeySet>,
}

pub type Cursor = elektra_sys::cursor_t;

bitflags! {
    // Results in default value with bits: 0
    #[derive(Default)]
    pub struct LookupOption: elektra_sys::option_t {
        const KDB_O_NONE = elektra_sys::KDB_O_NONE as elektra_sys::option_t;
        const KDB_O_DEL = elektra_sys::KDB_O_DEL as elektra_sys::option_t;
        const KDB_O_POP = elektra_sys::KDB_O_POP as elektra_sys::option_t;
        const KDB_O_NODIR = elektra_sys::KDB_O_NODIR as elektra_sys::option_t;
        const KDB_O_DIRONLY = elektra_sys::KDB_O_DIRONLY as elektra_sys::option_t;
        const KDB_O_NOREMOVE = elektra_sys::KDB_O_NOREMOVE as elektra_sys::option_t;
        const KDB_O_REMOVEONLY = elektra_sys::KDB_O_REMOVEONLY as elektra_sys::option_t;
        const KDB_O_INACTIVE = elektra_sys::KDB_O_INACTIVE as elektra_sys::option_t;
        const KDB_O_SYNC = elektra_sys::KDB_O_SYNC as elektra_sys::option_t;
        const KDB_O_SORT = elektra_sys::KDB_O_SORT as elektra_sys::option_t;
        const KDB_O_NORECURSIVE = elektra_sys::KDB_O_NORECURSIVE as elektra_sys::option_t;
        const KDB_O_NOCASE = elektra_sys::KDB_O_NOCASE as elektra_sys::option_t;
        const KDB_O_WITHOWNER = elektra_sys::KDB_O_WITHOWNER as elektra_sys::option_t;
        const KDB_O_NOALL = elektra_sys::KDB_O_NOALL as elektra_sys::option_t;
    }
}

impl Drop for KeySet {
    fn drop(&mut self) {
        unsafe {
            elektra_sys::ksDel(self.as_ptr());
        }
    }
}

impl Default for KeySet {
    fn default() -> Self {
        Self::new()
    }
}

impl AsRef<elektra_sys::KeySet> for KeySet {
    fn as_ref(&self) -> &elektra_sys::KeySet {
        unsafe { self.ptr.as_ref() }
    }
}

impl KeySet {
    pub fn as_ptr(&mut self) -> *mut elektra_sys::KeySet {
        self.ptr.as_ptr()
    }

    /// Create a new empty KeySet
    pub fn new() -> Self {
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
            phantom: std::marker::PhantomData,
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

    /// Append a collection of keys to the KeySet.
    /// Calls append_key on every key. If any key returns an error it will be returned,
    /// but only after iterating over all keys.
    pub fn append_all<T>(&mut self, keys: T) -> Result<(), KeySetError>
    where
        T: IntoIterator,
        T::Item: WriteableKey,
    {
        let mut err = None;
        for key in keys {
            if let Err(ks_err) = self.append_key(key) {
                err = Some(ks_err);
            }
        }
        if let Some(error) = err {
            Err(error)
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

    /// Return the first key in the KeySet
    /// or None if the KeySet is empty.
    pub fn head(&self) -> Option<StringKey> {
        let key_ptr = unsafe { elektra_sys::ksHead(self.as_ref()) };
        if key_ptr.is_null() {
            None
        } else {
            Some(StringKey::from_ptr(key_ptr))
        }
    }

    /// Return the key pointed at by the internal cursor
    /// or None if the end is reached or after [`rewind`].
    ///
    /// [`rewind`]: #method.rewind
    pub fn current(&self) -> Option<StringKey> {
        let key_ptr = unsafe { elektra_sys::ksCurrent(self.as_ref()) };
        if key_ptr.is_null() {
            None
        } else {
            Some(StringKey::from_ptr(key_ptr))
        }
    }

    /// Return the last key in the KeySet
    /// or None if the KeySet is empty.
    pub fn tail(&self) -> Option<StringKey> {
        let key_ptr = unsafe { elektra_sys::ksTail(self.as_ref()) };
        if key_ptr.is_null() {
            None
        } else {
            Some(StringKey::from_ptr(key_ptr))
        }
    }

    /// Get the KeySet internal cursor.
    pub fn get_cursor(&self) -> Cursor {
        unsafe { elektra_sys::ksGetCursor(self.as_ref()) }
    }

    /// Return key at given cursor position or None if the cursor
    /// has a negative position or a position that does not lie within the keyset.
    pub fn at_cursor(&mut self, cursor: Cursor) -> Option<StringKey> {
        let key_ptr = unsafe { elektra_sys::ksAtCursor(self.as_ptr(), cursor) };
        if key_ptr.is_null() {
            None
        } else {
            Some(StringKey::from_ptr(key_ptr))
        }
    }

    /// Set the KeySet internal cursor.
    pub fn set_cursor(&mut self, cursor: Cursor) {
        unsafe {
            elektra_sys::ksSetCursor(self.as_ptr(), cursor);
        }
    }

    /// Lookup a given key in the keyset.
    /// See also [`lookup_by_name`].
    ///
    /// [`lookup_by_name`]: #method.lookup_by_name
    pub fn lookup(&mut self, mut key: StringKey, options: LookupOption) -> Option<StringKey<'_>> {
        let key_ptr = unsafe {
            elektra_sys::ksLookup(
                self.as_ptr(),
                key.as_ptr(),
                options.bits() as elektra_sys::option_t,
            )
        };
        if options.contains(LookupOption::KDB_O_DEL) {
            std::mem::forget(key);
        }

        if key_ptr.is_null() {
            None
        } else {
            // let dup_ptr = unsafe { elektra_sys::keyDup(key_ptr as *const elektra_sys::Key) };
            Some(StringKey::from_ptr(key_ptr))
        }
    }

    /// Lookup a key by name.
    /// Returns a KeyError::InvalidName if the provided string is an invalid name.
    /// Otherwise identical to [`lookup`].
    ///
    /// [`lookup`]: #method.lookup
    pub fn lookup_by_name(
        &mut self,
        name: &str,
        options: LookupOption,
    ) -> Result<Option<StringKey>, KeyError> {
        let key = StringKey::new(name)?;
        Ok(self.lookup(key, options))
    }

    /// Returns an iterator that should be used to immutably iterate over a keyset.
    /// It has to take &mut self because the internal cursor of the keyset is modified.
    pub fn iter(&mut self) -> ReadOnlyStringKeyIter<'_> {
        ReadOnlyStringKeyIter {
            cursor: None,
            keyset: self,
        }
    }
    /// Returns an iterator that returns StringKey
    /// that can be modified.
    /// Note that you should not change the name of the key.
    pub fn iter_mut(&mut self) -> StringKeyIter<'_> {
        StringKeyIter {
            cursor: None,
            keyset: self,
        }
    }
}

use std::error::Error;
use std::fmt;

#[derive(Debug, PartialEq)]
pub enum KeySetError {
    InsertionFailure,
}

impl fmt::Display for KeySetError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        match self {
            KeySetError::InsertionFailure => write!(f, "Key could not be inserted."),
            // _ => unimplemented!(),
        }
    }
}

impl Error for KeySetError {}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::KeyBuilder;

    #[test]
    fn can_build_simple_keyset() -> Result<(), KeyError> {
        let mut ks = KeySet::new();
        ks.append_key(
            KeyBuilder::<StringKey>::new("user/sw/org/app/bool")?
                .value("true")
                .build(),
        )
        .unwrap();
        // Make sure that the returned key from head can be dropped
        // without deleting the actual key
        {
            let head = ks.head().unwrap();
            assert_eq!(head.value(), "true");
        }
        {
            let head = ks.head().unwrap();
            assert_eq!(head.value(), "true");
        }
        Ok(())
    }

    #[test]
    fn can_iterate_simple_keyset() -> Result<(), KeyError> {
        let names = ["user/test/key1", "user/test/key2", "user/test/key3"];
        let values = ["value1", "value2", "value3"];

        let mut ks = KeySet::with_capacity(3);

        ks.append_all(vec![
            KeyBuilder::<StringKey>::new(names[0])?
                .value(values[0])
                .build(),
            KeyBuilder::<StringKey>::new(names[1])?
                .value(values[1])
                .build(),
            KeyBuilder::<StringKey>::new(names[2])?
                .value(values[2])
                .build(),
        ])
        .unwrap();

        ks.rewind();
        let new_values = ["Newvalue1", "Newvalue2", "Newvalue3"];
        let mut did_iterate = false;
        for (i, mut key) in ks.iter_mut().enumerate() {
            did_iterate = true;
            assert_eq!(key.value(), values[i]);
            key.set_value(new_values[i]);
        }
        assert!(did_iterate);

        ks.rewind();
        did_iterate = false;
        for (i, key) in ks.iter().enumerate() {
            did_iterate = true;
            assert_eq!(key.value(), new_values[i]);
            assert_eq!(key.name(), names[i]);
        }
        assert!(did_iterate);
        // Check that the iterator did not consume the keyset
        assert_eq!(ks.get_size(), 3);
        Ok(())
    }

    fn setup_keyset() -> KeySet {
        let names = ["system/test/key", "user/test/key"];
        let values = ["value1", "value3"];

        let mut ks = KeySet::with_capacity(3);

        ks.append_all(vec![
            KeyBuilder::<StringKey>::new(names[0])
                .unwrap()
                .value(values[0])
                .build(),
            KeyBuilder::<StringKey>::new(names[1])
                .unwrap()
                .value(values[1])
                .build(),
        ])
        .unwrap();
        ks
    }

    #[test]
    fn can_lookup_key_with_none_option() {
        let mut ks = setup_keyset();
        let lookup_key = StringKey::new("/test/key").unwrap();
        let ret_val = ks.lookup(lookup_key, LookupOption::KDB_O_NONE);
        assert_eq!(ret_val.unwrap().name(), "user/test/key");
        assert_eq!(ks.get_size(), 2);
        assert_eq!(ks.tail().unwrap().name(), "user/test/key");
    }

    #[test]
    fn can_lookup_key_with_del_option() {
        let mut ks = setup_keyset();
        let lookup_key = StringKey::new("/test/key").unwrap();
        let key = ks.lookup(lookup_key, LookupOption::KDB_O_DEL);
        assert_eq!(key.unwrap().name(), "user/test/key");
        assert_eq!(ks.get_size(), 2);
        assert_eq!(ks.head().unwrap().name(), "system/test/key");
    }

    #[test]
    fn can_lookup_by_name_and_duplicate_key() -> Result<(), KeyError> {
        // Make sure that a duplicate of a key that is from a keyset
        // can be used after the KeySet has been freed
        let key;
        {
            // let lookup_key = StringKey::new("/test/key").unwrap();
            let mut ks = setup_keyset();
            key = ks
                .lookup_by_name("/test/key", LookupOption::KDB_O_DEL)?
                .unwrap()
                .duplicate();
            assert_eq!(ks.get_size(), 2);
            assert_eq!(ks.head().unwrap().name(), "system/test/key");
        }
        assert_eq!(key.name(), "user/test/key");
        Ok(())
    }

}
