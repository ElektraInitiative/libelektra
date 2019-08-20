extern crate elektra_sys;

use crate::{KeySetError, ReadableKey, StrKey, StringKey, WriteableKey};
use bitflags::bitflags;
use std::convert::TryInto;
// use std::ffi::CString;

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
        println!("Drop {:?}", self);
        unsafe {
            elektra_sys::ksDel(self.as_ptr());
        }
    }
}

impl Iterator for KeySet {
    type Item = StringKey;
    fn next(&mut self) -> Option<Self::Item> {
        let key_ptr = unsafe { elektra_sys::ksNext(self.as_ptr()) };
        if (key_ptr as *const elektra_sys::Key).is_null() {
            None
        } else {
            Some(StringKey::from_ptr(key_ptr as *mut elektra_sys::Key))
        }
    }
}

impl Default for KeySet {
    fn default() -> Self {
        Self::new()
    }
}

impl KeySet {
    fn as_ptr(&mut self) -> *mut elektra_sys::KeySet {
        self.ptr.as_ptr()
    }

    fn as_ref(&self) -> &elektra_sys::KeySet {
        unsafe { self.ptr.as_ref() }
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

    /// Append a vector of keys to the KeySet.
    /// It will call append_key on every key. If any key returns an error it will be returned,
    /// but only after iterating over all keys.
    pub fn append_all<T: WriteableKey>(&mut self, keys: Vec<T>) -> Result<(), KeySetError> {
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
        if (key_ptr as *const elektra_sys::Key).is_null() {
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
        if (key_ptr as *const elektra_sys::Key).is_null() {
            None
        } else {
            Some(StringKey::from_ptr(key_ptr))
        }
    }

    /// Return the last key in the KeySet
    /// or None if the KeySet is empty.
    pub fn tail(&self) -> Option<StringKey> {
        let key_ptr = unsafe { elektra_sys::ksTail(self.as_ref()) };
        if (key_ptr as *const elektra_sys::Key).is_null() {
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
        if (key_ptr as *const elektra_sys::Key).is_null() {
            None
        } else {
            Some(StringKey::from_ptr(key_ptr))
        }
    }

    /// Set the KeySet cursor.
    pub fn set_cursor(&mut self, cursor: Cursor) {
        unsafe {
            elektra_sys::ksSetCursor(self.as_ptr(), cursor);
        }
    }

    pub fn lookup(
        &mut self,
        mut key: StringKey,
        options: LookupOption,
    ) -> Option<StrKey<'_>> {
        println!("Got flags {:?}", options.bits() as elektra_sys::option_t);
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

        if (key_ptr as *const elektra_sys::Key).is_null() {
            None
        } else {
            // let dup_ptr = unsafe { elektra_sys::keyDup(key_ptr as *const elektra_sys::Key) };
            Some(StrKey::from_ptr(key_ptr))
        }
    }

    // pub fn lookup_by_name(&mut self, name: &str, options: LookupOption) -> Option<StringKey> {
    //     println!("Got flags {:?}", options.bits() as elektra_sys::option_t);
    //     let cname = CString::new(name).unwrap();
    //     let key_ptr = unsafe {
    //         elektra_sys::ksLookupByName(
    //             self.as_ptr(),
    //             cname.as_ptr(),
    //             options.bits() as elektra_sys::option_t,
    //         )
    //     };
    //     println!("key_ptr is {:?}", key_ptr);

    //     if key_ptr as *const elektra_sys::Key == std::ptr::null() {
    //         println!("is null");
    //         None
    //     } else {
    //         if options.contains(LookupOption::KDB_O_DEL) {
    //             println!("contains del");
    //             None
    //         } else {
    //             let dup_ptr = unsafe { elektra_sys::keyDup(key_ptr as *const elektra_sys::Key) };
    //             Some(StringKey::from_ptr(dup_ptr))
    //         }
    //     }
    // }

    pub fn iter(&mut self) -> StrKeyIter<'_> {
        StrKeyIter {
            cursor: None,
            keyset: self,
        }
    }
}

pub struct StrKeyIter<'a> {
    cursor: Option<Cursor>,
    keyset: &'a mut KeySet,
}

impl<'a> Iterator for StrKeyIter<'a> {
    type Item = StrKey<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        match self.cursor {
            None => {
                let key_ptr = unsafe { elektra_sys::ksNext(self.keyset.as_ptr()) };
                if (key_ptr as *const elektra_sys::Key).is_null() {
                    None
                } else {
                    self.cursor = Some(self.keyset.get_cursor());
                    Some(StrKey::from_ptr(key_ptr))
                }
            }
            Some(cursor) => {
                self.cursor = Some(cursor + 1);
                self.keyset.set_cursor(self.cursor.unwrap());
                let key_ptr = unsafe { elektra_sys::ksCurrent(self.keyset.as_ptr()) };

                if (key_ptr as *const elektra_sys::Key).is_null() {
                    self.cursor = None;
                    None
                } else {
                    Some(StrKey::from_ptr(key_ptr))
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::KeyBuilder;

    #[test]
    fn can_build_simple_keyset() {
        let mut ks = KeySet::new();
        ks.append_key(
            KeyBuilder::<StringKey>::new("user/sw/org/app/bool")
                .value("true")
                .build(),
        )
        .unwrap();
        // Make sure that the returned key from head can be dropped
        // without deleting the actual key
        {
            let head = ks.head().unwrap();
            assert_eq!(head.get_value(), "true");
        }
        {
            let head = ks.head().unwrap();
            assert_eq!(head.get_value(), "true");
        }
    }

    #[test]
    fn can_iterate_simple_keyset_immutably() {
        let names = ["user/test/key1", "user/test/key2", "user/test/key3"];
        let values = ["value1", "value2", "value3"];

        let mut ks = KeySet::with_capacity(3);

        ks.append_all(vec![
            KeyBuilder::<StringKey>::new(names[0])
                .value(values[0])
                .build(),
            KeyBuilder::<StringKey>::new(names[1])
                .value(values[1])
                .build(),
            KeyBuilder::<StringKey>::new(names[2])
                .value(values[2])
                .build(),
        ])
        .unwrap();
        ks.rewind();

        let mut did_iterate = false;
        for (i, key) in ks.iter().enumerate() {
            did_iterate = true;
            assert_eq!(key.get_value(), values[i]);
            assert_eq!(key.get_name(), names[i]);
        }
        assert!(did_iterate);

        // Make sure that calling a method that takes &mut self
        // does not produce a compile error here
        ks.set_cursor(0);
    }

    #[test]
    fn can_iterate_simple_keyset() {
        let names = ["user/test/key1", "user/test/key2", "user/test/key3"];
        let values = ["value1", "value2", "value3"];

        let mut ks = KeySet::with_capacity(3);

        ks.append_all(vec![
            KeyBuilder::<StringKey>::new(names[0])
                .value(values[0])
                .build(),
            KeyBuilder::<StringKey>::new(names[1])
                .value(values[1])
                .build(),
            KeyBuilder::<StringKey>::new(names[2])
                .value(values[2])
                .build(),
        ])
        .unwrap();
        ks.rewind();

        let mut did_iterate = false;
        for (i, key) in ks.enumerate() {
            did_iterate = true;
            assert_eq!(key.get_value(), values[i]);
            assert_eq!(key.get_name(), names[i]);
        }
        assert!(did_iterate);
    }

    fn setup_keyset() -> KeySet {
        let names = ["system/test/key", "user/test/key"];
        let values = ["value1", "value3"];

        let mut ks = KeySet::with_capacity(3);

        ks.append_all(vec![
            KeyBuilder::<StringKey>::new(names[0])
                .value(values[0])
                .build(),
            KeyBuilder::<StringKey>::new(names[1])
                .value(values[1])
                .build(),
            // KeyBuilder::<StringKey>::new(names[2])
            //     .value(values[2])
            //     .build(),
        ])
        .unwrap();
        ks
    }

    #[test]
    fn can_lookup_key_with_none_option() {
        let mut ks = setup_keyset();
        let lookup_key = StringKey::new("/test/key").unwrap();
        println!("Lookup_key is {:?}", lookup_key);
        let ret_val = ks.lookup(lookup_key, LookupOption::KDB_O_NONE);
        assert_eq!(ret_val.unwrap().get_name(), "user/test/key");
        assert_eq!(ks.get_size(), 2);
        assert_eq!(ks.tail().unwrap().get_name(), "user/test/key");
    }

    #[test]
    fn can_lookup_key_with_del_option() {
        let mut ks = setup_keyset();
        let lookup_key = StringKey::new("/test/key").unwrap();
        let key = ks.lookup(lookup_key, LookupOption::KDB_O_DEL);
        assert_eq!(key.unwrap().get_name(), "user/test/key");
        assert_eq!(ks.get_size(), 2);
        assert_eq!(ks.head().unwrap().get_name(), "system/test/key");
    }

    #[test]
    fn can_lookup_and_duplicate_key() {
        // Make sure that a duplicate of a key that is from a keyset
        // can be used after the KeySet has been freed
        let key;
        {
            let lookup_key = StringKey::new("/test/key").unwrap();
            let mut ks = setup_keyset();
            key = ks
                .lookup(lookup_key, LookupOption::KDB_O_DEL)
                .unwrap()
                .to_owned();
            assert_eq!(ks.get_size(), 2);
            assert_eq!(ks.head().unwrap().get_name(), "system/test/key");
        }
        assert_eq!(key.get_name(), "user/test/key");
    }

}
