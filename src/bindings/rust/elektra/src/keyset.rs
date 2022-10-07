//! A `KeySet` is a set of `StringKey`s.
//!
//! You can think of it as a specialized version of `Vec`.
//!
//! While you can also store Keys in a `Vec`, you will need to
//! use the `KeySet` to interact with the key database.
//!
//! # Example
//! You can use the [`keyset!`](../macro.keyset.html) macro to create a KeySet. It works just like `vec!`.
//! ```
//! # use elektra::{KeyBuilder, keyset, KeySet, StringKey, WriteableKey, ReadableKey};
//! # fn main() -> Result<(), Box<dyn std::error::Error>> {
//! let keyset = keyset![
//!     KeyBuilder::<StringKey>::new("user:/sw/app/#1/host")?
//!         .value("localhost")
//!         .build(),
//!     KeyBuilder::<StringKey>::new("user:/sw/app/#1/port")?
//!         .value("8080")
//!         .build(),
//! ];
//! # Ok(())
//! # }
//! ```
//!
//! ## BinaryKeys
//!
//! A KeySet only holds `StringKey`s because they are by far the most
//! common type of key when interacting with the key database. In a typical
//! setup where you read configuration values from the kdb, you will not
//! encounter any BinaryKeys, especially if you set it up yourself.
//! However since the underlying KeySet holds generic `Key`s, `BinaryKey`s
//! can occur. You can cast between the two keys, using `from`. This is safe
//! memory-wise, but can be unsafe if you cast a `BinaryKey` holding arbitrary
//! bytes to a `StringKey`. You can use `is_string` or `is_binary` to find out
//! if the cast is safe.
//!
//! ## Example
//! ```
//! # use elektra::{BinaryKey, StringKey, WriteableKey, ReadableKey};
//! # fn main() -> Result<(), Box<dyn std::error::Error>> {
//! # let mut s_key = StringKey::new("user:/test/language")?;
//! # s_key.set_value("rust".into());
//! # let key = BinaryKey::from(s_key);
//! // Assume we have an arbitrary key that is actually a StringKey
//! if key.is_string() {
//!     let str_key = StringKey::from(key);
//!     assert_eq!(str_key.value(), "rust")
//! } else {
//!     panic!("Was not a StringKey!");
//! }
//! # Ok(())
//! # }
//! ```

use crate::{ReadOnly, ReadableKey, StringKey, WriteableKey};
use bitflags::bitflags;
use std::convert::TryInto;

/// A set of StringKeys.
#[derive(Debug)]
pub struct KeySet {
    ptr: std::ptr::NonNull<elektra_sys::KeySet>,
    _marker: std::marker::PhantomData<elektra_sys::KeySet>,
}

/// The internal cursor for the KeySet.
pub type Cursor = elektra_sys::elektraCursor;

bitflags! {
    /// Bitflags to be passed to [`lookup`](struct.KeySet.html#method.lookup) and [`lookup_by_name`](struct.KeySet.html#method.lookup_by_name).
    #[derive(Default)]
    pub struct LookupOption: elektra_sys::elektraLookupFlags {
        /// No Option set.
        const KDB_O_NONE = elektra_sys::KDB_O_NONE as elektra_sys::elektraLookupFlags;
        /// The found key will be popped from the keyset.
        const KDB_O_POP = elektra_sys::KDB_O_POP as elektra_sys::elektraLookupFlags;
    }
}

#[macro_export]
macro_rules! replace_expr {
    ($_t:expr, $sub:expr) => {
        $sub
    };
}

#[macro_export]
macro_rules! count_exprs {
    ($($key:expr),*) => {<[()]>::len(&[$($crate::replace_expr!($key, ())),*])};
}

/// Creates a KeySet with the given keys.
///
/// Enough memory is allocated before appending all
/// keys, such that it also performs well with a
/// large number of keys.
///
/// # Examples
/// ```
/// # use elektra::{KeySet, StringKey, WriteableKey, KeyBuilder, keyset};
/// # fn main() -> Result<(), Box<dyn std::error::Error>> {
/// let ks = keyset![
///     StringKey::new("user:/test/key1")?,
///     KeyBuilder::<StringKey>::new("user:/test/key2")?
///                .meta("metakey1", "metavalue1")?
///                .meta("metakey2", "metavalue2")?
///                .build(),
///     StringKey::new("user:/test/key3")?,
/// ];
/// assert_eq!(ks.size(), 3);
/// # Ok(())
/// # }
/// ```
#[macro_export]
macro_rules! keyset {
    // For keyset![];
    () => { KeySet::new() };
    ($($key:expr),*) => {{
        let capacity = $crate::count_exprs!( $($key),*);
        let mut keyset = KeySet::with_capacity(capacity);
        $(
            keyset.append_key($key);
        )*
        keyset
    }};
    // To also allow a trailing comma in the key list
    ($($key:expr,)*) => {{
        keyset!($($key),*)
    }};
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
    /// Returns the raw pointer of the KeySet.
    /// Should be used with caution. In particular,
    /// the pointer should only be modified with
    /// `elektra_sys::ks*` functions, but `ksDel`
    /// should not be called.
    pub fn as_ptr(&mut self) -> *mut elektra_sys::KeySet {
        self.ptr.as_ptr()
    }

    /// Create a new empty KeySet.
    ///
    /// # Panics
    /// Panics if an allocation error (out of memory) occurs in the C-constructor.
    pub fn new() -> Self {
        let ks_ptr = unsafe { elektra_sys::ksNew(0, elektra_sys::KS_END) };
        unsafe { KeySet::from_ptr(ks_ptr) }
    }

    /// Create a new KeySet that allocates enough space for the
    /// given capacity of keys.
    ///
    /// # Panics
    /// Panics if an allocation error (out of memory) occurs in the C-constructor.
    pub fn with_capacity(capacity: usize) -> Self {
        let ks_ptr = unsafe { elektra_sys::ksNew(capacity, elektra_sys::KS_END) };
        unsafe { Self::from_ptr(ks_ptr) }
    }

    /// Construct a new KeySet from a raw KeySet pointer
    ///
    /// # Panics
    /// Panics if the provided pointer is null.
    unsafe fn from_ptr(keyset_ptr: *mut elektra_sys::KeySet) -> KeySet {
        KeySet {
            ptr: std::ptr::NonNull::new(keyset_ptr).unwrap(),
            _marker: std::marker::PhantomData,
        }
    }

    /// Append a KeySet to self.
    pub fn append(&mut self, to_append: &KeySet) -> isize {
        unsafe { elektra_sys::ksAppend(self.as_ptr(), to_append.as_ref()) }
    }

    /// Append a key to the keyset.
    ///
    /// # Panics
    /// Panics on out-of-memory errors.
    pub fn append_key<T: WriteableKey>(&mut self, mut key: T) {
        let ret_val = unsafe { elektra_sys::ksAppendKey(self.as_ptr(), key.as_ptr()) };
        if ret_val == -1 {
            panic!("ksAppendKey: Out of memory");
        }
    }

    /// Return a duplicate of a keyset.
    pub fn duplicate(&self) -> Self {
        let ks_ptr = unsafe { elektra_sys::ksDup(self.as_ref()) };
        unsafe { Self::from_ptr(ks_ptr) }
    }

    /// Replace the contents of a keyset with those of `source`.
    /// Copies the contents of `source` into self.
    pub fn copy(&mut self, source: &Self) {
        unsafe { elektra_sys::ksCopy(self.as_ptr(), source.as_ref()) };
    }

    /// Return the number of keys that the keyset contains.
    pub fn size(&self) -> usize {
        unsafe { elektra_sys::ksGetSize(self.as_ref()).try_into().unwrap() }
    }

    /// Rewinds the KeySet's internal cursor.
    pub fn rewind(&mut self) {
        unsafe {
            elektra_sys::ksRewind(self.as_ptr());
        }
    }

    /// Removes and returns the last key of the KeySet.
    /// Returns None if the KeySet is empty.
    ///
    /// # Examples
    /// ```
    /// # use elektra::{KeySet, StringKey, WriteableKey, ReadableKey};
    /// # fn main() -> Result<(), Box<dyn std::error::Error>> {
    /// let key = StringKey::new("user:/key/elektra")?;
    /// let mut keyset = KeySet::with_capacity(1);
    /// keyset.append_key(key);
    /// if let Some(popped_key) = keyset.pop() {
    ///     assert_eq!(popped_key.basename(), "elektra");
    /// } else {
    ///     panic!("Could not pop a key!");
    /// }
    /// #
    /// #     Ok(())
    /// # }
    /// ```
    pub fn pop<'a, 'b>(&'a mut self) -> Option<StringKey<'b>> {
        let key_ptr = unsafe { elektra_sys::ksPop(self.as_ptr()) };
        if key_ptr.is_null() {
            None
        } else {
            Some(unsafe { StringKey::from_ptr(key_ptr) })
        }
    }

    /// Cuts the keys that are below `cut_point` from self.
    /// If the provided key has no name or the `cut_point` is not found,
    /// an empty KeySet is returned.
    ///
    /// # Examples
    /// ```
    /// # use elektra::{KeySet, StringKey, WriteableKey, ReadableKey, keyset};
    /// # use std::iter::FromIterator;
    /// # fn main() -> Result<(), Box<dyn std::error::Error>> {
    /// let mut keyset = keyset![
    ///     StringKey::new("user:/key/parent")?,
    ///     StringKey::new("user:/key/parent/below")?,
    ///     StringKey::new("user:/key/other")?,
    /// ];
    /// let cut_key = StringKey::new("user:/key/parent")?;
    /// let cut_keyset = keyset.cut(&cut_key);
    /// assert_eq!(keyset.size(), 1);
    /// assert_eq!(cut_keyset.size(), 2);
    /// assert_eq!(cut_keyset.head().unwrap().name(), "user:/key/parent");
    /// assert_eq!(cut_keyset.tail().unwrap().name(), "user:/key/parent/below");
    /// #
    /// #     Ok(())
    /// # }
    /// ```
    pub fn cut(&mut self, cut_point: &StringKey) -> KeySet {
        let ks_ptr = unsafe { elektra_sys::ksCut(self.as_ptr(), cut_point.as_ref()) };
        if ks_ptr.is_null() {
            KeySet::new()
        } else {
            unsafe { KeySet::from_ptr(ks_ptr) }
        }
    }

    /// Return the first key in the KeySet
    /// or None if the KeySet is empty.
    pub fn head(&self) -> Option<StringKey> {
        let key_ptr = unsafe { elektra_sys::ksAtCursor(self.as_ref(), 0) };
        if key_ptr.is_null() {
            None
        } else {
            Some(unsafe { StringKey::from_ptr(key_ptr) })
        }
    }

    /// Return the key pointed at by the internal cursor
    /// or None if the end is reached or after [`rewind`](#method.rewind).
    pub fn current(&self) -> Option<StringKey> {
        let key_ptr = unsafe { elektra_sys::ksCurrent(self.as_ref()) };
        if key_ptr.is_null() {
            None
        } else {
            Some(unsafe { StringKey::from_ptr(key_ptr) })
        }
    }

    /// Return the last key in the KeySet
    /// or None if the KeySet is empty.
    pub fn tail(&self) -> Option<StringKey> {
        let key_ptr = unsafe { elektra_sys::ksAtCursor(self.as_ref(), (self.size() - 1).try_into().unwrap()) };
        if key_ptr.is_null() {
            None
        } else {
            Some(unsafe { StringKey::from_ptr(key_ptr) })
        }
    }

    /// Return key at given cursor position or None if the cursor
    /// has a negative position or a position that does not lie within the keyset.
    pub fn at_cursor(&mut self, cursor: Cursor) -> Option<StringKey> {
        let key_ptr = unsafe { elektra_sys::ksAtCursor(self.as_ptr(), cursor) };
        if key_ptr.is_null() {
            None
        } else {
            Some(unsafe { StringKey::from_ptr(key_ptr) })
        }
    }

    /// Lookup a given key in the keyset.
    /// See also [`lookup_by_name`](#method.lookup_by_name).
    ///
    /// # Notes
    /// Note that the returned key is only alive for as long as the keyset is. This is still true
    /// when you pass `LookupOption::KDB_O_POP`, although not expected. To get around this limitation,
    /// you can duplicate the returned key, such that it is no longer bound by the lifetime of the KeySet.
    ///
    /// # Examples
    /// ```
    /// # use elektra::{KeySet, StringKey, LookupOption, WriteableKey, ReadableKey};
    /// # use std::iter::FromIterator;
    /// # fn main() -> Result<(), Box<dyn std::error::Error>> {
    /// let key = StringKey::new("user:/key/elektra")?;
    /// let mut ks = KeySet::with_capacity(1);
    /// ks.append_key(key);
    ///
    /// let lookup_key = StringKey::new("user:/key/elektra")?;
    /// if let Some(mut key) = ks.lookup(lookup_key, LookupOption::KDB_O_NONE) {
    ///     key.set_value("newvalue");
    /// } else {
    ///     panic!("Key was not found!");
    /// }
    /// // The key in the keyset was changed
    /// assert_eq!(ks.head().unwrap().value(), "newvalue");
    /// # Ok(())
    /// # }
    /// ```
    pub fn lookup(&mut self, mut key: StringKey, options: LookupOption) -> Option<StringKey<'_>> {
        let key_ptr = unsafe {
            elektra_sys::ksLookup(
                self.as_ptr(),
                key.as_ptr(),
                options.bits() as elektra_sys::elektraLookupFlags,
            )
        };

        if key_ptr.is_null() {
            None
        } else {
            Some(unsafe { StringKey::from_ptr(key_ptr) })
        }
    }

    /// Lookup a key by name.
    /// Returns None if the provided string is an invalid name.
    /// Otherwise identical to [`lookup`](#method.lookup).
    pub fn lookup_by_name(&mut self, name: &str, options: LookupOption) -> Option<StringKey> {
        if let Ok(key) = StringKey::new(name) {
            return self.lookup(key, options);
        }
        None
    }

    /// Returns an iterator that should be used to immutably iterate over a keyset.
    pub fn iter(&self) -> ReadOnlyStringKeyIter<'_> {
        ReadOnlyStringKeyIter {
            cursor: Some(0),
            keyset: self,
        }
    }
    /// Returns an iterator that returns StringKey
    /// that can be modified.
    /// Note that you should not change the name of the key.
    pub fn iter_mut(&mut self) -> StringKeyIter<'_> {
        StringKeyIter {
            cursor: Some(0),
            keyset: self,
        }
    }
}

impl<'a> std::iter::FromIterator<StringKey<'a>> for KeySet {
    fn from_iter<I: IntoIterator<Item = StringKey<'a>>>(iter: I) -> Self {
        let mut ks = KeySet::new();
        for item in iter {
            ks.append_key(item);
        }
        ks
    }
}

impl<'a> Extend<StringKey<'a>> for KeySet {
    fn extend<T: IntoIterator<Item = StringKey<'a>>>(&mut self, iter: T) {
        for item in iter {
            self.append_key(item);
        }
    }
}

fn next<T: ReadableKey>(cursor: &Option<Cursor>, keyset: &KeySet) -> (Option<Cursor>, Option<T>) {
    match cursor {
        None => (None, None),
        Some(cursor) => {
            let key_ptr = unsafe {
                // The cast is necessary, such that the ReadOnly iterator can take &KeySet instead of &mut KeySet
                // Since ReadOnly prevents any modification, this is fine.
                elektra_sys::ksAtCursor(
                    keyset.as_ref() as *const elektra_sys::KeySet as *mut elektra_sys::KeySet,
                    *cursor,
                )
            };

            if key_ptr.is_null() {
                (None, None)
            } else {
                let new_cursor = Some(cursor + 1);
                (new_cursor, Some(unsafe { T::from_ptr(key_ptr) }))
            }
        }
    }
}

/// A iterator over immutable keys.
pub struct ReadOnlyStringKeyIter<'a> {
    cursor: Option<Cursor>,
    keyset: &'a KeySet,
}

impl<'a> Iterator for ReadOnlyStringKeyIter<'a> {
    type Item = ReadOnly<StringKey<'a>>;

    fn next(&mut self) -> Option<Self::Item> {
        let (new_cursor, item) = next(&self.cursor, &self.keyset);
        self.cursor = new_cursor;
        item
    }
}

/// A iterator over mutable keys.
pub struct StringKeyIter<'a> {
    cursor: Option<Cursor>,
    keyset: &'a mut KeySet,
}

impl<'a> Iterator for StringKeyIter<'a> {
    type Item = StringKey<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        let (new_cursor, item) = next(&self.cursor, &self.keyset);
        self.cursor = new_cursor;
        item
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{KeyBuilder, KeyNameInvalidError, CopyOption};
    use std::iter::FromIterator;

    #[test]
    fn can_build_simple_keyset() -> Result<(), KeyNameInvalidError> {
        let mut ks = KeySet::new();
        ks.append_key(
            KeyBuilder::<StringKey>::new("user:/sw/org/app/bool")?
                .value("true")
                .build(),
        );
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
    fn can_iterate_simple_keyset() -> Result<(), KeyNameInvalidError> {
        let names = ["user:/test/key1", "user:/test/key2", "user:/test/key3"];
        let values = ["value1", "value2", "value3"];

        let mut ks = KeySet::from_iter(vec![
            KeyBuilder::<StringKey>::new(names[0])?
                .value(values[0])
                .build(),
            KeyBuilder::<StringKey>::new(names[1])?
                .value(values[1])
                .build(),
            KeyBuilder::<StringKey>::new(names[2])?
                .value(values[2])
                .build(),
        ]);

        let new_values = ["Newvalue1", "Newvalue2", "Newvalue3"];
        let mut did_iterate = false;
        for (i, mut key) in ks.iter_mut().enumerate() {
            did_iterate = true;
            assert_eq!(key.value(), values[i]);
            key.set_value(new_values[i]);
        }
        assert!(did_iterate);

        did_iterate = false;
        for (i, key) in ks.iter().enumerate() {
            did_iterate = true;
            assert_eq!(key.value(), new_values[i]);
            assert_eq!(key.name(), names[i]);
        }
        assert!(did_iterate);
        // Check that the iterator did not consume the keyset
        assert_eq!(ks.size(), 3);
        Ok(())
    }

    #[test]
    fn can_use_popped_key_after_keyset_freed() {
        let popped_key;
        {
            let key = StringKey::new("user:/key/k2").unwrap();
            let mut keyset = KeySet::with_capacity(1);
            keyset.append_key(key);
            popped_key = keyset.pop().unwrap();
            assert_eq!(keyset.size(), 0);
        }
        assert_eq!(popped_key.basename(), "k2");
    }

    #[test]
    fn pop_on_empty_keyset_returns_none() {
        let mut keyset = KeySet::new();
        let p = keyset.pop();
        assert!(p.is_none());
    }

    fn setup_keyset() -> KeySet {
        let names = ["system:/test/key", "user:/test/key"];
        let values = ["value1", "value2"];

        KeySet::from_iter(vec![
            KeyBuilder::<StringKey>::new(names[0])
                .unwrap()
                .value(values[0])
                .build(),
            KeyBuilder::<StringKey>::new(names[1])
                .unwrap()
                .value(values[1])
                .build(),
        ])
    }

    #[test]
    fn can_duplicate_keyset() {
        // Make sure that freeing the original ks does not invalidate the duplicate
        let ks_dup;
        {
            let ks = setup_keyset();
            ks_dup = ks.duplicate();
        }
        assert_eq!(ks_dup.size(), 2);
    }

    #[test]
    fn extend_keyset_and_append_are_equal() {
        let mut ks = setup_keyset();
        let mut ks2 = KeySet::with_capacity(1);
        let k = StringKey::new("user:/test/key").unwrap();
        ks2.append_key(k);

        // Test append
        ks.append(&ks2);
        assert_eq!(ks.size(), 2);
        assert_eq!(ks.head().unwrap().name(), "user:/test/key");
        assert_eq!(ks.head().unwrap().value(), "");

        // Test extend from the Extend trait
        let mut ksext = setup_keyset();
        ksext.extend(ks2.iter_mut());

        assert_eq!(ksext.size(), 2);
        assert_eq!(ksext.head().unwrap().name(), "user:/test/key");
        assert_eq!(ksext.head().unwrap().value(), "");
    }

    #[test]
    fn can_lookup_key_with_none_option() {
        let mut ks = setup_keyset();
        let lookup_key = StringKey::new("/test/key").unwrap();
        let ret_val = ks.lookup(lookup_key, LookupOption::KDB_O_NONE);
        assert_eq!(ret_val.unwrap().name(), "user:/test/key");
        assert_eq!(ks.size(), 2);
        assert_eq!(ks.head().unwrap().name(), "user:/test/key");
        assert_eq!(ks.tail().unwrap().name(), "system:/test/key");
    }

    #[test]
    fn can_lookup_key_with_pop_option() {
        let mut ks = setup_keyset();
        let lookup_key = StringKey::new("/test/key").unwrap();
        let key = ks.lookup(lookup_key, LookupOption::KDB_O_POP);
        assert_eq!(key.unwrap().name(), "user:/test/key");
        assert_eq!(ks.size(), 1);
        assert_eq!(ks.head().unwrap().name(), "system:/test/key");
    }

    #[test]
    fn can_lookup_by_name_and_duplicate_key() -> Result<(), KeyNameInvalidError> {
        // Make sure that a duplicate of a key that is from a keyset
        // can be used after the KeySet has been freed
        let key;
        {
            let mut ks = setup_keyset();
            key = ks
                .lookup_by_name("/test/key", LookupOption::KDB_O_NONE)
                .unwrap()
                .duplicate(CopyOption::KEY_CP_ALL);
            assert_eq!(ks.size(), 2);
            assert_eq!(ks.head().unwrap().name(), "user:/test/key");
            assert_eq!(ks.tail().unwrap().name(), "system:/test/key");
        }
        assert_eq!(key.name(), "user:/test/key");
        Ok(())
    }

    #[test]
    fn test_keyset_macro() {
        let ks = keyset![];
        assert_eq!(0, ks.size());

        let ks = keyset![
            StringKey::new("user:/test1").unwrap(),
            StringKey::new("user:/test2").unwrap(),
            StringKey::new("user:/test3").unwrap(),
            StringKey::new("user:/test4").unwrap()
        ];
        assert_eq!(4, ks.size());
        assert_eq!("user:/test1", ks.head().unwrap().name());
        assert_eq!("user:/test4", ks.tail().unwrap().name());

        let ks = keyset![
            KeyBuilder::<StringKey>::new("user:/test1")
                .unwrap()
                .meta("metakey1", "metavalue1")
                .unwrap()
                .meta("metakey2", "metavalue2")
                .unwrap()
                .build(),
            // Macro invocation also works with a trailing comma
            StringKey::new("user:/test2").unwrap(),
        ];
        assert_eq!(2, ks.size());
        assert_eq!("user:/test1", ks.head().unwrap().name());
        assert_eq!(
            "metavalue2",
            ks.head().unwrap().meta("metakey2").unwrap().value()
        );
    }
}
