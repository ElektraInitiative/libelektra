//! `StringKey` and `BinaryKey` are the essential structs that encapsulate name, value and metainfo.
//!
//! They are equivalent, except for the values that they hold.
//! Their common functionality is split into two traits, [`ReadableKey`](../readable/trait.ReadableKey.html)
//! and [`WriteableKey`](../writeable/trait.WriteableKey.html). Usually, you
//! will have to import both to make use of the methods they provide.
//! # Example
//! ```
//! # use elektra::{KeyBuilder,StringKey,WriteableKey,ReadableKey};
//! # fn main() -> Result<(), Box<dyn std::error::Error>> {
//! let mut key = StringKey::new("user:/test/language")?;
//! key.set_value("rust");
//!
//! assert_eq!(key.value(), "rust");
//! assert_eq!(key.name(), "user:/test/language");
//!
//! # Ok(())
//! # }
//! ```
//! # Example
//! The key can also hold an arbitrary number of metakeys, that are always `StringKey`s.
//! ```
//! # use elektra::{KeyBuilder,StringKey,WriteableKey,ReadableKey};
//! # fn main() -> Result<(), Box<dyn std::error::Error>> {
//! let mut key = StringKey::new("user:/test/meta")?;
//! key.set_meta("rust", "😃");
//!
//! assert_eq!(key.meta("rust")?.value(), "😃");
//! # Ok(())
//! # }
//! ```

use crate::{ReadableKey, WriteableKey};
use bitflags::bitflags;
use elektra_sys;
use std::borrow::Cow;
use std::convert::TryInto;
use std::ffi::{CStr, CString};
use std::ptr::NonNull;

#[derive(Debug)]
pub struct StringKey<'a> {
    ptr: NonNull<elektra_sys::Key>,
    _marker: std::marker::PhantomData<&'a mut elektra_sys::Key>,
}

#[derive(Debug)]
pub struct BinaryKey<'a> {
    ptr: NonNull<elektra_sys::Key>,
    _marker: std::marker::PhantomData<&'a mut elektra_sys::Key>,
}

bitflags! {
    /// Bitflags to be passed to [`duplicate`](struct.Key.html#method.duplicate).
    #[derive(Default)]
    pub struct CopyOption: elektra_sys::elektraCopyFlags {
        /// Copy the key name.
        const KEY_CP_NAME = elektra_sys::KEY_CP_NAME as elektra_sys::elektraCopyFlags;
        /// Copy the key value, if it is a string.
        const KEY_CP_STRING = elektra_sys::KEY_CP_STRING as elektra_sys::elektraCopyFlags;
        /// Copy the key value.
        const KEY_CP_VALUE = elektra_sys::KEY_CP_VALUE as elektra_sys::elektraCopyFlags;
        /// Copy the key metadata.
        const KEY_CP_META = elektra_sys::KEY_CP_META as elektra_sys::elektraCopyFlags;
        /// Shorthand for copying key name, value and metadata.
        const KEY_CP_ALL = Self::KEY_CP_NAME.bits | Self::KEY_CP_VALUE.bits | Self::KEY_CP_META.bits as elektra_sys::elektraCopyFlags;
    }
}

macro_rules! add_traits {
    ($($t:ty)*) => ($(
        impl PartialEq for $t {

            #[inline]
            fn eq(&self, other: &Self) -> bool {
                unsafe {
                    elektra_sys::keyCmp(
                        self.as_ref(),
                        other.as_ref(),
                    ) == 0
                }
            }
        }

        impl Eq for $t {}

        impl Ord for $t {
            fn cmp(&self, other: &Self) -> std::cmp::Ordering {
                let cmp = unsafe {
                    elektra_sys::keyCmp(
                        self.as_ref(),
                        other.as_ref(),
                    )
                };

                if cmp < 0 {
                    std::cmp::Ordering::Less
                } else if cmp == 0 {
                    std::cmp::Ordering::Equal
                } else {
                    std::cmp::Ordering::Greater
                }
            }
        }

        impl PartialOrd for $t {
            fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
                Some(self.cmp(other))
            }
        }

        impl AsRef<elektra_sys::Key> for $t {
            fn as_ref(&self) -> &elektra_sys::Key {
                unsafe { self.ptr.as_ref() }
            }
        }

        impl Drop for $t {
            fn drop(&mut self) {
                unsafe { elektra_sys::keyDel(self.as_ptr()) };
            }
        }

        impl Clone for $t {
            fn clone(&self) -> Self {
                self.duplicate(CopyOption::KEY_CP_ALL)
            }
        }

        impl Default for $t {
            fn default() -> Self {
                Self::new_empty()
            }
        }
    )*)
}

add_traits!(StringKey<'_>);
add_traits!(BinaryKey<'_>);

/// An iterator over the name.
pub struct NameIter<'a, T: ReadableKey> {
    key: &'a T,
    curr_offset: isize,
    name_len: isize,
}

impl<'a, T: ReadableKey> Iterator for NameIter<'a, T> {
    type Item = std::borrow::Cow<'a, str>;
    fn next(&mut self) -> Option<Self::Item> {
        if self.curr_offset >= self.name_len {
            return None;
        };
        let name_ptr = unsafe { elektra_sys::keyUnescapedName(self.key.as_ref()) };

        let cow_str = unsafe {
            CStr::from_ptr((name_ptr as *const std::os::raw::c_char).offset(self.curr_offset))
        }
        .to_string_lossy();
        self.curr_offset += cow_str.len() as isize + 1;
        Some(cow_str)
    }
}

impl<'a> StringKey<'a> {
    /// Sets the value of the key to the supplied string.
    ///
    /// # Panics
    /// Panics if the provided string contains interior nul bytes.
    fn set_string(&mut self, value: &str) {
        let cstr = CString::new(value).unwrap();
        unsafe { elektra_sys::keySetString(self.as_ptr(), cstr.as_ptr()) };
    }

    /// Returns the string value of the key or a Utf8Error if it cannot be converted.
    fn string(&self) -> Cow<'a, str> {
        let c_str = unsafe { CStr::from_ptr(elektra_sys::keyString(self.as_ref())) };
        c_str.to_string_lossy()
    }

    /// Returns a deep copy of the key.
    pub fn duplicate<'b>(&'a self, options: CopyOption) -> StringKey<'b> {
        let dup_ptr = unsafe {
            let name = CString::new("/").unwrap();
            elektra_sys::keyCopy(
                elektra_sys::keyNew (name.as_ptr(), elektra_sys::KEY_END),
                self.as_ref(),
                options.bits() as elektra_sys::elektraCopyFlags,
            )
        };
        unsafe { StringKey::from_ptr(dup_ptr) }
    }

    /// Returns an iterator over the key's name.
    pub fn name_iter<'b>(&'b self) -> NameIter<'b, StringKey<'a>> {
        NameIter {
            key: self,
            curr_offset: 0,
            name_len: unsafe {
                elektra_sys::keyGetUnescapedNameSize(self.as_ref())
                    .try_into()
                    .unwrap()
            },
        }
    }
}

impl<'a> BinaryKey<'a> {
    /// Sets the key's binary content to the supplied data.
    fn set_binary(&mut self, data: &[u8]) {
        // Make sure the binary flag is set even if data is empty
        if data.is_empty() {
            unsafe {
                elektra_sys::keySetBinary(self.as_ptr(), std::ptr::null(), 0);
            }
        } else {
            unsafe {
                elektra_sys::keySetBinary(
                    self.as_ptr(),
                    data.as_ptr() as *const std::os::raw::c_void,
                    data.len(),
                );
            }
        }
    }

    /// Returns the keys binary content
    fn binary(&self) -> Vec<u8> {
        let mut vec: Vec<u8> = Vec::with_capacity(self.value_size());

        let ret_val = unsafe {
            elektra_sys::keyGetBinary(
                self.as_ref(),
                vec.as_mut_ptr() as *mut std::os::raw::c_void,
                vec.capacity(),
            )
        };

        if ret_val > 0 {
            unsafe { vec.set_len(ret_val.try_into().unwrap()) };
        }

        vec
    }

    /// Returns a deep copy of the key.
    pub fn duplicate<'b>(&'a self, options: CopyOption) -> BinaryKey<'b> {
        let dup_ptr = unsafe {
            let name = CString::new("/").unwrap();
            elektra_sys::keyCopy(
                elektra_sys::keyNew (name.as_ptr(), elektra_sys::KEY_END),
                self.as_ref(),
                options.bits() as elektra_sys::elektraCopyFlags,
            )
        };
        unsafe { BinaryKey::from_ptr(dup_ptr) }
    }

    /// Returns an iterator over the key's name.
    pub fn name_iter<'b>(&'b self) -> NameIter<'b, BinaryKey<'a>> {
        NameIter {
            key: self,
            curr_offset: 0,
            name_len: unsafe {
                elektra_sys::keyGetUnescapedNameSize(self.as_ref())
                    .try_into()
                    .unwrap()
            },
        }
    }
}

impl<'a> ReadableKey for StringKey<'a> {
    type GetValue = Cow<'a, str>;

    unsafe fn from_ptr(ptr: *mut elektra_sys::Key) -> StringKey<'a> {
        StringKey {
            ptr: NonNull::new(ptr).unwrap(),
            _marker: std::marker::PhantomData,
        }
    }

    fn value(&self) -> Self::GetValue {
        self.string()
    }
}

impl<'a> ReadableKey for BinaryKey<'a> {
    type GetValue = Vec<u8>;

    unsafe fn from_ptr(ptr: *mut elektra_sys::Key) -> BinaryKey<'a> {
        BinaryKey {
            ptr: NonNull::new(ptr).unwrap(),
            _marker: std::marker::PhantomData,
        }
    }

    fn value(&self) -> Self::GetValue {
        self.binary()
    }
}

impl<'a> WriteableKey for BinaryKey<'a> {
    type SetValue = &'a [u8];

    fn as_ptr(&mut self) -> *mut elektra_sys::Key {
        self.ptr.as_ptr()
    }

    fn set_value(&mut self, value: Self::SetValue) {
        self.set_binary(value);
    }
}
impl<'a> WriteableKey for StringKey<'a> {
    type SetValue = &'a str;

    fn as_ptr(&mut self) -> *mut elektra_sys::Key {
        self.ptr.as_ptr()
    }
    fn set_value(&mut self, value: Self::SetValue) {
        self.set_string(value);
    }
}

impl From<StringKey<'_>> for BinaryKey<'_> {
    fn from(mut sk: StringKey) -> Self {
        let binary_key = unsafe { BinaryKey::from_ptr(sk.as_ptr()) };
        std::mem::forget(sk);
        binary_key
    }
}

impl From<BinaryKey<'_>> for StringKey<'_> {
    fn from(mut bk: BinaryKey) -> Self {
        let str_key = unsafe { StringKey::from_ptr(bk.as_ptr()) };
        std::mem::forget(bk);
        str_key
    }
}

/// An error indicating that a keys name is readonly.
#[derive(Debug, PartialEq)]
pub struct KeyNameReadOnlyError{
    name: String,
}
impl KeyNameReadOnlyError {
    pub fn new(name: String) -> Self {
        KeyNameReadOnlyError { name }
    }
    /// Returns the name of the key whose name is readonly.
    pub fn name(&self) -> &str {
        &self.name
    }
}
impl std::fmt::Display for KeyNameReadOnlyError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(f, r#"name of the key "{}" is read only"#, self.name())
    }
}

impl std::error::Error for KeyNameReadOnlyError {}

/// An error indicating that a provided name is invalid.
#[derive(Debug, PartialEq)]
pub struct KeyNameInvalidError{
    name: String,
}
impl KeyNameInvalidError {
    pub fn new(name: String) -> Self {
        KeyNameInvalidError { name }
    }
    /// Returns the name that is invalid.
    pub fn name(&self) -> &str {
        &self.name
    }
}
impl std::fmt::Display for KeyNameInvalidError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(f, r#"provided name "{}" is invalid"#, self.name())
    }
}

impl std::error::Error for KeyNameInvalidError {}

/// An error indicating that a metakey was not found.
#[derive(Debug, PartialEq)]
pub struct KeyNotFoundError {
    name: String,
}

impl KeyNotFoundError {
    pub fn new(name: String) -> Self {
        KeyNotFoundError { name }
    }
    /// Returns the name that caused the search failure.
    pub fn name(&self) -> &str {
        &self.name
    }
}

impl std::fmt::Display for KeyNotFoundError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(f, r#"metakey with name "{}" was not found"#, self.name())
    }
}

impl std::error::Error for KeyNotFoundError {}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn can_write_read_key() {
        let key_name = "user:/test/key";
        let key = StringKey::new(key_name).unwrap();
        assert_eq!(key.name(), key_name);
    }

    #[test]
    fn can_write_read_key_value() {
        let key_name = "user:/test/key";
        let utf8_value = "😃";
        let mut key: StringKey = StringKey::new(key_name).unwrap();
        key.set_string(utf8_value);
        assert_eq!(key.name(), key_name);
        assert_eq!(key.value(), utf8_value);
    }

    #[test]
    fn can_duplicate_key() {
        let key_name = "user:/test/key";
        let key_dup;
        {
            let key = StringKey::new(key_name).unwrap();
            key_dup = key.clone();
            // key is dropped here
        }
        assert_eq!(key_dup.name(), key_name);
    }

    #[test]
    fn can_write_read_binary() {
        let mut key = BinaryKey::new("user:/test/rust").unwrap();
        let binary_content: [u8; 7] = [25, 34, 0, 254, 1, 0, 7];
        key.set_binary(&binary_content);
        let read_content = key.binary();
        assert_eq!(read_content, binary_content);
    }
    #[test]
    fn can_write_read_empty_binary() {
        let mut key = BinaryKey::new("user:/test/binary").unwrap();
        let binary_content: [u8; 0] = [];
        key.set_binary(&binary_content);
        let vec = key.binary();
        assert_eq!(vec, binary_content);
    }

    #[allow(clippy::eq_op)]
    #[test]
    fn equality_is_exclusive() {
        let key = BinaryKey::new("user:/test/exclusive").unwrap();
        let key2 = BinaryKey::new("dir:/test/exclusive").unwrap();
        assert!(!(key != key));
        assert!(key == key);

        assert!(!(key == key2));
        assert!(key != key2);
    }

    #[allow(clippy::eq_op)]
    #[test]
    fn equality_is_reflexive() {
        let key = StringKey::new("user:/test/reflexive").unwrap();
        assert!(key == key);
    }

    #[test]
    fn equality_is_symmetric() {
        let key = BinaryKey::new("user:/test/symmetric").unwrap();
        let key_dup = BinaryKey::new("user:/test/symmetric").unwrap();

        assert!(key_dup == key);
        assert!(key == key_dup);
    }

    #[test]
    fn equality_is_transitive() {
        let key = BinaryKey::new("user:/test/transitive").unwrap();
        let key2 = BinaryKey::new("user:/test/transitive").unwrap();
        let key3 = BinaryKey::new("user:/test/transitive").unwrap();

        assert!(key == key2);
        assert!(key2 == key3);
        assert!(key == key3);
    }

    #[test]
    fn keys_are_ordered() {
        let key = BinaryKey::new("user:/test/a").unwrap();
        let key2 = BinaryKey::new("user:/test/b").unwrap();
        let key3 = BinaryKey::new("user:/test/c").unwrap();

        assert!(key != key2);
        assert!(key < key2);
        assert!(key2 > key);
        // Check for antisymmetry
        assert!(!(key > key2));
        assert!(!(key2 < key));

        // Check for transitivity
        assert!(key2 < key3);
        assert!(key < key3);
    }

    #[test]
    fn can_reference_count() {
        let mut key = BinaryKey::new("user:/test/a").unwrap();
        assert_eq!(key.get_ref(), 0);
        unsafe { key.inc_ref() };
        assert_eq!(key.get_ref(), 1);
        unsafe { key.dec_ref() };
        assert_eq!(key.get_ref(), 0);
    }

    #[test]
    fn error_on_missing_metaname() {
        let key = StringKey::new("user:/test/metatest").unwrap();
        assert!(key.meta("nonexistent metaname").is_err());
    }
    #[test]
    fn can_set_get_metavalue() {
        let mut key = StringKey::new_empty();
        key.set_meta("metakey", "metaval").unwrap();
        let meta_key = key.meta("metakey").unwrap();
        assert_eq!(meta_key.value(), "metaval");
    }

    /* TODO: Implement external iteration of metakeys */
    /*
    #[test]
    fn can_iterate_key() {
        let mut key = StringKey::new_empty();
        let meta = [("meta:/meta1", "val1"), ("meta:/meta2", "val2")];
        key.set_meta(meta[0].0, meta[0].1).unwrap();
        key.set_meta(meta[1].0, meta[1].1).unwrap();

        let mut did_iterate = false;

        for (i, metakey) in key.meta_iter().enumerate() {
            did_iterate = true;
            assert_eq!(metakey.name(), meta[i].0);
            assert_eq!(metakey.value(), meta[i].1);
        }
        assert!(did_iterate);
    }*/

    #[test]
    fn can_delete_metadata() {
        let mut key = StringKey::new_empty();
        key.set_meta("metakey", "metaval").unwrap();
        assert_eq!(key.delete_meta("metakey").unwrap(), 0);
        assert_eq!(
            key.meta("metakey").unwrap_err(),
            KeyNotFoundError {
                name: "metakey".to_owned()
            }
        );
    }

    #[test]
    fn can_cast_key_types() {
        let key = StringKey::new("user:/test/cast").unwrap();
        let mut bin_key = BinaryKey::from(key);
        let val = b"data";
        bin_key.set_value(val);
        assert_eq!(bin_key.value(), val);
    }

    #[test]
    fn can_iterate_name() -> Result<(), KeyNameInvalidError> {
        let names = ["user:", "test", "fulltest"];
        let key = StringKey::new(&names.join("/"))?;
        let mut did_iterate = false;
        for (i, name) in key.name_iter().enumerate().skip(1) {
            did_iterate = true;
            assert_eq!(name, names[i]);
        }
        assert!(did_iterate);

        let root_key = StringKey::new_empty();
        let mut iter = root_key.name_iter();
        assert_eq!(iter.next().unwrap(), "\u{1}"); // TODO: use KEY_NS_CASCADING instead of hardcoded value

        Ok(())
    }
}
