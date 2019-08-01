use elektra_sys;
use std::convert::TryInto;
use std::error::Error;
use std::ffi::{CStr, CString};
use std::fmt;
use std::ptr::NonNull;

#[derive(Debug, PartialEq)]
pub enum KeyError {
    InvalidName,
    TypeMismatch,
    ReadOnly, // TypeConversion,
    NotFound,
}

impl fmt::Display for KeyError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        match self {
            KeyError::InvalidName => write!(f, "Key has an invalid name"),
            KeyError::TypeMismatch => write!(f, "Binary/String key mismatch, use the appropriate method for your key type, get_string or get_binary"),
            KeyError::ReadOnly => write!(f, "Key is read only"),
            KeyError::NotFound => write!(f, "Key/Metakey was not found"),
            // _ => unimplemented!(),
        }
    }
}

impl Error for KeyError {}

#[derive(Debug)]
pub struct StringKey {
    ptr: NonNull<elektra_sys::Key>,
}

#[derive(Debug)]
pub struct BinaryKey {
    ptr: NonNull<elektra_sys::Key>,
}

#[derive(Debug)]
pub struct ReadOnlyStringKey {
    ptr: NonNull<elektra_sys::Key>,
}

macro_rules! add_readonly_traits {
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

        impl Clone for $t {
            fn clone(&self) -> Self {
                self.duplicate()
            }
        }

    )*)
}

add_readonly_traits!(StringKey);
add_readonly_traits!(BinaryKey);
add_readonly_traits!(ReadOnlyStringKey);

macro_rules! add_writeable_traits {
    ($($t:ty)*) => ($(
        impl Drop for $t {
            fn drop(&mut self) {
                println!("Drop {:?}", self);
                unsafe { elektra_sys::keyDel(self.as_ptr()) };
            }
        }

        impl Iterator for $t {
            type Item = ReadOnlyStringKey;
            fn next(&mut self) -> Option<Self::Item> {
                let key_ptr = unsafe { elektra_sys::keyNextMeta(self.as_ptr()) };
                if key_ptr == std::ptr::null() {
                    None
                } else {
                    Some(ReadOnlyStringKey::from_ptr(key_ptr as *mut elektra_sys::Key))
                }
            }
        }
    )*)
}

add_writeable_traits!(StringKey);
add_writeable_traits!(BinaryKey);

impl StringKey {
    /// Sets the value of the key to the supplied string.
    /// # Panics
    /// Panics if the provided string contains internal nul bytes.
    pub fn set_string(&mut self, value: &str) {
        let cptr = CString::new(value).unwrap();
        unsafe { elektra_sys::keySetString(self.as_ptr(), cptr.as_ptr()) };
    }

    /// Returns the string value of the key if the type of the key is string, an error if it's binary.
    /// # Panics
    /// Panics if the underlying string cannot be converted to UTF-8.
    pub fn get_string(&self) -> &str {
        let c_str = unsafe { CStr::from_ptr(elektra_sys::keyString(self.as_ref())) };
        c_str.to_str().unwrap()
    }
}

impl ReadOnlyStringKey {
    /// Returns the string value of the key if the type of the key is string, an error if it's binary.
    /// # Panics
    /// Panics if the underlying string cannot be converted to UTF-8.
    pub fn get_string(&self) -> &str {
        let c_str = unsafe { CStr::from_ptr(elektra_sys::keyString(self.as_ref())) };
        c_str.to_str().unwrap()
    }
}

impl BinaryKey {
    /// Sets the key's binary content to the supplied data.
    pub fn set_binary(&mut self, data: &[u8]) {
        unsafe {
            elektra_sys::keySetBinary(
                self.as_ptr(),
                data.as_ptr() as *const std::os::raw::c_void,
                data.len(),
            );
        }
    }

    /// Returns the keys binary content
    /// Returns a TypeMismatch if the key is of type string
    pub fn get_binary(&self) -> Vec<u8> {
        let mut vec: Vec<u8> = Vec::with_capacity(self.get_value_size());

        let ret_val = unsafe {
            elektra_sys::keyGetBinary(
                self.as_ref(),
                vec.as_mut_ptr() as *mut std::os::raw::c_void,
                vec.capacity(),
            )
        };

        if ret_val > 0 {
            unsafe { vec.set_len(ret_val as usize) };
            vec
        } else {
            unsafe { vec.set_len(0) }
            vec
        }
    }
}

impl ReadableKey for StringKey {
    fn as_ref(&self) -> &elektra_sys::Key {
        unsafe { self.ptr.as_ref() }
    }

    fn from_ptr(ptr: *mut elektra_sys::Key) -> Self {
        StringKey {
            ptr: NonNull::new(ptr).unwrap(),
        }
    }
}

impl ReadableKey for BinaryKey {
    fn as_ref(&self) -> &elektra_sys::Key {
        unsafe { self.ptr.as_ref() }
    }

    fn from_ptr(ptr: *mut elektra_sys::Key) -> Self {
        BinaryKey {
            ptr: NonNull::new(ptr).unwrap(),
        }
    }
}

impl ReadableKey for ReadOnlyStringKey {
    fn as_ref(&self) -> &elektra_sys::Key {
        unsafe { self.ptr.as_ref() }
    }

    fn from_ptr(ptr: *mut elektra_sys::Key) -> Self {
        ReadOnlyStringKey {
            ptr: NonNull::new(ptr).unwrap(),
        }
    }
}

impl WriteableKey for BinaryKey {
    fn as_ptr(&mut self) -> *mut elektra_sys::Key {
        self.ptr.as_ptr()
    }
}
impl WriteableKey for StringKey {
    fn as_ptr(&mut self) -> *mut elektra_sys::Key {
        self.ptr.as_ptr()
    }
}

pub trait StringableKey {}

pub trait WriteableKey: ReadableKey {
    fn as_ptr(&mut self) -> *mut elektra_sys::Key;
    /// Construct a new key with a name
    fn new(name: &str) -> Result<Self, KeyError>
    where
        Self: Sized,
    {
        let mut key = Self::new_empty();
        key.set_name(name)?;
        Ok(key)
    }
    /// Construct a new empty key
    fn new_empty() -> Self
    where
        Self: Sized,
    {
        let key_ptr = unsafe { elektra_sys::keyNew(0 as *const i8) };
        Self::from_ptr(key_ptr)
    }

    /// Decrement the viability of a key object.
    /// Returns the value of the new reference counter.
    fn dec_ref(&mut self) -> isize {
        unsafe { elektra_sys::keyDecRef(self.as_ptr()) }
    }

    /// Clears the key.
    /// After this call you will receive a fresh key.
    fn clear(&mut self) {
        unsafe {
            elektra_sys::keyClear(self.as_ptr());
        }
    }

    /// Increment the viability of a key object.
    /// Returns the value of the new reference counter.
    fn inc_ref(&mut self) -> isize {
        unsafe { elektra_sys::keyIncRef(self.as_ptr()) }
    }

    /// Return how many references the key has.
    fn get_ref(&self) -> isize {
        unsafe { elektra_sys::keyGetRef(self.as_ref()) }
    }

    /// Set the name of a key. Must adhere to the rules for keynames otherwise an error is returned.
    /// Returns the size in bytes of this new key name including the ending NUL.
    /// # Examples
    /// ```
    /// use elektra::{StringKey,WriteableKey,ReadableKey};
    /// let mut key = StringKey::new_empty();
    /// key.set_name("user/test/rust").unwrap();
    /// assert_eq!(key.get_name(), "user/test/rust");
    /// ```
    /// # Panics
    /// Panics if the provided string contains internal nul bytes.
    fn set_name(&mut self, name: &str) -> Result<u32, KeyError> {
        let cptr = CString::new(name).unwrap();
        let ret_val = unsafe { elektra_sys::keySetName(self.as_ptr(), cptr.as_ptr()) };

        if ret_val > 0 {
            Ok(ret_val as u32)
        } else {
            // TODO: May also be a ReadOnly Error...
            Err(KeyError::InvalidName)
        }
    }
    /// Set the basename of the key
    /// # Examples
    /// ```
    /// use elektra::{StringKey,WriteableKey,ReadableKey};
    /// let mut key = StringKey::new("user/test/key").unwrap();
    /// key.set_basename("rust").unwrap();
    /// assert_eq!(key.get_name(), "user/test/rust");
    /// ```
    fn set_basename(&mut self, basename: &str) -> Result<(), KeyError> {
        let cstr = CString::new(basename).unwrap();
        let ret_val = unsafe { elektra_sys::keySetBaseName(self.as_ptr(), cstr.as_ptr()) };
        // TODO: Is read only a correct description of the error?
        if ret_val == -1 {
            Err(KeyError::ReadOnly)
        } else {
            Ok(())
        }
    }

    /// Add a basename to the key
    /// # Examples
    /// ```
    /// use elektra::{StringKey,WriteableKey,ReadableKey};
    /// let mut key = StringKey::new("user/test/key").unwrap();
    /// key.add_basename("rust").unwrap();
    /// assert_eq!(key.get_name(), "user/test/key/rust");
    /// ```
    fn add_basename(&mut self, basename: &str) -> Result<(), KeyError> {
        let cstr = CString::new(basename).unwrap();
        let ret_val = unsafe { elektra_sys::keyAddBaseName(self.as_ptr(), cstr.as_ptr()) };
        // TODO: Is read only a correct description of the error?
        if ret_val == -1 {
            Err(KeyError::ReadOnly)
        } else {
            Ok(())
        }
    }

    /// Add an already escaped name to the keyname.
    /// # Examples
    /// ```
    /// use elektra::{StringKey,WriteableKey,ReadableKey};
    /// let mut key = StringKey::new("user/x/r").unwrap();
    /// key.add_name("../y/a//././z").unwrap();
    /// assert_eq!(key.get_name(), "user/x/y/a/z");
    /// ```
    fn add_name(&mut self, name: &str) -> Result<(), KeyError> {
        let cstr = CString::new(name).unwrap();
        let ret_val = unsafe { elektra_sys::keyAddName(self.as_ptr(), cstr.as_ptr()) };
        // TODO: Is read only a correct description of the error?
        if ret_val <= 0 {
            Err(KeyError::InvalidName)
        } else {
            Ok(())
        }
    }
    /// Sets the value of the key to the supplied string.
    /// # Panics
    /// Panics if the provided string contains internal nul bytes.
    fn set_string(&mut self, value: &str) {
        let cptr = CString::new(value).unwrap();
        unsafe { elektra_sys::keySetString(self.as_ptr(), cptr.as_ptr()) };
    }
    /// Copies all metadata from source to the self
    fn copy_all_meta(&mut self, source: &Self) {
        unsafe {
            elektra_sys::keyCopyAllMeta(self.as_ptr(), source.as_ref());
        }
    }

    /// Copy metakey with name metaname from source to self
    /// # Examples
    /// ```
    /// use elektra::{StringKey,WriteableKey,ReadableKey};
    /// let mut key = StringKey::new_empty();
    /// key.set_meta("meta", "value");
    /// let mut key2 = StringKey::new_empty();
    /// key2.copy_meta(&key, "meta");
    /// assert_eq!(key2.get_meta("meta").unwrap().get_string(), "value");
    /// ```
    fn copy_meta(&mut self, source: &Self, metaname: &str) -> i32 {
        let cstr = CString::new(metaname).unwrap();
        unsafe { elektra_sys::keyCopyMeta(self.as_ptr(), source.as_ref(), cstr.as_ptr()) }
    }
    /// Set a new meta-information.
    fn set_meta(&mut self, metaname: &str, metavalue: &str) -> isize {
        let name = CString::new(metaname).unwrap();
        let value = CString::new(metavalue).unwrap();
        unsafe { elektra_sys::keySetMeta(self.as_ptr(), name.as_ptr(), value.as_ptr()) }
    }

    /// Delete the metadata at metaname
    fn delete_meta(&mut self, metaname: &str) -> isize {
        let name = CString::new(metaname).unwrap();
        unsafe { elektra_sys::keySetMeta(self.as_ptr(), name.as_ptr(), std::ptr::null()) }
    }

    /// Rewind the internal iterator to first metadata.
    fn rewind_meta(&mut self) {
        unsafe {
            elektra_sys::keyRewindMeta(self.as_ptr());
        }
    }

    /// Returns the value of a meta-information which is current.
    fn current_meta(&self) -> ReadOnlyStringKey {
        let key_ptr = unsafe { elektra_sys::keyCurrentMeta(self.as_ref()) };
        ReadOnlyStringKey::from_ptr(key_ptr as *mut elektra_sys::Key)
    }
}

pub trait ReadableKey {
    fn as_ref(&self) -> &elektra_sys::Key;
    // key methods
    /// Return a duplicate of the key.
    fn duplicate(&self) -> Self
    where
        Self: Sized,
    {
        let dup_ptr = unsafe { elektra_sys::keyDup(self.as_ref()) };
        Self::from_ptr(dup_ptr)
    }

    /// Construct a new key from a raw key pointer
    fn from_ptr(ptr: *mut elektra_sys::Key) -> Self
    where
        Self: Sized;

    // TODO keyCopy?

    // keyname methods

    /// Return the name of the key as a borrowed slice.
    /// # Panics
    /// Panics if the underlying string cannot be converted to UTF-8.
    fn get_name(&self) -> &str {
        let c_str = unsafe { CStr::from_ptr(elektra_sys::keyName(self.as_ref())) };
        c_str.to_str().unwrap()
    }

    /// Return the basename of the key as a borrowed slice.
    /// # Panics
    /// Panics if the underlying string cannot be converted to UTF-8.
    fn get_basename(&self) -> &str {
        let c_str = unsafe { CStr::from_ptr(elektra_sys::keyBaseName(self.as_ref())) };
        c_str.to_str().unwrap()
    }

    /// Calculates number of bytes needed to store basename of key.
    fn get_basename_size(&self) -> isize {
        unsafe { elektra_sys::keyGetBaseNameSize(self.as_ref()) }
    }

    /// Bytes needed to store the key name including user domain and ending NULL.
    fn get_fullname_size(&self) -> usize {
        unsafe {
            elektra_sys::keyGetFullNameSize(self.as_ref())
                .try_into()
                .unwrap()
        }
    }

    /// Get key full name, including the user domain name.
    /// # Panics
    /// Panics if the underlying c_string contains interior nul bytes
    /// or cannot be converted to UTF-8
    fn get_fullname(&self) -> String {
        let mut vec: Vec<u8> = Vec::with_capacity(self.get_fullname_size());

        let ret_val = unsafe {
            elektra_sys::keyGetFullName(
                self.as_ref(),
                vec.as_mut_ptr() as *mut std::os::raw::c_char,
                vec.capacity(),
            )
        };
        unsafe { vec.set_len(ret_val.try_into().unwrap()) };
        CStr::from_bytes_with_nul(&vec)
            .unwrap()
            .to_str()
            .unwrap()
            .to_owned()
    }

    fn get_namespace(&self) -> u32 {
        unsafe { elektra_sys::keyGetNamespace(self.as_ref()) as u32 }
    }

    /// Determines if the key is in the spec namespace
    fn is_spec(&self) -> bool {
        self.get_namespace() == elektra_sys::KEY_NS_SPEC
    }

    /// Determines if the key is in the dir namespace
    fn is_dir(&self) -> bool {
        self.get_namespace() == elektra_sys::KEY_NS_DIR
    }

    /// Determines if the key is in the proc namespace
    fn is_proc(&self) -> bool {
        self.get_namespace() == elektra_sys::KEY_NS_PROC
    }

    /// Determines if the key is in the user namespace
    fn is_user(&self) -> bool {
        self.get_namespace() == elektra_sys::KEY_NS_USER
    }

    /// Determines if the key is in the system namespace
    fn is_system(&self) -> bool {
        self.get_namespace() == elektra_sys::KEY_NS_SYSTEM
    }

    /// Determines if the key is in the dir namespace
    fn is_cascading(&self) -> bool {
        self.get_namespace() == elektra_sys::KEY_NS_CASCADING
    }

    // keyvalue methds

    // Omitted keyValue() due to return value of void pointer, which cannot be used safely in Rust

    /// Returns the number of bytes needed to store the key value, including the
    /// NULL terminator.
    fn get_value_size(&self) -> usize {
        let ret_val = unsafe { elektra_sys::keyGetValueSize(self.as_ref()) };
        // keyGetValueSize returns -1 on null pointers, but we can be sure self.ptr is valid
        // so this conversion is safe
        ret_val.try_into().unwrap()
    }

    /// Returns the string value of the key if the type of the key is string, an error if it's binary.
    /// # Panics
    /// Panics if the underlying string cannot be converted to UTF-8.
    fn get_string(&self) -> Result<&str, KeyError> {
        if self.is_binary() {
            return Err(KeyError::TypeMismatch);
        }
        let c_str = unsafe { CStr::from_ptr(elektra_sys::keyString(self.as_ref())) };
        Ok(c_str.to_str().unwrap())
    }

    // keytest methods

    /// Returns true if the key has a binary value.
    fn is_binary(&self) -> bool {
        unsafe { elektra_sys::keyIsBinary(self.as_ref()) == 1 }
    }

    /// Returns true if the key has a string value.
    fn is_string(&self) -> bool {
        unsafe { elektra_sys::keyIsString(self.as_ref()) == 1 }
    }

    /// Returns true if other is below self
    /// # Examples
    /// ```
    /// use elektra::{StringKey,WriteableKey,ReadableKey};
    /// let key = StringKey::new("user/sw/app").unwrap();
    /// let key2 = StringKey::new("user/sw/app/folder/key").unwrap();
    /// assert!(key2.is_below(&key));
    /// ```
    fn is_below(&self, other: &Self) -> bool {
        unsafe { elektra_sys::keyIsBelow(other.as_ref(), self.as_ref()) == 1 }
    }

    /// Returns true if other is *directly* below self
    /// # Examples
    /// ```
    /// use elektra::{StringKey,WriteableKey,ReadableKey};
    /// let key = StringKey::new("user/sw/app").unwrap();
    /// let key2 = StringKey::new("user/sw/app/key").unwrap();
    /// assert!(key2.is_direct_below(&key));
    /// ```
    fn is_direct_below(&self, other: &Self) -> bool {
        unsafe { elektra_sys::keyIsDirectBelow(other.as_ref(), self.as_ref()) == 1 }
    }

    /// Returns true if the key is inactive.
    /// In Elektra terminology a hierarchy of keys is inactive if the
    /// rootkey's basename starts with '.'. So a key is also inactive
    /// if it is below an inactive key. For example, `user/key/.hidden`
    /// is inactive and so is `user/.hidden/below`.
    fn is_inactive(&self) -> bool {
        unsafe { elektra_sys::keyIsInactive(self.as_ref()) == 1 }
    }

    // keymeta methods
    /// Returns the metadata with the given metaname
    fn get_meta(&self, metaname: &str) -> Result<ReadOnlyStringKey, KeyError>
    where
        Self: Sized,
    {
        let cstr = CString::new(metaname).unwrap();
        let key_ptr = unsafe { elektra_sys::keyGetMeta(self.as_ref(), cstr.as_ptr()) };
        if key_ptr == std::ptr::null() {
            Err(KeyError::NotFound)
        } else {
            let key = ReadOnlyStringKey::from_ptr(key_ptr as *mut elektra_sys::Key);
            Ok(key)
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn can_write_read_key() {
        let key_name = "user/test/key";
        let key = StringKey::new(key_name).unwrap();
        assert_eq!(key.get_name(), key_name);
    }

    #[test]
    fn can_write_read_key_value() {
        let key_name = "user/test/key";
        let utf8_value = "😃";
        let mut key: StringKey = StringKey::new(key_name).unwrap();
        key.set_string(utf8_value);
        assert_eq!(key.get_name(), key_name);
        assert_eq!(key.get_string(), utf8_value);
    }

    #[test]
    fn can_duplicate_key() {
        let key_name = "user/test/key";
        let mut key_dup;
        {
            let key = StringKey::new(key_name).unwrap();
            key_dup = Some(key.duplicate());
            // key is dropped here
        }
        assert_eq!(key_dup.unwrap().get_name(), key_name);
    }

    #[test]
    fn can_write_read_binary() {
        let mut key = BinaryKey::new("user/test/rust").unwrap();
        let binary_content: [u8; 7] = [25, 34, 0, 254, 1, 0, 7];
        key.set_binary(&binary_content);
        let read_content = key.get_binary();
        assert_eq!(read_content, binary_content);
    }
    #[test]
    fn can_write_read_empty_binary() {
        let mut key = BinaryKey::new("user/test/binary").unwrap();
        let binary_content: [u8; 0] = [];
        key.set_binary(&binary_content);
        // set_binary does not set binary flag, size is 0
        // so get_binary should return empty vec
        let err = key.get_binary();
        assert_eq!(err, binary_content);
    }

    #[test]
    fn equality_is_exclusive() {
        let key = BinaryKey::new("user/test/exclusive").unwrap();
        let key2 = BinaryKey::new("dir/test/exclusive").unwrap();

        assert!(!(key != key));
        assert!(key == key);

        assert!(!(key == key2));
        assert!(key != key2);
    }

    #[test]
    fn equality_is_reflexive() {
        let key = StringKey::new("user/test/reflexive").unwrap();
        assert!(key == key);
    }

    #[test]
    fn equality_is_symmetric() {
        let key = BinaryKey::new("user/test/symmetric").unwrap();
        let key_dup = BinaryKey::new("user/test/symmetric").unwrap();

        assert!(key_dup == key);
        assert!(key == key_dup);
    }

    #[test]
    fn equality_is_transitive() {
        let key = BinaryKey::new("user/test/transitive").unwrap();
        let key2 = BinaryKey::new("user/test/transitive").unwrap();
        let key3 = BinaryKey::new("user/test/transitive").unwrap();

        assert!(key == key2);
        assert!(key2 == key3);
        assert!(key == key3);
    }

    #[test]
    fn keys_are_ordered() {
        let key = BinaryKey::new("user/test/a").unwrap();
        let key2 = BinaryKey::new("user/test/b").unwrap();
        let key3 = BinaryKey::new("user/test/c").unwrap();

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
    fn keys_are_ordered_with_metadata() {
        // TODO Add once it's possible to add metadata
        //Key * k1 = keyNew("user/a", KEY_OWNER, "markus", KEY_END);
        //Key * k2 = keyNew("user/a", KEY_OWNER, "max", KEY_END);
        // keyCmp(k1,k2) < 0
        // keyCmp(k2,k1) > 0
        assert!(5 + 2 == 7);
    }

    #[test]
    fn can_reference_count() {
        let mut key = BinaryKey::new("user/test/a").unwrap();
        assert_eq!(key.get_ref(), 0);
        key.inc_ref();
        assert_eq!(key.get_ref(), 1);
        key.dec_ref();
        assert_eq!(key.get_ref(), 0);
    }

    #[test]
    fn error_on_missing_metaname() {
        let key = StringKey::new("user/test/metatest").unwrap();
        assert!(key.get_meta("nonexistent metaname").is_err());
    }
    #[test]
    fn can_set_get_metavalue() {
        let mut key = StringKey::new_empty();
        key.set_meta("metakey", "metaval");
        assert_eq!(key.get_meta("metakey").unwrap().get_string(), "metaval");
    }

    #[test]
    fn can_iterate_key() {
        let mut key = StringKey::new_empty();
        let meta = [("meta1", "val1"), ("meta2", "val2")];
        key.set_meta(meta[0].0, meta[0].1);
        key.set_meta(meta[1].0, meta[1].1);
        key.rewind_meta();

        let mut did_iterate = false;
        for (i, metakey) in key.enumerate() {
            did_iterate = true;
            assert_eq!(metakey.get_name(), meta[i].0);
            assert_eq!(metakey.get_string(), meta[i].1);
        }
        assert!(did_iterate);
    }

    #[test]
    fn can_delete_metadata() {
        let mut key = StringKey::new_empty();
        key.set_meta("metakey", "metaval");
        assert_eq!(key.delete_meta("metakey"), 0);
        assert_eq!(key.get_meta("metakey").unwrap_err(), KeyError::NotFound);
    }
}
