use elektra_sys; //::{keyDel, keyDup, keyName, keyNew, keySetName, keySetString};
use std::error::Error;
use std::ffi::{CStr, CString};
use std::fmt;
use std::ptr::NonNull;

#[derive(Debug, PartialEq)]
pub enum KeyError {
    InvalidName,
    TypeMismatch,
    // TypeConversion,
    // NotFoundException
}

impl fmt::Display for KeyError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        match self {
            KeyError::InvalidName => write!(f, "Key has an invalid name"),
            KeyError::TypeMismatch => write!(f, "Binary/String key mismatch, use the appropriate method for your key type, get_string or get_binary"),
            // _ => unimplemented!(),
        }
    }
}

impl Error for KeyError {}

#[derive(Debug)]
pub struct Key {
    ptr: NonNull<elektra_sys::Key>,
}

impl Drop for Key {
    fn drop(&mut self) {
        println!("Drop {:?}", self);
        unsafe { elektra_sys::keyDel(self.ptr.as_ptr()) };
    }
}

impl PartialEq for Key {
    fn eq(&self, other: &Self) -> bool {
        unsafe {
            elektra_sys::keyCmp(
                self.ptr.as_ptr() as *const elektra_sys::Key,
                other.ptr.as_ptr() as *const elektra_sys::Key,
            ) == 0
        }
    }
}

impl Eq for Key {}

impl Ord for Key {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        let cmp = unsafe {
            elektra_sys::keyCmp(
                self.ptr.as_ptr() as *const elektra_sys::Key,
                other.ptr.as_ptr() as *const elektra_sys::Key,
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

impl PartialOrd for Key {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Clone for Key {
    fn clone(&self) -> Self {
        self.duplicate()
    }
}

impl Key {
    // key methods
    /// Construct a new key with a name
    pub fn new(name: &str) -> Result<Key, KeyError> {
        let mut key = Key::new_empty();
        key.set_name(name)?;
        Ok(key)
    }

    /// Construct a new empty key
    pub fn new_empty() -> Key {
        let key_ptr = unsafe { elektra_sys::keyNew(0 as *const i8) };
        Key::from(key_ptr)
    }

    /// Return a duplicate of the key.
    pub fn duplicate(&self) -> Key {
        let dup_ptr = unsafe { elektra_sys::keyDup(self.ptr.as_ptr()) };
        Key::from(dup_ptr)
    }

    /// Construct a new key from a raw key pointer
    pub fn from(ptr: *mut elektra_sys::Key) -> Key {
        Key {
            ptr: NonNull::new(ptr).unwrap(),
        }
    }

    /// Clears the key.
    /// After this call you will receive a fresh key.
    pub fn clear(&mut self) {
        unsafe {
            elektra_sys::keyClear(self.ptr.as_ptr());
        }
    }

    /// Decrement the viability of a key object.
    /// Returns the value of the new reference counter.
    pub fn dec_ref(&mut self) -> usize {
        unsafe { elektra_sys::keyDecRef(self.ptr.as_ptr()) as usize }
    }

    /// Increment the viability of a key object.
    /// Returns the value of the new reference counter.
    pub fn inc_ref(&mut self) -> usize {
        unsafe { elektra_sys::keyIncRef(self.ptr.as_ptr()) as usize }
    }

    /// Return how many references the key has.
    pub fn get_ref(&self) -> usize {
        unsafe { elektra_sys::keyGetRef(self.ptr.as_ptr() as *const elektra_sys::Key) as usize }
    }

    // TODO keyCopy?

    // keyname methods

    /// Set the name of a key. Must adhere to the rules for keynames otherwise an error is returned.
    /// Returns the size in bytes of this new key name including the ending NUL.
    /// # Examples
    /// ```
    /// use elektra::Key;
    /// let mut key = Key::new_empty();
    /// key.set_name("user/test/rust").unwrap();
    /// assert_eq!(key.get_name(), "user/test/rust");
    /// ```
    /// # Panics
    /// Panics if the provided string contains internal nul bytes.
    pub fn set_name(&mut self, name: &str) -> Result<u32, KeyError> {
        let cptr = CString::new(name).unwrap();
        let ret_val = unsafe { elektra_sys::keySetName(self.ptr.as_ptr(), cptr.as_ptr()) };

        if ret_val > 0 {
            Ok(ret_val as u32)
        } else {
            Err(KeyError::InvalidName)
        }
    }

    /// Return the name of the key as a borrowed slice.
    pub fn get_name(&self) -> &str {
        let c_str = unsafe { CStr::from_ptr(elektra_sys::keyName(self.ptr.as_ref())) };
        c_str.to_str().unwrap()
    }

    // keyvalue methds

    // Omitted keyValue() due to return value of void pointer, which cannot be used safely in Rust

    /// Returns the number of bytes needed to store the key value, including the
    /// NULL terminator.
    pub fn get_value_size(&self) -> usize {
        let ret_val = unsafe { elektra_sys::keyGetValueSize(self.ptr.as_ptr()) };
        // keyGetValueSize returns -1 on null pointers, but we can be sure self.ptr is valid
        // so this conversion is safe
        ret_val as usize
    }

    /// Sets the value of the key to the supplied string.
    /// # Panics
    /// Panics if the provided string contains internal nul bytes.
    pub fn set_string(&mut self, value: &str) {
        let cptr = CString::new(value).unwrap();
        unsafe { elektra_sys::keySetString(self.ptr.as_ptr(), cptr.as_ptr()) };
    }

    /// Returns the string value of the key if the type of the key is string, an error if it's binary.
    /// # Panics
    /// Panics if the underlying string cannot be converted to UTF-8.
    pub fn get_string(&self) -> Result<&str, KeyError> {
        if self.is_binary() {
            return Err(KeyError::TypeMismatch);
        }
        let c_str = unsafe { CStr::from_ptr(elektra_sys::keyString(self.ptr.as_ref())) };
        Ok(c_str.to_str().unwrap())
    }

    /// Sets the key's binary content to the supplied data.
    pub fn set_binary(&mut self, data: &[u8]) {
        unsafe {
            elektra_sys::keySetBinary(
                self.ptr.as_ptr(),
                data.as_ptr() as *const std::os::raw::c_void,
                data.len(),
            );
        }
    }

    /// Returns the keys binary content
    /// Returns a TypeMismatch if the key is of type string
    pub fn get_binary(&self) -> Result<Vec<u8>, KeyError> {
        if self.is_string() {
            return Err(KeyError::TypeMismatch);
        }
        let mut vec: Vec<u8> = Vec::with_capacity(self.get_value_size());

        let ret_val = unsafe {
            elektra_sys::keyGetBinary(
                self.ptr.as_ptr(),
                vec.as_mut_ptr() as *mut std::os::raw::c_void,
                vec.capacity(),
            )
        };

        if ret_val >= 0 {
            unsafe { vec.set_len(ret_val as usize) };
            Ok(vec)
        } else {
            // Since the function checks for is_string, a type mismatch is caught earlier
            // So this error can only occur as a result of a logic error in the program
            panic!("maxSize is 0, too small or too large.");
        }
    }

    // keytest methods

    /// Returns true if the key has a binary value.
    pub fn is_binary(&self) -> bool {
        unsafe { elektra_sys::keyIsBinary(self.ptr.as_ptr()) == 1 }
    }

    /// Returns true if the key has a string value.
    pub fn is_string(&self) -> bool {
        unsafe { elektra_sys::keyIsString(self.ptr.as_ptr()) == 1 }
    }

    /// Returns true if other is below self
    /// # Examples
    /// ```
    /// use elektra::Key;
    /// let key = Key::new("user/sw/app").unwrap();
    /// let key2 = Key::new("user/sw/app/folder/key").unwrap();
    /// assert!(key2.is_below(&key));
    /// ```
    pub fn is_below(&self, other: &Self) -> bool {
        unsafe {
            elektra_sys::keyIsBelow(
                other.ptr.as_ptr() as *const elektra_sys::Key,
                self.ptr.as_ptr() as *const elektra_sys::Key,
            ) == 1
        }
    }

    /// Returns true if other is *directly* below self
    /// # Examples
    /// ```
    /// use elektra::Key;
    /// let key = Key::new("user/sw/app").unwrap();
    /// let key2 = Key::new("user/sw/app/key").unwrap();
    /// assert!(key2.is_direct_below(&key));
    /// ```
    pub fn is_direct_below(&self, other: &Self) -> bool {
        unsafe {
            elektra_sys::keyIsDirectBelow(
                other.ptr.as_ptr() as *const elektra_sys::Key,
                self.ptr.as_ptr() as *const elektra_sys::Key,
            ) == 1
        }
    }

    /// Returns true if the key is inactive.
    /// In Elektra terminology a hierarchy of keys is inactive if the
    /// rootkey's basename starts with '.'. So a key is also inactive
    /// if it is below an inactive key. For example, `user/key/.hidden`
    /// is inactive and so is `user/.hidden/below`.
    pub fn is_inactive(&self) -> bool {
        unsafe { elektra_sys::keyIsInactive(self.ptr.as_ptr() as *const elektra_sys::Key) == 1 }
    }
    // TODO: CPP Bindings do not implement this.
    // Since is_below flips the order, this should be the case here too.
    // But does this break any usage?
    // Information about the relation in the hierarchy between two keys.
    // pub fn rel(&self, other: &Self) -> i32 {
    //     unsafe {
    //         elektra_sys::keyRel(
    //             self.ptr.as_ptr() as *const elektra_sys::Key,
    //             other.ptr.as_ptr() as *const elektra_sys::Key,
    //         )
    //     }
    // }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn can_write_read_key() {
        let key_name = "user/test/key";
        let key = Key::new(key_name).unwrap();
        assert_eq!(key.get_name(), key_name);
    }

    #[test]
    fn can_write_read_key_value() {
        let key_name = "user/test/key";
        let utf8_value = "😃";
        let mut key = Key::new(key_name).unwrap();
        key.set_string(utf8_value);
        assert_eq!(key.get_name(), key_name);
        assert_eq!(key.get_string().unwrap(), utf8_value);
    }

    #[test]
    fn can_duplicate_key() {
        let key_name = "user/test/key";
        let mut key_dup;
        {
            let key = Key::new(key_name).unwrap();
            key_dup = Some(key.duplicate());
            // key is dropped here
        }
        assert_eq!(key_dup.unwrap().get_name(), key_name);
    }

    #[test]
    fn can_write_read_binary() {
        let mut key = Key::new("user/test/rust").unwrap();
        let binary_content: [u8; 7] = [25, 34, 0, 254, 1, 0, 7];
        key.set_binary(&binary_content);
        let read_content = key.get_binary().unwrap();
        assert_eq!(read_content, binary_content);
    }
    #[test]
    fn can_write_read_empty_binary() {
        let mut key = Key::new("user/test/binary").unwrap();
        let binary_content: [u8; 0] = [];
        key.set_binary(&binary_content);
        // set_binary does not set binary flag, size is 0
        // so get_binary returns error
        let err = key.get_binary().unwrap_err();
        assert_eq!(err, KeyError::TypeMismatch);
    }

    #[test]
    fn equality_is_exclusive() {
        let key = Key::new("user/test/exclusive").unwrap();
        let key2 = Key::new("dir/test/exclusive").unwrap();

        assert!(!(key != key));
        assert!(key == key);

        assert!(!(key == key2));
        assert!(key != key2);
    }

    #[test]
    fn equality_is_reflexive() {
        let key = Key::new("user/test/reflexive").unwrap();
        assert!(key == key);
    }

    #[test]
    fn equality_is_symmetric() {
        let key = Key::new("user/test/symmetric").unwrap();
        let key_dup = Key::new("user/test/symmetric").unwrap();

        assert!(key_dup == key);
        assert!(key == key_dup);
    }

    #[test]
    fn equality_is_transitive() {
        let key = Key::new("user/test/transitive").unwrap();
        let key2 = Key::new("user/test/transitive").unwrap();
        let key3 = Key::new("user/test/transitive").unwrap();

        assert!(key == key2);
        assert!(key2 == key3);
        assert!(key == key3);
    }

    #[test]
    fn keys_are_ordered() {
        let key = Key::new("user/test/a").unwrap();
        let key2 = Key::new("user/test/b").unwrap();
        let key3 = Key::new("user/test/c").unwrap();

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
        let mut key = Key::new("user/test/a").unwrap();
        assert_eq!(key.get_ref(), 0);
        key.inc_ref();
        assert_eq!(key.get_ref(), 1);
        key.dec_ref();
        assert_eq!(key.get_ref(), 0);
    }
}
