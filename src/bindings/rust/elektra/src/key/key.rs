use elektra_sys::{keyDel, keyDup, keyName, keyNew, keySetName, keySetString};
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
        println!("Drop Key {:?}", self);
        unsafe { keyDel(self.ptr.as_ptr()) };
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

impl Key {
    /// Construct a new empty key
    pub fn new() -> Key {
        let key_ptr = unsafe { keyNew(0 as *const i8) };
        Key::from(key_ptr)
    }

    /// Construct a new key from a raw key pointer
    pub fn from(ptr: *mut elektra_sys::Key) -> Key {
        Key {
            ptr: NonNull::new(ptr).unwrap(),
        }
    }

    /// Set the name of a key. Must adhere to the rules for keynames otherwise an error is returned.
    /// Returns the size in bytes of this new key name including the ending NUL.
    /// # Examples
    /// ```
    /// use elektra::Key;
    /// let mut key = Key::new();
    /// key.set_name("user/test/rust").unwrap();
    /// assert_eq!(key.get_name(), "user/test/rust");
    /// ```
    /// # Panics
    /// Panics if the provided string contains internal nul bytes.
    pub fn set_name(&mut self, name: &str) -> Result<u32, KeyError> {
        let cptr = CString::new(name).unwrap();
        let ret_val = unsafe { keySetName(self.ptr.as_ptr(), cptr.as_ptr()) };

        if ret_val > 0 {
            Ok(ret_val as u32)
        } else {
            Err(KeyError::InvalidName)
        }
    }

    /// Return the name of the key as a borrowed slice.
    pub fn get_name(&self) -> &str {
        let c_str = unsafe { CStr::from_ptr(keyName(self.ptr.as_ref())) };
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
        unsafe { keySetString(self.ptr.as_ptr(), cptr.as_ptr()) };
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

    /// Return a duplicate of the key.
    pub fn duplicate(&self) -> Key {
        let dup_ptr = unsafe { keyDup(self.ptr.as_ptr()) };
        Key::from(dup_ptr)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn can_write_read_key() {
        let key_name = "user/test/key";
        let mut key = Key::new();
        key.set_name(key_name).unwrap();
        assert_eq!(key.get_name(), key_name);
    }

    #[test]
    fn can_write_read_key_value() {
        let key_name = "user/test/key";
        let utf8_value = "😃";
        let mut key = Key::new();
        key.set_name(key_name).unwrap();
        key.set_string(utf8_value);
        assert_eq!(key.get_name(), key_name);
        assert_eq!(key.get_string().unwrap(), utf8_value);
    }

    #[test]
    fn can_duplicate_key() {
        let key_name = "user/test/key";
        let mut key_dup;
        {
            let mut key = Key::new();
            key.set_name(key_name).unwrap();
            key_dup = Some(key.duplicate());
            // key is dropped here
        }
        assert_eq!(key_dup.unwrap().get_name(), key_name);
    }

    #[test]
    fn can_write_read_binary() {
        let mut key = Key::new();
        let binary_content: [u8; 7] = [25, 34, 0, 254, 1, 0, 7];
        key.set_name("user/test/rust").unwrap();
        key.set_binary(&binary_content);
        let read_content = key.get_binary().unwrap();
        assert_eq!(read_content, binary_content);
    }
    #[test]
    fn can_write_read_empty_binary() {
        let mut key = Key::new();
        let binary_content: [u8; 0] = [];
        key.set_name("user/test/binary").unwrap();
        key.set_binary(&binary_content);
        // set_binary does not set binary flag, size is 0
        // so get_binary returns error
        let err = key.get_binary().unwrap_err();
        assert_eq!(err, KeyError::TypeMismatch);
    }

    #[test]
    fn equality_is_exclusive() {
        let mut key = Key::new();
        key.set_name("user/test/exclusive").unwrap();
        let mut key2 = Key::new();
        key2.set_name("dir/test/exclusive").unwrap();

        assert!(!(key != key));
        assert!(key == key);

        assert!(!(key == key2));
        assert!(key != key2);
    }

    #[test]
    fn equality_is_reflexive() {
        let mut key = Key::new();
        key.set_name("user/test/reflexive").unwrap();
        assert!(key == key);
    }

    #[test]
    fn equality_is_symmetric() {
        let mut key = Key::new();
        key.set_name("user/test/symmetric").unwrap();
        let mut key_dup = Key::new();
        key_dup.set_name("user/test/symmetric").unwrap();

        assert!(key_dup == key);
        assert!(key == key_dup);
    }

    #[test]
    fn equality_is_transitive() {
        let mut key = Key::new();
        key.set_name("user/test/transitive").unwrap();

        let mut key2 = Key::new();
        key2.set_name("user/test/transitive").unwrap();

        let mut key3 = Key::new();
        key3.set_name("user/test/transitive").unwrap();
        assert!(key == key2);
        assert!(key2 == key3);
        assert!(key == key3);
    }

    #[test]
    fn keys_are_ordered() {
        let mut key = Key::new();
        key.set_name("user/test/a").unwrap();

        let mut key2 = Key::new();
        key2.set_name("user/test/b").unwrap();

        let mut key3 = Key::new();
        key3.set_name("user/test/c").unwrap();

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
}
