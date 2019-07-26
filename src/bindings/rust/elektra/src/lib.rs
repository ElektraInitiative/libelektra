extern crate elektra_sys;

use elektra_sys::*;
use std::boxed::Box;
use std::error::Error;
use std::ffi::{CStr, CString};
use std::fmt;

trait KeyBuilder {
    fn new() -> Self;
    fn name<T: Into<Vec<u8>>>(&mut self, name: T) -> Self;
    fn build(self) -> Key;
}

#[derive(Debug)]
enum KeyError {
    InvalidName,
    // TypeMismatch,
    // TypeConversion,
    // NotFoundException
}

impl fmt::Display for KeyError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        match self {
            KeyError::InvalidName => write!(f, "Key has an invalid name"),
            // _ => unimplemented!(),
        }
    }
}

impl Error for KeyError {}

trait KeyProperties {
    fn new() -> Box<Key>;
    fn set_name(&mut self, name: &str) -> Result<u32, KeyError>;
    fn name(&self) -> &str;
    fn duplicate(&self) -> Box<Key>;
}

impl KeyProperties for Key {
    /// Initializes an empty key object
    fn new() -> Box<Key> {
        unsafe { Box::from_raw(keyNew(0 as *const i8)) }
    }

    /// Set the name of a key
    /// # Examples
    /// ```
    /// let key = Key::new().name("user/test/rust");
    /// assert_eq!(key.get_name(), "user/test/rust");
    /// ```
    fn set_name(&mut self, name: &str) -> Result<u32, KeyError> {
        let cptr = CString::new(name).unwrap();
        let ret_val = unsafe { keySetName(self, cptr.as_ptr()) };

        if ret_val > 0 {
            Ok(ret_val as u32)
        } else {
            Err(KeyError::InvalidName)
        }
    }

    /// Return the name of the key as a borrowed slice
    fn name(&self) -> &str {
        let c_str = unsafe { CStr::from_ptr(keyName(self)) };
        c_str.to_str().unwrap()
    }

    /// Return a duplicate of a key
    fn duplicate(&self) -> Box<Key> {
        unsafe { Box::from_raw(keyDup(self)) }
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
        assert_eq!(key.name(), key_name);
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
        assert_eq!(key_dup.unwrap().name(), key_name);
    }
}
