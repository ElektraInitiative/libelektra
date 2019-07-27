extern crate elektra_sys;

use elektra_sys::{keyDel, keyDup, keyName, keyNew, keySetName};
use std::error::Error;
use std::ffi::{CStr, CString};
use std::fmt;
use std::ptr::NonNull;

trait KeyBuilder {
    fn new() -> Self;
    fn name<T: Into<Vec<u8>>>(&mut self, name: T) -> Self;
    fn build(self) -> Key;
}

#[derive(Debug)]
pub enum KeyError {
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

impl Key {
    /// Construct a new empty key
    pub fn new() -> Key {
        let key_ptr = unsafe { keyNew(0 as *const i8) };
        Key::from(key_ptr)
    }

    /// Construct a new key from a raw key pointer
    fn from(ptr: *mut elektra_sys::Key) -> Key {
        Key {
            ptr: NonNull::new(ptr).unwrap(),
        }
    }

    /// Set the name of a key
    /// # Examples
    /// ```
    /// use elektra::Key;
    /// let mut key = Key::new();
    /// key.set_name("user/test/rust").unwrap();
    /// assert_eq!(key.name(), "user/test/rust");
    /// ```
    pub fn set_name(&mut self, name: &str) -> Result<u32, KeyError> {
        let cptr = CString::new(name).unwrap();
        let ret_val = unsafe { keySetName(self.ptr.as_ptr(), cptr.as_ptr()) };

        if ret_val > 0 {
            Ok(ret_val as u32)
        } else {
            Err(KeyError::InvalidName)
        }
    }

    /// Return the name of the key as a borrowed slice
    pub fn name(&self) -> &str {
        let c_str = unsafe { CStr::from_ptr(keyName(self.ptr.as_ref())) };
        c_str.to_str().unwrap()
    }

    /// Return a duplicate of a key
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
