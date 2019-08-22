use crate::{KeyError, ReadableKey};
use std::ffi::CString;
use std::convert::TryInto;

pub trait WriteableKey: ReadableKey {
    fn as_ptr(&mut self) -> *mut elektra_sys::Key;
    fn set_value<T: Into<Vec<u8>>>(&mut self, t: T)
    where
        Self: Sized;

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
        let key_ptr = unsafe { elektra_sys::keyNew(std::ptr::null()) };
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

    /// Set the name of a key. Must adhere to the rules for keynames otherwise an error is returned.
    /// Returns the size in bytes of this new key name including the ending NUL.
    /// 
    /// # Examples
    /// ```
    /// use elektra::{StringKey,WriteableKey,ReadableKey};
    /// let mut key = StringKey::new_empty();
    /// key.set_name("user/test/rust").unwrap();
    /// assert_eq!(key.name(), "user/test/rust");
    /// ```
    fn set_name(&mut self, name: &str) -> Result<u32, KeyError> {
        let cptr = unsafe { CString::from_vec_unchecked(name.as_bytes().to_vec()) };
        let ret_val = unsafe { elektra_sys::keySetName(self.as_ptr(), cptr.as_ptr()) };

        if ret_val > 0 {
            Ok(ret_val as u32)
        } else {
            // TODO: May also be a NameReadOnly Error...
            Err(KeyError::InvalidName)
        }
    }
    /// Set the basename of the key
    /// 
    /// # Errors
    /// Returns a `KeyError::NameReadOnly` if the key is part of a keyset.
    /// 
    /// # Examples
    /// ```
    /// # use std::error::Error;
    /// # use elektra::{StringKey,WriteableKey,ReadableKey};
    /// # fn main() -> Result<(), Box<dyn Error>> {
    /// let mut key = StringKey::new("user/test/key")?;
    /// key.set_basename("rust")?;
    /// assert_eq!(key.name(), "user/test/rust");
    /// #
    /// #     Ok(())
    /// # }
    /// ```
    fn set_basename(&mut self, basename: &str) -> Result<(), KeyError> {
        let cstr = unsafe { CString::from_vec_unchecked(basename.as_bytes().to_vec()) };
        let ret_val = unsafe { elektra_sys::keySetBaseName(self.as_ptr(), cstr.as_ptr()) };
        if ret_val == -1 {
            Err(KeyError::NameReadOnly)
        } else {
            Ok(())
        }
    }

    /// Add a basename to the key
    /// 
    /// # Errors
    /// Returns a `KeyError::NameReadOnly` if the key is part of a keyset.
    /// 
    /// # Examples
    /// ```
    /// # use std::error::Error;
    /// # use elektra::{StringKey,WriteableKey,ReadableKey};
    /// # fn main() -> Result<(), Box<dyn Error>> {
    /// let mut key = StringKey::new("user/test/key")?;
    /// key.add_basename("rust")?;
    /// assert_eq!(key.name(), "user/test/key/rust");
    /// #
    /// #     Ok(())
    /// # }
    /// ```
    fn add_basename(&mut self, basename: &str) -> Result<(), KeyError> {
        let cstr = unsafe { CString::from_vec_unchecked(basename.as_bytes().to_vec()) };
        let ret_val = unsafe { elektra_sys::keyAddBaseName(self.as_ptr(), cstr.as_ptr()) };
        if ret_val == -1 {
            Err(KeyError::NameReadOnly)
        } else {
            Ok(())
        }
    }

    /// Add an already escaped name to the keyname.
    /// 
    /// # Examples
    /// ```
    /// # use std::error::Error;
    /// # use elektra::{StringKey,WriteableKey,ReadableKey};
    /// # fn main() -> Result<(), Box<dyn Error>> {
    /// let mut key = StringKey::new("user/x/r").unwrap();
    /// key.add_name("../y/a//././z").unwrap();
    /// assert_eq!(key.name(), "user/x/y/a/z");
    /// #
    /// #     Ok(())
    /// # }
    /// ```
    fn add_name(&mut self, name: &str) -> Result<(), KeyError> {
        let cstr = unsafe { CString::from_vec_unchecked(name.as_bytes().to_vec()) };
        let ret_val = unsafe { elektra_sys::keyAddName(self.as_ptr(), cstr.as_ptr()) };
        if ret_val <= 0 {
            Err(KeyError::InvalidName)
        } else {
            Ok(())
        }
    }

    /// Copies all metadata from source to the self
    /// 
    /// # Examples
    /// ```
    /// # use std::error::Error;
    /// # use elektra::{StringKey,WriteableKey,ReadableKey};
    /// # fn main() -> Result<(), Box<dyn Error>> {
    /// let mut key = StringKey::new("user/test/mykey")?;
    /// let mut key2 = StringKey::new("user/test/mykey")?;
    /// key.set_meta("rusty", "metal");
    /// key2.copy_all_meta(&key);
    /// assert_eq!(key.meta("rusty")?.value(), "metal");
    /// #
    /// #     Ok(())
    /// # }
    /// ```
    fn copy_all_meta(&mut self, source: &Self)
    where
        Self: Sized,
    {
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
    /// assert_eq!(key2.meta("meta").unwrap().value(), "value");
    /// ```
    fn copy_meta(&mut self, source: &Self, metaname: &str) -> i32
    where
        Self: Sized,
    {
        let cstr = unsafe { CString::from_vec_unchecked(metaname.as_bytes().to_vec()) };
        unsafe { elektra_sys::keyCopyMeta(self.as_ptr(), source.as_ref(), cstr.as_ptr()) }
    }

    /// Set a new meta-information.
    /// Returns the size of the new meta information on success,
    /// or a `KeyError::InvalidName` if the name is invalid or out of memory.
    fn set_meta(&mut self, metaname: &str, metavalue: &str) -> Result<usize, KeyError> {
        let name = unsafe { CString::from_vec_unchecked(metaname.as_bytes().to_vec()) };
        let value = unsafe { CString::from_vec_unchecked(metavalue.as_bytes().to_vec()) };
        let ret_val = unsafe { elektra_sys::keySetMeta(self.as_ptr(), name.as_ptr(), value.as_ptr()) };
        if ret_val < 0 {
            Err(KeyError::InvalidName)
        } else {
            Ok(ret_val.try_into().unwrap())
        }
    }

    /// Delete the metadata at metaname
    /// Returns the size of the new meta information on success,
    /// or a `KeyError::InvalidName` if the name is invalid or out of memory.
    fn delete_meta(&mut self, metaname: &str) -> Result<usize, KeyError> {
        let name = unsafe { CString::from_vec_unchecked(metaname.as_bytes().to_vec()) };
        let ret_val = unsafe { elektra_sys::keySetMeta(self.as_ptr(), name.as_ptr(), std::ptr::null()) };
        if ret_val < 0 {
            Err(KeyError::InvalidName)
        } else {
            Ok(ret_val.try_into().unwrap())
        }
    }

    /// Rewind the internal iterator to first metadata.
    fn rewind_meta(&mut self) {
        unsafe {
            elektra_sys::keyRewindMeta(self.as_ptr());
        }
    }
}
