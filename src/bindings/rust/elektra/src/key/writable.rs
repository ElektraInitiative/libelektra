use crate::{KeyError,StringKey,ReadOnly,ReadableKey};
use std::ffi::CString;

pub trait WriteableKey: ReadableKey {
    fn as_ptr(&mut self) -> *mut elektra_sys::Key;
    fn set_value<T: Into<Vec<u8>>>(&mut self, t: T);

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
    /// assert_eq!(key2.get_meta("meta").unwrap().get_value(), "value");
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
    fn current_meta(&self) -> ReadOnly<StringKey> {
        let key_ptr = unsafe { elektra_sys::keyCurrentMeta(self.as_ref()) };
        ReadOnly::from_ptr(key_ptr as *mut elektra_sys::Key)
    }
}
