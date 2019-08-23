use crate::{KeyError, ReadableKey};
use std::convert::TryInto;
use std::ffi::CString;

pub trait WriteableKey: ReadableKey {
    /// The value that is passed to set_value.
    type SetValue;

    /// Returns the raw pointer of the key.
    /// Should be used with caution. In particular,
    /// `keyDel` should not be called and the pointer
    /// should not be modified with anything other than
    /// `elektra_sys::key*` functions.
    /// 
    /// You can use it to call functions in the raw bindings
    /// that modify the key, if the safe API doesn't fulfill your usecase.
    /// 
    /// # Examples
    /// ```
    /// # use elektra::{StringKey,WriteableKey,ReadableKey};
    /// # use elektra_sys;
    /// # use std::ffi::CString;
    /// # fn main() -> Result<(), Box<dyn std::error::Error>> {
    /// let mut key = StringKey::new("user/test/key")?;
    /// let cstr = CString::new("newbasename").unwrap();
    /// let ret_val = unsafe { elektra_sys::keySetBaseName(key.as_ptr(), cstr.as_ptr()) };
    /// assert_eq!(key.name(), "user/test/newbasename");
    /// #
    /// #     Ok(())
    /// # }
    /// ```
    fn as_ptr(&mut self) -> *mut elektra_sys::Key;

    /// Set the value of the key.
    fn set_value(&mut self, value: Self::SetValue)
    where
        Self: Sized;

    /// Construct a new key with a name.
    ///
    /// # Panics
    /// Panics when an allocation error (out of memory) in the C-constructor occurs.
    fn new(name: &str) -> Result<Self, KeyError>
    where
        Self: Sized,
    {
        let mut key = Self::new_empty();
        key.set_name(name)?;
        Ok(key)
    }

    /// Construct a new nameless key.
    /// 
    /// # Panics
    /// Panics when an allocation error (out of memory) in the C-constructor occurs.
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
    /// # use elektra::{StringKey,WriteableKey,ReadableKey};
    /// # fn main() -> Result<(), Box<dyn std::error::Error>> {
    /// let mut key = StringKey::new_empty();
    /// key.set_name("user/test/rust")?;
    /// assert_eq!(key.name(), "user/test/rust");
    /// #
    /// #     Ok(())
    /// # }
    /// ```
    /// 
    /// # Panics
    /// Panics if the provided string contains interior nul bytes.
    fn set_name(&mut self, name: &str) -> Result<u32, KeyError> {
        let cstr = CString::new(name).unwrap();
        let ret_val = unsafe { elektra_sys::keySetName(self.as_ptr(), cstr.as_ptr()) };

        if ret_val > 0 {
            Ok(ret_val as u32)
        } else {
            // This error is either InvalidName or NameReadOnly, but only one can be returned.
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
    /// # use elektra::{StringKey,WriteableKey,ReadableKey};
    /// # fn main() -> Result<(), Box<dyn std::error::Error>> {
    /// let mut key = StringKey::new("user/test/key")?;
    /// key.set_basename("rust")?;
    /// assert_eq!(key.name(), "user/test/rust");
    /// #
    /// #     Ok(())
    /// # }
    /// ```
    ///
    /// # Panics
    /// Panics if the provided string contains interior nul bytes.
    fn set_basename(&mut self, basename: &str) -> Result<(), KeyError> {
        let cstr = CString::new(basename).unwrap();
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
    /// # use elektra::{StringKey,WriteableKey,ReadableKey};
    /// # fn main() -> Result<(), Box<dyn std::error::Error>> {
    /// let mut key = StringKey::new("user/test/key")?;
    /// key.add_basename("rust")?;
    /// assert_eq!(key.name(), "user/test/key/rust");
    /// #
    /// #     Ok(())
    /// # }
    /// ```
    ///
    /// # Panics
    /// Panics if the provided string contains interior nul bytes.
    fn add_basename(&mut self, basename: &str) -> Result<(), KeyError> {
        let cstr = CString::new(basename).unwrap();
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
    /// # use elektra::{StringKey,WriteableKey,ReadableKey};
    /// # fn main() -> Result<(), Box<dyn std::error::Error>> {
    /// let mut key = StringKey::new("user/x/r").unwrap();
    /// key.add_name("../y/a//././z").unwrap();
    /// assert_eq!(key.name(), "user/x/y/a/z");
    /// #
    /// #     Ok(())
    /// # }
    /// ```
    ///
    /// # Panics
    /// Panics if the provided string contains interior nul bytes.
    fn add_name(&mut self, name: &str) -> Result<(), KeyError> {
        let cstr = CString::new(name).unwrap();
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
    /// # fn main() -> Result<(), Box<dyn std::error::Error>> {
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
    ///
    /// # Panics
    /// Panics if the provided string contains interior nul bytes.
    fn copy_meta(&mut self, source: &Self, metaname: &str) -> i32
    where
        Self: Sized,
    {
        let cstr = CString::new(metaname).unwrap();
        unsafe { elektra_sys::keyCopyMeta(self.as_ptr(), source.as_ref(), cstr.as_ptr()) }
    }

    /// Set a new meta-information.
    /// Returns the size of the new meta information on success,
    /// or a `KeyError::InvalidName` if the name is invalid.
    ///
    /// # Panics
    /// Panics if any of the provided strings contains interior nul bytes.
    fn set_meta(&mut self, metaname: &str, metavalue: &str) -> Result<usize, KeyError> {
        let name = CString::new(metaname).unwrap();
        let value = CString::new(metavalue).unwrap();
        let ret_val =
            unsafe { elektra_sys::keySetMeta(self.as_ptr(), name.as_ptr(), value.as_ptr()) };
        if ret_val < 0 {
            Err(KeyError::InvalidName)
        } else {
            Ok(ret_val.try_into().unwrap())
        }
    }

    /// Delete the metadata at metaname
    /// Returns the size of the new meta information on success,
    /// or a `KeyError::InvalidName` if the name is invalid or out of memory.
    ///
    /// # Panics
    /// Panics if the provided string contains interior nul bytes.
    fn delete_meta(&mut self, metaname: &str) -> Result<usize, KeyError> {
        let name = CString::new(metaname).unwrap();
        let ret_val =
            unsafe { elektra_sys::keySetMeta(self.as_ptr(), name.as_ptr(), std::ptr::null()) };
        if ret_val < 0 {
            Err(KeyError::InvalidName)
        } else {
            Ok(ret_val.try_into().unwrap())
        }
    }

    /// Rewind the internal iterator to first metadata.
    /// This method should always be called *before* iterating
    /// over the metakeys.
    fn rewind_meta(&mut self) {
        unsafe {
            elektra_sys::keyRewindMeta(self.as_ptr());
        }
    }
}
