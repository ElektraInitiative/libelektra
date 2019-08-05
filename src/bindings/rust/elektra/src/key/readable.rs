use crate::{KeyError, ReadOnly, StringKey};
use std::convert::TryInto;
use std::ffi::{CStr, CString};

pub trait ReadableKey {
    fn as_ref(&self) -> &elektra_sys::Key;
    type Value;
    fn get_value(&self) -> Self::Value;
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
    fn is_below(&self, other: &Self) -> bool where Self: Sized {
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
    fn is_direct_below(&self, other: &Self) -> bool where Self: Sized {
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
    fn get_meta(&self, metaname: &str) -> Result<ReadOnly<StringKey>, KeyError>
    where
        Self: Sized,
    {
        let cstr = CString::new(metaname).unwrap();
        let key_ptr = unsafe { elektra_sys::keyGetMeta(self.as_ref(), cstr.as_ptr()) };
        if key_ptr == std::ptr::null() {
            Err(KeyError::NotFound)
        } else {
            let key: ReadOnly<StringKey> = ReadOnly::from_ptr(key_ptr as *mut elektra_sys::Key);
            Ok(key)
        }
    }
}
