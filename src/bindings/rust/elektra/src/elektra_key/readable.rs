use crate::{KeyError, ReadOnly, StringKey};
use std::convert::TryInto;
use std::ffi::{CStr, CString};

pub trait ReadableKey: AsRef<elektra_sys::Key> {
    type Value;

    fn value(&self) -> Self::Value;

    /// Construct a new key from a raw key pointer
    fn from_ptr(ptr: *mut elektra_sys::Key) -> Self
    where
        Self: Sized;

    // TODO keyCopy?

    // keyname methods

    /// Return the name of the key as a borrowed slice.
    /// # Panics
    /// Panics if the underlying string cannot be converted to UTF-8.
    fn name(&self) -> &str {
        let c_str = unsafe { CStr::from_ptr(elektra_sys::keyName(self.as_ref())) };
        c_str.to_str().unwrap()
    }

    /// Return the basename of the key as a borrowed slice.
    /// # Panics
    /// Panics if the underlying string cannot be converted to UTF-8.
    fn basename(&self) -> &str {
        let c_str = unsafe { CStr::from_ptr(elektra_sys::keyBaseName(self.as_ref())) };
        c_str.to_str().unwrap()
    }

    /// Calculates number of bytes needed to store basename of key.
    fn basename_size(&self) -> isize {
        unsafe { elektra_sys::keyGetBaseNameSize(self.as_ref()) }
    }

    /// Bytes needed to store the key name including user domain and ending NULL.
    fn fullname_size(&self) -> usize {
        unsafe {
            elektra_sys::keyGetFullNameSize(self.as_ref())
                .try_into()
                .unwrap()
        }
    }

    /// Return how many references the key has.
    fn get_ref(&self) -> isize {
        unsafe { elektra_sys::keyGetRef(self.as_ref()) }
    }

    /// Returns the value of a meta-information which is current.
    fn current_meta(&self) -> ReadOnly<StringKey> {
        let key_ptr = unsafe { elektra_sys::keyCurrentMeta(self.as_ref()) };
        ReadOnly::from_ptr(key_ptr as *mut elektra_sys::Key)
    }

    /// Get key full name, including the user domain name.
    /// # Panics
    /// Panics if the underlying c_string contains interior nul bytes
    /// or cannot be converted to UTF-8
    fn fullname(&self) -> String {
        let mut vec: Vec<u8> = Vec::with_capacity(self.fullname_size());

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

    fn namespace(&self) -> u32 {
        unsafe { elektra_sys::keyGetNamespace(self.as_ref()) as u32 }
    }

    /// Determines if the key is in the spec namespace
    fn is_spec(&self) -> bool {
        self.namespace() == elektra_sys::KEY_NS_SPEC
    }

    /// Determines if the key is in the dir namespace
    fn is_dir(&self) -> bool {
        self.namespace() == elektra_sys::KEY_NS_DIR
    }

    /// Determines if the key is in the proc namespace
    fn is_proc(&self) -> bool {
        self.namespace() == elektra_sys::KEY_NS_PROC
    }

    /// Determines if the key is in the user namespace
    fn is_user(&self) -> bool {
        self.namespace() == elektra_sys::KEY_NS_USER
    }

    /// Determines if the key is in the system namespace
    fn is_system(&self) -> bool {
        self.namespace() == elektra_sys::KEY_NS_SYSTEM
    }

    /// Determines if the key is in the dir namespace
    fn is_cascading(&self) -> bool {
        self.namespace() == elektra_sys::KEY_NS_CASCADING
    }

    // Omitted keyValue() due to return value of void pointer, which cannot be used safely in Rust

    /// Returns the number of bytes needed to store the key's value, including the
    /// NULL terminator.
    /// 
    /// # Examples
    /// ```
    /// # use std::error::Error;
    /// # use elektra::{BinaryKey,WriteableKey,ReadableKey};
    /// # fn main() -> Result<(), Box<dyn Error>> {
    /// let mut key = BinaryKey::new("user/sw/app")?;
    /// let binary_content = b"12345".to_vec();
    /// key.set_value(binary_content);
    /// assert_eq!(key.value_size(), 5);
    /// #
    /// #     Ok(())
    /// # }
    /// ```
    fn value_size(&self) -> usize {
        let ret_val = unsafe { elektra_sys::keyGetValueSize(self.as_ref()) };
        // keyGetValueSize returns -1 on null pointers, but we can be sure self.ptr is valid
        // so this conversion is safe
        ret_val.try_into().unwrap()
    }

    /// Returns true if the key has a binary value.
    /// 
    /// # Notes
    /// Note that this does not return true for a newly created BinaryKey,
    /// but only when actual binary data has been set, due to the underlying
    /// generic Key.
    /// 
    /// # Examples
    /// ```
    /// # use std::error::Error;
    /// # use elektra::{BinaryKey,WriteableKey,ReadableKey};
    /// # fn main() -> Result<(), Box<dyn Error>> {
    /// let mut key = BinaryKey::new("user/sw/app")?;
    /// let binary_content = b"0".to_vec();
    /// key.set_value(binary_content);
    /// assert!(key.is_binary());
    /// #
    /// #     Ok(())
    /// # }
    /// ```
    fn is_binary(&self) -> bool {
        unsafe { elektra_sys::keyIsBinary(self.as_ref()) == 1 }
    }

    /// Returns true if the key has a string value.
    /// 
    /// # Examples
    /// ```
    /// # use std::error::Error;
    /// # use elektra::{StringKey,WriteableKey,ReadableKey};
    /// # fn main() -> Result<(), Box<dyn Error>> {
    /// let key = StringKey::new("user/sw/app")?;
    /// assert!(key.is_string());
    /// #
    /// #     Ok(())
    /// # }
    /// ```
    fn is_string(&self) -> bool {
        unsafe { elektra_sys::keyIsString(self.as_ref()) == 1 }
    }

    /// Returns true if other is below self
    /// 
    /// # Examples
    /// ```
    /// # use std::error::Error;
    /// # use elektra::{StringKey,WriteableKey,ReadableKey};
    /// # fn main() -> Result<(), Box<dyn Error>> {
    /// let key = StringKey::new("user/sw/app")?;
    /// let key2 = StringKey::new("user/sw/app/folder/key")?;
    /// assert!(key2.is_below(&key));
    /// #
    /// #     Ok(())
    /// # }
    /// ```
    fn is_below(&self, other: &Self) -> bool
    where
        Self: Sized,
    {
        unsafe { elektra_sys::keyIsBelow(other.as_ref(), self.as_ref()) == 1 }
    }

    /// Returns true if other is *directly* below self
    /// 
    /// # Examples
    /// ```
    /// # use std::error::Error;
    /// # use elektra::{StringKey,WriteableKey,ReadableKey};
    /// # fn main() -> Result<(), Box<dyn Error>> {
    /// let key = StringKey::new("user/sw/app")?;
    /// let key2 = StringKey::new("user/sw/app/key")?;
    /// assert!(key2.is_direct_below(&key));
    /// #
    /// #     Ok(())
    /// # }
    /// ```
    fn is_direct_below(&self, other: &Self) -> bool
    where
        Self: Sized,
    {
        unsafe { elektra_sys::keyIsDirectBelow(other.as_ref(), self.as_ref()) == 1 }
    }

    /// Returns true if the key is inactive.
    /// 
    /// In Elektra terminology a hierarchy of keys is inactive if the
    /// rootkey's basename starts with '.'. So a key is also inactive
    /// if it is below an inactive key.
    /// # Examples
    /// ```
    /// # use std::error::Error;
    /// # use elektra::{StringKey,WriteableKey,ReadableKey};
    /// # fn main() -> Result<(), Box<dyn Error>> {
    /// let key = StringKey::new("user/key/.hidden")?;
    /// assert!(key.is_inactive());
    /// #
    /// #     Ok(())
    /// # }
    /// ```
    fn is_inactive(&self) -> bool {
        unsafe { elektra_sys::keyIsInactive(self.as_ref()) == 1 }
    }

    /// Returns the metadata with the given metaname
    fn meta(&self, metaname: &str) -> Result<ReadOnly<StringKey<'_>>, KeyError>
    where
        Self: Sized,
    {
        let cstr = CString::new(metaname).unwrap();
        let key_ptr = unsafe { elektra_sys::keyGetMeta(self.as_ref(), cstr.as_ptr()) };
        if key_ptr.is_null() {
            Err(KeyError::NotFound)
        } else {
            let key: ReadOnly<StringKey<'_>> = ReadOnly::from_ptr(key_ptr as *mut elektra_sys::Key);
            Ok(key)
        }
    }
}
