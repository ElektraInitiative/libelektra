use crate::{KeyNotFoundError, ReadOnly, StringKey};
use std::borrow::Cow;
use std::convert::TryInto;
use std::ffi::{CStr, CString};

pub trait ReadableKey: AsRef<elektra_sys::Key> + PartialEq + Eq + PartialOrd + Ord {
    /// The type returned by value.
    type GetValue;

    /// Returns the value this key holds.
    /// # Examples
    /// ```
    /// # use elektra::{StringKey,WriteableKey,ReadableKey};
    /// # fn main() -> Result<(), Box<dyn std::error::Error>> {
    /// let mut key = StringKey::new("user:/sw/app")?;
    /// key.set_value("myvalue");
    /// assert_eq!(key.value(), "myvalue");
    /// #
    /// #     Ok(())
    /// # }
    /// ```
    fn value(&self) -> Self::GetValue;

    /// Construct a new key from a raw key pointer
    unsafe fn from_ptr(ptr: *mut elektra_sys::Key) -> Self
    where
        Self: Sized;

    /// Return the name of the key as a borrowed slice.
    fn name(&self) -> Cow<str> {
        let c_str = unsafe { CStr::from_ptr(elektra_sys::keyName(self.as_ref())) };
        c_str.to_string_lossy()
    }

    /// Return the basename of the key as a borrowed slice.
    fn basename(&self) -> Cow<str> {
        let c_str = unsafe { CStr::from_ptr(elektra_sys::keyBaseName(self.as_ref())) };
        c_str.to_string_lossy()
    }

    /// Calculates number of bytes needed to store basename of key.
    fn basename_size(&self) -> isize {
        unsafe { elektra_sys::keyGetBaseNameSize(self.as_ref()) }
    }

    /// Return how many references the key has.
    fn get_ref(&self) -> u16 {
        unsafe { elektra_sys::keyGetRef(self.as_ref()) }
    }


    /// Returns the namespace of the name of this key.
    /// Note that there are some convenience methods implemented.
    /// 
    /// # Examples
    /// ```
    /// # use elektra::{BinaryKey,WriteableKey,ReadableKey};
    /// # use elektra_sys;
    /// # fn main() -> Result<(), Box<dyn std::error::Error>> {
    /// let mut key = BinaryKey::new("user:/sw/app")?;
    /// assert_eq!(key.namespace(), elektra_sys::KEY_NS_USER);
    /// #
    /// #     Ok(())
    /// # }
    /// ```
    fn namespace(&self) -> u32 {
        unsafe { elektra_sys::keyGetNamespace(self.as_ref())  as u32 }
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

    /// Determines if the key is a cascading key
    fn is_cascading(&self) -> bool {
        self.namespace() == elektra_sys::KEY_NS_CASCADING
    }

    /// Returns the number of bytes needed to store the key's value, including the
    /// NULL terminator.
    ///
    /// # Examples
    /// ```
    /// # use elektra::{BinaryKey,WriteableKey,ReadableKey};
    /// # fn main() -> Result<(), Box<dyn std::error::Error>> {
    /// let mut key = BinaryKey::new("user:/sw/app")?;
    /// key.set_value(b"12345");
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
    /// # use elektra::{BinaryKey,WriteableKey,ReadableKey};
    /// # fn main() -> Result<(), Box<dyn std::error::Error>> {
    /// let mut key = BinaryKey::new("user:/sw/app")?;
    /// key.set_value(b"");
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
    /// # use elektra::{StringKey,WriteableKey,ReadableKey};
    /// # fn main() -> Result<(), Box<dyn std::error::Error>> {
    /// let key = StringKey::new("user:/sw/app")?;
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
    /// # use elektra::{StringKey,WriteableKey,ReadableKey};
    /// # fn main() -> Result<(), Box<dyn std::error::Error>> {
    /// let key = StringKey::new("user:/sw/app")?;
    /// let key2 = StringKey::new("user:/sw/app/folder/key")?;
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
    /// # use elektra::{StringKey,WriteableKey,ReadableKey};
    /// # fn main() -> Result<(), Box<dyn std::error::Error>> {
    /// let key = StringKey::new("user:/sw/app")?;
    /// let key2 = StringKey::new("user:/sw/app/key")?;
    /// assert!(key2.is_directly_below(&key));
    /// #
    /// #     Ok(())
    /// # }
    /// ```
    fn is_directly_below(&self, other: &Self) -> bool
    where
        Self: Sized,
    {
        unsafe { elektra_sys::keyIsDirectlyBelow(other.as_ref(), self.as_ref()) == 1 }
    }

    /// Returns the metadata with the given metaname
    ///
    /// # Errors
    /// Returns `KeyNotFoundError` if no metakey with the given name was found.
    /// 
    /// # Panics
    /// Panics if the provided string contains interior nul bytes.
    fn meta(&self, metaname: &str) -> Result<ReadOnly<StringKey<'_>>, KeyNotFoundError>
    where
        Self: Sized,
    {
        let cstr = CString::new(metaname).unwrap();
        let key_ptr = unsafe { elektra_sys::keyGetMeta(self.as_ref(), cstr.as_ptr()) };
        if key_ptr.is_null() {
            Err(KeyNotFoundError::new(metaname.to_owned()))
        } else {
            let key: ReadOnly<StringKey<'_>> = unsafe { ReadOnly::from_ptr(key_ptr as *mut elektra_sys::Key) };
            Ok(key)
        }
    }
}
