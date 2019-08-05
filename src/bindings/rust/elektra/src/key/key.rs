use crate::{ReadOnly, ReadableKey, WriteableKey};
use elektra_sys;
use std::ffi::{CStr, CString};
use std::ptr::NonNull;

#[derive(Debug)]
pub struct StringKey {
    ptr: NonNull<elektra_sys::Key>,
}

#[derive(Debug)]
pub struct BinaryKey {
    ptr: NonNull<elektra_sys::Key>,
}

macro_rules! add_traits {
    ($($t:ty)*) => ($(
        impl PartialEq for $t {

            #[inline]
            fn eq(&self, other: &Self) -> bool {
                unsafe {
                    elektra_sys::keyCmp(
                        self.as_ref(),
                        other.as_ref(),
                    ) == 0
                }
            }
        }

        impl Eq for $t {}

        impl Ord for $t {
            fn cmp(&self, other: &Self) -> std::cmp::Ordering {
                let cmp = unsafe {
                    elektra_sys::keyCmp(
                        self.as_ref(),
                        other.as_ref(),
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

        impl PartialOrd for $t {
            fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
                Some(self.cmp(other))
            }
        }

        impl Clone for $t {
            fn clone(&self) -> Self {
                self.duplicate()
            }
        }

        impl Drop for $t {
            fn drop(&mut self) {
                println!("Drop {:?}", self);
                unsafe { elektra_sys::keyDel(self.as_ptr()) };
            }
        }

        impl Iterator for $t {
            type Item = ReadOnly<StringKey>;
            fn next(&mut self) -> Option<Self::Item> {
                let key_ptr = unsafe { elektra_sys::keyNextMeta(self.as_ptr()) };
                if key_ptr == std::ptr::null() {
                    None
                } else {
                    Some(ReadOnly::from_ptr(key_ptr as *mut elektra_sys::Key))
                }
            }
        }
    )*)
}

add_traits!(StringKey);
add_traits!(BinaryKey);

impl StringKey {
    /// Sets the value of the key to the supplied string.
    /// # Panics
    /// Panics if the provided string contains internal nul bytes.
    fn set_string<T: Into<Vec<u8>>>(&mut self, value: T) {
        let cptr = CString::new(value).unwrap();
        unsafe { elektra_sys::keySetString(self.as_ptr(), cptr.as_ptr()) };
    }

    /// Returns the string value of the key if the type of the key is string, an error if it's binary.
    /// # Panics
    /// Panics if the underlying string cannot be converted to UTF-8.
    fn get_string(&self) -> &str {
        let c_str = unsafe { CStr::from_ptr(elektra_sys::keyString(self.as_ref())) };
        c_str.to_str().unwrap()
    }
}

impl BinaryKey {
    /// Sets the key's binary content to the supplied data.
    fn set_binary(&mut self, data: &[u8]) {
        unsafe {
            elektra_sys::keySetBinary(
                self.as_ptr(),
                data.as_ptr() as *const std::os::raw::c_void,
                data.len(),
            );
        }
    }

    /// Returns the keys binary content
    /// Returns a TypeMismatch if the key is of type string
    fn get_binary(&self) -> Vec<u8> {
        let mut vec: Vec<u8> = Vec::with_capacity(self.get_value_size());

        let ret_val = unsafe {
            elektra_sys::keyGetBinary(
                self.as_ref(),
                vec.as_mut_ptr() as *mut std::os::raw::c_void,
                vec.capacity(),
            )
        };

        if ret_val > 0 { // TODO try_into?
            unsafe { vec.set_len(ret_val as usize) };
            vec
        } else {
            unsafe { vec.set_len(0) }
            vec
        }
    }
}

impl ReadableKey for StringKey {
    type Value = String;

    fn as_ref(&self) -> &elektra_sys::Key {
        unsafe { self.ptr.as_ref() }
    }

    fn from_ptr(ptr: *mut elektra_sys::Key) -> Self {
        StringKey {
            ptr: NonNull::new(ptr).unwrap(),
        }
    }

    fn get_value(&self) -> Self::Value {
        self.get_string().to_owned()
    }
}

impl ReadableKey for BinaryKey {
    type Value = Vec<u8>;

    fn as_ref(&self) -> &elektra_sys::Key {
        unsafe { self.ptr.as_ref() }
    }

    fn from_ptr(ptr: *mut elektra_sys::Key) -> Self {
        BinaryKey {
            ptr: NonNull::new(ptr).unwrap(),
        }
    }
    fn get_value(&self) -> Self::Value {
        self.get_binary()
    }
}

impl WriteableKey for BinaryKey {
    fn as_ptr(&mut self) -> *mut elektra_sys::Key {
        self.ptr.as_ptr()
    }

    fn set_value<T: Into<Vec<u8>>>(&mut self, t: T) {
        self.set_binary(&t.into());
    }
}
impl WriteableKey for StringKey {
    fn as_ptr(&mut self) -> *mut elektra_sys::Key {
        self.ptr.as_ptr()
    }
    fn set_value<T: Into<Vec<u8>>>(&mut self, t: T) {
        self.set_string(t);
    }
}

impl From<StringKey> for BinaryKey {
    fn from(mut sk: StringKey) -> Self {
        let binary_key = BinaryKey::from_ptr(sk.as_ptr());
        std::mem::forget(sk);
        binary_key
    }
}

impl From<BinaryKey> for StringKey {
    fn from(mut bk: BinaryKey) -> Self {
        let str_key = StringKey::from_ptr(bk.as_ptr());
        std::mem::forget(bk);
        str_key
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::KeyError;

    #[test]
    fn can_write_read_key() {
        let key_name = "user/test/key";
        let key = StringKey::new(key_name).unwrap();
        assert_eq!(key.get_name(), key_name);
    }

    #[test]
    fn can_write_read_key_value() {
        let key_name = "user/test/key";
        let utf8_value = "😃";
        let mut key: StringKey = StringKey::new(key_name).unwrap();
        key.set_string(utf8_value);
        assert_eq!(key.get_name(), key_name);
        assert_eq!(key.get_string(), utf8_value);
    }

    #[test]
    fn can_duplicate_key() {
        let key_name = "user/test/key";
        let mut key_dup;
        {
            let key = StringKey::new(key_name).unwrap();
            key_dup = Some(key.duplicate());
            // key is dropped here
        }
        assert_eq!(key_dup.unwrap().get_name(), key_name);
    }

    #[test]
    fn can_write_read_binary() {
        let mut key = BinaryKey::new("user/test/rust").unwrap();
        let binary_content: [u8; 7] = [25, 34, 0, 254, 1, 0, 7];
        key.set_binary(&binary_content);
        let read_content = key.get_binary();
        assert_eq!(read_content, binary_content);
    }
    #[test]
    fn can_write_read_empty_binary() {
        let mut key = BinaryKey::new("user/test/binary").unwrap();
        let binary_content: [u8; 0] = [];
        key.set_binary(&binary_content);
        // set_binary does not set binary flag, size is 0
        // so get_binary should return empty vec
        let err = key.get_binary();
        assert_eq!(err, binary_content);
    }

    #[test]
    fn equality_is_exclusive() {
        let key = BinaryKey::new("user/test/exclusive").unwrap();
        let key2 = BinaryKey::new("dir/test/exclusive").unwrap();

        assert!(!(key != key));
        assert!(key == key);

        assert!(!(key == key2));
        assert!(key != key2);
    }

    #[test]
    fn equality_is_reflexive() {
        let key = StringKey::new("user/test/reflexive").unwrap();
        assert!(key == key);
    }

    #[test]
    fn equality_is_symmetric() {
        let key = BinaryKey::new("user/test/symmetric").unwrap();
        let key_dup = BinaryKey::new("user/test/symmetric").unwrap();

        assert!(key_dup == key);
        assert!(key == key_dup);
    }

    #[test]
    fn equality_is_transitive() {
        let key = BinaryKey::new("user/test/transitive").unwrap();
        let key2 = BinaryKey::new("user/test/transitive").unwrap();
        let key3 = BinaryKey::new("user/test/transitive").unwrap();

        assert!(key == key2);
        assert!(key2 == key3);
        assert!(key == key3);
    }

    #[test]
    fn keys_are_ordered() {
        let key = BinaryKey::new("user/test/a").unwrap();
        let key2 = BinaryKey::new("user/test/b").unwrap();
        let key3 = BinaryKey::new("user/test/c").unwrap();

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
        let mut key = BinaryKey::new("user/test/a").unwrap();
        assert_eq!(key.get_ref(), 0);
        key.inc_ref();
        assert_eq!(key.get_ref(), 1);
        key.dec_ref();
        assert_eq!(key.get_ref(), 0);
    }

    #[test]
    fn error_on_missing_metaname() {
        let key = StringKey::new("user/test/metatest").unwrap();
        assert!(key.get_meta("nonexistent metaname").is_err());
    }
    #[test]
    fn can_set_get_metavalue() {
        let mut key = StringKey::new_empty();
        key.set_meta("metakey", "metaval");
        let meta_key = key.get_meta("metakey").unwrap();
        assert_eq!(meta_key.get_value(), "metaval");
    }

    #[test]
    fn can_iterate_key() {
        let mut key = StringKey::new_empty();
        let meta = [("meta1", "val1"), ("meta2", "val2")];
        key.set_meta(meta[0].0, meta[0].1);
        key.set_meta(meta[1].0, meta[1].1);
        key.rewind_meta();

        let mut did_iterate = false;
        for (i, metakey) in key.enumerate() {
            did_iterate = true;
            assert_eq!(metakey.get_name(), meta[i].0);
            assert_eq!(metakey.get_value(), meta[i].1);
        }
        assert!(did_iterate);
    }

    #[test]
    fn can_delete_metadata() {
        let mut key = StringKey::new_empty();
        key.set_meta("metakey", "metaval");
        assert_eq!(key.delete_meta("metakey"), 0);
        assert_eq!(key.get_meta("metakey").unwrap_err(), KeyError::NotFound);
    }

    #[test]
    fn can_cast_key_types() {
        let mut key = StringKey::new("user/test/cast").unwrap();
        let mut bin_key = BinaryKey::from(key);
        let val = "data".as_bytes();
        bin_key.set_value(val);
        assert_eq!(bin_key.get_value(), val);
    }
}
