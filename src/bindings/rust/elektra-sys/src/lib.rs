//! Raw bindings for libelektra.
//! For documentation on the functions, see the [C API documentation](https://doc.libelektra.org/api/latest/html/index.html).

#![allow(non_upper_case_globals)]
#![allow(non_camel_case_types)]
#![allow(non_snake_case)]

// Include the created bindings
include!(concat!(env!("OUT_DIR"), "/bindings.rs"));

pub const KEY_END: *const std::ffi::c_void = std::ptr::null();
pub const KS_END: *const std::ffi::c_void = std::ptr::null();

#[cfg(test)]
mod tests {
    use super::{
        keyDel, keyName, keyNew, keyString, ksAppendKey, ksDel, ksNew, ksAtCursor, Key,
        KEY_END, KEY_VALUE,
    };
    use std::ffi::{CStr, CString};

    #[test]
    fn can_write_read_key() {
        let key_name = CString::new("user:/test/key").unwrap();
        let key_val = CString::new("rust-bindings").unwrap();
        let key = unsafe { keyNew(key_name.as_ptr(), KEY_VALUE, key_val.as_ptr(), KEY_END) };
        let ret_val_str = unsafe { CStr::from_ptr(keyString(key)) };
        assert_eq!(ret_val_str, key_val.as_c_str());
        assert_eq!(unsafe { keyDel(key) }, 0);
    }

    #[test]
    fn can_iterate_keyset() {
        let key_name = CString::new("user:/test/key").unwrap();
        let key_name2 = CString::new("user:/test/key2").unwrap();

        let key = unsafe { keyNew(key_name.as_ptr(), KEY_END) };
        let key2 = unsafe { keyNew(key_name2.as_ptr(), KEY_END) };
        let ks = unsafe { ksNew(2) };

        let mut append_res = unsafe { ksAppendKey(ks, key) };
        assert_eq!(append_res, 1);

        append_res = unsafe { ksAppendKey(ks, key2) };
        assert_eq!(append_res, 2);

        let mut key_next = unsafe { ksAtCursor(ks, 0) };
        assert_eq!(key_name.as_c_str(), unsafe {
            CStr::from_ptr(keyName(key_next))
        });

        key_next = unsafe { ksAtCursor(ks, 1) };
        assert_eq!(key_name2.as_c_str(), unsafe {
            CStr::from_ptr(keyName(key_next))
        });

        key_next = unsafe { ksAtCursor(ks, 2) };
        let key_next_ptr: *const Key = key_next;
        assert!(key_next_ptr.is_null());

        // Deletes ks and both keys
        assert_eq!(unsafe { ksDel(ks) }, 0);
    }

}
