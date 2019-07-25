#![allow(non_upper_case_globals)]
#![allow(non_camel_case_types)]
#![allow(non_snake_case)]

// Include the created bindings
include!(concat!(env!("OUT_DIR"), "/bindings.rs"));

#[cfg(test)]
mod tests {
    use super::*;
    use std::ffi::{CStr, CString};

    #[test]
    fn can_write_read_key() {
        let key_name = CString::new("user/test/key").unwrap();
        let key_val = CString::new("rust-bindings").unwrap();
        unsafe {
            let key = keyNew(key_name.as_ptr(), KEY_VALUE, key_val.as_ptr(), KEY_END);
            let ret_val_str = CStr::from_ptr(keyString(key));
            assert_eq!(ret_val_str, key_val.as_c_str());
            assert_eq!(keyDel(key), 0);
        }
    }

    #[test]
    fn can_iterate_keyset() {
        let key_name = CString::new("user/test/key").unwrap();
        let key_name2 = CString::new("user/test/key2").unwrap();

        unsafe {
            let key = keyNew(key_name.as_ptr(), KEY_END);
            let key2 = keyNew(key_name2.as_ptr(), KEY_END);
            let ks = ksNew(0);

            let mut append_res = ksAppendKey(ks, key);
            assert_eq!(append_res, 1);

            append_res = ksAppendKey(ks, key2);
            assert_eq!(append_res, 2);

            assert_eq!(ksRewind(ks), 0);

            let mut key_next = ksNext(ks);
            assert_eq!(key_name.as_c_str(), CStr::from_ptr(keyName(key_next)));

            key_next = ksNext(ks);
            assert_eq!(key_name2.as_c_str(), CStr::from_ptr(keyName(key_next)));

            key_next = ksNext(ks);
            let key_next_ptr: *const Key = key_next;
            assert!(key_next_ptr.is_null());

            // Deletes ks and both keys
            assert_eq!(ksDel(ks), 0);
        }
    }

}
