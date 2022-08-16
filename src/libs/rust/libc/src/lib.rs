#![allow(non_snake_case)]
#![allow(non_camel_case_types)]
#![allow(unused_variables)]

use std::ffi::{CStr, CString};
use std::{ptr, slice};
use std::str::FromStr;
use std::convert::TryFrom;
use libc::{ssize_t, size_t, c_char, c_int, c_void};

mod structs;

use crate::structs::{
    CKey, CKeySet,
    elektraNamespace, elektraCopyFlags, elektraLockFlags,
};

use crate::elektraNamespace::KEY_NS_NONE;

use elektra_rust::key::{Key, KeyBuilder, KeyName, KeyNamespace, KeySet};

use enumflags2::{BitFlags};

#[no_mangle]
pub unsafe extern "C" fn elektraKeyNew(keyname: *const c_char) -> *mut CKey {
    if keyname.is_null() {
        return ptr::null_mut();
    }

    let cstr = CStr::from_ptr(keyname);
    let keyNameStr = cstr.to_str()
        .expect("key name cannot be cast to string");

    if let Ok(builder) = KeyBuilder::from_str(keyNameStr) {
        let keyResult = builder.build();

        if let Ok(key) = keyResult {
            return Box::into_raw(
                Box::new(key.into())
            );
        }
    }

    return ptr::null_mut()
}

#[no_mangle]
pub extern "C" fn elektraKeyCopy(dest: *mut CKey, source: *const CKey, flags: elektraCopyFlags) -> *mut CKey {
    if dest.is_null() || source.is_null() {
        return ptr::null_mut();
    }

    let c_key = unsafe { &*dest };
    let mut rust_key = match Key::try_from(c_key) {
        Ok(x) => x,
        Err(_) => return ptr::null_mut(),
    };

    let other_c_key = unsafe { &*source };
    let other_rust_key = match Key::try_from(other_c_key) {
        Ok(x) => x,
        Err(_) => return ptr::null_mut(),
    };

    // todo: parse Key flags properly
    rust_key.copy_from(&other_rust_key, BitFlags::default());
    CKey::overwrite(dest, rust_key);

    dest
}

#[no_mangle]
#[deprecated]
pub extern "C" fn elektraKeyClear(key: *mut CKey) -> c_int {
    if key.is_null() {
        return -1;
    }

    let c_key = unsafe { &*key };
    let mut rust_key = match Key::try_from(c_key) {
        Ok(x) => x,
        Err(_) => return -1,
    };

    rust_key.set_name(":/").unwrap();

    match rust_key.reset_value() {
        Ok(x) => (),
        Err(_) => return -1,
    };

    rust_key.set_meta(KeySet::default());

    CKey::overwrite(key, rust_key);

    1
}

#[no_mangle]
pub extern "C" fn elektraKeyDel(key: *mut CKey) -> c_int {
    if key.is_null() {
        return -1;
    }

    CKey::destroy(key);

    return 0;
}

#[no_mangle]
pub extern "C" fn elektraKeyMeta(key: *mut CKey) -> *mut CKeySet {
    if key.is_null() {
        return ptr::null_mut();
    }

    unsafe {
        (*key).meta
    }
}


#[no_mangle]
pub extern "C" fn elektraKeySetMeta(key: *mut CKey, meta: *mut CKeySet) -> c_int {
    if key.is_null() || meta.is_null() {
        return -1;
    }

    let c_key = unsafe { &*key };
    let mut rust_key = match Key::try_from(c_key) {
        Ok(x) => x,
        Err(_) => return -1,
    };

    let c_keyset = unsafe { &*meta };
    let rust_keyset = match KeySet::try_from(c_keyset) {
        Ok(x) => x,
        Err(_) => return -1,
    };

    let length = rust_keyset.len();

    rust_key.set_meta(rust_keyset);
    CKey::overwrite(key, rust_key);

    length as c_int
}

#[no_mangle]
pub extern "C" fn elektraKeyCompareName(k1: *const CKey, k2: *const CKey) -> c_int {
    if k1.is_null() || k2.is_null() {
        return -1;
    }

    let c_key1 = unsafe { &*k1 };
    let c_key2 = unsafe { &*k2 };

    let that_key = match Key::try_from(c_key1) {
        Ok(x) => x,
        Err(_) => return -1,
    };

    let other_key = match Key::try_from(c_key2) {
        Ok(x) => x,
        Err(_) => return -1,
    };

    return that_key.cmp(&other_key) as c_int;
}

#[no_mangle]
pub extern "C" fn elektraKeyIsBelow(key: *mut CKey, check: *mut CKey) -> c_int {
    if key.is_null() || check.is_null() {
        return -1;
    }

    let c_key = unsafe { &*key };
    let that_key = match Key::try_from(c_key) {
        Ok(x) => x,
        Err(_) => return -1,
    };

    let c_key_check = unsafe { &*check };
    let other_key = match Key::try_from(c_key_check) {
        Ok(x) => x,
        Err(_) => return -1,
    };

    that_key.name().is_below(other_key.name()) as c_int
}

#[no_mangle]
pub extern "C" fn elektraKeyIsBelowOrSame(key: *mut CKey, check: *mut CKey) -> c_int {
    if key.is_null() || check.is_null() {
        return -1;
    }

    let c_key = unsafe { &*key };
    let that_key = match Key::try_from(c_key) {
        Ok(x) => x,
        Err(_) => return -1,
    };

    let c_key_check = unsafe { &*check };
    let other_key = match Key::try_from(c_key_check) {
        Ok(x) => x,
        Err(_) => return -1,
    };

    that_key.name().is_below_or_same(other_key.name()) as c_int
}

#[no_mangle]
pub extern "C" fn elektraKeyIsDirectlyBelow(key: *const CKey, check: *const CKey) -> c_int {
    if key.is_null() || check.is_null() {
        return -1;
    }

    let c_key = unsafe { &*key };
    let that_key = match Key::try_from(c_key) {
        Ok(x) => x,
        Err(_) => return -1,
    };

    let c_key_check = unsafe { &*check };
    let other_key = match Key::try_from(c_key_check) {
        Ok(x) => x,
        Err(_) => return -1,
    };

    that_key.name().is_directly_below(other_key.name()) as c_int
}

#[no_mangle]
pub extern "C" fn elektraKeyName(key: *const CKey) -> *const c_char {
    if key.is_null() {
        return ptr::null_mut();
    }

    let c_key = unsafe { &*key };
    return c_key.key;
}

#[no_mangle]
pub extern "C" fn elektraKeyNameSize(key: *const CKey) -> ssize_t {
    if key.is_null() {
        return -1;
    }

    let c_key = unsafe { &*key };
    let rust_key = match Key::try_from(c_key) {
        Ok(x) => x,
        Err(_) => return -1,
    };

    let key_name = rust_key
        .name()
        .to_string();

    CString::new(key_name)
        .unwrap()
        .to_bytes_with_nul()
        .len() as ssize_t
}

#[no_mangle]
pub extern "C" fn elektraKeySetName(key: *mut CKey, newname: *const c_char) -> ssize_t {
    if key.is_null() || newname.is_null() {
        return -1;
    }

    let cstr = unsafe { CStr::from_ptr(newname) };
    let newNameStr = match cstr.to_str() {
        Ok(x) => x,
        Err(_) => return -1,
    };

    if let Ok(key_name) = KeyName::from_str(newNameStr) {
        let c_key = unsafe { &*key };

        let mut rust_key = match Key::try_from(c_key) {
            Ok(x) => x,
            Err(_) => return -1,
        };

        rust_key.set_keyname(key_name);
        CKey::overwrite(key, rust_key);
        return elektraKeyNameSize(key);
    }

    return -1;
}

#[no_mangle]
pub extern "C" fn elektraKeyAddName(key: *mut CKey, addName: *const c_char) -> ssize_t {
    if key.is_null() || addName.is_null() {
        return -1;
    }

    let cstr = unsafe { CStr::from_ptr(addName) };
    let addNameStr = match cstr.to_str() {
        Ok(x) => x,
        Err(_) => return -1,
    };

    let c_key = unsafe { &*key };
    let mut rust_key = match Key::try_from(c_key) {
        Ok(x) => x,
        Err(_) => return -1,
    };

    rust_key
        .name_mut()
        .append_name(addNameStr);

    CKey::overwrite(key, rust_key);
    return elektraKeyNameSize(key);
}

/// You have to free the returned string manually, otherwise there will be memory leaks
#[no_mangle]
pub extern "C" fn elektraKeyEscapedName(key: *const CKey) -> *const c_char {
    todo!()
}

#[no_mangle]
pub extern "C" fn elektraKeyEscapedNameSize(key: *const CKey) -> ssize_t {
    todo!()
}

/// You have to free the returned string manually, otherwise there will be memory leaks
#[no_mangle]
pub extern "C" fn elektraKeyBaseName(key: *const CKey) -> *mut c_char {
    if key.is_null() {
        return ptr::null_mut();
    }

    let c_key = unsafe { &*key };
    let rust_key = match Key::try_from(c_key) {
        Ok(x) => x,
        Err(_) => return ptr::null_mut(),
    };

    let key_name = rust_key
        .name()
        .base_name();

    return match key_name {
        Some(name) => CString::new(name)
            .unwrap()
            .into_raw(),
        None => CString::new("".as_bytes())
            .unwrap()
            .into_raw(),
    }
}

#[no_mangle]
pub extern "C" fn elektraKeyBaseNameSize(key: *const CKey) -> ssize_t {
    if key.is_null() {
        return -1;
    }

    let c_key = unsafe { &*key };
    let rust_key = match Key::try_from(c_key) {
        Ok(x) => x,
        Err(_) => return -1,
    };

    let key_name = rust_key
        .name()
        .base_name();

    return match key_name {
        Some(name) => CString::new(name)
            .unwrap()
            .to_bytes_with_nul()
            .len() as ssize_t,
        None => 0,
    }
}

#[no_mangle]
pub extern "C" fn elektraKeySetBaseName(key: *mut CKey, baseName: *const c_char) -> ssize_t {
    if key.is_null() || baseName.is_null() {
        return -1;
    }

    let cstr = unsafe { CStr::from_ptr(baseName) };
    let setNameStr = match cstr.to_str() {
        Ok(x) => x,
        Err(_) => return -1,
    };

    let c_key = unsafe { &*key };
    let mut rust_key = match Key::try_from(c_key) {
        Ok(x) => x,
        Err(_) => return -1,
    };

    rust_key
        .name_mut()
        .set_base_name(setNameStr);

    CKey::overwrite(key, rust_key);
    return cstr.to_bytes_with_nul().len() as ssize_t;
}

#[no_mangle]
pub extern "C" fn elektraKeyAddBaseName(key: *mut CKey, baseName: *const c_char) -> ssize_t {
    if key.is_null() || baseName.is_null() {
        return -1;
    }

    let cstr = unsafe { CStr::from_ptr(baseName) };
    let addNameStr = match cstr.to_str() {
        Ok(x) => x,
        Err(_) => return -1,
    };

    let c_key = unsafe { &*key };
    let mut rust_key = match Key::try_from(c_key) {
        Ok(x) => x,
        Err(_) => return -1,
    };

    rust_key
        .name_mut()
        .append_name(addNameStr);

    CKey::overwrite(key, rust_key);
    return elektraKeyNameSize(key);
}

#[no_mangle]
pub extern "C" fn elektraKeyNamespace(key: *const CKey) -> elektraNamespace {
    if key.is_null() {
        return elektraNamespace::KEY_NS_NONE;
    }

    let c_key = unsafe { &*key };
    let rust_key = match Key::try_from(c_key) {
        Ok(x) => x,
        Err(_) => return KEY_NS_NONE,
    };

    elektraNamespace::from(rust_key.name().namespace())
}

#[no_mangle]
pub extern "C" fn elektraKeySetNamespace(key: *mut CKey, ns: elektraNamespace) -> ssize_t {
    if key.is_null() {
        return -1
    }

    let c_key = unsafe { &*key };
    let mut rust_key = match Key::try_from(c_key) {
        Ok(x) => x,
        Err(_) => return -1,
    };

    let namespace = KeyNamespace::from(ns);

    rust_key
        .name_mut()
        .set_namespace(namespace);

    namespace.to_string().len() as ssize_t
}

#[no_mangle]
pub extern "C" fn elektraKeyValue(key: *const CKey) -> *const c_void {
    if key.is_null() {
        return ptr::null_mut();
    }

    let c_key = unsafe { &*key };
    return c_key.data;
}

#[no_mangle]
pub extern "C" fn elektraKeyValueSize(key: *const CKey) -> ssize_t {
    if key.is_null() {
        return -1;
    }

    let c_key = unsafe { &*key };
    let rust_key = match Key::try_from(c_key) {
        Ok(x) => x,
        Err(_) => return -1,
    };

    if let Some(value) = rust_key.value() {
        return value.len() as ssize_t;
    }

    return 0;
}

#[no_mangle]
pub extern "C" fn elektraKeySetValue(key: *mut CKey, value: *const c_void, valueSize: size_t) -> ssize_t {
    if key.is_null() || value.is_null() {
        return -1;
    }

    let c_key = unsafe { &*key };
    let mut rust_key = match Key::try_from(c_key) {
        Ok(x) => x,
        Err(_) => return -1,
    };

    let newValue = unsafe {
        slice::from_raw_parts(value as *const u8, valueSize)
    };

    rust_key.set_value(newValue);

    CKey::overwrite(key, rust_key);

    valueSize as ssize_t
}

#[no_mangle]
pub extern "C" fn elektraKeyLock(key: *mut CKey, what: elektraLockFlags) -> c_int {
    todo!()
}

#[no_mangle]
pub extern "C" fn elektraKeyIsLocked(key: *const CKey, what: elektraLockFlags) -> c_int {
    todo!()
}

#[no_mangle]
pub extern "C" fn elektraKeysetNew(alloc: size_t) -> *mut CKeySet {
    let ks = KeySet::with_capacity(alloc);

    Box::into_raw(
        Box::new(ks.into())
    )
}

#[no_mangle]
pub extern "C" fn elektraKeysetIncRef(ks: *mut CKeySet) -> u16 {
    if ks.is_null() {
        return u16::MAX;
    }

    let c_keyset = unsafe { &*ks };
    let mut rust_keyset = match KeySet::try_from(c_keyset) {
        Ok(x) => x,
        Err(_) => return 0,
    };

    let reference_count = rust_keyset.increase_reference_counter();

    CKeySet::overwrite(ks, rust_keyset);

    reference_count
}

#[no_mangle]
pub extern "C" fn elektraKeysetDecRef(ks: *mut CKeySet) -> u16 {
    if ks.is_null() {
        return u16::MAX;
    }

    let c_keyset = unsafe { &*ks };
    let mut rust_keyset = match KeySet::try_from(c_keyset) {
        Ok(x) => x,
        Err(_) => return 0,
    };

    let reference_count = rust_keyset.decrease_reference_counter();

    CKeySet::overwrite(ks, rust_keyset);

    reference_count
}

#[no_mangle]
pub extern "C" fn elektraKeysetGetRef(ks: *const CKeySet) -> u16 {
    if ks.is_null() {
        return u16::MAX;
    }

    let c_keyset = unsafe { &*ks };
    let rust_keyset = match KeySet::try_from(c_keyset) {
        Ok(x) => x,
        Err(_) => return 0,
    };

    rust_keyset.reference_counter()
}

#[no_mangle]
pub extern "C" fn elektraKeysetClear(ks: *mut CKeySet) -> c_int {
    if ks.is_null() {
        return -1;
    }

    let c_keyset = unsafe { &*ks };
    let mut rust_keyset = match KeySet::try_from(c_keyset) {
        Ok(x) => x,
        Err(_) => return -1,
    };

    rust_keyset.clear();

    CKeySet::overwrite(ks, rust_keyset);

    0
}

#[no_mangle]
pub extern "C" fn elektraKeysetDel(ks: *mut CKeySet) -> c_int {
    if ks.is_null() {
        return -1;
    }

    CKeySet::destroy(ks);

    return 0;
}

#[no_mangle]
pub extern "C" fn elektraKeysetSize(ks: *const CKeySet) -> ssize_t {
    if ks.is_null() {
        return -1;
    }

    let c_keyset = unsafe { &*ks };
    let rust_keyset = match KeySet::try_from(c_keyset) {
        Ok(x) => x,
        Err(_) => return -1,
    };

    rust_keyset.len() as ssize_t
}

#[no_mangle]
pub extern "C" fn elektraKeysetAdd(ks: *mut CKeySet, key: *mut CKey) -> ssize_t {
    if ks.is_null() || key.is_null() {
        return -1;
    }

    let c_keyset = unsafe { &*ks };
    let mut rust_keyset = match KeySet::try_from(c_keyset) {
        Ok(x) => x,
        Err(_) => return -1,
    };

    let c_key = unsafe { &*key };
    let rust_key = match Key::try_from(c_key) {
        Ok(x) => x,
        Err(_) => return -1,
    };

    rust_keyset.append(rust_key);
    let length = rust_keyset.len();

    CKeySet::overwrite(ks, rust_keyset);

    length as ssize_t
}

#[no_mangle]
pub extern "C" fn elektraKeysetGet(ks: *const CKeySet, index: ssize_t) -> *mut CKey {
    if ks.is_null() {
        return ptr::null_mut();
    }

    let c_keyset = unsafe { &*ks };
    let rust_keyset = match KeySet::try_from(c_keyset) {
        Ok(x) => x,
        Err(_) => return ptr::null_mut(),
    };

    if let Some(key) = rust_keyset.get(index) {
        let c_key = key.clone().into();

        Box::into_raw(
            Box::new(c_key)
        )
    } else {
        ptr::null_mut()
    }
}

#[no_mangle]
pub extern "C" fn elektraKeysetRemove(ks: *mut CKeySet, index: ssize_t) -> *mut CKey {
    if ks.is_null() {
        return ptr::null_mut();
    }

    let c_keyset = unsafe { &*ks };
    let mut rust_keyset = match KeySet::try_from(c_keyset) {
        Ok(x) => x,
        Err(_) => return ptr::null_mut(),
    };

    if let Some(key) = rust_keyset.remove(index) {
        CKeySet::overwrite(ks, rust_keyset);

        let c_key = key.clone().into();

        Box::into_raw(
            Box::new(c_key)
        )
    } else {
        ptr::null_mut()
    }
}

#[no_mangle]
pub extern "C" fn elektraKeysetAddAll(ks: *mut CKeySet, other: *const CKeySet) -> ssize_t {
    if ks.is_null() || other.is_null() {
        return -1;
    }

    let c_keyset = unsafe { &*ks };
    let mut rust_keyset = match KeySet::try_from(c_keyset) {
        Ok(x) => x,
        Err(_) => return -1,
    };

    let c_keyset_other = unsafe { &*other };
    let rust_keyset_other = match KeySet::try_from(c_keyset_other) {
        Ok(x) => x,
        Err(_) => return -1,
    };

    rust_keyset.append_all(&rust_keyset_other);
    let length = rust_keyset.len();

    CKeySet::overwrite(ks, rust_keyset);

    length as ssize_t
}

#[no_mangle]
pub extern "C" fn elektraKeysetLookup(ks: *const CKeySet, key: *mut CKey) -> *mut CKey {
    if ks.is_null() || key.is_null() {
        return ptr::null_mut();
    }

    let c_keyset = unsafe { &*ks };
    let rust_keyset = match KeySet::try_from(c_keyset) {
        Ok(x) => x,
        Err(_) => return ptr::null_mut(),
    };

    let c_key = unsafe { &*key };
    let rust_key = match Key::try_from(c_key) {
        Ok(x) => x,
        Err(_) => return ptr::null_mut(),
    };

    if let Some(key) = rust_keyset.lookup_key(&rust_key) {
        let c_key = key.clone().into();

        Box::into_raw(
            Box::new(c_key)
        )
    } else {
        ptr::null_mut()
    }
}

#[no_mangle]
pub extern "C" fn elektraKeysetLookupByName(ks: *const CKeySet, name: *const c_char) -> *mut CKey {
    if ks.is_null() || name.is_null() {
        return ptr::null_mut();
    }

    let c_keyset = unsafe { &*ks };
    let rust_keyset = match KeySet::try_from(c_keyset) {
        Ok(x) => x,
        Err(_) => return ptr::null_mut(),
    };

    let cstr = unsafe { CStr::from_ptr(name) };
    let key_name = match cstr.to_str() {
        Ok(x) => x,
        Err(_) => return ptr::null_mut(),
    };

    if let Some(key) = rust_keyset.lookup(key_name) {
        let c_key = key.clone().into();

        Box::into_raw(
            Box::new(c_key)
        )
    } else {
        ptr::null_mut()
    }
}

#[no_mangle]
pub extern "C" fn elektraKeysetSearch(ks: *const CKeySet, k: *const CKey) -> ssize_t {
    /*
    let c_keyset = unsafe { &*ks };
    let rust_keyset = match KeySet::try_from(c_keyset) {
        Ok(x) => x,
        Err(_) => return -1,
    };
    */
    todo!()
}

/*
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_key_name() {
        unsafe {
            ElektraKey * key = elektraKeyNew("user:/test/qwe/asd");
            assert!("user:/test/qwe/asd" == elektraKeyName(key));

            ElektraKey * key2 = elektraKeyNew("user:/test/qwe/asd/qwe");
            assert!("user:/test/qwe/asd/qwe" == elektraKeyName(key));
        }

        assert!(elektraKeyIsBelow(key, key2) == 0);
        assert!(elektraKeyIsBelow(key2, key) == 1);

        assert!(elektraKeyAddName(key, "yyyyyyy") == 27);
        assert!(elektraKeyName(key) == "user:/test/qwe/asd/yyyyyyy");
        assert!(elektraKeyBaseName(key) == "yyyyyyy");
        assert!(elektraKeyBaseNameSize(key) == 8);

        assert!(elektraKeySetName(key, "user:/asd/qwe/asd") == 18);
        assert!(elektraKeyName(key) == "user:/asd/qwe/asd");
        assert!(elektraKeyBaseName(key) == "asd");
        assert!(elektraKeyBaseNameSize(key) == 4);

        assert!(elektraKeySetBaseName(key, "") == 1);
        assert!(elektraKeyName(key) == "user:/asd/qwe");
        assert!(elektraKeyBaseName(key) == "qwe");
        assert!(elektraKeyBaseNameSize(key) == 4);

        elektraKeyDel (key);
        elektraKeyDel (key2);
    }

    fn test_key_value() {
        unsafe {
            CKey * key = elektraKeyNew("user:/test/qwe/asd");
        }

        assert!(elektraKeyClear(key) == 1);

        let value: * char = elektraKeyValue(key);
        assert!(value == 0);

        assert!(elektraKeySetValue(key, "abcd", 5) == 5);
        value = elektraKeyValue(key);
        assert!(value = "abcd");

        assert!(elektraKeySetValue(key, "abcd", 5) == 5);
        value = elektraKeyValue(key);
        assert!(value = "abcd");

        elektraKeyDel (key);
    }

    fn test_keyset() {
        ElektraKeySet * ks = elektraKeysetNew (1);
        assert!(elektraKeysetSize (ks) == 0);
        assert!(elektraKeysetIncRef (ks) == 1);
        assert!(elektraKeysetIncRef (ks) == 2);
        assert!(elektraKeysetIncRef (ks) == 3);
        assert!(elektraKeysetIncRef (ks) == 4);
        assert!(elektraKeysetIncRef (ks) == 5);
        assert!(elektraKeysetSize (ks) == 0);

        unsafe {
            ElektraKey * key = elektraKeyNew("user:/test/qwe/asd");
            ElektraKey * key2 = elektraKeyNew("user:/test/qwe/asd/qwe");
        }

        assert!(elektraKeysetAdd(ks, key) == 1);
        assert!(elektraKeysetSize(ks) == 1);

        assert!(elektraKeysetAdd(ks, key2) == 1);
        assert!(elektraKeysetSize(ks) == 2);

        assert!(elektraKeyName (elektraKeysetLookup (ks, key)) == "user:/test/qwe/asd");
        assert!(elektraKeyName (elektraKeysetLookup (ks, key2)) == "user:/test/qwe/asd/qwe");

        elektraKeysetDel (ks);

        elektraKeyDel (key);
        elektraKeyDel (key2);
    }
}
*/