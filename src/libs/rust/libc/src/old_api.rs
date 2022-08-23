#![allow(non_snake_case)]
#![allow(non_camel_case_types)]

use libc::{ssize_t, size_t, c_char, c_int, c_void};
use elektra_rs_libc::structs::CKeySet;

type KeySet = CKeySet;
type Key = elektra_rs_libc::structs::CKey;

/**************************************
 *
 * Key methods
 *
 **************************************/

/* Basic Methods */
// pub extern "C" fn Key *keyNew (const char *keyname, ...) ELEKTRA_SENTINEL;
// pub extern "C" fn Key *keyVNew (const char *keyname, va_list ap);

#[no_mangle]
pub extern "C" fn keyCopy(dest: *mut Key, source: *const Key, flags: elektraCopyFlags) -> *mut Key {
    elektraKeyCopy(dest, source, flags)
}

#[no_mangle]
pub extern "C" fn keyClear(key: *mut Key) -> c_int {
    elektraKeyClear(key)
}

#[no_mangle]
pub extern "C" fn keyDel(key: *mut Key) -> c_int {
    elektraKeyDel(key)
}


#[no_mangle]
pub extern "C" fn keyIncRef(key: *mut Key) -> u16 {
    1
}

#[no_mangle]
pub extern "C" fn keyDecRef(key: *mut Key) -> u16 {
    1
}

#[no_mangle]
pub extern "C" fn keyGetRef(key: *const Key) -> u16 {
    1
}


/* Meta Info */

// TODO
//pub extern "C" fn const Key *keyNextMeta (Key *key); /* TODO: remove, currently only needed for directoryvalue-plugin */
// TODO
//pub extern "C" fn int keyCopyMeta (Key *dest, const Key *source, const char *metaName);
// TODO
//pub extern "C" fn int keyCopyAllMeta (Key *dest, const Key *source);

// TODO
//pub extern "C" fn const Key *keyGetMeta (const Key *key, const char* metaName);
// TODO
//pub extern "C" fn ssize_t    keySetMeta (Key *key, const char* metaName, const char *newMetaString);

#[no_mangle]
pub extern "C" fn keyMeta(key: *mut Key) -> *mut KeySet {
    elektraKeyMeta(key)
}

/* Methods for Making Tests */
#[no_mangle]
pub extern "C" fn keyCmp(k1: *const Key, k2: *const Key) -> c_int {
    elektraKeyCompareName(k1, k2)
}

#[no_mangle]
pub extern "C" fn keyNeedSync(key: *const Key) -> c_int {
    0
}

#[no_mangle]
pub extern "C" fn keyIsBelow(key: *mut Key, check: *mut Key) -> c_int {
    elektraKeyIsBelow(key, check)
}

#[no_mangle]
pub extern "C" fn keyIsBelowOrSame(key: *mut Key, check: *mut Key) -> c_int {
    elektraKeyIsBelowOrSame(key, check)
}

#[no_mangle]
pub extern "C" fn keyIsDirectlyBelow(key: *const Key, check: *const Key) -> c_int {
    elektraKeyIsDirectlyBelow(key, check)
}

//pub extern "C" fn int keyIsBinary (const Key *key);
//pub extern "C" fn int keyIsString (const Key *key);

/* Name Manipulation Methods */
#[no_mangle]
pub extern "C" fn keyName(key: *const Key) -> *const c_char {
    elektraKeyName(key)
}

#[no_mangle]
pub extern "C" fn keyGetNameSize(key: *const Key) -> ssize_t {
    elektraKeyNameSize(key)
}

#[no_mangle]
pub extern "C" fn keySetName(key: *mut Key, newname: *const c_char) -> ssize_t {
    elektraKeySetName(key, newname)
}

#[no_mangle]
pub extern "C" fn keyAddName(key: *mut Key, addname: *const c_char) -> ssize_t {
    elektraKeyAddName(key, addname)
}

// pub extern "C" fn const void *keyUnescapedName (const Key *key);
// pub extern "C" fn ssize_t keyGetUnescapedNameSize (const Key *key);

#[no_mangle]
pub extern "C" fn keyBaseName(key: *const Key) -> *const c_char {
    elektraKeyBaseName(key)
}

#[no_mangle]
pub extern "C" fn keyGetBaseNameSize(key: *const Key) -> ssize_t {
    elektraKeyBaseNameSize(key)
}

// pub extern "C" fn keyGetBaseName (const Key *key, char *returned, size_t maxSize) -> ssize_t {

#[no_mangle]
pub extern "C" fn keySetBaseName(key: *mut Key, baseName: *const c_char) -> ssize_t {
    elektraKeySetBaseName(key, baseName)
}

#[no_mangle]
pub extern "C" fn keyAddBaseName(key: *mut Key, baseName: *const c_char) -> ssize_t {
    elektraKeyAddBaseName(key, baseName)
}

#[no_mangle]
pub extern "C" fn keyNamespace(key: *const Key) -> elektraNamespace {
    elektraKeyNamespace(key)
}

#[no_mangle]
pub extern "C" fn keySetNamespace(key: *mut Key, ns: elektraNamespace) -> ssize_t {
    elektraKeySetNamespace(key, ns)
}

#[no_mangle]
pub extern "C" fn keyValue(key: *const Key) -> *const c_void {
    elektraKeyValue(key)
}

#[no_mangle]
pub extern "C" fn keyGetValueSize(key: *const Key) -> ssize_t {
    elektraKeyValueSize(key)
}

// pub extern "C" fn keyString (const Key *key) -> *const c_char;
// pub extern "C" fn keyGetString (key: *const Key, returnedString: *mut c_char, size_t maxSize) -> ssize_t {
// pub extern "C" fn keySetString (key: *mut Key, const char *newString) -> ssize_t {
// pub extern "C" fn keyGetBinary (key: *const Key, void *returnedBinary, size_t maxSize) -> ssize_t {
// pub extern "C" fn keySetBinary (key: *mut Key, const void *newBinary, size_t dataSize) -> ssize_t {

#[no_mangle]
pub extern "C" fn keyLock(key: *mut Key, what: elektraLockFlags) -> c_int {
    elektraKeyLock(key, what)
}

#[no_mangle]
pub extern "C" fn keyIsLocked(key: *const Key, what: elektraLockFlags) -> c_int {
    elektraKeyIsLocked(key, what)
}

// static inline Key *keyDup (const Key *source, elektraCopyFlags flags)

/**************************************
 *
 * KeySet methods
 *
 **************************************/

//pub extern "C" fn KeySet *ksNew (size_t alloc, ...) ELEKTRA_SENTINEL;
//pub extern "C" fn KeySet *ksVNew (size_t alloc, va_list ap);

// pub extern "C" fn KeySet *ksDup (const KeySet * source);
// pub extern "C" fn int ksCopy (KeySet *dest, const KeySet *source);

#[no_mangle]
pub extern "C" fn ksIncRef(ks: *mut KeySet) -> u16 {
    elektraKeysetIncRef(ks)
}

#[no_mangle]
pub extern "C" fn ksDecRef(ks: *mut KeySet) -> u16 {
    elektraKeysetDecRef(ks)
}

#[no_mangle]
pub extern "C" fn ksGetRef(ks: *const KeySet) -> u16 {
    elektraKeysetGetRef(ks)
}

#[no_mangle]
pub extern "C" fn ksClear(ks: *mut KeySet) -> c_int {
    elektraKeysetClear(ks)
}

#[no_mangle]
pub extern "C" fn ksDel(ks: *mut KeySet) -> c_int {
    elektraKeysetDel(ks)
}

#[no_mangle]
pub extern "C" fn ksGetSize(ks: *const KeySet) -> ssize_t {
    elektraKeysetSize(ks)
}

#[no_mangle]
pub extern "C" fn ksAppendKey(ks: *mut KeySet, toAppend: *mut Key) -> ssize_t {
    elektraKeysetAdd(ks, toAppend)
}

#[no_mangle]
pub extern "C" fn ksAppend(ks: *mut KeySet, toAppend: *const KeySet) -> ssize_t {
    elektraKeysetAddAll(ks, toAppend)
}

// pub extern "C" fn KeySet *ksCut (KeySet *ks, const Key *cutpoint);

// pub extern "C" fn Key *ksPop (KeySet *ks);

// pub extern "C" fn int ksRewind (KeySet *ks);
// pub extern "C" fn Key *ksNext (KeySet *ks);
// pub extern "C" fn Key *ksCurrent (const KeySet *ks);

// pub extern "C" fn elektraCursor ksGetCursor (const KeySet *ks);
// pub extern "C" fn int ksSetCursor (KeySet *ks, elektraCursor cursor);
// pub extern "C" fn Key *ksAtCursor (const KeySet *ks, elektraCursor cursor);

#[no_mangle]
pub extern "C" fn ksLookup(ks: *mut KeySet, k: *mut Key, options: elektraLookupFlags) -> *mut Key {
    elektraKeysetLookup(ks, k)
}

#[no_mangle]
pub extern "C" fn ksLookupByName(ks: *mut KeySet, name: *const c_char, options: elektraLookupFlags) -> *mut Key {
    elektraKeysetLookupByName(ks, name)
}

#[no_mangle]
pub extern "C" fn ksSearch(ks: *const KeySet, toAppend: *const Key) -> ssize_t {
    elektraKeysetSearch(ks, toAppend)
}