//! General methods to access the Key database.
//!
//! For example usage see the [Readme](https://github.com/ElektraInitiative/libelektra/tree/master/src/bindings/rust).

use crate::ReadableKey;
use crate::{CopyOption, KeySet, StringKey, WriteableKey};
use std::error::Error;
use std::fmt::{self, Display, Formatter};
use std::ptr::NonNull;

#[derive(Debug)]
pub struct KDB {
    ptr: NonNull<elektra_sys::KDB>,
    _marker: std::marker::PhantomData<elektra_sys::KDB>,
}

impl Drop for KDB {
    fn drop(&mut self) {
        let mut err_key = StringKey::new_empty();
        unsafe {
            elektra_sys::elektraKdbClose(self.as_ptr(), err_key.as_ptr());
        }
    }
}

impl KDB {
    /// Opens the session with the Key database.
    pub fn open<'a, C>(contract: C) -> Result<Self, KDBError<'a>>
    where
        C: Into<Option<KeySet>>,
    {
        let mut key = StringKey::new_empty();
        let contract_opt = contract.into();

        let kdb_ptr = match contract_opt {
            Some(mut contract) => unsafe { elektra_sys::elektraKdbOpen(contract.as_ptr(), key.as_ptr()) },
            None => unsafe { elektra_sys::elektraKdbOpen(std::ptr::null_mut(), key.as_ptr()) },
        };

        if kdb_ptr.is_null() {
            Err(KDBError::new(key.duplicate(CopyOption::KEY_CP_ALL)))
        } else {
            Ok(KDB {
                ptr: NonNull::new(kdb_ptr).unwrap(),
                _marker: std::marker::PhantomData,
            })
        }
    }

    /// Retrieve keys in an atomic and universal way.
    /// Note that the provided keyset is modified and contains the result.
    /// The provided `key` is used to give a hint about which keys should be retrieved.
    /// The return value is true, if the keys were successfully retrieved
    /// and false if there were no changes to the keyset.
    pub fn get<'a>(
        &mut self,
        keyset: &mut KeySet,
        key: &mut StringKey<'a>,
    ) -> Result<bool, KDBError<'a>> {
        let ret_val = unsafe { elektra_sys::elektraKdbGet(self.as_ptr(), keyset.as_ptr(), key.as_ptr()) };

        if ret_val == 1 {
            Ok(true)
        } else if ret_val == 0 {
            Ok(false)
        } else {
            Err(KDBError::new(key.duplicate(CopyOption::KEY_CP_ALL)))
        }
    }

    /// Set keys in an atomic and universal way.
    /// The provided `key` is used to give a hint about which keys should be stored.
    /// The return value is true on success,
    /// and false if there were no changes to the KDB.
    /// # Notes
    /// You have to call [`get`](#method.get) with `keyset` first.
    pub fn set<'a>(
        &mut self,
        keyset: &mut KeySet,
        key: &mut StringKey<'a>,
    ) -> Result<bool, KDBError<'a>> {
        let ret_val = unsafe { elektra_sys::elektraKdbSet(self.as_ptr(), keyset.as_ptr(), key.as_ptr()) };

        if ret_val == 1 {
            Ok(true)
        } else if ret_val == 0 {
            Ok(false)
        } else {
            Err(KDBError::new(key.duplicate(CopyOption::KEY_CP_ALL)))
        }
    }

    /// Returns the raw pointer of the KDB object.
    /// Should be used with caution. In particular,
    /// the pointer should only be modified with
    /// `elektra_sys::kdb*` functions, but `elektraKdbClose`
    /// should not be called.
    pub fn as_ptr(&mut self) -> *mut elektra_sys::KDB {
        self.ptr.as_ptr()
    }
}

impl AsRef<elektra_sys::KDB> for KDB {
    fn as_ref(&self) -> &elektra_sys::KDB {
        unsafe { self.ptr.as_ref() }
    }
}

const ELEKTRA_ERROR_PERMANENT: &str = "C01";
const ELEKTRA_ERROR_RESOURCE: &str = "C011";
const ELEKTRA_ERROR_OUT_OF_MEMORY: &str = "C01110";
const ELEKTRA_ERROR_INSTALLATION: &str = "C012";
const ELEKTRA_ERROR_LOGICAL: &str = "C013";
const ELEKTRA_ERROR_INTERNAL: &str = "C01310";
const ELEKTRA_ERROR_INTERFACE: &str = "C01320";
const ELEKTRA_ERROR_PLUGIN_MISBEHAVIOR: &str = "C01330";
const ELEKTRA_ERROR_CONFLICTING_STATE: &str = "C02";
const ELEKTRA_ERROR_VALIDATION: &str = "C03";
const ELEKTRA_ERROR_VALIDATION_SYNTACTIC: &str = "C03100";
const ELEKTRA_ERROR_VALIDATION_SEMANTIC: &str = "C03200";

/// Represents the failure of a KDB operation.
/// Refer to the [error codes doc](https://master.libelektra.org/doc/decisions/error_codes.md#decision) to see how the errors relate to each other.
///
/// Various `is_` methods are provided to find out which error occurred.
///
/// # Examples
/// ```
/// # use elektra::{KDBError, KeyBuilder, StringKey, WriteableKey};
/// # fn main() {
/// # let error_key = KeyBuilder::new_empty().meta("error/number", "C03100").unwrap().build();
/// # let kdb_error = KDBError::new(error_key);
/// // If we are dealing with a Validation Syntactic error, both is_syntactic
/// // and is_validation will return true
/// assert!(kdb_error.is_syntactic() && kdb_error.is_validation());
/// # }
/// ```
#[derive(Debug)]
pub struct KDBError<'a> {
    error_key: StringKey<'a>,
}

impl<'a> KDBError<'a> {
    /// Constructs a new KDBError from a StringKey.
    /// Only pass keys where the metakeys error/* are set.
    pub fn new(error_key: StringKey) -> KDBError {
        KDBError { error_key }
    }

    fn is_error(&self, error_str: &str) -> bool {
        self.number().starts_with(error_str)
    }

    pub fn is_permanent(&self) -> bool {
        self.is_error(ELEKTRA_ERROR_PERMANENT)
    }

    pub fn is_resource(&self) -> bool {
        self.is_error(ELEKTRA_ERROR_RESOURCE)
    }

    pub fn is_out_of_memory(&self) -> bool {
        self.is_error(ELEKTRA_ERROR_OUT_OF_MEMORY)
    }

    pub fn is_installation(&self) -> bool {
        self.is_error(ELEKTRA_ERROR_INSTALLATION)
    }

    pub fn is_logical(&self) -> bool {
        self.is_error(ELEKTRA_ERROR_LOGICAL)
    }

    pub fn is_internal(&self) -> bool {
        self.is_error(ELEKTRA_ERROR_INTERNAL)
    }

    pub fn is_interface(&self) -> bool {
        self.is_error(ELEKTRA_ERROR_INTERFACE)
    }

    pub fn is_plugin_misbehavior(&self) -> bool {
        self.is_error(ELEKTRA_ERROR_PLUGIN_MISBEHAVIOR)
    }

    pub fn is_conflicting_state(&self) -> bool {
        self.is_error(ELEKTRA_ERROR_CONFLICTING_STATE)
    }

    pub fn is_validation(&self) -> bool {
        self.is_error(ELEKTRA_ERROR_VALIDATION)
    }

    pub fn is_syntactic(&self) -> bool {
        self.is_error(ELEKTRA_ERROR_VALIDATION_SYNTACTIC)
    }

    pub fn is_semantic(&self) -> bool {
        self.is_error(ELEKTRA_ERROR_VALIDATION_SEMANTIC)
    }

    /// Returns the error number or an empty string if unavailable.
    pub fn number(&self) -> String {
        self.error_key_or_empty_string("error/number")
    }

    /// Returns the error reason or an empty string if unavailable.
    pub fn reason(&self) -> String {
        self.error_key_or_empty_string("error/reason")
    }

    /// Returns the module where the error occured or an empty string if unavailable.
    pub fn module(&self) -> String {
        self.error_key_or_empty_string("error/module")
    }

    /// Returns a description of the error or an empty string if unavailable.
    pub fn description(&self) -> String {
        self.error_key_or_empty_string("error/description")
    }

    /// Returns the source file from where the error information comes or an empty string if unavailable.
    pub fn file(&self) -> String {
        self.error_key_or_empty_string("error/file")
    }

    /// Returns the exact line of that source file or an empty string if unavailable.
    pub fn line(&self) -> String {
        self.error_key_or_empty_string("error/line")
    }

    fn error_key_or_empty_string(&self, error_key: &str) -> String {
        if let Ok(meta) = self.error_key.meta(error_key) {
            meta.value().to_owned().to_string()
        } else {
            "".into()
        }
    }

    /// Returns a formatted error message
    pub fn to_error_message(&self) -> String {
        format!(
            "Sorry, module {module} issued error {error_number}:\n{description}: {reason}",
            module = self.module(),
            error_number = self.number(),
            description = self.description(),
            reason = self.reason()
        )
    }
}

impl<'a> Display for KDBError<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), fmt::Error> {
        write!(f, "{}", self.to_error_message())
    }
}

impl<'a> Error for KDBError<'a> {}

#[cfg(test)]
mod error_tests {
    use super::*;
    use crate::KeyBuilder;

    #[rustfmt::skip]
    const ERROR_CODES: [&str; 12] = [
        ELEKTRA_ERROR_PERMANENT,
            ELEKTRA_ERROR_RESOURCE,
                ELEKTRA_ERROR_OUT_OF_MEMORY,
            ELEKTRA_ERROR_INSTALLATION,
            ELEKTRA_ERROR_LOGICAL,
                ELEKTRA_ERROR_INTERNAL,
                ELEKTRA_ERROR_INTERFACE,
                ELEKTRA_ERROR_PLUGIN_MISBEHAVIOR,
        ELEKTRA_ERROR_CONFLICTING_STATE,
        ELEKTRA_ERROR_VALIDATION,
            ELEKTRA_ERROR_VALIDATION_SYNTACTIC,
            ELEKTRA_ERROR_VALIDATION_SEMANTIC,
    ];

    fn test_error(kdb_error: &KDBError, error_codes: &[&str]) {
        for err_code in error_codes {
            println!("Should succeed {}", err_code);
            assert!(kdb_error.is_error(err_code));
        }

        for err_code in ERROR_CODES.iter().filter(|e| !error_codes.contains(e)) {
            println!("Should not succeed {}", err_code);
            assert!(!kdb_error.is_error(err_code));
        }
    }

    #[test]
    fn kdb_test_err_out_of_memory() {
        let error_key = KeyBuilder::new_empty()
            .meta("error/number", ELEKTRA_ERROR_OUT_OF_MEMORY)
            .unwrap()
            .build();
        let kdb_error = KDBError::new(error_key);
        test_error(
            &kdb_error,
            &[
                ELEKTRA_ERROR_PERMANENT,
                ELEKTRA_ERROR_RESOURCE,
                ELEKTRA_ERROR_OUT_OF_MEMORY,
            ],
        );
    }

    #[test]
    fn kdb_test_err_resource() {
        let error_key = KeyBuilder::new_empty()
            .meta("error/number", ELEKTRA_ERROR_RESOURCE)
            .unwrap()
            .build();
        let kdb_error = KDBError::new(error_key);
        test_error(
            &kdb_error,
            &[ELEKTRA_ERROR_PERMANENT, ELEKTRA_ERROR_RESOURCE],
        );
    }

    #[test]
    fn kdb_test_err_permanent() {
        let error_key = KeyBuilder::new_empty()
            .meta("error/number", ELEKTRA_ERROR_PERMANENT)
            .unwrap()
            .build();
        let kdb_error = KDBError::new(error_key);
        test_error(&kdb_error, &[ELEKTRA_ERROR_PERMANENT]);
    }

    #[test]
    fn kdb_test_err_installation() {
        let error_key = KeyBuilder::new_empty()
            .meta("error/number", ELEKTRA_ERROR_INSTALLATION)
            .unwrap()
            .build();
        let kdb_error = KDBError::new(error_key);
        test_error(
            &kdb_error,
            &[ELEKTRA_ERROR_PERMANENT, ELEKTRA_ERROR_INSTALLATION],
        );
    }

    #[test]
    fn kdb_test_err_logical() {
        let error_key = KeyBuilder::new_empty()
            .meta("error/number", ELEKTRA_ERROR_LOGICAL)
            .unwrap()
            .build();
        let kdb_error = KDBError::new(error_key);
        test_error(
            &kdb_error,
            &[ELEKTRA_ERROR_PERMANENT, ELEKTRA_ERROR_LOGICAL],
        );
    }

    #[test]
    fn kdb_test_err_internal() {
        let error_key = KeyBuilder::new_empty()
            .meta("error/number", ELEKTRA_ERROR_INTERNAL)
            .unwrap()
            .build();
        let kdb_error = KDBError::new(error_key);
        test_error(
            &kdb_error,
            &[
                ELEKTRA_ERROR_PERMANENT,
                ELEKTRA_ERROR_LOGICAL,
                ELEKTRA_ERROR_INTERNAL,
            ],
        );
    }

    #[test]
    fn kdb_test_err_interface() {
        let error_key = KeyBuilder::new_empty()
            .meta("error/number", ELEKTRA_ERROR_INTERFACE)
            .unwrap()
            .build();
        let kdb_error = KDBError::new(error_key);
        test_error(
            &kdb_error,
            &[
                ELEKTRA_ERROR_PERMANENT,
                ELEKTRA_ERROR_LOGICAL,
                ELEKTRA_ERROR_INTERFACE,
            ],
        );
    }

    #[test]
    fn kdb_test_err_plugin_misbehavior() {
        let error_key = KeyBuilder::new_empty()
            .meta("error/number", ELEKTRA_ERROR_PLUGIN_MISBEHAVIOR)
            .unwrap()
            .build();
        let kdb_error = KDBError::new(error_key);
        test_error(
            &kdb_error,
            &[
                ELEKTRA_ERROR_PERMANENT,
                ELEKTRA_ERROR_LOGICAL,
                ELEKTRA_ERROR_PLUGIN_MISBEHAVIOR,
            ],
        );
    }

    #[test]
    fn kdb_test_err_conflicting_state() {
        let error_key = KeyBuilder::new_empty()
            .meta("error/number", ELEKTRA_ERROR_CONFLICTING_STATE)
            .unwrap()
            .build();
        let kdb_error = KDBError::new(error_key);
        test_error(&kdb_error, &[ELEKTRA_ERROR_CONFLICTING_STATE]);
    }

    #[test]
    fn kdb_test_err_validation() {
        let error_key = KeyBuilder::new_empty()
            .meta("error/number", ELEKTRA_ERROR_VALIDATION)
            .unwrap()
            .build();
        let kdb_error = KDBError::new(error_key);
        test_error(&kdb_error, &[ELEKTRA_ERROR_VALIDATION]);
    }

    #[test]
    fn kdb_test_err_validation_syntactic() {
        let error_key = KeyBuilder::new_empty()
            .meta("error/number", ELEKTRA_ERROR_VALIDATION_SYNTACTIC)
            .unwrap()
            .build();
        let kdb_error = KDBError::new(error_key);
        test_error(
            &kdb_error,
            &[ELEKTRA_ERROR_VALIDATION, ELEKTRA_ERROR_VALIDATION_SYNTACTIC],
        );
    }

    #[test]
    fn kdb_test_err_validation_semantic() {
        let error_key = KeyBuilder::new_empty()
            .meta("error/number", ELEKTRA_ERROR_VALIDATION_SEMANTIC)
            .unwrap()
            .build();
        let kdb_error = KDBError::new(error_key);
        test_error(
            &kdb_error,
            &[ELEKTRA_ERROR_VALIDATION, ELEKTRA_ERROR_VALIDATION_SEMANTIC],
        );
    }
}

/*
// FIXME [new_backend]: tests disabled
#[cfg(test)]
mod test {
    use super::*;
    use crate::{KeyBuilder, LookupOption};

    const PARENT_KEY: &str = "user:/sw/tests/rust/1/";
    const KEY_1_NAME: &str = "user:/sw/tests/rust/1/key_name";
    const KEY_2_NAME: &str = "user:/sw/tests/rust/1/key_name/2";

    const KEY_1_VALUE: &str = "key_value_1";
    const KEY_2_VALUE: &str = "key_value_2";

    #[test]
    fn test_kdb() {
        set_kdb();
        get_kdb();
        remove_test_keys();
    }

    fn get_parent_key<'a>() -> StringKey<'a> {
        StringKey::new(PARENT_KEY).unwrap_or_else(|e| panic!("{}", e))
    }

    fn set_kdb() {
        let mut parent_key = get_parent_key();
        let mut kdb = KDB::open(None).unwrap_or_else(|e| panic!("{}", e));
        let mut ks = KeySet::with_capacity(10);
        kdb.get(&mut ks, &mut parent_key)
            .unwrap_or_else(|e| panic!("{}", e));

        let key1: StringKey = KeyBuilder::new(KEY_1_NAME)
            .unwrap()
            .value(KEY_1_VALUE)
            .build();
        let key2: StringKey = KeyBuilder::new(KEY_2_NAME)
            .unwrap()
            .value(KEY_2_VALUE)
            .build();

        ks.append_key(key1);
        ks.append_key(key2);
        let set_res = kdb
            .set(&mut ks, &mut parent_key)
            .unwrap_or_else(|e| panic!("{}", e));
        assert_eq!(set_res, true);
    }

    fn get_kdb() {
        let mut parent_key = get_parent_key();
        let mut kdb = KDB::open(None).unwrap_or_else(|e| panic!("{}", e));
        let mut ks = KeySet::with_capacity(2);
        let get_res = kdb
            .get(&mut ks, &mut parent_key)
            .unwrap_or_else(|e| panic!("{}", e));
        assert_eq!(get_res, true);

        let key1_lookup = ks
            .lookup_by_name(KEY_1_NAME, Default::default())
            .unwrap()
            .duplicate(CopyOption::KEY_CP_ALL);
        assert_eq!(key1_lookup.value(), KEY_1_VALUE);

        let key2_lookup = ks
            .lookup_by_name(KEY_2_NAME, Default::default())
            .unwrap()
            .duplicate(CopyOption::KEY_CP_ALL);
        assert_eq!(key2_lookup.value(), KEY_2_VALUE);
    }

    fn remove_test_keys() {
        let mut parent_key = get_parent_key();
        let mut kdb = KDB::open(None).unwrap_or_else(|e| panic!("{}", e));
        let mut ks = KeySet::with_capacity(10);
        let get_res = kdb
            .get(&mut ks, &mut parent_key)
            .unwrap_or_else(|e| panic!("{}", e));
        assert_eq!(get_res, true);

        ks.lookup_by_name(KEY_1_NAME, LookupOption::KDB_O_POP)
            .unwrap();
        ks.lookup_by_name(KEY_2_NAME, LookupOption::KDB_O_POP)
            .unwrap();
        let set_res = kdb
            .set(&mut ks, &mut parent_key)
            .unwrap_or_else(|e| panic!("{}", e));
        assert_eq!(set_res, true);
    }
}
*/
