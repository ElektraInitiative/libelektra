use crate::ReadableKey;
use crate::{KeySet, StringKey, WriteableKey};
use std::error::Error;
use std::fmt::{self, Display, Formatter};
use std::ptr::NonNull;
use {KDBError::*, LogicalError::*, PermanentError::*, ResourceError::*, ValidationError::*};

/// General methods to access the Key database.
/// For example usage see [the Readme](https://github.com/ElektraInitiative/libelektra/tree/master/src/bindings/rust).
#[derive(Debug)]
pub struct KDB {
    ptr: NonNull<elektra_sys::KDB>,
    _marker: std::marker::PhantomData<elektra_sys::KDB>,
}

impl Drop for KDB {
    fn drop(&mut self) {
        let mut err_key = StringKey::new_empty();
        unsafe {
            elektra_sys::kdbClose(self.as_ptr(), err_key.as_ptr());
        }
    }
}

impl KDB {
    /// Opens the session with the Key database.
    pub fn open<'a>() -> Result<Self, KDBError<'a>> {
        let mut key = StringKey::new_empty();
        let kdb_ptr = unsafe { elektra_sys::kdbOpen(key.as_ptr()) };

        if kdb_ptr.is_null() {
            Err(map_kdb_error(&key))
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
        let ret_val = unsafe { elektra_sys::kdbGet(self.as_ptr(), keyset.as_ptr(), key.as_ptr()) };

        if ret_val == 1 {
            Ok(true)
        } else if ret_val == 0 {
            Ok(false)
        } else {
            Err(map_kdb_error(key))
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
        let ret_val = unsafe { elektra_sys::kdbSet(self.as_ptr(), keyset.as_ptr(), key.as_ptr()) };

        if ret_val == 1 {
            Ok(true)
        } else if ret_val == 0 {
            Ok(false)
        } else {
            Err(map_kdb_error(key))
        }
    }

    /// This method can be used the given KDB handle meets certain clauses, specified in contract.
    /// The return value is true on success,
    /// and false if clauses of the contract are unmet.
    pub fn ensure<'a>(
        &mut self,
        keyset: &mut KeySet,
        key: &mut StringKey<'a>,
    ) -> Result<bool, KDBError<'a>> {
        let ret_val =
            unsafe { elektra_sys::kdbEnsure(self.as_ptr(), keyset.as_ptr(), key.as_ptr()) };

        if ret_val == 0 {
            Ok(true)
        } else if ret_val == 1 {
            Ok(false)
        } else {
            Err(map_kdb_error(key))
        }
    }
    /// Returns the raw pointer of the KDB object.
    /// Should be used with caution. In particular,
    /// the pointer should only be modified with
    /// `elektra_sys::kdb*` functions, but `kdbClose`
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
const ELEKTRA_ERROR_RESOURCE: &str = "C01100";
const ELEKTRA_ERROR_OUT_OF_MEMORY: &str = "C01110";
const ELEKTRA_ERROR_INTERNAL: &str = "C01310";
const ELEKTRA_ERROR_INSTALLATION: &str = "C01200";
const ELEKTRA_ERROR_INTERFACE: &str = "C01320";
const ELEKTRA_ERROR_PLUGIN_MISBEHAVIOR: &str = "C01330";
const ELEKTRA_ERROR_CONFLICTING_STATE: &str = "C02000";
const ELEKTRA_ERROR_VALIDATION_SYNTACTIC: &str = "C03100";
const ELEKTRA_ERROR_VALIDATION_SEMANTIC: &str = "C03200";

#[derive(Debug)]
pub enum KDBError<'a> {
    Permanent(PermanentError<'a>),
    ConflictingState(KDBErrorWrapper<'a>),
    Validation(ValidationError<'a>),
}

#[derive(Debug)]
pub enum PermanentError<'a> {
    Resource(ResourceError<'a>),
    Logical(LogicalError<'a>),
    Installation(KDBErrorWrapper<'a>),
}

#[derive(Debug)]
pub enum LogicalError<'a> {
    Internal(KDBErrorWrapper<'a>),
    Interface(KDBErrorWrapper<'a>),
    PluginMisbehavior(KDBErrorWrapper<'a>),
}

#[derive(Debug)]
pub enum ResourceError<'a> {
    GeneralResourceError(KDBErrorWrapper<'a>),
    OutOfMemory(KDBErrorWrapper<'a>),
}

#[derive(Debug)]
pub enum ValidationError<'a> {
    Syntactic(KDBErrorWrapper<'a>),
    Semantic(KDBErrorWrapper<'a>),
}

/// Wraps a key that contains error metakeys
#[derive(Debug)]
pub struct KDBErrorWrapper<'a> {
    error_key: StringKey<'a>,
}

impl<'a> KDBErrorWrapper<'a> {
    /// Constructs a new KDBErrorWrapper from a StringKey.
    /// Only pass keys where the metakeys error/* are set.
    pub fn new(error_key: StringKey) -> KDBErrorWrapper {
        KDBErrorWrapper { error_key }
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

// Display + Error impl for all variants. Needed such that an error can be catched an printed, instead of
// matched all the way down, since only KDBErrorWrapper implements the actual message conversion.

impl<'a> Display for KDBError<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), fmt::Error> {
        match self {
            KDBError::ConflictingState(err) => write!(f, "{}", err),
            KDBError::Permanent(err) => write!(f, "{}", err),
            KDBError::Validation(err) => write!(f, "{}", err),
        }
    }
}

impl<'a> Error for KDBError<'a> {}

impl<'a> Display for PermanentError<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), fmt::Error> {
        match self {
            PermanentError::Installation(err) => write!(f, "{}", err),
            PermanentError::Logical(err) => write!(f, "{}", err),
            PermanentError::Resource(err) => write!(f, "{}", err),
        }
    }
}

impl<'a> Error for PermanentError<'a> {}

impl<'a> Display for ResourceError<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), fmt::Error> {
        match self {
            ResourceError::GeneralResourceError(err) => write!(f, "{}", err),
            ResourceError::OutOfMemory(err) => write!(f, "{}", err),
        }
    }
}

impl<'a> Error for ResourceError<'a> {}

impl<'a> Display for LogicalError<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), fmt::Error> {
        match self {
            LogicalError::Interface(err) => write!(f, "{}", err),
            LogicalError::Internal(err) => write!(f, "{}", err),
            LogicalError::PluginMisbehavior(err) => write!(f, "{}", err),
        }
    }
}

impl<'a> Error for LogicalError<'a> {}

impl<'a> Display for ValidationError<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), fmt::Error> {
        match self {
            ValidationError::Semantic(err) => write!(f, "{}", err),
            ValidationError::Syntactic(err) => write!(f, "{}", err),
        }
    }
}

impl<'a> Error for ValidationError<'a> {}

impl<'a> Display for KDBErrorWrapper<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), fmt::Error> {
        write!(f, "{}", self.to_error_message())
    }
}

impl<'a> Error for KDBErrorWrapper<'a> {}

fn map_kdb_error<'a, 'b>(error_key: &'a StringKey) -> KDBError<'b> {
    let err_num_key_res = error_key.meta("error/number");
    if let Ok(err_num_key) = err_num_key_res {
        let err_wrapper = KDBErrorWrapper::new(error_key.duplicate());

        match err_num_key.value().to_owned().to_string().as_str() {
            ELEKTRA_ERROR_RESOURCE => {
                return Permanent(Resource(GeneralResourceError(err_wrapper)));
            }
            ELEKTRA_ERROR_OUT_OF_MEMORY => {
                return Permanent(Resource(OutOfMemory(err_wrapper)));
            }
            ELEKTRA_ERROR_INSTALLATION => {
                return Permanent(Installation(err_wrapper));
            }
            ELEKTRA_ERROR_INTERNAL => {
                return Permanent(Logical(Internal(err_wrapper)));
            }
            ELEKTRA_ERROR_INTERFACE => {
                return Permanent(Logical(Interface(err_wrapper)));
            }
            ELEKTRA_ERROR_PLUGIN_MISBEHAVIOR => {
                return Permanent(Logical(PluginMisbehavior(err_wrapper)));
            }
            ELEKTRA_ERROR_CONFLICTING_STATE => {
                return ConflictingState(err_wrapper);
            }
            ELEKTRA_ERROR_VALIDATION_SYNTACTIC => {
                return Validation(Syntactic(err_wrapper));
            }
            ELEKTRA_ERROR_VALIDATION_SEMANTIC => {
                return Validation(Semantic(err_wrapper));
            }
            _ => {
                panic!("Unknown error code {}. Error Message: {}", err_num_key.value(), err_wrapper.to_error_message());
            }
        }
    }
    panic!("No error/number metakey is available.")
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::{KeyBuilder, LookupOption};

    const PARENT_KEY: &str = "user/sw/tests/rust/1/";
    const KEY_1_NAME: &str = "user/sw/tests/rust/1/key_name";
    const KEY_2_NAME: &str = "user/sw/tests/rust/1/key_name/2";

    const KEY_1_VALUE: &str = "key_value_1";
    const KEY_2_VALUE: &str = "key_value_2";

    fn create_test_keys<'a>() -> (StringKey<'a>, StringKey<'a>, StringKey<'a>) {
        let parent_key = StringKey::new(PARENT_KEY).unwrap_or_else(|e| panic!("{}", e));
        let key1: StringKey = KeyBuilder::new(KEY_1_NAME)
            .unwrap()
            .value(KEY_1_VALUE)
            .build();
        let key2: StringKey = KeyBuilder::new(KEY_2_NAME)
            .unwrap()
            .value(KEY_2_VALUE)
            .build();
        (parent_key, key1, key2)
    }

    #[test]
    fn can_use_kdb() {
        let (mut parent_key, key1, key2) = create_test_keys();
        {
            let mut kdb = KDB::open().unwrap_or_else(|e| panic!("{}", e));
            let mut ks = KeySet::with_capacity(10);
            kdb.get(&mut ks, &mut parent_key).unwrap_or_else(|e| panic!("{}", e));
            ks.append_key(key1);
            ks.append_key(key2);
            kdb.set(&mut ks, &mut parent_key).unwrap_or_else(|e| panic!("{}", e));
        }
        {
            let mut kdb = KDB::open().unwrap_or_else(|e| panic!("{}", e));
            let mut ks = KeySet::with_capacity(2);
            kdb.get(&mut ks, &mut parent_key).unwrap_or_else(|e| panic!("{}", e));
            let key1_lookup = ks
                .lookup_by_name(KEY_1_NAME, Default::default())
                .unwrap()
                .duplicate();
            assert_eq!(key1_lookup.value(), KEY_1_VALUE);

            let key2_lookup = ks
                .lookup_by_name(KEY_2_NAME, Default::default())
                .unwrap()
                .duplicate();
            assert_eq!(key2_lookup.value(), KEY_2_VALUE);
        }
        remove_test_keys();
    }

    fn remove_test_keys() {
        let (mut parent_key, _, _) = create_test_keys();
        let mut kdb = KDB::open().unwrap_or_else(|e| panic!("{}", e));
        let mut ks = KeySet::with_capacity(10);
        kdb.get(&mut ks, &mut parent_key).unwrap_or_else(|e| panic!("{}", e));
        ks.lookup_by_name(KEY_1_NAME, LookupOption::KDB_O_POP)
            .unwrap();
        ks.lookup_by_name(KEY_2_NAME, LookupOption::KDB_O_POP)
            .unwrap();
        kdb.set(&mut ks, &mut parent_key).unwrap_or_else(|e| panic!("{}", e));
    }
}
