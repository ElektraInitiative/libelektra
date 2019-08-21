use crate::{ReadableKey, StringKey};
use KDBError::*;
use LogicalError::*;
use PermanentError::*;
use ResourceError::*;
use ValidationError::*;

// const ELEKTRA_ERROR_RESOURCE: &str = "C01100";
const ELEKTRA_ERROR_OUT_OF_MEMORY: &str = "C01110";
// const ELEKTRA_ERROR_INSTALLATION: &str = "C01200";
const ELEKTRA_ERROR_INTERNAL: &str = "C01310";
const ELEKTRA_ERROR_INTERFACE: &str = "C01320";
const ELEKTRA_ERROR_PLUGIN_MISBEHAVIOR: &str = "C01330";
const ELEKTRA_ERROR_CONFLICTING_STATE: &str = "C02000";
const ELEKTRA_ERROR_VALIDATION_SYNTACTIC: &str = "C03100";
const ELEKTRA_ERROR_VALIDATION_SEMANTIC: &str = "C03200";

pub enum KDBError {
    Permanent(PermanentError),
    ConflictingState(KDBErrorWrapper),
    Validation(ValidationError),
}

pub enum PermanentError {
    Resource(ResourceError),
    Logical(LogicalError),
    Installation(KDBErrorWrapper),
}

pub enum LogicalError {
    Internal(KDBErrorWrapper),
    Interface(KDBErrorWrapper),
    PluginMisbehavior(KDBErrorWrapper),
}

pub enum ResourceError {
    OutOfMemory(KDBErrorWrapper),
}

pub enum ValidationError {
    Syntactic(KDBErrorWrapper),
    Semantic(KDBErrorWrapper),
}

#[derive(Debug)]
pub struct KDBErrorWrapper {
    error_key: StringKey,
}

impl KDBErrorWrapper {
    /// Constructs a new KDBErrorWrapper from a StringKey.
    /// Only pass keys where the metakeys error/* are set.
    pub fn new(error_key: StringKey) -> KDBErrorWrapper {
        KDBErrorWrapper { error_key }
    }

    // TODO: For which of these error/* can we be sure that they exist?

    /// Returns the error number.
    pub fn number(&self) -> String {
        self.error_key
            .get_meta("error/number")
            .unwrap()
            .get_value()
            .to_owned()
    }

    /// Returns the error reason.
    pub fn reason(&self) -> String {
        self.error_key
            .get_meta("error/reason")
            .unwrap()
            .get_value()
            .to_owned()
    }

    /// Returns the module where the error occured.
    pub fn module(&self) -> String {
        self.error_key
            .get_meta("error/module")
            .unwrap()
            .get_value()
            .to_owned()
    }

    /// Returns a description of the error.
    pub fn description(&self) -> String {
        self.error_key
            .get_meta("error/description")
            .unwrap()
            .get_value()
            .to_owned()
    }

    /// Returns the source file from where the error information comes.
    pub fn file(&self) -> String {
        self.error_key
            .get_meta("error/file")
            .unwrap()
            .get_value()
            .to_owned()
    }

    /// Returns the the exact line of that source file.
    pub fn line(&self) -> String {
        self.error_key
            .get_meta("error/line")
            .unwrap()
            .get_value()
            .to_owned()
    }

    pub fn to_error_message(&self) -> String {
        format!("Sorry, module {module} issued error {error_number}:\n{description}: Validation of key \"{key}\" with string \"{string}\" failed.", 
            module = self.module(),
            error_number = self.number(),
            description = self.description(),
            key = self.error_key.get_name(),
            string = self.error_key.get_value())
    }
}

impl std::fmt::Display for KDBErrorWrapper {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(f, "{}", self.to_error_message())
    }
}

impl std::error::Error for KDBErrorWrapper {}

pub fn map_kdb_error(error_key: StringKey) -> KDBError {
    let err_num_key_res = error_key.get_meta("error/number");
    if let Ok(err_num_key) = err_num_key_res {
        let err_wrapper = KDBErrorWrapper::new(error_key);

        match err_num_key.get_value().as_str() {
            ELEKTRA_ERROR_OUT_OF_MEMORY => {
                return Permanent(Resource(OutOfMemory(err_wrapper)));
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
                unreachable!();
            }
        }
    }
    unreachable!()
}
