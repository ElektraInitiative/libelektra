use std::error::Error;
use std::fmt;
// TODO: Separate into structs
#[derive(Debug, PartialEq)]
pub enum KeyError {
    InvalidName,
    TypeMismatch,
    NameReadOnly,
    NotFound,
}

impl fmt::Display for KeyError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        match self {
            KeyError::InvalidName => write!(f, "Key has an invalid name"),
            KeyError::TypeMismatch => write!(f, "Binary/String key mismatch, use the appropriate method for your key type, get_string or get_binary"),
            KeyError::NameReadOnly => write!(f, "Key is read only"),
            KeyError::NotFound => write!(f, "Key/Metakey was not found"),
            // _ => unimplemented!(),
        }
    }
}

impl Error for KeyError {}