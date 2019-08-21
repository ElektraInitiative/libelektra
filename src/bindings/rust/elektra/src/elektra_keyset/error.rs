use std::error::Error;
use std::fmt;

#[derive(Debug, PartialEq)]
pub enum KeySetError {
    InsertionFailure,
}

impl fmt::Display for KeySetError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        match self {
            KeySetError::InsertionFailure => write!(f, "Key could not be inserted."),
            // _ => unimplemented!(),
        }
    }
}

impl Error for KeySetError {}
