extern crate elektra_sys;

extern crate bitflags;

pub mod elektra_kdb;
pub mod elektra_key;
pub mod elektra_keyset;

pub use self::elektra_key::{
    BinaryKey, KeyBuilder, KeyError, ReadOnly, ReadableKey, StringKey, WriteableKey,
};

pub use self::elektra_keyset::{KeySet, KeySetError};

pub use self::elektra_kdb::{
    KDBError, KDBErrorWrapper, LogicalError, PermanentError, ResourceError, ValidationError, KDB,
};
