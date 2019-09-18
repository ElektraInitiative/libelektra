//! # Elektra
//! Safe bindings for libelektra.
//! 
//! See the [project's readme](https://master.libelektra.org/src/bindings/rust) for an introduction and examples.

extern crate bitflags;
extern crate elektra_sys;

/// `StringKey` and `BinaryKey` are the essential structs that encapsulate name, value and metainfo.
pub mod key;
/// `KeyBuilder` can easily build keys with many meta values.
pub mod keybuilder;
/// Trait to read values from a key.
pub mod readable;
/// A wrapper Trait to make keys readonly.
pub mod readonly;
/// Trait to write values to a key.
pub mod writable;
/// `KeySet` is a set of keys.
pub mod keyset;
/// General methods to access the Key database.
pub mod kdb;

pub use self::key::{BinaryKey, StringKey, MetaIter, NameIter, KeyNameInvalidError, KeyNameReadOnlyError, KeyNotFoundError};
pub use self::keybuilder::KeyBuilder;
pub use self::readable::ReadableKey;
pub use self::readonly::ReadOnly;
pub use self::writable::WriteableKey;
pub use self::keyset::{KeySet, ReadOnlyStringKeyIter, StringKeyIter, Cursor, LookupOption};
pub use self::kdb::{KDB, KDBError, KDBErrorWrapper, LogicalError, PermanentError, ResourceError, ValidationError};
