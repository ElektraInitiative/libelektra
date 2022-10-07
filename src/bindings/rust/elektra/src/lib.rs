//! # Elektra
//! Safe bindings for [libelektra](https://www.libelektra.org).
//!
//! See the [project's readme](https://master.libelektra.org/src/bindings/rust) for an introduction and examples.
//!
//! The crate consists of three major parts.
//!
//! - The [keys](key/index.html) that encapsulate name, value and metainfo
//! - A [`KeySet`](keyset/index.html) holds a set of `StringKey`s, since these are the most common type of key
//! - [`KDB`](kdb/index.html) allows access to the persistent key database by reading or writing `KeySet`s
//!
//! Refer to the documentation of the modules to learn more about each.

extern crate bitflags;
extern crate elektra_sys;

pub mod key;
pub mod keybuilder;
/// Trait to read values from a key.
pub mod readable;
/// A wrapper Trait to make keys readonly.
pub mod readonly;
/// Trait to write values to a key.
pub mod writeable;
pub mod keyset;
pub mod kdb;

pub use self::key::{BinaryKey, StringKey, NameIter, KeyNameInvalidError, KeyNameReadOnlyError, KeyNotFoundError, CopyOption};
pub use self::keybuilder::KeyBuilder;
pub use self::readable::ReadableKey;
pub use self::readonly::ReadOnly;
pub use self::writeable::WriteableKey;
pub use self::keyset::{KeySet, ReadOnlyStringKeyIter, StringKeyIter, Cursor, LookupOption};
pub use self::kdb::{KDB, KDBError};
