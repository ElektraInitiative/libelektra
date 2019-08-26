- infos =
- infos/author = Philipp Gackstatter <philipp.gackstatter@student.tuwien.ac.at>
- infos/status = experimental maintained
- infos/provides =
- infos/description =

# Rust Bindings

Rust bindings for libelektra.

## Usage

### Raw Bindings

Safe wrappers are provided in the `elektra` crate, however you can also use the raw bindings from `elektra_sys` directly. Rust for instance does not allow the definition of variadic functions, but allows calling them. So you can call `keyNew` as you would in C.

```rust
use elektra_sys::{keyNew, keyDel, keyString, KEY_VALUE, KEY_END};

fn main() {
    let key_name = CString::new("user/test/key").unwrap();
    let key_val = CString::new("rust-bindings").unwrap();
    let key = unsafe { keyNew(key_name.as_ptr(), KEY_VALUE, key_val.as_ptr(), KEY_END) };
    let ret_val_str = unsafe { CStr::from_ptr(keyString(key)) };
    assert_eq!(ret_val_str, key_val.as_c_str());
    assert_eq!(unsafe { keyDel(key) }, 0);
}
```

### Key

A full example for using some of the key functionality. For all methods, see the documentation.

```rust
fn main() -> Result<(), KeyError> {
    // To create a simple key with a name and value
    let mut key = StringKey::new("user/test/language")?;
    key.set_value("rust");
    assert_eq!(key.name(), "user/test/language");
    assert_eq!(key.value(), "rust");

    // To iterate over the name
    for name in key.name_iter() {
        println!("Name: {}", name);
    }

    // Duplicate a key
    let key_duplicate = key.duplicate();

    // And compare them
    assert_eq!(key, key_duplicate);

    // To create a key with multiple meta values, use the KeyBuilder
    let mut key: StringKey = KeyBuilder::new("user/test/fruits")?
        .meta("banana", "🍌")?
        .meta("pineapple", "🍍")?
        .meta("strawberry", "🍓")?
        .build();
    assert_eq!(key.meta("pineapple")?.value(), "🍍");

    // We can iterate over the metakeys
    key.rewind_meta();
    for metakey in key.iter() {
        println!("Key: {}, Value: {}", metakey.name(), metakey.value());
    }

    // Delete a metakey
    key.delete_meta("banana")?;

    // Check if key is in the user namespace
    assert!(key.is_user());
}
```

## Generation

Bindings are generated when buildling the `elektra-sys` crate using `rust-bindgen`. The `build.rs` script in the `elektra-sys` crate calls and configures bindgen. It also emits additional configuration for `rustc` to tell it what library to link against, and where to find it.
Bindgen expects a `wrapper.h` file that includes all headers that bindings should be generated for. Since these headers could have additional includes, the `build.rs` also passes the elektra source and binary directory to clang, such that these includes can be found.
Finally, bindgen outputs the bindings into a file, that is then included in the `elektra-sys/lib.rs` file, where it can be used from other crates.

## Troubleshooting

Rust-bindgen needs clang to generate the bindings, so if you encounter the following error, make sure clang (3.9 or higher) is installed.

```
/usr/include/limits.h:123:16: fatal error: 'limits.h' file not found
/usr/include/limits.h:123:16: fatal error: 'limits.h' file not found, err: true
thread 'main' panicked at 'Unable to generate bindings: ()', src/libcore/result.rs:999:5
note: Run with `RUST_BACKTRACE=1` environment variable to display a backtrace.
```

## Usage

### Error Handling

#### KDB

For improved readability, import all the enum variants directly. Through error nesting, you can catch specific errors and ignore other ones.
TODO: Complete example

```rust
use KDBError::*;
use LogicalError::*;
use PermanentError::*;
use ResourceError::*;
use ValidationError::*;

fn call_kdb_function() {
    let res = kdbGet().unwrap_err();
    if let KDBError::Permanent(PermanentError::Logical(LogicalError::Internal(err))) = res {
        // Handle Assertion error
        println!("{:?}", err);
    } else if let KDBError::Validation(ValidationError::Semantic(err)) = res {
        // Handle Semantic error
        println!("{:?}", err);
    } else {
        // Ignore Conflicting State errors
    }
}
```
