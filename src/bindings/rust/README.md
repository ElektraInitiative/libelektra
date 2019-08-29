- infos =
- infos/author = Philipp Gackstatter <philipp.gackstatter@student.tuwien.ac.at>
- infos/status = experimental maintained
- infos/provides =
- infos/description =

# Rust Bindings

Rust bindings for libelektra.

## Build

To build the bindings explicitly as part of the elektra build process, we pass the option `-DBINDINGS="rust"` to cmake. After [building the project](../../../doc/COMPILE.md), we can now add the `elektra` crate to the dependencies. The exact path depends on your system.

```toml
[dependencies]
elektra = { version = "0.9.0", path = "~/git/libelektra/build/src/bindings/rust/elektra" }
```

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

A small example for using a Key. For a full example, see the [examples](elektra/src/examples.rs). For all methods, see the documentation.

```rust
extern crate elektra;
use elektra::{ReadableKey, StringKey, WriteableKey};

fn main() -> Result<(), Box<dyn std::error::Error>> {
    // To create a simple key with a name and value
    let mut key = StringKey::new("user/test/language")?;
    key.set_value("rust");

    assert_eq!(key.name(), "user/test/language");
    assert_eq!(key.value(), "rust");

    Ok(())
}
```

Compared to the C-API, there are two distinct key types, `StringKey` and `BinaryKey`. With these, type mismatches such as calling `keyString` on a `BinaryKey` is not possible. The only difference between them is the type of value you can set and get from them. They are only wrappers over the `Key` from the C-API.

The functionality of the keys is split into two traits, `ReadableKey` and `WritableKey`, which define methods that only read information from a key, and modify a key, respectively. For example, the method to retrieve metakeys only returns a key that implements `ReadableKey`, which is named `ReadOnly`. The keys returned cannot be modified in accordance to the design.

## Generation

Bindings are generated when building the `elektra-sys` crate using `rust-bindgen`. The `build.rs` script in the `elektra-sys` crate calls and configures bindgen. It also emits additional configuration for `rustc` to tell it what library to link against, and where to find it.
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
