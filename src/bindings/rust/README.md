- infos =
- infos/author = Philipp Gackstatter <philipp.gackstatter@student.tuwien.ac.at>
- infos/status = experimental maintained
- infos/provides =
- infos/description =

# Rust Bindings for Elektra

<div align="center">
  <!-- Crates version -->
  <a href="https://crates.io/crates/elektra">
    <img src="https://img.shields.io/crates/v/elektra.svg"
    alt="Crates.io version" />
  </a>
  <!-- docs.rs docs -->
  <a href="https://docs.rs/elektra">
    <img src="https://img.shields.io/badge/docs-latest-blue.svg"
      alt="docs.rs docs" />
  </a>
</div>

_Elektra serves as a universal and secure framework to access configuration parameters in a global, hierarchical key database._

For more information about Elektra itself, visit the [website](https://libelektra.org).

## Build

Depending on how you installed libelektra, you should use different ways to get the bindings. If you installed it with a package manager, you should use the crates from [crates.io](https://crates.io/). If you built libelektra locally, you should use the bindings that are built in the `build` directory.

### Package Manager

If you installed elektra via a package manager, you should use the [elektra](https://crates.io/crate/elektra) crate or [elektra-sys](https://crates.io/crate/elektra-sys) crate if you need the raw bindings. In this case you will need `libelektra` itself, as well as the development headers (often called `libelektra-dev`) for bindings generation.
The `elektra-sys`, as well as the `elektra` crate have a feature called `pkg-config` that you can enable to find the installation of elektra and its headers. It is not enabled by default, but recommended and you can do so by adding `features = ["pkg-config"]` to the dependency section as seen below. The `pkg-config` utility has to be installed then. Your Cargo.toml dependencies might thus look like this

```toml
[dependencies]
elektra = { version = "0.9.10", features = ["pkg-config"] }
# Directly depending on elektra-sys is only needed if you need to use the raw bindings
elektra-sys = { version = "0.9.10", features = ["pkg-config"] }
```

If you don't use the `pkg-config` feature, the build script will look for the Elektra installation in `/usr/local/include/elektra` and `/usr/include/elektra`.

With this in place, the bindings should be built when you run `cargo build`.

### Local Build

To build the bindings explicitly as part of the Elektra build process, we add the option `rust` to `-DBINDINGS`. Now [build libelektra](https://master.libelektra.org/doc/COMPILE.md) and the bindings will be built as part of this process.

Your Cargo.toml dependencies might then look like this

```toml
[dependencies]
elektra = { path = "../libelektra/build/src/bindings/rust/elektra/"}
```

## Example

Note that your dynamic linker must be able to find `libelektra-{core,meta,kdb}`. If you just compiled it, you can run `source ../scripts/dev/run_env` from the `build` directory to modify your `PATH` appropriately.

See the `example` directory for a fully setup project. To run it, change directories into `build/src/bindings/rust/example/` and run `cargo run --bin key`.

To start with a new project, use `cargo new elektra_rust`. Now add the `elektra` crate to the dependencies. The crate is in the `src/bindings/rust` subdirectory of your `build` directory, so the exact paths depends on your system. Change the paths (and possibly version) appropriately and add the following dependencies to your `Cargo.toml`.

```toml
[dependencies]
elektra = { version = "0.9.10", path = "~/git/libelektra/build/src/bindings/rust/elektra" }
# Directly depending on elektra-sys is only needed if you need to use the raw bindings
elektra-sys = { version = "0.9.10", path = "~/git/libelektra/build/src/bindings/rust/elektra-sys" }
```

If you run `cargo run` and everything builds correctly and prints `Hello, world!`, you can replace the contents of `main.rs` with the examples shown in the next section.

## Usage

### Key

An example for using a `StringKey`. Run it from the `example` directory using `cargo run --bin key`. See the [full example](https://master.libelektra.org/src/bindings/rust/example/src/bin/key.rs) for more.

```rust
extern crate elektra;
use elektra::{ReadableKey, StringKey, WriteableKey};

fn main() -> Result<(), Box<dyn std::error::Error>> {
    // To create a simple key with a name and value
    let mut key = StringKey::new("user:/test/language")?;
    key.set_value("rust");

    println!("Key with name {} has value {}", key.name(), key.value());

    Ok(())
}
```

Compared to the C-API, there are two distinct key types, `StringKey` and `BinaryKey`. With these, type mismatches such as calling `keyString` on a `BinaryKey` is not possible. The only difference between them is the type of value you can set and get from them. They are only wrappers over the `Key` from the C-API.

Use a `BinaryKey` for setting arbitrary byte values.

```rust
extern crate elektra;
use elektra::{BinaryKey, ReadableKey, WriteableKey};

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let binary_content: [u8; 7] = [25, 34, 0, 254, 1, 0, 7];
    let mut key = BinaryKey::new("user:/test/rust")?;
    key.set_value(&binary_content);
    let read_content = key.value();

    println!(
        "Key with name {} holds bytes {:?}",
        key.name(),
        read_content
    );

    Ok(())
}
```

The functionality of the keys is split into two traits, `ReadableKey` and `WritableKey`, which define methods that only read information from a key, and modify a key, respectively. For example, the method to retrieve metakeys only returns a key that implements `ReadableKey`, which is one of the keys in a `ReadOnly` wrapper.

### KeySet

A KeySet is a set of StringKeys.

- You can create an empty keyset with `new` or preallocate space for a number of keys with `with_capacity`.
- It has two implementations of the `Iterator` trait, so you can iterate immutably or mutably.

See the [full example](https://master.libelektra.org/src/bindings/rust/example/src/bin/keyset.rs) for more. Run it from the `example` directory using `cargo run --bin keyset`.

```rust
extern crate elektra;
use elektra::{KeyBuilder, KeySet, ReadableKey, StringKey, keyset};

fn main() -> Result<(), Box<dyn std::error::Error>> {
    // keyset! works just like vec!
    let keyset = keyset![
        KeyBuilder::<StringKey>::new("user:/sw/app/#1/host")?
            .value("localhost")
            .build(),
        KeyBuilder::<StringKey>::new("user:/sw/app/#1/port")?
            .value("8080")
            .build(),
    ];

    // Iterate the keyset
    for key in keyset.iter() {
        println!("Key ({}, {})", key.name(), key.value());
    }

    Ok(())
}
```

A `KeySet` only contains `StringKey`s, since they are far more prevalent than `BinaryKey`s. However since the underlying KeySet holds generic `Key`s, `BinaryKey`s can occur. You can cast between the two keys, by using the `From` trait. This is safe memory-wise, but can be unsafe if you cast a `BinaryKey` holding arbitrary bytes to a `StringKey`. You can use `is_string` or `is_binary` to find out whether the cast is safe.

```rust
let mut key = StringKey::new("user:/test/language")?;

// Cast the StringKey to BinaryKey
let binary_key = BinaryKey::from(key);

// And cast it back
let string_key = StringKey::from(binary_key);
```

### KDB

With the `KDB` struct you can access the key database.
See the [full example](https://master.libelektra.org/src/bindings/rust/example/src/bin/kdb.rs) for more. Run it from the `example` directory using `cargo run --bin kdb`.

The KDB error types are nested, so you can match on a high-level or a specific one. You might want to match all validation errors using `kdb_error.is_validation()` which would include both syntactic and semantic validation errors.
For an in-depth explanation of the error types, see the [error guideline](https://master.libelektra.org/doc/dev/error-categorization.md).

```rust
extern crate elektra;

use elektra::{KeySet, StringKey, WriteableKey, KDB};

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let contract = KeySet::with_capacity(0);

    // Open a KDB session
    let mut kdb = KDB::open(contract)?;

    // Create a keyset that will hold the keys we get from the get call
    let mut ks = KeySet::with_capacity(10);

    // Get the current state of the key database
    let mut parent_key = StringKey::new("user:/test")?;
    let get_res = kdb.get(&mut ks, &mut parent_key);

    if let Err(kdb_error) = get_res {
        if kdb_error.is_validation() {
            // Handle the validation error, which could be syntactic or semantic
            // You could use is_semantic() or is_syntactic() to match further.
            Ok(())
        } else {
            // Otherwise propagate the error up
            Err(Box::new(kdb_error))
        }
    } else {
        Ok(())
    }
}
```

### Raw Bindings

Safe wrappers are provided in the `elektra` crate, however you can also use the raw bindings from `elektra_sys` directly. Rust for instance does not allow the definition of variadic functions, but allows calling them. So you can call `keyNew` as you would in C.

```rust
extern crate elektra_sys;
use elektra_sys::{keyDel, keyName, keyNew, keyString, KEY_END, KEY_VALUE};
use std::ffi::{CStr, CString};

fn main() {
    let key_name = CString::new("user:/test/key").unwrap();
    let key_val = CString::new("rust-bindings").unwrap();
    let key = unsafe { keyNew(key_name.as_ptr(), KEY_VALUE, key_val.as_ptr(), KEY_END) };
    let name_str = unsafe { CStr::from_ptr(keyName(key)) };
    let val_str = unsafe { CStr::from_ptr(keyString(key)) };
    println!("Key with name {:?} has value {:?}", name_str, val_str);
    assert_eq!(unsafe { keyDel(key) }, 0);
}
```

## Documentation

Is automatically built on `docs.rs` for [elektra](https://docs.rs/elektra) and [elektra-sys](https://docs.rs/elektra-sys/). Note that since `elektra-sys` is a one-to-one translation of the C API, it doesn't have documentation and you should instead use the [C docs](https://doc.libelektra.org/api/latest/html/index.html) directly.

Documentation can also be built in the `src/bindings/rust/` subdirectory of the **build** directory, by running `cargo doc` and opening `target/doc/elektra/index.html`.

## Generation

Bindings are generated when building the `elektra-sys` crate using `rust-bindgen`. The `build.rs` script in the `elektra-sys` crate calls and configures bindgen. It also emits additional configuration for `rustc` to tell it what library to link against, and where to find it.
Bindgen expects a `wrapper.h` file that includes all headers that bindings should be generated for. Finally, bindgen outputs the bindings into a file, that is then included in the `elektra-sys/lib.rs` file, where it can be used from other crates.

## Troubleshooting

Rust-bindgen needs clang to generate the bindings, so if you encounter the following error, make sure clang (3.9 or higher) is installed.

```
/usr/include/limits.h:123:16: fatal error: 'limits.h' file not found
/usr/include/limits.h:123:16: fatal error: 'limits.h' file not found, err: true
thread 'main' panicked at 'Unable to generate bindings: ()', src/libcore/result.rs:999:5
note: Run with `RUST_BACKTRACE=1` environment variable to display a backtrace.
```
