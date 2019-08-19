- infos =
- infos/author = Philipp Gackstatter <philipp.gackstatter@student.tuwien.ac.at>
- infos/status = experimental maintained
- infos/provides =
- infos/description =

# Rust Bindings

Rust bindings for libelektra.

## Generation

Using the `buildelektra-stretch-full` docker image, we have to install cargo and rustc via [rustup](https://rustup.rs).
We also need to install rusts formattings tools with `rustup component add rustfmt`.

Bindings are generated when buildling the `elektra-sys` crate using `rust-bindgen`. The `build.rs` script in the `elektra-sys` crate calls and configures bindgen. It also emits additional configuration for `rustc` to tell it what library to link against, and where to find it.
Bindgen expects a `wrapper.h` file that includes all headers that bindings should be generated for. Since these headers could have additional includes, the `build.rs` also passes the elektra source and binary directory to clang, such that these includes can be found.
Finally, bindgen outputs the bindings into a file, that is then included in the `elektra-sys/lib.rs` file, where it can be used from other crates.

## Troublshooting

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
