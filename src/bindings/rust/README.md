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

Bindings are generated when buildling the `elektra-sys` crate using `rust-bindgen`.

Additionally, clang is needed (`clang-4.0` worked), otherwise

```
/usr/include/limits.h:123:16: fatal error: 'limits.h' file not found
/usr/include/limits.h:123:16: fatal error: 'limits.h' file not found, err: true
thread 'main' panicked at 'Unable to generate bindings: ()', src/libcore/result.rs:999:5
note: Run with `RUST_BACKTRACE=1` environment variable to display a backtrace.
```
