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
