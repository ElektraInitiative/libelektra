# Publish to crates.io

Ideally, make a copy of the elektra or elektra-sys crate respectively before publishing a new version.
Switch to that new directory, then

1. Delete any \*.in files, since they are only meant for cmake.
2. Rename the publish_Cargo.toml and publish_build.rs to Cargo.toml and build.rs respectively.
3. Set the updated version correctly in Cargo.toml
4. `cargo login $api_token`, then `cargo publish` (possibly with a --dry-run first)
