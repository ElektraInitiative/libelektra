cmake_minimum_required(VERSION 3.12)

find_package(Corrosion REQUIRED)

corrosion_import_crate(MANIFEST_PATH libc/Cargo.toml)