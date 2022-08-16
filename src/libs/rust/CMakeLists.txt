cmake_minimum_required(VERSION 3.12)

find_package(Corrosion REQUIRED)

corrosion_import_crate(MANIFEST_PATH libc/Cargo.toml)

add_executable(rust-libc-test libc/examples/basic/main.c)

target_include_directories(rust-libc-test PUBLIC libc/resources/headers)

target_link_libraries(rust-libc-test PUBLIC elektra_rs_libc)
