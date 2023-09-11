extern crate bindgen;
#[cfg(feature = "pkg-config")]
extern crate pkg_config;

use std::env;
use std::path::PathBuf;

fn main() {

    print_libdir();
    println!("cargo:rustc-link-lib=dylib=elektra-core");
    println!("cargo:rustc-link-lib=dylib=elektra-meta");
    println!("cargo:rustc-link-lib=dylib=elektra-kdb");

    let elektra_include_dir = get_include_dir();

    // The bindgen::Builder is the main entry point
    // to bindgen, and lets you build up options for
    // the resulting bindings.
    let bindings = bindgen::Builder::default()
        // The input header we would like to generate
        // bindings for.
        .header("wrapper.h")
        // Include only the necessary functions and enums
        .allowlist_function("(key|ks|kdb).*")
        .allowlist_var("(KEY|KDB).*")
        // bindgen uses clang for anything C-related.
        // Here we set the necessary include directories
        // such that any includes in the wrapper can be found.
        .clang_arg(format!("-I{}", elektra_include_dir))
        .clang_arg("-I/usr/local/include/elektra")
        // Finish the builder and generate the bindings.
        .generate()
        // Unwrap the Result and panic on failure.
        .expect("Unable to generate bindings");

    // Write the bindings to the $OUT_DIR/bindings.rs file.
    let out_path = PathBuf::from(env::var("OUT_DIR").unwrap());
    bindings
        .write_to_file(out_path.join("bindings.rs"))
        .expect("Couldn't write bindings!");
}

fn get_include_dir() -> String {
    #[cfg(feature = "pkg-config")]
    return pkg_config::get_variable("elektra", "includedir").unwrap_or_else(|e| panic!("pkg_config error {}", e));
    #[cfg(not(feature = "pkg-config"))]
    "/usr/include/elektra".to_owned()
}

fn print_libdir() {
    #[cfg(feature = "pkg-config")]
    println!("cargo:rustc-link-search={}", pkg_config::get_variable("elektra", "libdir").unwrap_or_else(|e| panic!("pkg_config error {}", e)));
}
