# Library Directory Structure

## Problem

Currently `src/libs/elektra` contains the source of several libraries. Each of the libraries should have its own folder.
Furthermore, we need to allow rust source to exist in parallel.

## Constraints

- It must be possible to set up include-paths via CMake so that `#include <elektra/foo/header.h>` works the same in the build directory and when installed.

## Assumptions

## Considered Alternatives

## Decision

We use this directory tree (YAML syntax for highlighting):

```yaml
doc:
examples:
scripts:
src:
  include:
   # headers that don't belong to a specific library
  libs:
    core-c: C implementation of libelektra-core.so
    core-rust: Rust implementation of libelektra-core.so
    opts: libelektra-opts.so has only a single implementation, so no suffix
  # plugins and tools use the same suffix idea as above
  plugins:
  tools:
  bindings:
    rust: Rust bindings
    cpp: C++ bindings
    java: Java bindings
  tests:
    shell: shell script tests
    cframework: framework for C tests
    gtest-framework: framework for C++ tests
    abi: ABI compatibility tests
  # build tool configuration files to create a single root project for every language
  # this is improves IDE support
  CMakeLists.txt
  build.gradle
  Cargo.toml
```

Unit tests (\*) (e.g. the old `tests/ctest`, `tests/kdb`) should be bundled with the code they are testing:
e.g. in Rust test code is in the same file, in C/C++ it should be in the same folder and in Java a separate source set in the same module is used.
This makes it easier to find the tests for a library, and also creates a uniform structure (in Rust or Java it would be harder to put tests into the current structure).
System tests that test multiple components live in the `src/tests` folder.

> (\*) The term "unit test" is used very loosely here.
> Most tests for e.g. the `kdb` tool will not, strictly speaking, be unit tests, since they don't replace dependencies with mocks/fakes and instead test the whole `kdb` tool.

Benchmarks should be put into a separate git repository.
They are not necessary for normal development and only create clutter.
If a benchmark also serves as an example, either a separate example version should be created, or a git submodule may be used.

## Rationale

Separating the `src` folder directly by language, allows using the directory structure that best fits the language.
For example, `src/java` might be organized as a single Gradle project with subprojects (including plugins).

## Implications


## Related Decisions

- [Header File Structure](header_file_structure.md)

## Notes
