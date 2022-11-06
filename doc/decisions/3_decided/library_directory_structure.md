# Library Directory Structure

## Problem

Currently `src/libs/elektra` contains the source of several libraries. Each of the libraries should have its own folder.
Furthermore, we need to allow rust source to exist in parallel.

## Constraints

- It must be possible to set up include-paths via CMake so that `#include <elektra/foo/header.h>` works the same in the build directory and when installed.

## Assumptions

## Considered Alternatives

- One folder per language, then split by code type (`src/<lang>/plugins`, `src/<lang>/libs`, etc.):
  - Pro: Makes the per language setup easier, since everything is contained in a single directory and that directory is for a single language, just as if it was any other standalone project in that language.
  - Con: When navigating the source-code you need to know what language e.g. a plugin is written in to find the correct folder.
- First split by code type, then one folder per language (`src/plugins/<lang>`, `src/libs/<lang>`, etc.)
  - Similar to the variant above, but has more downsides. "single folder per language" no longer applies and you still need to know the language to find the folder.

## Decision

We use this directory tree (YAML syntax for highlighting):

```yaml
doc:
benchmarks:
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

For now benchmarks will remain in the top-level `benchmarks` directory, unless there is a language specific reason to have them next to the code they benchmark (like unit-tests).
In future this may change to facilitate automated performance-regression tests.

## Rationale

This setup, allows to find a plugin/library/etc. just by knowing its code type (library/plugin/etc.) and its name.
At the same time, using language suffixes allows having multiple implementations in different languages.
Finally, whenever possible there should be project config file in the top-level `src` directory for every language.
This will improve IDE support and make it possible to treat all code from one language as a single project.
However, it is important that modularity is not lost in the process.
It must still be possible to depend on individual libraries in CMake, without having to build everything else that is written in the same language.

Therefore, the top-level project should only exist, if the language allows projects that only include sub-modules.
Otherwise, fully separate projects for every plugin/library/etc. should be used to preserve modularity.

## Implications

## Related Decisions

- [Header File Structure](../0_drafts/header_file_structure.md)

## Notes
