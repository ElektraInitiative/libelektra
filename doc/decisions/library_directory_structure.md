# Library Directory Structure

## Problem

Currently `src/libs/elektra` contains the source of several libraries. Each of the libraries should have its own folder.
Furthermore, we need to allow rust source to exist in parallel.

## Constraints

- It must be possible to set up include-paths via CMake so that `#include "header.h"` works the same in the build directory and when installed.

## Assumptions

## Considered Alternatives

## Decision

We use this directory tree (YAML syntax for highlighting):

```yaml
doc:
examples:
scripts:
src:
 c:
  libs: one folders per library written in C, contains headers and source
  plugins: one folder per plugin written in C
  tools: one folder per tool written in C
  tests: tests written in C
 rust:
  binding: Rust binding of C API
  libs:
   core: Rust alternative of libelektra-core
   _: {other libraries written in Rust}
  _: {other folders as needed for Rust, unit tests are normally in the same file as source}
 cpp:
  binding: C++ binding of C API
  libs: one folder per lib, contains headers and source, written in C++
  plugins: one folder per plugin, only plugins written in C++
  tools: one folder per tool, only tools written in C++
  tests: tests written in C++
 _: {
         folders for other languages,
         "binding" is always the folder for the binding to the C API,
         rest depends on the language
    }
```

TODO: open questions:

- benchmarks: separate repos with gitmodule?
- tests for libs in C: as subfolder `src/c/libs/foo/tests` or in `src/c/tests`?

## Rationale

Separating the `src` folder directly by language, allows using the directory structure that best fits the language.
For example, `src/java` might be organized as a single Gradle project with subprojects (including plugins).

## Implications

- Because there is no single folder for plugins anymore, `src/plugins/README.md` is moved to `doc/PLUGINS.md`.

## Related Decisions

- [Header Include](header_include.md)
- [Header File Structure](header_file_structure.md)

## Notes
