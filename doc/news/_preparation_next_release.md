# <<VERSION>> Release

This release did not happen yet.

Please always update this file within **every PR**:

1. write what changed
2. use links pointing to your change (See [Documentation Guidelines](/doc/contrib/documentation.md))
3. add your name at the end of the line **Syntax:** _(your name)_

For example, Max would write:

```
- Added a new [doc plugin](https://www.libelektra.org/plugins/doc) _(Max)_
```

Pick a random line to write your changes to minimize the chances of conflicts in this file.

For non-trivial changes, you can choose to be part of the highlighted changes.
Please write a highlight section in this case.

After the horizontal line the release notes for the next version starts.

---

<<`scripts/generate-news-entry`>>

We are proud to release Elektra <<VERSION>>.

## What is Elektra?

Elektra serves as a universal and secure framework to access configuration settings in a global, hierarchical key database.
For more information, visit [https://libelektra.org](https://libelektra.org).

You can also read the news [on our website](https://www.libelektra.org/news/<<VERSION>>-release).

You can try out the latest Elektra release using our docker image [elektra/elektra](https://hub.docker.com/r/elektra/elektra).
This is the quickest way to get started with Elektra without compiling and other obstacles, simply run:

```sh
docker pull elektra/elektra
docker run -it elektra/elektra
```

## Highlights

- <<HIGHLIGHT>>
- <<HIGHLIGHT>>
- <<HIGHLIGHT>>

### BREAKING: New header and library structure

We implemented the previously decided, new [Header File Structure](/doc/decisions/6_implemented/header_file_structure.md), [Including Headers](/doc/decisions/6_implemented/header_include.md), [Library Directory Structure](/doc/decisions/6_implemented/library_directory_structure.md) and [Library Split](/doc/decisions/6_implemented/library_split.md).
With this change it should be easier to find things in source code and know which headers are public (i.e., included in packages) and which are just internal to the repository.

#### Changed headers

For developers building applications/libraries that link against Elektra, this means a breaking change.
The biggest part is the source-breaking change of the header structure.
Please look at `src/include/elektra` to find the headers containing the symbols you need and include them in your code as e.g.

```c
#include <elektra/kdb.h>
```

Importantly, it is no longer required to set additional include paths.
Only the standard include paths (`/usr/include`, or `/usr/local/include` when you install from source) are required.

The new `src/include/elektra` folder includes all headers that Elektra installs (\*), it is copied directly to `/usr/include` on install.
The layout of `src/include/elektra` (and consequently the layout of installed headers), follows a fixed pattern:

```c
// include all/default symbols of library libelektra-[LIB].so
// almost all libraries provide such a header
#include <elektra/[LIB].h>

// include only foo symbols of library libelektra-[LIB].so
// not all libraries provide modularized headers like this
#include <elektra/[LIB]/foo.h>
```

(\*) For now, some bindings (e.g., the C++ binding) still install additional headers located in other places.
These will also be moved to `src/include/elektra` in the future.

#### Changed libraries

For the actual libraries `libelektra-*.so`, the biggest change was restructuring the source code for easier development.
However, there are a few breaking changes for third party code as well:

- `libelektra-types.so` is a new library implementing the type conversion functions `elektraKeyTo*` and `elektra*ToString`, which were previously part of `libelektra-ease.so`.
- `libelektra-utility.so` is a new library implementing some low-level helper functions that don't require Elektra-specific types, which were previously part of `libelektra-core.so`.
  Because of the way these libraries are built internally, both `libelektra-utility.so` and `libelektra-core.so` are dependency-free.
  You can still use `libelektra-core.so` without linking against additional libraries.
  This is works, because the functions exported by `libelektra-utility.so` which are required by `libelektra-core.so` are statically linked into `libelektra-core.so` and not exported there.
  If you need e.g., `elektraFree` then link against `libelektra-utility.so`.
  While `elektraFree` is included in `libelektra-core.so`, it is not exported anymore.

### <<HIGHLIGHT>>

### <<HIGHLIGHT>>

## Plugins

The following text lists news about the [plugins](https://www.libelektra.org/plugins/readme) we updated in this release.

### <<Plugin>>

- <<TODO>>
- <<TODO>>
- <<TODO>>

### <<Plugin>>

- <<TODO>>
- <<TODO>>
- <<TODO>>

### <<Plugin>>

- <<TODO>>
- <<TODO>>
- <<TODO>>

### <<Plugin>>

- <<TODO>>
- <<TODO>>
- <<TODO>>

### <<Plugin>>

- <<TODO>>
- <<TODO>>
- <<TODO>>

### <<Plugin>>

- <<TODO>>
- <<TODO>>
- <<TODO>>

### <<Plugin>>

- <<TODO>>
- <<TODO>>
- <<TODO>>

## Libraries

The text below summarizes updates to the [C (and C++)-based libraries](https://www.libelektra.org/libraries/readme) of Elektra.

### Compatibility

- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>

### Core

- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>

### <<Library>>

- <<TODO>>
- <<TODO>>
- <<TODO>>

### <<Library>>

- <<TODO>>
- <<TODO>>
- <<TODO>>

### <<Library>>

- <<TODO>>
- <<TODO>>
- <<TODO>>

### <<Library>>

- <<TODO>>
- <<TODO>>
- <<TODO>>

### <<Library>>

- <<TODO>>
- <<TODO>>
- <<TODO>>

## Bindings

Bindings allow you to utilize Elektra using [various programming languages](https://www.libelektra.org/bindings/readme).
This section keeps you up-to-date with the multi-language support provided by Elektra.

### <<Binding>>

- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>

### C++

- Provide getter for the underlying C object of KDB _(Maximilian Irlinger @atmaxinger)_
- Add `ElektraDiff` binding for C++ _(Maximilian Irlinger @atmaxinger)_
- The `dup` method of `KeySet` now returns a wrapped object _(Maximilian Irlinger @atmaxinger)_
- Add an overload for `KeySet::cut` that accepts a string for the keyname _(Maximilian Irlinger)_
- The `dup` method of `Key` now returns a wrapped object _(Maximilian Irlinger @atmaxinger)_
- Add overloads for `Key::isBelow`, `Key::isBelowOrSame` and `Key::isDirectBelow` that accept a string as the key name _(Maximilian Irlinger @atmaxinger)_
- Include the header `cstdint` in `key.hpp`. It is needed for an enum of type `std::uint8_t` _(Florian Lindner @flo91)_

### <<Binding>>

- <<TODO>>
- <<TODO>>
- <<TODO>>

### Python

- Add `ElektraDiff` binding _(Maximilian Irlinger @atmaxinger)_
- The `__meta__` attribute on a key now returns a proper keyset _(Maximilian Irlinger @atmaxinger)_
- Add new module `kdb.errors` to simplify extracting errors and warnings from keys _(Maximilian Irlinger @atmaxinger)_
- Add new module `kdb.record` for interfacing with the session recording capabilities of Elektra _(Maximilian Irlinger @atmaxinger)_
- Add `getConflictingKeys` method to `kdb.merge.MergeResult`. _(Maximilian Irlinger @atmaxinger)_

### <<Binding>>

- <<TODO>>
- <<TODO>>
- <<TODO>>

### <<Binding>>

- <<TODO>>
- <<TODO>>
- <<TODO>>

### <<Binding>>

- <<TODO>>
- <<TODO>>
- <<TODO>>

### Xfconf

- Implemented the first revision of the Xfconf binding. _(Richard Stöckl @Eiskasten)_
- This allows to use elektra as a drop-in replacement for applications which use Xfconf. _(Richard Stöckl @Eiskasten)_
- Xfconf applications can now read and write configuration settings to elektra. _(Richard Stöckl @Eiskasten)_
- <<TODO>>

## Tools

### <<Tool>>

- <<TODO>>
- <<TODO>>
- <<TODO>>

### <<Tool>>

- <<TODO>>
- <<TODO>>
- <<TODO>>

### <<Tool>>

- <<TODO>>
- <<TODO>>
- <<TODO>>

### <<Tool>>

- <<TODO>>
- <<TODO>>
- <<TODO>>

## Scripts

- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- Added `scripts/dev/check-includes.sh` which checks our [new rules](/doc/decisions/6_implemented/header_include.md) for `#include`s _(@kodebach)_
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>

## Documentation

- <<TODO>>
- <<TODO>>
- Fix bug in Doxygen comment for `const char * keyName (const Key * key)` which lead to failed building of the refman.pdf on recent TeX Live releases _(Florian Lindner @flo91)_
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>

### Use Cases

- <<TODO>>
- Add specification use case for [array-specification](../usecases/specification/array-specification.md) _(Tomislav Makar @tmakar)_
- Add specification use case for [underline-specification](../usecases/specification/underline-specification.md) _(Tomislav Makar @tmakar)_
- Add specification use case for [simple-specification](../usecases/specification/simple-specification.md) _(Tomislav Makar @tmakar)_
- Add specification use case for [enum-specification](../usecases/specification/enum-specification.md) _(Tomislav Makar @tmakar)_
- Add complete specification for `dockerd` configuration file (`daemon.json`) _(Tomislav Makar @tmakar)_
- Add end-user and developer integration use case _(Hannes Laimer @hannes99)_
- <<TODO>>
- <<TODO>>
- <<TODO>>
- Started implementing the decisions [Header File Structure](/doc/decisions/6_implemented/header_file_structure.md), [Including Headers](/doc/decisions/6_implemented/header_include.md), [Library Directory Structure](/doc/decisions/6_implemented/library_directory_structure.md) and [Library Split](/doc/decisions/6_implemented/library_split.md). _(@kodebach)_
- <<TODO>>
- Continued implementing the decisions [Header File Structure](/doc/decisions/6_implemented/header_file_structure.md), [Including Headers](/doc/decisions/6_implemented/header_include.md), [Library Directory Structure](/doc/decisions/6_implemented/library_directory_structure.md) and [Library Split](/doc/decisions/6_implemented/library_split.md). _(@kodebach)_
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- Finished implementing the decisions [Header File Structure](/doc/decisions/6_implemented/header_file_structure.md), [Including Headers](/doc/decisions/6_implemented/header_include.md), [Library Directory Structure](/doc/decisions/6_implemented/library_directory_structure.md) and [Library Split](/doc/decisions/6_implemented/library_split.md). _(@kodebach)_
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>

### Decisions

- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>

### Tutorials

- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>

### Man Pages

- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>

## Tests

- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>

### C

- <<TODO>>
- <<TODO>>
- <<TODO>>

### Shell Recorder

- <<TODO>>
- <<TODO>>
- <<TODO>>

### C++

- <<TODO>>
- <<TODO>>
- <<TODO>>

## Packaging

- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>

## Build

### CMake

- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>

### Docker

- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>

## Infrastructure

### Jenkins

- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>

### Cirrus

- <<TODO>>
- <<TODO>>
- The arch package `texlive-most` is no longer available, replaced it with other texlive packages. See https://archlinux.org/packages/?q=texlive _(Florian Lindner @flo91)_
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>

### GitHub Actions

- Added workflow which runs the new `check-includes` script _(@kodebach)_
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>

## Website

The website is generated from the repository, so all information about plugins, bindings and tools are always up-to-date. Furthermore, we changed:

- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>

## Outlook

We are currently working on following topics:

- <<TODO>>
- <<TODO>>
- <<TODO>>

## Statistics

We closed [<<NUMISSUES>> issues](https://github.com/ElektraInitiative/libelektra/milestone/<<MILESTONE>>?closed=1) for this release.

<<`scripts/git-release-stats <<VERSION>>.VER-1 <<VERSION>>`>>

Thanks to all authors for making this release possible!

## Join the Initiative!

We welcome new contributors!
Read [here](https://www.libelektra.org/devgettingstarted/ideas) about how to get started.

As first step, you could give us feedback about these release notes.
Contact us via our [issue tracker](https://issues.libelektra.org).

## Get the Release!

You can download the release from

- [here](https://www.libelektra.org/ftp/elektra/releases/elektra-<<VERSION>>.tar.gz) or
- [GitHub](https://github.com/ElektraInitiative/ftp/blob/master/releases/elektra-<<VERSION>>.tar.gz?raw=true)

The [hashsums are:](https://github.com/ElektraInitiative/ftp/blob/master/releases/elektra-<<VERSION>>.tar.gz.hashsum?raw=true)

<<`scripts/generate-hashsums elektra-<<VERSION>>.tar.gz`>>

The release tarball is also available signed using GnuPG from

- [here](https://www.libelektra.org/ftp/elektra/releases/elektra-<<VERSION>>.tar.gz.gpg) or
- [GitHub](https://github.com/ElektraInitiative/ftp/blob/master/releases/elektra-<<VERSION>>.tar.gz.gpg?raw=true)

The following GPG Key was used to sign this release: 12CC44541E1B8AD9B66AFAD55262E7353324914A

Already built API documentation can be found

- [here](https://doc.libelektra.org/api/<<VERSION>>/html/) or
- [GitHub](https://github.com/ElektraInitiative/doc/tree/master/api/<<VERSION>>).

## Stay tuned!

Subscribe to the [RSS feed](https://www.libelektra.org/news/feed.rss) to always get the release notifications.

If you also want to participate, or for any questions and comments, please contact us via our issue tracker [on GitHub](http://issues.libelektra.org).

[Permalink to this NEWS entry](https://www.libelektra.org/news/<<VERSION>>-release)

For more information, see [https://libelektra.org](https://libelektra.org).

Best regards,
[Elektra Initiative](https://www.libelektra.org/developers/authors)
