# 0.9.<<VERSION>> Release

This release did not happen yet.

Please update this file within PRs accordingly.
For non-trivial changes, you can choose to be
part of the highlighted changes.

Please add your name at the end of every contribution.
**Syntax:** _(your name)_

Pick a random line to write your changes to minimize
the chances of conflicts in this file.

<<`scripts/generate-news-entry`>>

We are proud to release Elektra 0.9.<<VERSION>>.

## What is Elektra?

Elektra serves as a universal and secure framework to access
configuration settings in a global, hierarchical key database.
For more information, visit [https://libelektra.org](https://libelektra.org).

You can also read the news [on our website](https://www.libelektra.org/news/0.9.<<VERSION>>-release).

You can try out the latest Elektra release using our docker image [elektra/elektra](https://hub.docker.com/r/elektra/elektra).
This is the quickest way to get started with Elektra without compiling and other obstacles, simply run:

```sh
docker pull elektra/elektra
docker run -it elektra/elektra
```

## Highlights

- <<HIGHLIGHT>>
- <<HIGHLIGHT>>
- Breaking change to iterators: Remove `keyRewindMeta`, `keyCurrentMeta`, `ksHead`, and `ksTail` functions for internal iteration of `Keyset`s and Metadata of `Key`s. External iterators are now the way to go (see [Iterators](/doc/dev/iterators.md)) _(Florian Lindner @flo91)_
- New Binding for Kotlin with convenience functions and various utilities. There is also the possibility to convert KeySets to Kotlin data classes or collections and back. _(@Gratla & @mandoway)_

### <<HIGHLIGHT>>

### <<HIGHLIGHT>>

### <<HIGHLIGHT>>

## Plugins

The following section lists news about the [plugins](https://www.libelektra.org/plugins/readme) we updated in this release.

- Change all plugins, except `directoryvalue` to use external iteration of `KeySet`s _(Florian Lindner @flo91)_

### Python

- Added new DNS plugin _(Lukas Hartl @lukashartl, Leonard Guelmino @leothetryhard)_
- <<TODO>>
- <<TODO>>

### <<Plugin>>

- <<TODO>>
- <<TODO>>
- <<TODO>>

### lineendings - Plugin

- Enable emitting of warnings during kdbGet(), refactor and update methods and return values to match the conventions
  (e.g. #defined constants for return values) _(Michael Langhammer @milangs, Florian Lindner @flo91)_
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

### Length

- Warnings are now added on `kdb get` _(@mandoway)_

### <<Plugin>>

- <<TODO>>
- <<TODO>>
- <<TODO>>

### Sorted

- Added new validation plugin: Sorted. It checks whether an Elektra array is sorted by its value or a given key in a configurable direction _(@mandoway @Gratla)_
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

- Remove `keyRewindMeta`, `keyCurrentMeta`, `ksHead`, and `ksTail` functions for internal iteration of `Keyset`s and Metadata of `Key`s _(Florian Lindner @flo91)_
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

## Bindings

Bindings allow you to utilize Elektra using [various programming languages](https://www.libelektra.org/bindings/readme). This section keeps
you up-to-date with the multi-language support provided by Elektra.

- Remove internal iterators for SWIG (Python, Lua, Ruby) and go-bindings _(Florian Lindner @flo91)_

### Java

- Added a java example for meta keys and arrays _(@mandoway)_

### JNA

- Added example which shows how to add a basename for a key. _(Philipp Leeb @Gratla)_
- Introduced Key#setNull, ReadableKey#isNull _(Michael Tucek @tucek)_
- Fixed Key#setBoolean, ReadableKey#isBoolean _(Michael Tucek @tucek)_
- Fixed Java Whitelist plugin tests _(Michael Tucek @tucek)_
- Fixed missing Javadoc in Java Sorted plugin _(Michael Tucek @tucek)_
- <<TODO>>

### <<Binding>>

- <<TODO>>
- <<TODO>>
- <<TODO>>

### <<Binding>>

- <<TODO>>
- <<TODO>>
- <<TODO>>

### Kotlin

- Added new JNA subproject which builds an Elektra extension library for Kotlin _(@mandoway & @Gratla)_
- Added get(), getOrNull() extension with type inference for primitive types _(@mandoway)_
- Added set() extension with type inference for primitive types _(@mandoway)_
- Added keySet serialization capabilities (to any format and data classes, with array support) _(@mandoway)_
- Added keyOf() extension and keyOf{} builder for key creation _(@Gratla)_
- Added keySetOf() extension and keySetOf{} builder for keySet creation _(@Gratla)_
- Added withKDB() extension which wraps the try block _(@Gratla)_
- Added nameParts extension value which provides a sequence of key name parts _(@mandoway)_
- Added various utility functions like Key.isEmpty, Key.getMetaOrNull, ... _(@Gratla & @mandoway)_
- Added example project for kotlin binding _(@Gratla & @mandoway)_

### Python

- Deleted occurrences of removed property key.fullname _(@mandoway)_

## Tools

### elektrad

- improve logging in `elektrad` _(Lukas Hartl @lukashartl, Leonard Guelmino @leothetryhard)_
- Update `elektrad` to use last version of the `go`-bindings without internal iterators for `Keyset`s and Metadata _(Florian Lindner @flo91)_
- <<TODO>>

### `webui`

- fix issues from namespace-overhaul _(Lukas Hartl @lukashartl, Leonard Guelmino @leothetryhard)_
- apply non-breaking updates to packages _(Leonard Guelmino @leothetryhard, Lukas Hartl @lukashartl)_
- <<TODO>>
- <<TODO>>

### `webd`

- fix path building for requests to `elektrad` _(Lukas Hartl @lukashartl, Leonard Guelmino @leothetryhard)_
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
- <<TODO>>
- <<TODO>>
- Fix `kdb reset`. _(Markus Raab)_
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>

## Documentation

- Small readability improvement _(@Toniboyyy)_
- Python: add guide for Debian 11 (bullseye) _(Lukas Hartl @lukashartl)_
- Fix some errors in the tutorials `Cascading Lookups` and `Command-line Options` _(Florian Lindner @flo91)_
- <<TODO>>
- Extend and update the tutorial for writing specifications,
  add section about using specs in production _(Florian Lindner @flo91)_
- <<TODO>>
- Add readme-file [Iterators](/doc/dev/iterators.md) about cm2022s project showcasing usage in various programming languages _(Florian Lindner @flo91 and Michael Langhammer @Milangs)_
- Remove usage of internal iterators from the examples
- <<TODO>>
- <<TODO>>
- Improve jna documentation _(Burkhard Hampl @bhampl)_
- <<TODO>>
- Add Stream API example in Java binding documentation _(Richard Stöckl @Eiskasten)_
- Add Stream API example in Java binding documentation \_(Richard Stöckl @Eiskasten)
- <<TODO>>
- <<TODO>>
- <<TODO>>
- Minor readability improvement _(@mandoway)_
- <<TODO>>
- <<TODO>>
- <<TODO>>
- Minor readability improvement in `CODING.md` _(@loessberth)_
- <<TODO>>
- <<TODO>>
- Fix dead link and compile instructions _(Burkhard Hampl @bhampl)_
- Update links from certificate section _(Richard Stöckl @Eiskasten)_
- <<TODO>>
- <<TODO>>
- Fix wrong KDBException reference in java tutorial and improve it _(Burkhard Hampl @bhampl and Richard Stöckl @Eiskasten)_
- <<TODO>>
- Apply fix spelling to more files. _(Markus Raab)_
- <<TODO>>

### Tutorials

- <<TODO>>
- The tutorial for [Contributing from Windows](../tutorials/contributing-windows.md) has been updated. _(@kodebach)_
- The tutorial for [CLion](../tutorials/contributing-clion.md) now contains a section for setting up the WSL compiler _(@mandoway)_
- <<TODO>>
- Rephrased sentence in code-generator.md to enhance readability _(@Gratla)_
- <<TODO>>

### Man Pages

- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- Update [FAQ](/doc/help/elektra-faq.md). _(Markus Raab)_

## Tests

- Add tests for the Error/Warnings-Factory in libtools _(Florian Lindner @flo91)_
- Add tests for keySet in the python binary _(Lukas Hartl @lukashartl, Leonard Guelmino @leothetryhard)_
- <<TODO>>
- Added test for JNA KDB which checks if both get-method implementations return the same result. _(Philipp Leeb @Gratla)_
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

- CMake now automatically detects all JNA plugins that are added to Gradle. _(@kodebach)_
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

### Cirrus

- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>

### GitHub Actions

- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>

### Git

- We added a `.gitattributes` file to make it easier to build Elektra with WSL. _(@kodebach)_

## Website

The website is generated from the repository, so all information about
plugins, bindings and tools are always up-to-date. Furthermore, we changed:

- Overhauled the `Get Started` page by adding a brief `kdb` introduction. _(@Milangs)_
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

<<`scripts/git-release-stats 0.9.VER-1 0.9.<<VERSION>>`>>

Thanks to all authors for making this release possible!

## Join the Initiative!

We welcome new contributors!
Read [here](https://www.libelektra.org/devgettingstarted/ideas) about how to get started.

As first step, you could give us feedback about these release notes.
Contact us via our [issue tracker](https://issues.libelektra.org).

## Get the Release!

You can download the release from [here](https://www.libelektra.org/ftp/elektra/releases/elektra-0.9.<<VERSION>>.tar.gz)
or [GitHub](https://github.com/ElektraInitiative/ftp/blob/master/releases/elektra-0.9.<<VERSION>>.tar.gz?raw=true)

The [hashsums are:](https://github.com/ElektraInitiative/ftp/blob/master/releases/elektra-0.9.<<VERSION>>.tar.gz.hashsum?raw=true)

<<`scripts/generate-hashsums elektra-0.9.<<VERSION>>.tar.gz`>>

The release tarball is also available signed using GnuPG from
[here](https://www.libelektra.org/ftp/elektra/releases/elektra-0.9.<<VERSION>>.tar.gz.gpg) or on
[GitHub](https://github.com/ElektraInitiative/ftp/blob/master/releases/elektra-0.9.<<VERSION>>.tar.gz.gpg?raw=true)

The following GPG Key was used to sign this release: 12CC44541E1B8AD9B66AFAD55262E7353324914A

Already built API-Docu can be found [here](https://doc.libelektra.org/api/0.9.<<VERSION>>/html/)
or on [GitHub](https://github.com/ElektraInitiative/doc/tree/master/api/0.9.<<VERSION>>).

## Stay tuned!

Subscribe to the
[RSS feed](https://www.libelektra.org/news/feed.rss)
to always get the release notifications.

If you also want to participate, or for any questions and comments
please contact us via our issue tracker [on GitHub](http://issues.libelektra.org).

[Permalink to this NEWS entry](https://www.libelektra.org/news/0.9.<<VERSION>>-release)

For more information, see [https://libelektra.org](https://libelektra.org).

Best regards,
[Elektra Initiative](https://www.libelektra.org/developers/authors)
