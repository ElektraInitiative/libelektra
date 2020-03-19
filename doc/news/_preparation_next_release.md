# 0.9.<<VERSION>> Release

This release did not happen yet.

Please update this file within PRs accordingly.
For non-trivial changes, you can choose to be
part of the highlighted changes. Please make
sure to add some short tutorial (checked by
shell recorder) or asciinema for highlighted items.

Please add your name at the end of every contribution.
**Syntax:** _(your name)_

<<`scripts/generate-news-entry`>>

We are proud to release Elektra 0.9.<<VERSION>>.

## What is Elektra?

Elektra serves as a universal and secure framework to access
configuration settings in a global, hierarchical key database.
For more information, visit [https://libelektra.org](https://libelektra.org).

You can also read the news [on our website](https://www.libelektra.org/news/0.9.<<VERSION>>-release)

## Highlights

- <<HIGHLIGHT1>>
- <<HIGHLIGHT2>>
- <<HIGHLIGHT3>>

### <<HIGHLIGHT1>>

### <<HIGHLIGHT2>>

### <<HIGHLIGHT2>>

## Plugins

The following section lists news about the [modules](https://www.libelektra.org/plugins/readme) we updated in this release.

### Augeas

- Improved error message for augeas to show lensPath. _(Michael Zronek)_

### CCode

- The [Markdown Shell Recorder][] test of the plugin does not require Bash any more. _(René Schwaiger)_

[markdown shell recorder]: https://master.libelektra.org/tests/shell/shell_recorder/tutorial_wrapper

### Directory Value

- The plugin now only interprets a key set as [array](../tutorials/arrays.md) if the parent contains the meta key `array`. _(René Schwaiger)_

### KConfig

- We implemented the methods that save a KeySet into a file with the KConfig Ini format. _(Dardan Haxhimustafa)_

### SWIG

- Configure line (-DBINDINGS="..") for SWIG based bindings have been changed from `swig_foo` to `foo`. _(Manuel Mausz)_

### SWIG/python

- Added bindings for libelektratools. _(Manuel Mausz)_

### SWIG/python2

- Removed. _(Manuel Mausz)_

### Tcl

- The [Markdown Shell Recorder][] test of the plugin now correctly requires the [`xmltool` plugin](../../src/plugins/xmltool). _(René Schwaiger)_

### YAMBi

- We removed the plugin in favor of [Yan LR](../../src/plugins/yanlr). _(René Schwaiger)_

### YAML CPP

- The plugin now always prints a newline at the end of the YAML output. _(René Schwaiger)_
- The plugin does not interpret a key set such as

  ```
  user/example
  user/example/#0
  user/example/#1
  user/example/#2
  ```

  as array unless the parent key `user/example` contains the meta key `array`. _(René Schwaiger)_

- YAML CPP now always sets and requires the metakey `type` with the value `boolean` for boolean data. _(René Schwaiger)_

### Yan LR

- The CMake code of the plugin does not print error messages produced by the tool `ldd` any more. _(René Schwaiger)_
- The plugin now also supports ANTLR 4.8. _(René Schwaiger)_

## Libraries

The text below summarizes updates to the [C (and C++)-based libraries](https://www.libelektra.org/libraries/readme) of Elektra.

### Compatibility

- We clarified compatibility requirements for Elektra and its plugins and bindings.
  Furthermore, we renamed `system/elektra/version/constants/KDB_VERSION_MICRO`
  to `system/elektra/version/constants/KDB_VERSION_PATCH` to be compatible
  with [Semantic Versioning 2.0.0](https://semver.org/). _(Markus Raab)_
- <<TODO>>
- <<TODO>>
- <<TODO>>

### Core

- <<TODO>>
- <<TODO>>
- <<TODO>>

### <<Library1>>

- <<TODO>>
- <<TODO>>
- <<TODO>>

### <<Library2>>

- <<TODO>>
- <<TODO>>
- <<TODO>>

### <<Library3>>

- <<TODO>>
- <<TODO>>
- <<TODO>>

## Bindings

Bindings allow you to utilize Elektra using [various programming languages](https://www.libelektra.org/bindings/readme). This section keeps
you up to date with the multi-language support provided by Elektra.

### python2

- Removed. _(Manuel Mausz)_

### Rust

- Published `elektra` and `elektra-sys` versions `0.9.1` to crates.io. _(Philipp Gackstatter)_

## Tools

- [elektrad](../../src/tools/elektrad/README.md) is completely rewritten in Go - which drastically improves the performance by leveraging the new [go-elektra](https://github.com/ElektraInitiative/go-elektra/) bindings instead of calling the `kdb` command-line tool on every request. _(Raphael Gruber)_
- Update `kdb cache` tool synopsis to reflect man page. _(Mihael Pranjić)_
- Pull elektrad, webui and webd out of shared web folder to allow fine grained selection of tools. _(Raphael Gruber)_

## Scripts

- The [fish completion script](../../scripts/completion/kdb.fish) now recognizes the new names of subcommands (e.g. `meta-set` instead of `setmeta` ) introduced with Elektra `0.9.1`. _(René Schwaiger)_
- The script [reformat-cmake](../../scripts/dev/reformat-cmake) now reformats the code with `cmake-format` 0.6.3. _(René Schwaiger)_
- The script [reformat-shell](../../scripts/dev/reformat-shell) now makes sure that you do not use `shfmt` 3, which formats parts of the code base slightly differently. _(René Schwaiger)_

## Documentation

- improved formatting of the [`validation tutorial`](../../doc/tutorials/validation.md) _(Anton Hößl)_
- We fixed some minor spelling mistakes. _(René Schwaiger)_
- We updated the man pages of the [`web`](../tutorials/install-webui.md) tool. _(René Schwaiger)_

## Tests

- We now use [Google Test](https://github.com/google/googletest) `1.10` to test Elektra. _(René Schwaiger)_
- The C++ test code does not produce warnings about a missing macro argument for `...` any more. _(René Schwaiger)_
- Whitelisted some broken links. _(Mihael Pranjić)_
- The [formatting check](../../tests/shell/check_formatting.sh) now also works correctly, if it is invoked multiple times. _(René Schwaiger)_

## Build

### Compilation

- We do not use implicit typing in the code of the

  - `augeas`,
  - `base64`, and
  - `blockresolver`

  plugin any more. After this update, the code compiles without any warnings, even though we now use the compiler switch `-Wconversion`. _(René Schwaiger)_

### Support

- Debian 9 “stretch” (oldstable) is now the oldest supported platform. _(René Schwaiger)_

### CMake

- We fixed warnings about CMake policy [CMP0078](https://cmake.org/cmake/help/latest/policy/CMP0078.html) and [CMP0086](https://cmake.org/cmake/help/latest/policy/CMP0086.html). _(René Schwaiger)_
- The CMake functions `add_msr_test` and `add_msr_test_plugin` do not export the list of required plugins as environment variable any more. _(René Schwaiger)_
- The CMake code of the code generation does not print warnings about unknown regex operators any more. _(René Schwaiger)_
- Generating the build system now requires CMake `3.4` (released in November 2015). _(René Schwaiger)_

### Docker

- We updated some of the software in the [Dockerfile for Debian sid](../../scripts/docker/debian/sid/Dockerfile). _(René Schwaiger)_
- Building the [documentation Dockerfile for Debian Stretch](../../scripts/docker/debian/stretch/doc.Dockerfile) works again. _(René Schwaiger)_
- <<TODO>>

## Infrastructure

### Cirrus

- We fixed a minor problem with the package install procedure on macOS build jobs. _(René Schwaiger)_
- We updated the startup command for D-Bus on macOS. _(René Schwaiger)_
- We removed python2 (EOL and removed from homebrew). _(Mihael Pranjić)_
- Use latest macOS catalina Xcode stable. _(Mihael Pranjić)_
- Use newer FreeBSD images and use image family instead of concrete image names. _(Mihael Pranjić)_
- Disable tcl on FreeBSD images because of test failures (see #3353). _(Mihael Pranjić)_

### Jenkins

- Fixed [coveralls](https://coveralls.io/github/ElektraInitiative/libelektra) coverage report. _(Mihael Pranjić)_
- The build jobs `debian-unstable-clang-asan` and `debian-unstable-full-clang` now use Clang 9 to compile Elektra. _(René Schwaiger)_
- <<TODO>>

### Travis

- Use newer Xcode 11.3 and ruby 2.6.4 on macOS builds. _(Mihael Pranjić)_
- <<TODO>>
- <<TODO>>

## Website

The website is generated from the repository, so all information about
plugins, bindings and tools are always up to date. Furthermore, we changed:

- Re-enable website auto-deployment. _(Mihael Pranjić)_
- Update docker images for website frontend and backend to debian buster. Update dependencies to newer versions. _(Mihael Pranjić)_
- <<TODO>>

## Outlook

We are currently working on following topics:

- <<TODO>>
- <<TODO>>
- <<TODO>>

## Statistics

<<`scripts/git-release-stats 0.9.VER-1 0.9.<<VERSION>>`>>

## Finished Thesis

[René Schwaiger](https://github.com/sanssecours) finished [his thesis](https://github.com/sanssecours/Configuration-File-Parsing/releases) about parsing techniques and parsing tools for configuration files.

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

The release tarball is also available signed by Markus Raab using GnuPG from
[here](https://www.libelektra.org/ftp/elektra/releases/elektra-0.9.<<VERSION>>.tar.gz.gpg) or on
[GitHub](https://github.com/ElektraInitiative/ftp/blob/master/releases/elektra-0.9.<<VERSION>>.tar.gz.gpg?raw=true)

Already built API-Docu can be found [here](https://doc.libelektra.org/api/0.9.<<VERSION>>/html/)
or on [GitHub](https://github.com/ElektraInitiative/doc/tree/master/api/0.9.<<VERSION>>).

## Stay tuned!

Subscribe to the
[RSS feed](https://www.libelektra.org/news/feed.rss)
to always get the release notifications.

If you also want to participate, or for any questions and comments
please contact us via our issue tracker [on GitHub](http://issues.libelektra.org).

[Permalink to this NEWS entry](https://www.libelektra.org/news/0.9.<<VERSION>>-release)

For more information, see [https://libelektra.org](https://libelektra.org)

Best regards,
[Elektra Initiative](https://www.libelektra.org/developers/authors)
