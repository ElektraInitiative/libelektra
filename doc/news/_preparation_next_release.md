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

- Code generation
- <<HIGHLIGHT2>>
- <<HIGHLIGHT3>>

### Code Generation

While the new `kdb gen` was already included in the last release, it is now fully functional and ready for productive use. To get started take a look
at the new man-page for [`kdb-gen(1)`](https://www.libelektra.org/manpages/kdb-gen).

If you specifically want to use it with the High-Level API take a look at [this tutorial](https://www.libelektra.org/tutorials/high-level-api).

We also created a new CMake function that will be available, if you include Elektra via CMake's
`find_package`. The function is called `elektra_kdb_gen` and can be used to tell CMake about files
that are generated via `kdb gen`. _(Klemens Böswirth)_

### <<HIGHLIGHT2>>

### <<HIGHLIGHT2>>

## Plugins

The following section lists news about the [modules](https://www.libelektra.org/plugins/readme) we updated in this release.

### General

- We removed 12 obsolete or unfinished plugins:
  - `boolean`,
  - `cachefilter`,
  - `cpptype`,
  - `dini`,
  - `enum`,
  - `regexstore`,
  - `required`,
  - `haskell`,
  - `simplespeclang`,
  - `regexdispatcher`,
  - `typechecker`,
  - `struct`. _(Markus Raab, René Schwaiger)_
- We unified the name of the config check function of the plugins to `nameOfPluginCheckConf`. Before this update some plugins used the name `nameOfPluginCheckConfig` instead. _(René Schwaiger)_
- Fixed some typos and links in the documentation and add new iterate example. _(Philipp Gackstatter)_

### Camel

We removed the experimental plugin. For a plugin that is able to parse similar syntax, please take a look at the

- [YAJL](../../src/plugins/yajl) , and
- [YAML CPP](../../src/plugins/yamlcpp)

plugins. _(René Schwaiger)_

### GOpts

- The error message, if non of the gopts variants can be compiled, was improved. _(Klemens Böswirth)_
- A better error, if the plugin fails to load `argv` from the system, was added. _(Klemens Böswirth)_
- A function to detect help mode, without invoking `elektraGetOpts` was added. It simply checks, whether `--help` is one
  of the string in `argv`. _(Klemens Böswirth)_

### Path

- The [Markdown Shell Recorder][] test of the plugin now also works, if you execute it as root user. _(René Schwaiger)_

[markdown shell recorder]: https://master.libelektra.org/tests/shell/shell_recorder/tutorial_wrapper

### Tcl

- We made sure that building the plugin works, if you use the latest version of CMake (`3.15.3`) and Boost (`1.71`). _(René Schwaiger)_

### YAwn

- We removed the plugin in favor of [Yan LR](../../src/plugins/yanlr/README.md). _(René Schwaiger)_

### YAy PEG

- We removed the plugin in favor of [Yan LR](../../src/plugins/yanlr/README.md). _(René Schwaiger)_

### Type

- We added an option to disable the restoring of boolean values. This useful for storage formats like YAML that have
  native boolean types. _(Klemens Böswirth)_

### Noresolver

- The plugin now correctly sets the path in the `parentKey`. It therefore now supports set calls. _(Klemens Böswirth)_

### Specload

- We now treat relative paths as relative to `KDB_DB_SPEC` instead of the current working directory. _(Klemens Böswirth)_

### Spec

- There is now the config key `missing/log` that allows logging of all missing `require`d keys. _(Klemens Böswirth)_
- `spec` now internally handles errors differently. There should be no external impact apart from better performance. _(Klemens Böswirth)_

### Yajl

- Yajl now correctly supports Elektras boolean types using the `type` plugin. For example, setting `on`, `enable` or `true` all map to JSONs native `true` value. See the [type](../../src/plugins/type/README.md) plugin for more details about boolean types. _(Philipp Gackstatter)_

## Libraries

The text below summarizes updates to the [C (and C++)-based libraries](https://www.libelektra.org/libraries/readme) of Elektra.

### Compatibility

- The library `libelektra`, which is a collection of different elektra libraries, is now removed.
  Users of CMake or pkg-config should not be affected. Otherwise change `-lelektra` to `-lelektra-core -lelektra-kdb`
  or whatever parts of Elektra your application uses. _(Markus Raab)_
- The conversion functions `elektraKeyTo*` and `elektra*ToString` are now part of the `elektra-ease` library instead of
  the `elektra-highlevel` library. This should not cause any breaking changes since `elektra-highlevel` already depends
  on `elektra-ease`. In addition the header `elektra/conversion.h` is kept for compatibility. _(Klemens Böswirth)_

### Core

- A new plugin function, `kdbCommit`, was implemented. The function is carried out in the `commit` phase of `kdbSet` and separates the commit functionality from the `kdbSet()` function. _(Vid Leskovar)_
- `kdbconfig.h` is no longer included in the installed headers. This is because it could cause conflicts with other
  `config.h`-type headers from applications. _(Klemens Böswirth)_
- `ksAppendKey`: state that it only fail on memory problems. _(Markus Raab)_
- `keyIsDirectBelow` was renamed to `keyIsDirectlyBelow`. _(Philipp Gackstatter)_

### Opts

- The option `-h` is no longer used to indicate help mode. Only `--help`, will invoke help mode. _(Klemens Böswirth)_

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

- Warnings about cmake policies are avoided. _(Markus Raab)_
- We removed the haskell and GI bindings. _(Markus Raab)_

### Java

- Upgraded maven dependencies for Java binding _(Michael Zronek)_
- Completely overhauled the Java binding to be able to use Elektra plugins directly. For an example see the [test case](../../src/bindings/jna/libelektra4j/src/test/java/org/libelektra/PluginLoaderIT.java). _(Michael Zronek)_
- The java binding now supports the [error codes](../decisions/error_codes.md) in a native way. All exceptions contain the necessary information. _(Michael Zronek)_
- <<TODO>>

### Rust

- Add the `elektra-sys` crate which contains raw bindings to libelektra for Rust. _(Philipp Gackstatter)_
- Add the `elektra` crate which contains safe wrapper methods for the raw bindings. The crate contains bindings for the low-level API, which means that the data types `Key` and `KeySet` can now safely be used from Rust. The Rust version of the API has been designed to take advantage of Rust's type system and to be in accordance with the memory safety of Rust. For instance, the Key has been divided into `StringKey` and `BinaryKey`, to prevent type mismatches at compile time. With the binding for `KDB`, one can take advantage of the elektra ecosystem from Rust. See the [Readme](../../src/bindings/rust/README.md) for more. _(Philipp Gackstatter)_
- The [elektra](https://crates.io/crates/elektra) and [elektra-sys](https://crates.io/crates/elektra-sys) crates have been published to crates.io for easier usage. _(Philipp Gackstatter)_

### <<Binding3>>

## Tools

- kdb can call [cmerge](../help/kdb-cmerge.md) and specify a [strategy](../help/elektra-cmerge-strategy.md) to resolve conflicts. _(Dominic Jäger)_
- Checks for `kdbCommit` have been added to [kdb plugin-check](../help/kdb-plugin-check.md). _(Vid Leskovar)_
- add PID file config setting for kdb-run-rest-frontend _(Markus Raab)_
- Added `kdb meta-show` command which prints out all metadata along with its values for a given key. _(Michael Zronek)_
- Renamed kdb plugin commands following a hierarchical structure. `kdb info` is now `kdb plugin-info`, `kdb check` is now `kdb plugin-check` and `kdb list` is now `kdb plugin-list`. We also removed the obsolete `kdb fstab` functionality. _(Philipp Gackstatter)_
- Renamed kdb meta commands:
  - `kdb getmeta` is now `kdb meta-get`
  - `kdb lsmeta` is now `kdb meta-ls`
  - `kdb showmeta` is now `kdb meta-show`
  - `kdb rmmeta` is now `kdb meta-rm`
  - `kdb setmeta` is now `kdb meta-set` _(Philipp Gackstatter)_
- <<TODO>>

## Scripts

- We structured the [scripts](/scripts). _(Markus Raab)_
- Removed the scripts

  - `scripts/elektra-merge`,
  - `scripts/elektra-mount`,
  - `scripts/elektra-umount`,
  - `convert-fstab`,
  - `convert-hosts`,
  - `convert-inittab`,
  - `convert-users`,
  - `scripts/benchmark_libsplit.sh`,
  - `scripts/zsh` and
  - `example-xorg`. _(Markus Raab)_

- The script [run_icheck](../../scripts/build/run_icheck) now also work correctly, if the last entry of [`icheck.suppression`](../../tests/icheck.suppression) does not end with a newline character. _(René Schwaiger)_
- The script [`draw-all-plugins`](../../scripts/dev/draw-all-plugins) now also works properly, if the repository path contains space characters. _(René Schwaiger)_
- The script [`link-checker`](../../scripts/link-checker) now deduplicates the list of links before checking them. The timeout and amount of retries was also reduced.
  Lastly the script now supports a whitelist. Any link stored in [`tests/linkchecker.whitelist`](../../tests/linkchecker.whitelist) will not be checked. _(Klemens Böswirth)_
- We removed a script used to compare the runtime performance of YAML plugins. _(René Schwaiger)_
- Cleanup: separation of dev, admin and completion scripts. _(Markus Raab, Rene Schwaiger)_
- Pre-commit hook `pre-commit-check-formatting` now lives in [`scripts/dev/pre-commit-check-formatting`](../../scripts/dev/pre-commit-check-formatting). _(Klemens Böswirth)_
- <<TODO>>

## Documentation

- Added decision for array concept of warnings. _(Michael Zronek)_
- We updated our [Doxygen configuration file](../../doc/Doxyfile), removing the outdated `PERL_PATH` and `MSCGEN_PATH` options. _(René Schwaiger)_
- Added a tutorial on how to write language bindings. Visit our new [README](../tutorials/language-bindings.md).
  _(Michael Zronek, Raphael Gruber, Philipp Gackstatter)_
- A [second tutorial](../tutorials/highlevel-bindings.md) on writing bindings for the high-level API was created as well. _(Klemens Böswirth, Raphael Gruber)_
- Added [info](../../src/plugins/xerces/README.md) on how to include xerces plugin with homebrew installation. _(Anton Hößl)_
- <<TODO>>

## Tests

- We changed how the [formatting test](../../tests/shell/check_formatting.sh) detects code differences. This update should get rid of transient errors as [reported here](https://issues.libelektra.org/2927#issuecomment-528058641). _(René Schwaiger)_
- <<TODO>>
- <<TODO>>

## Build

### CMake

- `kdbtypes.h` is now generated directly via a CMake `configure_file` call. _(Klemens Böswirth)_
- The variable `ELEKTRA_STAT_ST_SIZE_F` now contains the correct format specifier for the `st_size` member of the `stat` struct on macOS. _(René Schwaiger)_
- We simplified and unified the CMake code for the [Shell Tests](../../tests/shell) and the [Shell Recorder](../../tests/shell/shell_recorder). _(René Schwaiger)_
- CMake now prints warnings about missing man pages. _(René Schwaiger)_

### Compilation

- We now have a [setup for proper symbol versioning](../dev/symbol-versioning.md). _(Klemens Böswirth)_
- We do not use implicit typing in the code of the `conditionals` plugin any more. After this update, the code compiles without any warnings, even though we now use the compiler switch `-Wconversion`. _(René Schwaiger)_

### Docker

- <<TODO>>
- <<TODO>>
- <<TODO>>

### Other

- The reformatting script now checks that the correct version of `cmake-format` is used. _(Klemens Böswirth, René Schwaiger)_
- Improved various error messages and synchronized documentations. _(Michael Zronek)_

## Infrastructure

### Cirrus

- The `🔗 Check` build job now merges PRs before checking links. _(Klemens Böswirth)_
- We enabled logging in the build job `🍎 Clang`. This update makes sure that Elektra’s logging code compiles without warnings on macOS. _(René Schwaiger)_
- <<TODO>>

### Jenkins

- We upgraded all servers to buster so that debian buster docker image work. _(Markus Raab)_
- We now also build Debian buster packages. _(Markus Raab)_
- Enable WebUI build job again. _(Markus Raab)_
- Improve docu. _(Markus Raab)_
- Jenkins does not auto cancel build jobs of the `master` branch anymore. _(René Schwaiger)_
- <<TODO>>
- <<TODO>>

### Restyled

- [Restyled](https://restyled.io) now also formats Markdown files with [`prettier`](https://prettier.io). _(René Schwaiger)_

### Travis

- The build job `🍏 GCC` now uses the [Travis Homebrew addon](https://docs.travis-ci.com/user/installing-dependencies/#installing-packages-on-macos) to install dependencies. _(René Schwaiger)_
- We now build and test Elektra on Ubuntu `18.04` (Bionic Beaver) instead of Ubuntu `16.04` (Xenial Xerus). _(René Schwaiger)_
- <<TODO>>
- <<TODO>>

## Website

The website is generated from the repository, so all information about
plugins, bindings and tools are always up to date. Furthermore, we changed:

- The Website now lives in the folders [website-frontend](/src/tools/website-frontend) and [website-backend](/src/tools/website-backend) to avoid confusion with the REST backend of the Web-UI. _(Markus Raab)_
- <<TODO>>
- <<TODO>>

## Outlook

We are currently working on following topics:

- Merge tool in C99 (the same language as the core of Elektra). This is planned to supersede the existing merge tool. The goal is to reduce the number of merge conflicts in contrast to regular merge tools using the specific semantics of configuration files. _(Dominic Jäger)_
- <<TODO>>
- <<TODO>>
- <<TODO>>

## Statistics

<<`scripts/git-release-stats 0.9.VER-1 0.9.<<VERSION>>`>>

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
