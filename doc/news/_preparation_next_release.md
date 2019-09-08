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

While the new `kdb gen` was already included in the last release, it is now fully functional and ready for use. To get started take a look
at the new man-page for [`kdb-gen(1)`](https://www.libelektra.org/manpages/kdb-gen). _(Klemens Böswirth)_

If specifically want to use it with the High-Level API take a look at [this tutorial](https://www.libelektra.org/tutorials/high-level-api).

### <<HIGHLIGHT2>>

### <<HIGHLIGHT2>>

## Plugins

The following section lists news about the [modules](https://www.libelektra.org/plugins/readme) we updated in this release.

- We removed 9 obsolete or unfinished plugins: boolean cachefilter cpptype dini enum regexstore required simplespeclang struct. _(Markus Raab)_

### GOpts

- The error message, if non of the gopts variants can be compiled, was improved. _(Klemens Böswirth)_
- A better error, if the plugin fails to load `argv` from the system, was added. _(Klemens Böswirth)_

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

## Libraries

The text below summarizes updates to the [C (and C++)-based libraries](https://www.libelektra.org/libraries/readme) of Elektra.

### Compatibility

- The conversion functions `elektraKeyTo*` and `elektra*ToString` are now part of the `elektra-ease` library instead of
  the `elektra-highlevel` library. This should not cause any breaking changes since `elektra-highlevel` already depends
  on `elektra-ease`. In addition the header `elektra/conversion.h` is kept for compatibility. _(Klemens Böswirth)_

### Core

- A new plugin function, `kdbCommit`, was implemented. The function is carried out in the `commit` phase of `kdbSet` and separates the commit functionality from the `kdbSet()` function. _(Vid Leskovar)_
- `kdbconfig.h` is no longer included in the installed headers. This is because it could cause conflicts with other
  `config.h`-type headers from applications. _(Klemens Böswirth)_
- `kdb.h` was moved into the subfolder `elektra`, to prevent conflicts with header files with the same name. `kbd.h` can now be included via `elektra/kdb.h`. _(Jakob Fischer)_

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

### Java

- Upgraded maven dependencies for java binding _(Michael Zronek)_
- <<TODO>>

### Rust

- Add the `elektra-sys` crate which contains raw bindings to libelektra for Rust. _(Philipp Gackstatter)_

### <<Binding3>>

## Tools

- kdb can call [cmerge](../help/kdb-cmerge.md) and specify a [strategy](../help/elektra-cmerge-strategy.md) to resolve conflicts. _(Dominic Jäger)_
- Checks for `kdbCommit` have been added to [kdb check](../help/kdb-check.md). _(Vid Leskovar)_
- <<TODO>>
- <<TODO>>

## Scripts

- The script [run_icheck](../../scripts/run_icheck) now also work correctly, if the last entry of [`icheck.suppression`](../../tests/icheck.suppression) does not end with a newline character. _(René Schwaiger)_
- The script [`draw-all-plugins`](../../scripts/draw-all-plugins) now also works properly, if the repository path contains space characters. _(René Schwaiger)_
- The script [`link-checker`](../../scripts/link-checker) now deduplicates the list of links before checking them. The timeout and amount of retries was also reduced.
  Lastly the script now supports a whitelist. Any link stored in [`tests/linkchecker.whitelist`](../../tests/linkchecker.whitelist) will not be checked. _(Klemens Böswirth)_

## Documentation

- Added decision for array concept of warnings. _(Michael Zronek)_
- We updated our [Doxygen configuration file](../../doc/Doxyfile), removing the outdated `PERL_PATH` and `MSCGEN_PATH` options. _(René Schwaiger)_
- Added a tutorial on how to write language bindings. Visit our new [README](../tutorials/language-bindings.md).
  _(Michael Zronek, Raphael Gruber, Philipp Gackstatter)_
- A [second tutorial](../tutorials/highlevel-bindings.md) on writing bindings for the high-level API was created as well. _(Klemens Böswirth)_
- <<TODO>>

## Tests

- We changed how the [formatting test](../../tests/shell/check_formatting.sh) detects code differences. This update should get rid of transient errors as [reported here](https://issues.libelektra.org/2927#issuecomment-528058641). _(René Schwaiger)_
- <<TODO>>
- <<TODO>>

## Build

### CMake

- `kdbtypes.h` is now generated directly via a CMake `configure_file` call. _(Klemens Böswirth)_
- <<TODO>>
- <<TODO>>

### Compilation

- We now have a [setup for proper symbol versioning](../dev/symbol-versioning.md). _(Klemens Böswirth)_
- We do not use implicit typing in the code of the `conditionals` plugin any more. After this update, the code compiles without any warnings, even though we now use the compiler switch `-Wconversion`. _(René Schwaiger)_

### Docker

- <<TODO>>
- <<TODO>>
- <<TODO>>

### Other

- The reformatting script now checks that the correct version of `cmake-format` is used. _(Klemens Böswirth, René Schwaiger)_

## Infrastructure

### Cirrus

- The `🔗 Check` build job now merges PRs before checking links. _(Klemens Böswirth)_
- <<TODO>>
- <<TODO>>

### Jenkins

- We upgraded all servers to buster so that debian buster docker image work. _(Markus Raab)_
- We now also build Debian buster packages. _(Markus Raab)_
- Enable WebUI build job again. _(Markus Raab)_
- <<TODO>>
- <<TODO>>

### Travis

- The build job `🍏 GCC` now uses the [Travis Homebrew addon](https://docs.travis-ci.com/user/installing-dependencies/#installing-packages-on-macos) to install dependencies. _(René Schwaiger)_
- <<TODO>>
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
