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
- Code generation
- Elektra now has a new [merge library](../tutorials/merge.md) offering a number of [merge strategies](../help/elektra-merge-strategy.md). _(Dominic Jäger)_
- <<HIGHLIGHT2>>
- <<HIGHLIGHT3>>

### <<HIGHLIGHT1>>

### <<HIGHLIGHT2>>

### <<HIGHLIGHT2>>

## Plugins

The following section lists news about the [modules](https://www.libelektra.org/plugins/readme) we updated in this release.

### Augeas

- Improved error message for augeas to show lensPath. _(Michael Zronek)_

### KConfig

- We implemented the methods that save a KeySet into a file with the KConfig Ini format. _(Dardan Haxhimustafa)_

### SWIG

- Configure line (-DBINDINGS="..") for SWIG based bindings have been changed from `swig_foo` to `foo`. _(Manuel Mausz)_

### SWIG/python2

- Removed. _(Manuel Mausz)_

### YAML CPP

- The plugin now always prints a newline at the end of the YAML output. _(René Schwaiger)_

### <<Plugin3>>

- <<TODO>>
- <<TODO>>
- <<TODO>>

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

- <<TODO>>
- <<TODO>>
- `KDB_EXEC_PATH`, which can be used to add further external tools to `kdb`, now supports `:` to separate paths.
  `kdb list-tools` and `run_env` were improved to take advantage of this. _(Markus Raab)_
- Checks for `kdbCommit` have been added to [kdb plugin-check](../help/kdb-plugin-check.md). _(Vid Leskovar)_
- add PID file config setting for kdb-run-rest-frontend _(Markus Raab)_
- Added `kdb meta-show` command which prints out all metadata along with its values for a given key. _(Michael Zronek)_
- Removed `kdb vset` as it does not properly put meta-data to the spec namespace. _(Michael Zronek)_
- Renamed kdb plugin commands following a hierarchical structure. `kdb info` is now `kdb plugin-info`, `kdb check` is now `kdb plugin-check` and `kdb list` is now `kdb plugin-list`. We also removed the obsolete `kdb fstab`. _(Philipp Gackstatter)_
- Renamed kdb meta commands:
  - `kdb getmeta` is now `kdb meta-get`
  - `kdb lsmeta` is now `kdb meta-ls`
  - `kdb showmeta` is now `kdb meta-show`
  - `kdb rmmeta` is now `kdb meta-rm`
  - `kdb setmeta` is now `kdb meta-set` _(Philipp Gackstatter)_
- Fix test tool `gen-gpg-testkey` by giving a narrower GPG key description. Fixes mismatches with existing GPG keys that contain "elektra.org" as e-mail address. _(Peter Nirschl)_
- `kdb list-commands` and `kdb plugins-list` now sort their output in an alphabetical order _(Anton Hößl)_
- `kdb plugin-list` does now mention in the helptext that with option `-v` the output is sorted by the plugin status _(Anton Hößl)_
- `kdb import`, `kdb export` and `kdb editor` now search the plugin database for suitig plugins so it's now possible to run `kdb export /hello json` instead of having to specify the plugin for the desired format directly. _(Anton Hößl)_
- Tools like `kdb editor` use the new merge library. _(Dominic Jäger)_
- <<TODO>>

## Scripts

- <<TODO>>
- <<TODO>>
- <<TODO>>

## Documentation

- improved formatting of the [`validation tutorial`](../../doc/tutorials/validation.md) _(Anton Hößl)_
- <<TODO>>
- <<TODO>>

## Tests

- We now use [Google Test](https://github.com/google/googletest) `1.10` to test Elektra. _(René Schwaiger)_
- <<TODO>>
- <<TODO>>
- <<TODO>>

## Build

### CMake

- <<TODO>>
- <<TODO>>
- <<TODO>>

### Docker

- <<TODO>>
- <<TODO>>
- <<TODO>>
- Added [Dockerfile for Ubuntu Bionic](../../scripts/docker/ubuntu/bionic/Dockerfile) _(Djordje Bulatovic)_
- We removed all Haskell packages from the Dockerfiles in the folder [scripts/docker](../../scripts/docker). _(René Schwaiger)_
- We added a basic [Dockerfile for Arch Linux](../../scripts/docker/arch/Dockerfile). _(René Schwaiger)_
- We updated the [Dockerfile for Alpine Linux](../../scripts/docker/alpine). _(René Schwaiger)_

### Vagrant

- We added a [Vagrantfile](../../scripts/vagrant/freebsd/README.md) for a virtual machine based on FreeBSD 12. _(René Schwaiger)_

### Other

- The reformatting script now checks that the correct version of `cmake-format` is used. _(Klemens Böswirth, René Schwaiger)_
- Fixed augeas crash if fopen fails. _(Michael Zronek)_
- The reformatting scripts now run in parallel. _(Markus Raab)_
- Improved various error messages and synchronized documentations. _(Michael Zronek)_
- Improved `range` plugin error message. _(Michael Zronek)_
- Improved error codes documentation to clarify the hierarchy for developers. _(Michael Zronek)_
- Out of memory error messages are now uniform. _(Michael Zronek)_

## Infrastructure

### Cirrus

- <<TODO>>
- <<TODO>>
- <<TODO>>

### Jenkins

- Fixed [coveralls](https://coveralls.io/github/ElektraInitiative/libelektra) coverage report. _(Mihael Pranjić)_
- <<TODO>>
- <<TODO>>

### Travis

- <<TODO>>
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
