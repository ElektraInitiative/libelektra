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

- Session Recording
- ODBC Backend

### Session Recording

Elektra now comes with a powerful new feature that allows users to record and export changes made to the KDB: session recording.
This feature enables you to easily track changes made to the configuration database over time, which helps troubleshoot issues, diagnose errors, and improve system performance.
It even lets you undo the changes you've performed!

You can also export the changes as Ansible playbooks using the new Ansible storage plugin!
This makes it easy to automate and reproduce system configurations.
We think that this feature offers significant time savings and improved accuracy when managing complex systems.

Whether you're a system administrator, developer, or DevOps engineer, we believe that the session recording feature in Elektra will become an essential tool for managing and maintaining system configurations.
[Try it today](../tutorials/recording.md) and experience the benefits of streamlined configuration management.

**Note:** when you activate session recording, concurrency of Elektra will be somewhat limited.
As long as it is active, a global lock will be created to ensure no two processes will write data simultaneously.
This behavior is similar as to when multiple processes will write to the same configuration file.
Applications should already handle this case gracefully, and just retry writing their configuration.

### ODBC Backend

Based on the new and more versatile concept for [backends](/doc/dev/backend-plugins.md), where backends are implemented as plugins, a new backend-plugin that uses ODBC data sources for storing keys has been developed.
It was tested on Gentoo Linux with [unixODBC](https://www.unixodbc.org) using [SQLite](https://www.sqlite.org) and [PostgreSQL](https://www.postgresql.org) data sources.
The ODBC backend-plugin can only be built if the ODBC library is available on the build system. This can be accomplished by installing e.g. unixODBC.
Microsoft ODBC (on MS Windows) and [iODBC](https://www.iodbc.org) should also be supported, but were not tested yet.
If you use the plugin with another ODBC implementation as unixODBC, you are very welcome to update the documentation with your experiences!

The [tutorial](../tutorials/odbc-backend.md) is a good place for getting started with the new ODBC backend for Elektra.

## Plugins

The following text lists news about the [plugins](https://www.libelektra.org/plugins/readme) we updated in this release.

### General

- Updated target name of shared object files according to [#3486](https://issues.libelektra.org/3486)

### spec

- Remove metakeys from array elements correctly [#4961](https://issues.libelektra.org/4961) _(Tomislav Makar @tmakar)_

### recorder

- Add recorder plugin. _(Maximilian Irlinger @atmaxinger)_

### jdbc

- Fix formatting/spelling problem in README.md _(Hannes Laimer @hannes99)_

### backend_odbc

- Add ODBC backend _(Florian Lindner @flo91)_

### ansible

- Add `ansible` plugin for exporting keysets as [ansible-libelektra](https://galaxy.ansible.com/elektra_initiative/libelektra) playbooks. _(Maximilian Irlinger @atmaxinger)_

### toml

- Fix error reporting when unsupported metakey has been encountered. _(Maximilian Irlinger @atmaxinger)_

### length

- Remove experimental status. _(Maximilian Irlinger @atmaxinger)_

### Xfconf

- Implemented the first revision of the Xfconf binding. _(Richard Stöckl @Eiskasten)_
- Make Xfconf storage plugin compatible with new backend and noresolver. _(Richard Stöckl @Eiskasten)_
- This allows to use elektra as a drop-in replacement for applications which use Xfconf. _(Richard Stöckl @Eiskasten)_
- Xfconf applications can now read and write configuration settings to elektra. _(Richard Stöckl @Eiskasten)_
- Use cmake variables in the replace and restore scripts. _(Richard Stöckl @Eiskasten)_

### c

- Improve the c plugin: some refactoring, add documentation, extend README.md, add unit tests. _(Florian Lindner @flo91)_

## Libraries

The text below summarizes updates to the [C (and C++)-based libraries](https://www.libelektra.org/libraries/readme) of Elektra.

### kdb

- Add `elektraCopyError` function to copy error from one key to another _(Maximilian Irlinger @atmaxinger)_
- Add `elektraCopyWarnings` function to copy warnings from one key to another _(Maximilian Irlinger @atmaxinger)_
- Add `elektraCopyErrorAndWarnings` function to copy error and warnings from one key to another _(Maximilian Irlinger @atmaxinger)_

### record

- Add record library used for session recording. _(Maximilian Irlinger @atmaxinger)_

### ease

- Add `elektraArrayGetPrefix` function. _(Maximilian Irlinger @atmaxinger)_

## Bindings

Bindings allow you to utilize Elektra using [various programming languages](https://www.libelektra.org/bindings/readme).
This section keeps you up-to-date with the multi-language support provided by Elektra.

### C++

- Provide getter for the underlying C object of KDB _(Maximilian Irlinger @atmaxinger)_
- Add `ElektraDiff` binding for C++ _(Maximilian Irlinger @atmaxinger)_
- The `dup` method of `KeySet` now returns a wrapped object _(Maximilian Irlinger @atmaxinger)_
- Add an overload for `KeySet::cut` that accepts a string for the keyname _(Maximilian Irlinger)_
- The `dup` method of `Key` now returns a wrapped object _(Maximilian Irlinger @atmaxinger)_
- Add overloads for `Key::isBelow`, `Key::isBelowOrSame` and `Key::isDirectBelow` that accept a string as the key name _(Maximilian Irlinger @atmaxinger)_
- Include the header `cstdint` in `key.hpp`. It is needed for an enum of type `std::uint8_t` _(Florian Lindner @flo91)_

### Java

- Added documentation about the design of the binding. _(Michael Tucek @tucek)_

### Python

- Add `ElektraDiff` binding _(Maximilian Irlinger @atmaxinger)_
- The `__meta__` attribute on a key now returns a proper keyset _(Maximilian Irlinger @atmaxinger)_
- Add new module `kdb.errors` to simplify extracting errors and warnings from keys _(Maximilian Irlinger @atmaxinger)_
- Add new module `kdb.record` for interfacing with the session recording capabilities of Elektra _(Maximilian Irlinger @atmaxinger)_
- Add `getConflictingKeys` method to `kdb.merge.MergeResult`. _(Maximilian Irlinger @atmaxinger)_

### Rust

- Upgrade bindgen dependency to version 0.55.1. _(Maximilian Irlinger @atmaxinger)_

### Xfconf

- Implemented the first revision of the Xfconf binding. _(Richard Stöckl @Eiskasten)_
- This allows to use elektra as a drop-in replacement for applications which use Xfconf. _(Richard Stöckl @Eiskasten)_
- Xfconf applications can now read and write configuration settings to elektra. _(Richard Stöckl @Eiskasten)_
- Use cmake variables in the replace and restore scripts. _(Richard Stöckl @Eiskasten)_

## Tools

### kdb

- Add commands for session recording. _(Maximilian Irlinger @atmaxinger)_
- The `kdb mount` command will now automatically detect whether the given path is an absolute path. _(Maximilian Irlinger @atmaxinger)_
- Fix unwanted removal of subkeys when using mv _(Hannes Laimer @hannes99)_
- Fix inconsistent return values in code, tests and man pages _(Hannes Laimer @hannes99)_
- Remove `smount` alias _(Hannes Laimer @hannes99)_

## Documentation

- Fix bug in Doxygen comment for `const char * keyName (const Key * key)` which lead to failed building of the refman.pdf on recent TeX Live releases _(Florian Lindner @flo91)_
- Update release documentation regarding version tags. _(Mihael Pranjić @mpranj)_
- Add glossaries for [developers](/doc/dev/dev-glossary.md) and [contributors](/doc/contrib/contrib-glossary.md) _(Maximilian Irlinger @atmaxinger)_
- Add use cases for using the bindings. _(Richard Stöckl @Eiskasten)_
- Write the Xfconf tutorial. _(Richard Stöckl @Eiskasten)_
- Add use cases for libelektra-core _(@lawli3t)_

### Use Cases

- Add end-user and developer integration use case _(Hannes Laimer @hannes99)_
- Add use cases for using the bindings. _(Richard Stöckl @Eiskasten)_
- Add use cases for libelektra-core _(@lawli3t)_

### Decisions

- Add decision for [Elixir bindings](../decisions/3_decided/elixir_bindings.md). _(@0x6178656c)_

### Tutorials

- Add tutorial for [session recording](../tutorials/recording.md). _(Maximilian Irlinger @atmaxinger)_
- Add tutorial for [ODBC backend](../tutorials/odbc-backend.md). _(Florian Lindner @flo91)_

### Man Pages

- Add man pages for session recording

## Tests

- Add macro `succeed_if_keyset_contains_key_with_string` to assert that a certain key with a certain value must exist. _(Maximilian Irlinger @atmaxinger)_

### Docker

- Use openwrt/sdk instead of openwrtorg/sdk. _(Richard Stöckl @Eiskasten)_
- Reduce fedora docker image size (PR #4637) _(@4ydan)_

## Infrastructure

### Jenkins

- Disable openwrt build until we have found a new maintainer for it. _(Maximilian Irlinger @atmaxinger)_

### Cirrus

- The arch package `texlive-most` is no longer available, replaced it with other texlive packages. See https://archlinux.org/packages/?q=texlive _(Florian Lindner @flo91)_

## Outlook

We are currently working on following topics:

- Rewrite the `kdb` tool in C.

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
