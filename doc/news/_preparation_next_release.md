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

You can also read the news [on our website](https://www.libelektra.org/news/0.9.<<VERSION>>-release).

You can try out the latest Elektra release using our docker image [elektra/elektra](https://hub.docker.com/r/elektra/elektra).
This is the quickest way to get started with Elektra without compiling and other obstacles, simply run:

```sh
docker pull elektra/elektra
docker run -it elektra/elektra
```

## Highlights

- <<HIGHLIGHT1>>
- <<HIGHLIGHT2>>
- <<HIGHLIGHT3>>

### <<HIGHLIGHT1>>

### <<HIGHLIGHT2>>

### <<HIGHLIGHT2>>

## Plugins

The following section lists news about the [plugins](https://www.libelektra.org/plugins/readme) we updated in this release.

### <<Plugin1>>

- <<TODO>>
- <<TODO>>
- <<TODO>>

### <<Plugin2>>

- <<TODO>>
- <<TODO>>
- <<TODO>>

### <<Plugin3>>

- <<TODO>>
- <<TODO>>
- <<TODO>>

## Libraries

The text below summarizes updates to the [C (and C++)-based libraries](https://www.libelektra.org/libraries/readme) of Elektra.

### Compatibility

- <<TODO>>
- <<TODO>>
- <<TODO>>

### Core

- <<TODO>>
- <<TODO>>
- <<TODO>>
- Remove obsolete `ksNeedSync` function. _(Mihael Pranjić)_

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

### Java binding

- Renamed zero argument static factory method `Key::createNameless` to `Key::create`. To migrate to this change, just update calling code to use the new method name.
- Updated method documentation previously publishing the error key based error handling approach to the Java binding consumer. Such arguments are now explicitly only used for returning warning information in case no error occurred. In case of an exceptional state, appropriate exceptions are thrown. Such exceptions provide access to the underlying key containing warning and error information as meta data. Please review API usage to consider the more elaborated explanation of how Elektra uses this argument's value. Affected signatures:
  - Updated javadoc for `KDB::open(Key)`
  - Updated javadoc for `KDB::open(KeySet, Key)`
  - Updated javadoc for `KDB::close(Key)`
  - Updated javadoc for `KDB::get(Key)`
  - Updated javadoc for `KDB::get(KeySet, Key)`
  - Updated javadoc for `KDB::set(KeySet, Key)`, better explaining the relevance of the second argument `parentKey`.
- Introduced `KeySet::remove(Key)` and `KeySet::remove(String)`
- Removed `KeySet::lookup(Key, int)` and `KeySet::lookup(String, int)` as well as accompanying flag definitions `KeySet::KDB_O_NONE`, `KeySet::KDB_O_DEL` and `KeySet::KDB_O_POP`. Please use `KeySet::lookup(Key)` and `KeySet::lookup(String)` instead. Instead of `KeySet::KDB_O_DEL`, please consider using `Key::release`. The proper replacement for `KeySet::KDB_O_POP` is `KeySet::remove(Key)` or `KeySet::remove(String)`.
- Native library proxy interface `Elektra` is now package private (previously was public).
- Added example Java plugin `whitelist` (see [here](../../src/bindings/jna/plugins/whitelist/README.md))
- Changed `Key nextMeta()` to `Optional<Key> nextMeta ()` no longer throwing NoSuchElementException for non-exceptional behavior
- Added support of binary valued keys:
  - Renamed `KeyBinaryTypeNotSupportedException` to `KeyStringValueException`
  - Introduced `KeyBinaryValueException`
  - Improved `Key` test coverage
  - Introduced `Key::getBinary()` and `Key::setBinary(byte[])`
- Fixed example project in `examples/external/java/read-keys-example`
  - now works with a standard installation of Elektra
  - updated code to work with current Java binding

_(Michael Tucek)_

### <<Binding2>>

### <<Binding3>>

## Tools

- Really add all tools when using `-DTOOLS=ALL`. _(Markus Raab)_
- ZeroMQ Hub: fix compilation and man page. _(Markus Raab)_
- <<TODO>>
- <<TODO>>
- <<TODO>>

## Scripts

- <<TODO>>
- <<TODO>>
- <<TODO>>

## Documentation

- Add link and small improvements to [tutorial about writing specifications](/doc/tutorials/specification.md). _(Markus Raab)_
- Fix broken links _(@lawli3t)_
- Remove previous authors. _(Markus Raab)_
- Updated the news template. _(Mihael Pranjić)_
- Update tutorial and in-code comments for high-level API _(Tobias Schubert @qwepoizt)_
- <<TODO>>

## Tests

- <<TODO>>
- <<TODO>>
- <<TODO>>

## Packaging

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
- Enable BuildKit features to leverage tmpfs to speed up `docker build` commands. _(Mihael Pranjić)_
- <<TODO>>

## Infrastructure

### Jenkins

- <<TODO>>
- <<TODO>>
- <<TODO>>
- Use `tmpfs` in Docker to speed up the test suite. _(Mihael Pranjić)_

### Cirrus

- <<TODO>>
- <<TODO>>
- <<TODO>>
- Migrate most macOS build jobs to GitHub actions to speed up builds. _(Mihael Pranjić)_
- Bump FreeBSD images to 12.2 and 13.0 using the LLVM 12 toolchain, drop FreeBSD 11. _(Mihael Pranjić)_
- Fix cirrus-file parsing errors. _(Mihael Pranjić)_

### GitHub Actions

- <<TODO>>
- <<TODO>>
- <<TODO>>
- Upgrade macOS GCC build job to GCC 11. _(Mihael Pranjić)_

## Website

The website is generated from the repository, so all information about
plugins, bindings and tools are always up to date. Furthermore, we changed:

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
