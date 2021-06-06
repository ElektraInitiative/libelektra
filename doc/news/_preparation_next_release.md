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

You can try out the latest Elektra release using our docker image [elektra/elektra](https://hub.docker.com/r/elektra/elektra).
This is the quickest way to get started with Elektra without compiling and other obstacles, simply run
`docker run -it elektra/elektra`.

## Highlights

- Java fixes: JNI plugin fixed and JNA bindings improved
- Fresh Fedora 34 packages
- Documentation Improvements & Cleanups
- GCC 11 and Clang 12 compatibility

### JNI plugin fixed

The JNI plugin was encountering a double free on open. This has been fixed in conjunction with an update to JNA binding release mechanism. The previously disabled JNI test have been fixed and enabled.

For how to write plugins, please refer to [java-plugins.md](../tutorials/java-plugins.md) as well as the [JNI plugin](../../src/plugins/jni/README.md) and [JNA binding](../../src/bindings/jna/README.md) documentation.

## Plugins

The following section lists news about the [plugins](https://www.libelektra.org/plugins/readme) we updated in this release.

### JNI

- Fixed double free issues and re-enabled tests
- Updated documentation
- Increased minimum required JDK version to 9

_(Michael Tucek)_

Special thanks to _(Klemens Böswirth)_, _(Mihael Pranjić)_ and _(Robert Sowula)_ for helping with the problem analysis!

### Dbus

- Internal changes to ensure compatibility with the new `elektraNotificationContract`. _(Klemens Böswirth)_

### Dbusrecv

- Internal changes to ensure compatibility with the new `elektraNotificationContract`. _(Klemens Böswirth)_

### Xerces

- Store length of an array in metakey array according to [array decision](../decisions/array.md). _(Robert Sowula)_

### YAML Smith

- Removed plugin _(René Schwaiger)_

### Yan LR

- Removed plugin _(René Schwaiger)_

### Zeromqsend

- Internal changes to ensure compatibility with the new `elektraNotificationContract`. _(Klemens Böswirth)_

### ni

- Silence Clang 12 warnings about suspicious string literal concatenation. _(Mihael Pranjić)_

### Lua

- Removed outdated information from docs _(@a-kraschitzer)_

## Libraries

The text below summarizes updates to the [C (and C++)-based libraries](https://www.libelektra.org/libraries/readme) of Elektra.

### Core

- Remove keyCompareBy(Name)?Owner _(@a-kraschitzer)_

## Bindings

Bindings allow you to utilize Elektra using [various programming languages](https://www.libelektra.org/bindings/readme). This section keeps
you up to date with the multi-language support provided by Elektra.

### SWIG

- Remove `-Wno-shift-overflow warnings` option from SWIG bindings compile flags. _(Mihael Pranjić)_
- Suppress SWIG/Ruby bindings warning about `operator != ignored`. _(Mihael Pranjić)_

### JNA

- Gradle wrapper and docker images upgraded to 7.0.2
- Minumum Gradle version decreased to 6.0
- Upgraded JNA dependency from 4.5.2 to 5.8.0
- Increased minimum required JDK version to 11
- Updated Java binding API documentation
- Migrated native resource clean-up from `finalize()` to `Cleaner`
  - Please revisit the documentation for `Key::release` and `KeySet::release` for recommended resource release handling
  - Currently automated native key and key set resource clean-up is deactivated due to some possible race conditions wich might result in double free errors (#3869). Manual key release is also disabled to not break current API release semantic. This leads to memory leaks, which is a known issue and will be resolved in an upcoming release.
- Introduced multiple exceptions when native API calls fail - see updated java doc for details
- Introduced early parameter validation for values which would otherwise lead to unspecific errors in native API calls
- Several under the hood improvements
- Update `Key` API introducing the following changes:
  - Extracted exceptions from `Key` class
  - Fixed `Key::getCurrentMeta`
  - Moved `Elektra.KeyNewArgumentFlags` to `Key.NewArgumentTag`
  - Changed return types from `int` to `boolean` or enabled a fluent interface where appropriate
  - Renamed `Key::*Integer` to `Key::*Int`
  - Renamed `KeyInvalidNameException` to `KeyNameException`
  - Renamed `KeyTypeMismatchException` to `KeyBinaryTypeNotSupportedException`
  - Introduced `Key::get*AndRelease` convenience methods
  - Introduced `Key::createNameless`
  - Introduced `KeyReleasedException` being thrown when a released `Key` is being accessed
  - Introduced `KeyMetaException`
  - Removed unused `KeyTypeConversionException`
  - Removed `Key::needsSync`
  - Removed `Key::isNull`
    - `KeyReleasedException` is now being thrown when a released (= previously `isNull`) `Key` is being accessed
    - `Key`s with no backing native key pointer cannot be created anymore
- Updated `KeySet` API introducing the following changes:
  - Changed return type enabling a fluent interface where appropriate
  - Renamed `KeySet::head` to `KeySet::first`
  - Renamed `KeySet::tail` to `KeySet::last`
  - Introduced `KeySetReleasedException` being thrown when a released `KeySet` is being accessed
  - Introduced `KeySetAppendException`
  - Removed `KeySet::create(int, Object[])`
  - Removed `KeySet::needsSync`
  - Methods which have been returning a nullable `Key`, now return an `Optional<Key>´
    - `KeySet::lookup*` now returns `Optional<Key>`
    - `Key::getMeta` now returns `Optional<Key>`
    - Example:
      ```java
      // checking whether the key has been found BEFORE API change
      Key found = ks.lookup("/some/key");
      if (found != null) {
        // process found key
      }
      ```
      ```java
      // checking whether the key has been found AFTER API change
      ks.lookup("/some/key").ifPresent(k -> // process found key );
      ```
- Updated `KDB` API introducing the following changes:
  - Introduced `KDBClosedException` being thrown when a closed `KDB` session is being accessed
  - Introduced `KDB::get(Key)`
  - Introduced `KDB::open()`
  - Introduced `KDB::open(KeySet)`
  - Introduced `KDB::goptsContract(String[], String[], Key, KeySet)
  - Changed return type enabling a fluent interface where appropriate
- Updated tests accordingly

_(Michael Tucek)_

## Tools

- Remove `kdb set` functionality that creates a null key. _(Robert Sowula)_
- Rename elektraStrnDup to elektraMemDup _(@a-kraschitzer)_
- Update specmount error message _(@a-kraschitzer)_
- Update `elektraMemDup` to `void *` and update the documentation. _(Mihael Pranjić)_
- There have been a few bugfixes for elektrad. _(Klemens Böswirth)_
- Update `lodash` and `hosted-git-info` dependencies of `webd` due to security update. _(Mihael Pranjić)_

## Scripts

- Require clang-format 12 for reformatting C and Java. _(Mihael Pranjić)_
- Use basename of release file in `generate-hashsums`. _(Mihael Pranjić)_
- Use `shfmt` v3.2.4 to reformat shell scripts. _(Mihael Pranjić)_
- Use `cmake-format` v0.6.13 (`cmakelang`) to reformat CMake. _(Mihael Pranjić)_

## Documentation

- Added Reviews for all functions contained in the Elektra Core API _(@lawli3t)_
- Added packaging section to news template. _(Mihael Pranjić)_
- Minor readability improvement in [highlevel.md](/doc/tutorials/highlevel.md) _(Tobias Schubert @qwepoizt)_
- Fix examples of spec plugin. _(Robert Sowula)_
- Added Reviews for all functions contained in the Elektra Core API _(@lawli3t)_
- Document package names of plugins, bindings and tools. _(Robert Sowula)_
- Small update in API docu related to different namespaces in returned keys. _(Markus Raab)_
- improved docu of [noresolver](/src/plugins/noresolver). _(Markus Raab)_
- improved plugin tutorial. _(Markus Raab)_
- Adding info about syncing forks to `doc/GIT.md` _(Klemens Böswirth)_
- Work on [COMPILE.md](/doc/COMPILE.md) and [INSTALL.md](/doc/INSTALL.md) to help with understanding _(@a-kraschitzer)_
- Update and correct licensing information _(@a-kraschitzer)_
- Rename [RELEASE.md](/doc/todo/RELEASE.md) _(@a-kraschitzer)_
- Improved documentation for module kdb in Elektra Core. _(@lawli3t)_
- Improved documentation for module key in Elektra Core. _(@lawli3t)_
- Improved documentation for module keyname in Elektra Core. _(@lawli3t)_
- Improved documentation for module keyvalue in Elektra Core. _(@lawli3t)_
- Improved documentation for module keymeta in Elektra Core. _(@lawli3t)_
- Improved documentation for module keytest in Elektra Core. _(@lawli3t)_
- Improved documentation for module keyset in Elektra Core. _(@lawli3t)_
- Fixed example in the "mount-configuration-files" tutorial [#3722](https://github.com/ElektraInitiative/libelektra/issues/3722). _(Philipp Oppel)_
- Update and correct third party licensing information _(@a-kraschitzer)_
- Added contact details to `AUTHORS.md` _(Michael Tucek)_
- Use Ronn-NG instead of unmaintained ronn to generate man pages. _(Mihael Pranjić)_
- Re-generate man pages to add missing information and remove unnecessary symbols and escaping. _(Mihael Pranjić)_
- Update `doc/Doxyfile` to Doxygen 1.9.1 and fix a syntax error with the `FILTER_PATTERNS` directive. _(Mihael Pranjić)_

## Tests

- Fix failing `testshell_markdown_tutorial_crypto` on Mac OS and other OS with GnuPG version >= 2.3.1. _(Peter Nirschl @petermax2)_
- Use clang-format 12 for Restyled and update Restyled version. _(Mihael Pranjić)_
- Update all Restyled formatters to current versions. _(Mihael Pranjić)_
- Add additional test cases for module `key` _(@lawli3t)_
- <<TODO>>
- <<TODO>>

## Packaging

- We now package the ruby bindings, ruby plugin and the gitresolver plugin. _(Robert Sowula)_
- We added Fedora 34 packages. _(Mihael Pranjić)_
- We added Debian Bullseye packages. _(Robert Sowula)_

## Build

### CMake

- Disable binding tests when `BUILD_TESTING` is disabled. _(Robert Sowula)_
- Remove unused `FindCppCMS.cmake` CMake module and unused `Boost` variables. _(Mihael Pranjić)_

### Docker

- Add Fedora 34 images. _(Mihael Pranjić)_
- We added release images that come with pre-installed dependencies and sudo permissions for each distribution we build packages for. _(Robert Sowula)_
- Use Clang 12 and Gradle 7.0 in Debian Sid image. _(Mihael Pranjić)_
- Remove boost and some unused dependencies from all Docker images. _(Mihael Pranjić)_
- Use Gradle 7.0 and Ronn-NG `0.10.1.pre1` in Docker images. _(Mihael Pranjić)_
- Remove unused Debian Buster `doc` image. _(Mihael Pranjić)_

## Infrastructure

### Cirrus

- Use Clang 12 and Python 3.9 for macOS builds. _(Mihael Pranjić)_
- Pin GnuPG version to 2.2.x. _(Mihael Pranjić)_
- Update Fedora image to version 34. _(Mihael Pranjić)_
- Clean up unused dependencies in Arch Linux image and add Ronn-NG to generate man pages. _(Mihael Pranjić)_

### GitHub Actions

- Pin GnuPG version to 2.2.x. _(Mihael Pranjić)_
- Enable `jni` plugin and fix `JAVA_HOME` detection. _(Mihael Pranjić)_

### Jenkins

- We now build and test on Fedora 34 and 33. Fedora 32 was removed from the CI. _(Mihael Pranjić)_
- Build release documentation on Debian Sid, due to newer TeX and Doxygen packages. _(Mihael Pranjić)_

### Travis

- Update Ubuntu to Focal, use GCC 10 and clean up travis scripts. _(Mihael Pranjić)_

## Website

The website is generated from the repository, so all information about
plugins, bindings and tools are always up to date. Furthermore, we changed:

- Update `highlight.js` due to a [ReDOS vulnerability](https://github.com/advisories/GHSA-7wwv-vh3v-89cq) and upgrade other dependencies as well. _(Mihael Pranjić)_
- Catch errors when code highlighting fails. _(Mihael Pranjić)_
- Get rid of unused code: authentication, backend, users, snippets and conversion service. _(Mihael Pranjić)_
- Fix docsearch sourcemap error. _(Mihael Pranjić)_
- Update `lodash` dependency due to security update. _(Mihael Pranjić)_

## Outlook

We are currently working on following topics:

- Elektrify KDE _(Dardan Haxhimustafa)_, _(Felix Resch)_ and _(Mihael Pranjić)_
- 1.0 API _(Stefan Hanreich)_ and _(Klemens Böswirth)_
- Improve Java Development Experience _(Michael Tucek)_
- Elektrify GNOME _(Mihael Pranjić)_
- Continious Releases _(Robert Sowula)_
- KDB access using FUSE _(Alexander Firbas)_
- Default TOML plugin _(Jakob Fischer)_
- Improve Plugin Framework _(Vid Leskovar)_
- Improve 3-way merge _(Dominic Jäger)_
- Shell completion _(Ulrike Schäfer)_
- Ansible bindings _(Thomas Waser)_

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

For more information, see [https://libelektra.org](https://libelektra.org)

Best regards,
[Elektra Initiative](https://www.libelektra.org/developers/authors)
