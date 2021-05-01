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

### Zeromqrecv

- <<TODO>>
- <<TODO>>
- <<TODO>>

### Lua

- Removed outdated information from docs _(@a-kraschitzer)_

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

- Remove keyCompareBy(Name)?Owner _(@a-kraschitzer)_
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

### SWIG

- Remove `-Wno-shift-overflow warnings` option from SWIG bindings compile flags. _(Mihael Pranjić)_

### <<Binding2>>

### <<Binding3>>

## Tools

- Remove `kdb set` functionality that creates a null key. _(Robert Sowula)_
- Rename elektraStrnDup to elektraMemDup _(@a-kraschitzer)_
- Update specmount error message #3666 _(@a-kraschitzer)_
- Update specmount error message _(@a-kraschitzer)_
- Update `elektraMemDup` to `void *` and update the documentation. _(Mihael Pranjić)_
- There have been a few bugfixes for elektrad. _(Klemens Böswirth)_
- <<TODO>>
- <<TODO>>

## Scripts

- <<TODO>>
- <<TODO>>
- <<TODO>>

## Documentation

- Added Reviews for all functions contained in the Elektra Core API _(@lawli3t)_
- Added packaging section to news template. _(Mihael Pranjić)_
- Minor readability improvement in [highlevel.md](/doc/tutorials/highlevel.md) _(Tobias Schubert @qwepoizt)_
- Fix examples of spec plugin. _(Robert Sowula)_
- Added Reviews for all functions contained in the Elektra Core API _(@lawli3t)_
- Document package names of plugins, bindings and tools. _(Robert Sowula)_
- Small update in API docu related to different namespaces in returned keys. _(Markus Raab)_
- improved docu of [noresolver](/src/plugins/noresolver). _(Markus Raab)_
- <<TODO>>
- <<TODO>>
- Adding info about syncing forks to `doc/GIT.md` _(Klemens Böswirth)_
- Work on [COMPILE.md](/doc/COMPILE.md) and [INSTALL.md](/doc/INSTALL.md) to help with understanding _(@a-kraschitzer)_
- Update licensing information (year) _(@a-kraschitzer)_
- Rename [RELEASE.md](/doc/todo/RELEASE.md) _(@a-kraschitzer)_
- Improved documentation for module kdb in Elektra Core. _(@lawli3t)_
- Improved documentation for module key in Elektra Core. _(@lawli3t)_
- Improved documentation for module keyname in Elektra Core. _(@lawli3t)_
- Improved documentation for module keyvalue in Elektra Core. _(@lawli3t)_
- Improved documentation for module keymeta in Elektra Core. _(@lawli3t)_
- Improved documentation for module keytest in Elektra Core. _(@lawli3t)_
- Improved documentation for module keyset in Elektra Core. _(@lawli3t)_
- Fixed example in the "mount-configuration-files" tutorial [#3722](https://github.com/ElektraInitiative/libelektra/issues/3722). _(Philipp Oppel)_

## Tests

- <<TODO>>
- <<TODO>>
- <<TODO>>

## Packaging

- We now package the ruby bindings, ruby plugin and the gitresolver plugin. _(Robert Sowula)_
- We added Fedora 34 packages. _(Mihael Pranjić)_

## Build

### CMake

- Disable binding tests when `BUILD_TESTING` is disabled. _(Robert Sowula)_
- <<TODO>>
- <<TODO>>

### Docker

- Add Fedora 34 images. _(Mihael Pranjić)_
- <<TODO>>
- <<TODO>>

## Infrastructure

### Cirrus

- Use Clang 12 and Python 3.9 for macOS builds. _(Mihael Pranjić)_
- Pin GnuPG version to 2.2.x. _(Mihael Pranjić)_
- Update Fedora image to version 34. _(Mihael Pranjić)_
- <<TODO>>
- <<TODO>>

### GitHub Actions

- Pin GnuPG version to 2.2.x. _(Mihael Pranjić)_
- Enable `jni` plugin and fix `JAVA_HOME` detection. _(Mihael Pranjić)_
- <<TODO>>
- <<TODO>>

### Jenkins

- We now build and test on Fedora 34 and 33. Fedora 32 was removed from the CI. _(Mihael Pranjić)_
- <<TODO>>
- <<TODO>>

### Travis

- <<TODO>>
- <<TODO>>
- <<TODO>>

## Website

The website is generated from the repository, so all information about
plugins, bindings and tools are always up to date. Furthermore, we changed:

- Update `highlight.js` due to a [ReDOS vulnerability](https://github.com/advisories/GHSA-7wwv-vh3v-89cq) and upgrade other dependencies as well. _(Mihael Pranjić)_
- Catch errors when code highlighting fails. _(Mihael Pranjić)_
- Get rid of unused code: authentication, backend, users, snippets and conversion service. _(Mihael Pranjić)_
- Fix docsearch sourcemap error. _(Mihael Pranjić)_
- <<TODO>>
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
