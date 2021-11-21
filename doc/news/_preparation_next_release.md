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

### filecheck

- Removed unused variable that threw an error in filecheck.c. _(Vaibhav Ganesh @flackojr)_
- <<TODO>>
- <<TODO>>

### mmapstorage

- Removed unused variable that threw an error in mmapstorage.c. _(Vaibhav Ganesh @flackojr)_
- <<TODO>>
- <<TODO>>

### csvstorage

- Add `array` meta key to the parentKey of imported Keys _(@muskater)_ _(@4ydan)_ _(@lawli3t)_

### <<Plugin3>>

- <<TODO>>
- <<TODO>>
- <<TODO>>

### uname

- Minor improvement of source code readability in uname.c _(@lawli3t)_

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
- `KeySet` now also has a reference counter like `Key`. The new functions `ksIncRef` and `ksDecRef` behave like their counterparts `keyIncRef` and `keyDecRef`.
  `ksDel` also behaves like `keyDel` in regard to reference counting, i.e. it does nothing unless the reference count is 0.
  The reference counting is very useful for bindings (especially with automatic garbage collection). _(Klemens Böswirth)_
- Clarified that our reference counting mechanism is more related to a shared lock than to the concept of shared ownership. _(Klemens Böswirth)_
- Both the reference count for `Key` and for `KeySet` now use `uint16_t` to reduce memory usage. `Key` previously used `size_t`. _(Klemens Böswirth)_
- Reorder `Key` and `KeySet` struct members to aviod padding and make space for a new `uint16_t` member, reserved for future use. _(Mihael Pranjić)_
- Improve `keyReplacePrefix` by using new `keyCopy` function instead of manually copying the name of the `Key` _(@lawli3t)_
- Added else error to core for elektraGetCheckUpdateNeeded _(Aydan Ghazani @4ydan)_

- Fix check for valid namespace in keyname creation _(@JakobWonisch)_
- Fix `keyCopyMeta` not deleting non existant keys in destination (see #3981) _(@JakobWonisch)_

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

- Integrated the `HelloElektra` example as gradel sub-project to allow it to directly depend on the current binding

_(Michael Tucek)_

### FUSE Binding

- Added check for existence of accessed path before opening new file descriptor _(@lawli3t)_

### <<Binding3>>

## Tools

- <<TODO>>
- <<TODO>>
- <<TODO>>

## Scripts

- Updated `reformat-c` script to use clang-format version 13. _(Mihael Pranjić)_
- <<TODO>>
- Fix bug where the PATH environment variable would get overwritten in some of the Docker images. Reduce image size _(Ivaylo Ivanov)_

## Documentation

- Integrate missing pages to website _(Ivaylo Ivanov)_
- Improved compilation documentation _(Ivaylo Ivanov)_
- Start making Elektra [reuse](https://reuse.software) compliant. _(Markus Raab)_
- Fix Links in [README.md](/README.md) and small clarifications. _(Markus Raab)_
- Remove previous authors. _(Markus Raab)_
- add pre/postconditions and invariants to module keytest _(@lawli3t)_
- Updated the news template. _(Mihael Pranjić)_
- Update and improve tutorial and in-code comments for high-level API _(Tobias Schubert @qwepoizt)_
- Improve documentation of opts library _(Tobias Schubert @qwepoizt)_
- Update tutorial "High-level API (with code-generation)" to reflect change of `loadConfiguration()`'s signature in release 0.9.5 _(Tobias Schubert @qwepoizt)_
- add pre/postconditions and invariants to module keyvalue _(@lawli3t)_
- Update and improve inline documentation of `kdb gen`. _(Tobias Schubert @qwepoizt)_
- Fix broken links. _(Robert Sowula)_
- Emphasize that `type` is required when the HL API is used. _(Tobias Schubert @qwepoizt)_
- Add debugging tutorial. _(Tobias Schubert @qwepoizt)_
- Improve wording and formatting of DESIGN.md _(@lawli3t)_
- Correct various typing-, spelling- and grammar-errors in the .md-files in the directory doc and its subdirectories. _(Florian Lindner @flo91)_
- Added documentation for decision meeting from 15.07.2021 _(@lawli3t)_
- <<TODO>>
- explained in the docker test tutorial how to run the container with podman instead of docker. _(@muskater)_
- Add a new example on how to use keyCopy. _(@muskater)_
- <<TODO>>
- Fix some typos in the "Getting Started" page _(Ivaylo Ivanov)_
- Added debian buster tutorial to python bindings tutorial _(@4ydan)_
- Fixed some typos in the "namespaces.md" documentation _(@muskater)_

## Tests

- disable Rust from buster _(Markus Raab)_
- <<TODO>>
- Cleanup tests/linkchecker.whitelist and fix off-by-1 bug of the counter in the scripts/link-checker script (increase counter before printf) _(Florian Lindner @flo91)_
- add tests the env binding _(Ivaylo Ivanov)_
- add and improve checks in scripts/sed _(Florian Lindner @flo91)_
- change the cpp Key-class (key.hpp) to check the return values of the called c-functions
  and throw exceptions if values that indicate an error are returned + add tests that
  check for this exceptions _(Florian Lindner @flo91)_
- <<TODO>>
- <<TODO>>
- Added more test cases for the keyCopy function _(@muskater)_
- add exception tests for key C++ bindings _(Ivaylo Ivanov)_

## Packaging

- <<TODO>>
- <<TODO>>
- <<TODO>>

## Build

### CMake

- Marked certain variables as advanced and separated user modifiable and unaccessable variables. _(Vaibhav Ganesh @flackojr)_
- <<TODO>>
- <<TODO>>

### Docker

- <<TODO>>
- <<TODO>>
- <<TODO>>

## Infrastructure

### Jenkins

- <<TODO>>
- <<TODO>>
- <<TODO>>

### Cirrus

- <<TODO>>
- <<TODO>>
- <<TODO>>

### GitHub Actions

- <<TODO>>
- <<TODO>>
- <<TODO>>

## Website

The website is generated from the repository, so all information about
plugins, bindings and tools are always up to date. Furthermore, we changed:

- <<TODO>>
- Remove links to Travis CI and replace them with Github Actions (with badge). _(Mihael Pranjić)_
- <<TODO>>

## Other

- Make Elektra [reuse](https://reuse.software) reuse compliant _(Ivaylo Ivanov)_

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
