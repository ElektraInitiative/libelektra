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

- Important Changes to Keynames
- Debian Packaging with CPack
- <<HIGHLIGHT3>>

### Important Changes to Keynames

There have been significant changes to Elektra's Keynames.
The most important change is that you now need a `:` after the namespace.
So instead of `system/elektra/version` you have to use `system:/elektra/version`.

The second big change is to array elements.
From now on `keyNew ("/array/#10", KEY_END)` will create a `Key` with name `/array/#_10`, to make arrays more user-friendly while preserving numerical ordering.

The whole internal implementation for `keySetName`, `keyAddName`, etc. has been completely rewritten.
If you rely on specific behaviour of Elektra's Keynames and have already taken the two changes above into account, please refer to the newly created [Keyname documentation](../keynames) and (easier to read) [Python reference implementation](../keynames/keynames.py).

<!-- TODO (kodebach): finish -->

- Fix combining dot-dot parts and escapes in key names. _(Klemens Böswirth)_
- Fix adding more than 10 warnings to a key. _(Klemens Böswirth)_

### Debian Packaging with CPack

- We are now using CPack to generate modular Debian and Ubuntu packages. This simplifies the packaging process and solves problems where a PR that introduces changes to installed files, fails. We can now also set distribution specific dependencies with CPack, which is needed for some packages. _(Robert Sowula)_

### <<HIGHLIGHT3>>

### Cleanup

We removed the `ini` plugin (superseded by the TOML plugin), the `null` plugin (superseded by the base64 plugin) and the `tcl` plugin _(Markus Raab, Philipp Gackstatter)_

## Plugins

The following section lists news about the [modules](https://www.libelektra.org/plugins/readme) we updated in this release.

### jni

- Fix rare memleak when the `jni` plugin is closed. _(Mihael Pranjić)_

### mINI

- We changed the `provides` clause in the plugin contract. Now mINI offers support for the [properties format](https://en.wikipedia.org/wiki/.properties) (`storage/properties`) instead of the INI file format (`storage/ini`). This makes sense, since the plugin never supported the [section syntax][] of INI files. _(René Schwaiger)_

[section syntax]: https://en.m.wikipedia.org/wiki/INI_file#Sections

### Quickdump

- Support for the old quickdump v1 and v2 formats has been removed. _(Klemens Böswirth)_

### Simple INI

- The plugin contract now correctly states that the plugin offers support for the [properties format](https://en.wikipedia.org/wiki/.properties). Before it would state that the plugin offered support for the INI file format. This is not true, since the plugin does not support the [section syntax][] of the INI file format.

### YAML CPP

- We fixed an [use after free bug in the plugin](https://issues.libelektra.org/3561). _(René Schwaiger)_

### Yan LR

- The plugin now works (with and) requires [ANTLR `4.9`](https://github.com/antlr/antlr4/releases/tag/4.9). _(René Schwaiger)_

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

### <<Library1>>

- <<TODO>>
- <<TODO>>
- <<TODO>>

### <<Library2>>

- <<TODO>>
- <<TODO>>
- <<TODO>>

### Proposal

- Removed `elektraKeyGetMetaKeySet` and moved `keySetStringF` to the hosts plugin. _(Philipp Gackstatter)_
- Removed `ksPopAtCursor`. _(Philipp Gackstatter)_
- <<TODO>>

## Bindings

Bindings allow you to utilize Elektra using [various programming languages](https://www.libelektra.org/bindings/readme). This section keeps
you up to date with the multi-language support provided by Elektra.

### Lua

- Remove ipairs support and add our own iterator to add support for Lua 5.4, since `__ipairs` was deprecated. _(Manuel Mausz)_

### Ruby

- Enable `__declspec` attributes for Ruby 3.0. _(Mihael Pranjić)_

### <<Binding3>>

## Tools

- <<TODO>>
- <<TODO>>
- The QtGUI was updated to be compatible with the new key name structure. _(Klemens Böswirth)_

## Scripts

- <<TODO>>
- <<TODO>>
- <<TODO>>

## Documentation

- Finalize 1.0 decisions. _(Markus Raab)_
- <<TODO>>
- <<TODO>>

## Tests

- <<TODO>>
- <<TODO>>
- <<TODO>>

## Build

### CMake

- Use Lua 5.4 when available. _(Mihael Pranjić)_
- <<TODO>>
- Force `RTLD_NODELETE` on dlopen() when the `ENABLE_ASAN` CMake option is used. This enables ASAN to find symbols which otherwise might be unloaded. _(Mihael Pranjić)_
- <<TODO>>

### Docker

- We added a Docker image for [building the documentation on Debian sid](../../scripts/docker/debian/sid/doc.Dockerfile). _(René Schwaiger)_
- We removed the Docker image for building the documentation on Debian Stretch. _(René Schwaiger)_
- Add Fedora 33 Dockerfile for Cirrus and Jenkins CI. _(Mihael Pranjić)_
- <<TODO>>
- <<TODO>>

## Infrastructure

### Cirrus

- Upgrade Cirrus Fedora docker image to Fedora 33. _(Mihael Pranjić)_
- Upgrade to Ruby 3.0 for macOS builds. _(Mihael Pranjić)_
- <<TODO>>

### Jenkins

- We now use Debian sid to build the documentation instead of Debian stretch. The Doxygen version in Debian stretch [contains a bug](https://github.com/doxygen/doxygen/issues/6456) that causes the generation of the PDF documentation to fail. _(René Schwaiger)_
- Use Fedora 33 and 32, drop Fedora 31 use in Jenkins. _(Mihael Pranjić)_
- <<TODO>>
- <<TODO>>

### Travis

- Move macOS GCC 10 build job to Github Actions. _(Mihael Pranjić)_
- <<TODO>>
- <<TODO>>

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

The following GPG Key was used to sign this release: 9C18145C22F9E746D743DEC59ECC0F4CF0359C7B

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
