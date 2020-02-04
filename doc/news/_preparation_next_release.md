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

- Add **experimental** preview of Filesystem in User Space [FUSE](../../src/tools/fuse/README.md) tool. This tool enables the inspection and modification of the KDB, in the form of a classical filesystem. _(Alexander Firbas)_

- <<HIGHLIGHT1>>
- <<HIGHLIGHT2>>
- <<HIGHLIGHT3>>

### <<HIGHLIGHT1>>

### <<HIGHLIGHT2>>

### <<HIGHLIGHT2>>

## Plugins

The following section lists news about the [plugins](https://www.libelektra.org/plugins/readme) we updated in this release.

### [email](/src/plugins/email/README.md)

- Introduce email address validation plugin based on regex. _(a-kraschitzer)_

### <<Plugin1>>

- <<TODO>>
- <<TODO>>
- <<TODO>>

### Length

- Implement a plugin that validates that a string length is less or equal to given number. _(Philipp Oppel)_

### Blacklist

- Implement a blacklist plugin that rejects values specified in a metadata array. _(Robert Sowula)_

- <<TODO>>
- <<TODO>>
- <<TODO>>

### TOML

- Fixed a bug ([#3896](https://issues.libelektra.org/3896)) that caused the `toml` plugin to swallow the first letter of
  all keys (after the namespace), if the parent key was a root key (e.g. `user:/`). _(Klemens Böswirth)_
- The `type` metakey is now set for numbers on reading. _(Jakob Fischer)_
- Rewrote some error messages, to make them less technical. _(Jakob Fischer)_
- Fixed parsing of floats/empty keynames/multiline strings. _(Jakob Fischer)_

### Python

- Fix format string overflow and add error checking when appending to `sys.path`. _(Mihael Pranjić)_
- <<TODO>>
- <<TODO>>

## Libraries

The text below summarizes updates to the [C (and C++)-based libraries](https://www.libelektra.org/libraries/readme) of Elektra.

### Compatibility

- <<TODO>>
- <<TODO>>
- <<TODO>>

### Core

- A few rare bugs (mostly related to empty keyname parts `/%/`) in the keyname validation and canonicalization logic
  have been fixed. _(Klemens Böswirth)_
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

### JNA

- Currently the binding's automated clean-up of native keys and key sets is deactivated until JNI segmentation fault issues are resolved in an upcoming release. After these issues have been resolved, manual clean-up of native resources will be discouraged. Therefore the `Key::get*AndRelease` convenience methods introduced with the last release have been removed. _(Michael Tucek)_
- Upgrade Gradle to 7.1.1. _(Mihael Pranjić)_

### <<Binding2>>

### <<Binding3>>

### Gsettings

- Fix keynames used in `gsettings` bindings. _(Mihael Pranjić)_

## Tools

- <<TODO>>
- <<TODO>>
- <<TODO>>

## Scripts

- Add a script that automates the process of inserting source archive hashsums and git statistics into the release notes during a release. _(Robert Sowula)_
- <<TODO>>
- <<TODO>>

## Documentation

- JNI docu updates and small fixes. _(Markus Raab)_
- Small updates in notification tutorial. _(Markus Raab)_
- Add [/doc/tutorial/specification.md]. _(Aaron Abebe <aaron.abebe@gmail.com>)_
- Change GPG keyserver for receiving the apt key from keys.gnupg.net to keyserver.ubuntu.com _(Robert Sowula)_
- <<TODO>>
- The manpages now use the date of the last change recorded in git. _(Klemens Böswirth)_
- <<TODO>>

## Tests

- <<TODO>>
- Upgrade GoogleTest frameworks to version 1.11.0. _(Mihael Pranjić)_
- Add additional test cases for module `key`. _(@lawli3t)_
- Add additional test cases for module `keyname`. _(@lawli3t)_
- Add additional test cases for module `keyvalue`. _(@lawli3t)_
- Add tests for module `keyset`. _(@lawli3t)_
- <<TODO>>
- <<TODO>>

## Packaging

- Add packages for following bindings: `glib`, `io_ev`, `io_glib` and `io_uv`. _(Robert Sowula)_
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

## Infrastructure

### Cirrus

- <<TODO>>
- <<TODO>>
- <<TODO>>

### GitHub Actions

- <<TODO>>
- <<TODO>>
- <<TODO>>

### Jenkins

- Add the deployment of the [website](https://www.libelektra.org) to the release pipeline, therefore removing the need to wait until the main pipeline succeeds after a release. _(Robert Sowula)_
- Restructure the release job stages to make it more failsafe and enable a re-run without any version conflict until the last stage. _(Robert Sowula)_
- Fix invalid package artifact path in release pipeline. _(Robert Sowula)_
- Clean Jenkins workspaces after builds. _(Mihael Pranjić)_

### Travis

- <<TODO>>
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
