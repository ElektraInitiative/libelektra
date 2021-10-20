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

### <<Binding2>>

### <<Binding3>>

## Tools

- <<TODO>>
- <<TODO>>
- <<TODO>>

## Scripts

- <<TODO>>
- <<TODO>>
- <<TODO>>

## Documentation

- <<TODO>>
- <<TODO>>
- Start making Elektra [reuse](https://reuse.software) compliant. _(Markus Raab)_
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
