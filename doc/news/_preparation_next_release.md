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

- TOML plugin
- <<HIGHLIGHT>>
- Cleanup

### TOML

- Added the TOML plugin, which can read and write TOML files using flex and bison. _(Jakob Fischer)_
- The `type` metakey is now set for numbers on reading. _(Jakob Fischer)_

### <<HIGHLIGHT>>

### Cleanup

We removed the `ini` plugin (superseded by the TOML plugin) and the null plugin (superseded by the base64 plugin)

## Plugins

The following section lists news about the [modules](https://www.libelektra.org/plugins/readme) we updated in this release.

### <<Plugin1>>

- <<TODO>>
- <<TODO>>
- <<TODO>>

### <<Plugin2>>

- <<TODO>>
- <<TODO>>
- <<TODO>>

### Dump
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

### <<Binding1>>

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
- <<TODO>>

## Tests

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

## Infrastructure

### Cirrus

- <<TODO>>
- <<TODO>>
- <<TODO>>

### Jenkins

- <<TODO>>
- <<TODO>>
- <<TODO>>

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
