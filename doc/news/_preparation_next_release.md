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
- <<HIGHLIGHT2>>
- <<HIGHLIGHT3>>

### <<HIGHLIGHT1>>

### <<HIGHLIGHT2>>

### <<HIGHLIGHT2>>

## Try out Elektra

You can try out the latest Elektra release using our docker image [elektra/elektra](https://hub.docker.com/r/elektra/elektra). This is the quickest way to get started with Elektra without compiling and other obstacles.

Get started with Elektra by running `docker run -it elektra/elektra`.

## Plugins

The following section lists news about the [modules](https://www.libelektra.org/plugins/readme) we updated in this release.

### xmltool

- values of KDBStream changed to fit with elektraKeyFlags, recompilation of application is needed. _(Markus Raab)_
- <<TODO>>
- <<TODO>>

### TOML

- Added the TOML plugin, which can read and write TOML files using flex and bison. _(Jakob Fischer)_

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

- `keyswitch_t` renamed to elektraKeyFlags. _(Markus Raab)_
- `option_t` renamed to elektraLockFlags and elektraLookupFlags. _(Markus Raab)_
- `cursor_t` renamed to elektraCursor. _(Markus Raab)_
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

### JNA

- make examples work again _(Markus Raab)_

### <<Binding2>>

### <<Binding3>>

## Tools

- <<TODO>>
- <<TODO>>
- <<TODO>>

## Scripts

- Fix googletest framework path in debian configure script. _(Mihael Pranjić)_
- <<TODO>>
- <<TODO>>

## Documentation

- Describe hierarchy and limitations of [hosts plugin](https://www.libelektra.org/plugins/hosts). _(Markus Raab)_
- <<TODO>>
- <<TODO>>
- <<TODO>>

## Tests

- <<TODO>>
- <<TODO>>
- <<TODO>>

## Build

### CMake

- `make uninstall` also uninstalls symlinks. _(Markus Raab)_
- `external-links.txt` and `extra_install_manifest.txt` are cleaned up at cmake runs. _(Markus Raab)_
- <<TODO>>
- <<TODO>>
- <<TODO>>

### Docker

- Added alpine linux docker image with latest elektra installed. This image is published on docker hub as [elektra/elektra](https://hub.docker.com/r/elektra/elektra). We will update the image for each elektra release such that novices can easily test elektra without compiling or installing. _(Mihael Pranjić)_
- Remove unused `libgtest-dev` from docker images. _(Mihael Pranjić)_
- <<TODO>>

## Infrastructure

### Cirrus

- Update FreeBSD images from version 11.3 to 11.4. _(Mihael Pranjić)_
- Increase CPU count for containers to 4. _(Mihael Pranjić)_
- <<TODO>>
- <<TODO>>

### Jenkins

- Temporarily resolve cyclic dependency between go-elektra and libelektra builds. _(Mihael Pranjić)_
- <<TODO>>
- <<TODO>>

### Travis

- Update macOS builds to use gcc 10 and ruby 2.6.6. _(Mihael Pranjić)_
- Update Ruby to version 2.6.6 on macOS. _(Mihael Pranjić)_
- <<TODO>>
- <<TODO>>

## Website

The website is generated from the repository, so all information about
plugins, bindings and tools are always up to date. Furthermore, we changed:

- Renamed [website](/src/tools/website) and removed its backend. _(Markus Raab)_
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
