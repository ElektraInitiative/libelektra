# 0.9.<<VERSION>> Release

This release did not happen yet.

Please update this file within PRs accordingly.
For non-trivial changes, you can choose to be
part of the highlighted changes.

Please add your name at the end of every contribution.
**Syntax:** _(your name)_

Pick a random line to write your changes to minimize
the chances of conflicts in this file.

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

- <<HIGHLIGHT>>
- <<HIGHLIGHT>>
- <<HIGHLIGHT>>

### <<HIGHLIGHT>>

### <<HIGHLIGHT>>

### <<HIGHLIGHT>>

## Plugins

The following section lists news about the [plugins](https://www.libelektra.org/plugins/readme) we updated in this release.

### <<Plugin>>

- <<TODO>>
- <<TODO>>
- <<TODO>>

### <<Plugin>>

- <<TODO>>
- <<TODO>>
- <<TODO>>

### <<Plugin>>

- <<TODO>>
- <<TODO>>
- <<TODO>>

### <<Plugin>>

- <<TODO>>
- <<TODO>>
- <<TODO>>

### <<Plugin>>

- <<TODO>>
- <<TODO>>
- <<TODO>>

### <<Plugin>>

- <<TODO>>
- <<TODO>>
- <<TODO>>

## Libraries

The text below summarizes updates to the [C (and C++)-based libraries](https://www.libelektra.org/libraries/readme) of Elektra.

### Compatibility

- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>

### Core

- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>

### <<Library>>

- <<TODO>>
- <<TODO>>
- <<TODO>>

### <<Library>>

- <<TODO>>
- <<TODO>>
- <<TODO>>

### <<Library>>

- <<TODO>>
- <<TODO>>
- <<TODO>>

## Bindings

Bindings allow you to utilize Elektra using [various programming languages](https://www.libelektra.org/bindings/readme). This section keeps
you up-to-date with the multi-language support provided by Elektra.

### <<Binding>>

- <<TODO>>
- <<TODO>>
- <<TODO>>

### <<Binding>>

- <<TODO>>
- <<TODO>>
- <<TODO>>

### <<Binding>>

- <<TODO>>
- <<TODO>>
- <<TODO>>

## Tools

### <<Tool>>

- <<TODO>>
- <<TODO>>
- <<TODO>>

### <<Tool>>

- <<TODO>>
- <<TODO>>
- <<TODO>>

### <<Tool>>

- <<TODO>>
- <<TODO>>
- <<TODO>>

## Scripts

- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>

## Documentation

- Improve the readability of some sentence you found hard to understand. Retry _(Diana Strauß @DianaStrauss)_
- Integrate missing pages to website _(Ivaylo Ivanov)_
- Improved compilation documentation _(Ivaylo Ivanov)_
- Fix Links in [README.md](/README.md) and small clarifications. _(Markus Raab)_
- Remove previous authors. _(Markus Raab)_
- add pre/postconditions and invariants to module keytest _(@lawli3t)_
- Updated the news template. _(Mihael Pranjić @mpranj)_
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
- Continue 1.0 decisions. _(Markus Raab and @lawli3t)_
- Make version description in key names man page consistent _(@JakobWonisch)_
- Fix typo in elektra-backends man page _(@JakobWonisch)_
- Fix readability in bootstrapping man page _(JakobWonisch)_
- explained in the docker test tutorial how to run the container with podman instead of docker. _(@muskater)_
- Add a new example on how to use keyCopy. _(@muskater)_
- Fix small error in the "Get Started" guide: the build and test command used a wrong directory and would not work if they were copy and pasted. _(@muskater)_
- Added verification to the "Arrays" tutorial _(Ivaylo Ivanov)_
- Remove deprecated `type=int` from `.ini` files _(Ivaylo Ivanov)_
- Added verification to the "Validation" tutorial _(Ivaylo Ivanov)_
- Fix some typos in the "Getting Started" page _(Ivaylo Ivanov)_
- Added debian buster tutorial to python bindings tutorial _(@4ydan)_
- made the debian tutorial a bit more precise and removed sudo command _(@4ydan)_
- Fixed some typos in the "namespaces.md" documentation _(@muskater)_
- Fix an error and some overmatching problems in scripts/sed and fix errors in documentation
  (by running the scripts/dev/fix-spelling script) _(Florian Lindner @flo91)_
- Added some improvements to the core api documentation _(@muskater)_
- Update and improve the CLion tutorial (doc/tutorials/contributing-clion.md), add screenshots _(@flo91)_
- Improve documentation for storage plugins _(@lawli3t)_
- Add list of sources mentioning or linking to Elektra _(@JakobWonisch)_
- Linked to the installation instruction of the webui in its README file. _(@muskater)_ _(@lawli3t)_ _(Aydan Ghazani @4ydan)_
- Linked to the installation instruction of the webui in its README file and added references to Docker in the get-startde-guide. _(@muskater)_ _(@lawli3t)_ _(Aydan Ghazani @4ydan)_
- Linked to the installation instruction of the webui in its README file and added references to Docker in the get-started-guide. _(@muskater)_ _(@lawli3t)_ _(Aydan Ghazani @4ydan)_
- Added screenshots and a quick walk through in the Qt-GUI README. _(@muskater)_ _(@lawli3t)_ _(Aydan Ghazani @4ydan)_
- Improve example for kdb-restore man page and fix typos _(@JakobWonisch)_
- Improve example for kdb-restore man page and fix typos _(@JakobWonisch)_
- Fix some typos in the "Getting Started" page _(Ivaylo Ivanov)_
- Added some improvements to the core api documentation _(@muskater)_
- Add screenshots with hints to CLion PR tutorial _(@JakobWonisch)_
- Fix typo in elektra-backends man page _(@JakobWonisch)_
- Expanded the webside guid for easier understanding and linked to cmake.org. _(Philipp Nirnberger @nirnberger)_
- Expanded the webside guide for easier understanding and linked to cmake.org. _(Philipp Nirnberger @nirnberger)_
- Fix small error in CLion tutorial: CMake options would create a directory named `~` in home directory _(Maximilian Irlinger @atmaxinger)_


- Python: add guide for Debian 11 (bullseye) _(Lukas Hartl @lukashartl)_
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- Minor readability improvement _(@mandoway)_
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- Fix dead link and compile instructions _(Burkhard Hampl @bhampl)_
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>

### Tutorials

- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- Rephrased sentence in code-generator.md to enhance readability _(@Gratla)_
- <<TODO>>

### Man Pages

- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>

## Tests

- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>

### C

- <<TODO>>
- <<TODO>>
- <<TODO>>

### Shell Recorder

- <<TODO>>
- <<TODO>>
- <<TODO>>

### C++

- <<TODO>>
- <<TODO>>
- <<TODO>>

## Packaging

- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>

## Build

### CMake

- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>

### Docker

- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>

## Infrastructure

### Jenkins

- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>

### Cirrus

- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>

### GitHub Actions

- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>

## Website

The website is generated from the repository, so all information about
plugins, bindings and tools are always up-to-date. Furthermore, we changed:

- <<TODO>>
- <<TODO>>
- <<TODO>>
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
