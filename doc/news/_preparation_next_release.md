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
This release again brings us a big step towards Elektra 1.0.
It introduces the new soon-to-be-default storage plugin: TOML.

## What is Elektra?

Elektra serves as a universal and secure framework to access
configuration settings in a global, hierarchical key database.
For more information, visit [https://libelektra.org](https://libelektra.org).

You can also read the news [on our website](https://www.libelektra.org/news/0.9.<<VERSION>>-release)

You can try out the latest Elektra release using our docker image [elektra/elektra](https://hub.docker.com/r/elektra/elektra).
This is the quickest way to get started with Elektra without compiling and other obstacles.

Get started with Elektra by running `docker run -it elektra/elektra`.

## TOML

The highlight of this release is the TOML plugin.
The TOML plugin has a similar huge feature set as the INI plugin,
but is written in a much cleaner and more maintainable way using flex and bison.
Furthermore, it follows the popular TOML spec.

It has nearly no run-time dependency, it only needs `base64` if binary values are needed.

A huge thanks to Jakob Fischer for this amazing work!

> Warning: In one of the following `0.9.*` releases, INI will be removed and TOML will become the
> default plugin. If you are using INI, please migrate to TOML now.

## Plugins

The following section lists news about the [modules](https://www.libelektra.org/plugins/readme) we updated in this release.

### xmltool

- Values of KDBStream changed to fit with elektraKeyFlags: recompilation of application is needed. _(Markus Raab)_

### TOML

- Added the TOML plugin, which can read and write TOML files using flex and bison. _(Jakob Fischer)_
- Removed the `null` plugin dependency of the plugin. _(Jakob Fischer)_
- The `type` metakey is now set for numbers on reading. _(Jakob Fischer)_

### Dump

- The `dump` plugin got a major update. The new version can read old files, but only write new files.
  The new files **cannot** be read by the old version of the plugin and will result in a "version error" message. _(Klemens Böswirth)_
- The new version stores keynames relative to the mountpoint, so exported `dump` files can now be imported into a different mountpoint. _(Klemens Böswirth)_
- `dump` no longer write unnecessary zero-bytes into files. This means that as long as all key-values are human-readable, so is the `dump` output.
  This makes `dump` usable for tests and demo purposes, as it is a very simple format closely modelled after a KeySet's structure.
  This also makes it much easier to manually fix broken `dump` files.
  You only need a text editor most of the time. _(Klemens Böswirth)_

## Libraries

## Compatibility

Elektra `0.9.*` does not make any compatibility promises, as we want a clean 1.0.0 release.
In this release, we did the following changes:

- `keyswitch_t` renamed to elektraKeyFlags. _(Markus Raab)_
- `option_t` renamed to elektraLockFlags and elektraLookupFlags. _(Markus Raab)_
- `cursor_t` renamed to elektraCursor. _(Markus Raab)_

### Errors

- Improved error messages. _(Markus Raab)_

## Bindings

Bindings allow you to utilize Elektra using [various programming languages](https://www.libelektra.org/bindings/readme). This section keeps
you up to date with the multi-language support provided by Elektra.

### JNA

- make examples work again _(Markus Raab)_

## Scripts

- Fix googletest framework path in debian configure script. _(Mihael Pranjić)_
- The [fish completion script](../../scripts/completion/kdb.fish) does not fail any more, if you try to complete a simple
  command that expects a namespace. For example, completing

  ```sh
  kdb ls ‸
  ```

  with the tab key (<kbd>⇥</kbd>) (`‸` represents the current cursor position) should work correctly again. _(René Schwaiger)_

- The formatting scripts using prettier now use [`npx`](https://www.npmjs.com/package/npx), which is included in `npm`
  v5.2.0 or newer. This allows us to specify an exact version of prettier to use. _(Klemens Böswirth)_
- The `reformat-shell` script now accepts the alias `shfmt2`, in case `shfmt` v3 is needed for another project. _(Klemens Böswirth)_
- The config for restyled.io now specifies the image to use for each formatter. This clearly reflects the version used for
  formatting. _(Klemens Böswirth)_

## Documentation

- Describe hierarchy and limitations of [hosts plugin](https://www.libelektra.org/plugins/hosts). _(Markus Raab)_
- The Doxygen PDF documentation now also requires the packages
  - `stix` (part of `texlive-fonts-extra`) and
  - `stmaryrd` (part of `texlive-science` or `texlive-math-extra`). _(René Schwaiger)_
- Write down some fundamental decisions, mostly about key names and key set structure. _(Markus Raab in discussions with Klemens Böswirth)_

## Tests

- Fixed the `is_not_rw_storage` function. _(Lukas Kilian)_
- We now ensure that the `check_import` and `check_export` tests run for at least one plugin. _(Lukas Kilian)_

## Build

### CMake

- `make uninstall` also uninstalls symlinks. _(Markus Raab)_
- `external-links.txt` and `extra_install_manifest.txt` are cleaned up at cmake runs. _(Markus Raab)_
- Increased CTest timeout for `testscr_check_kdb_internal_suite` due to timouts reached on slow test machines. _(Mihael Pranjić)_

### Docker

- Added alpine linux docker image with latest elektra installed. This image is published on docker hub as [elektra/elektra](https://hub.docker.com/r/elektra/elektra). We will update the image for each elektra release such that novices can easily test elektra without compiling or installing. _(Mihael Pranjić)_
- Remove unused `libgtest-dev` from docker images. _(Mihael Pranjić)_

## Infrastructure

### Cirrus

- Update FreeBSD images from version 11.3 to 11.4. _(Mihael Pranjić)_
- Increase CPU count for containers to 4. _(Mihael Pranjić)_
- Use ruby 2.7 on macOS. _(Mihael Pranjić)_
- Do not use cask. _(Markus Raab)_

### Jenkins

- Temporarily resolve cyclic dependency between go-elektra and libelektra builds. _(Mihael Pranjić)_

### Travis

- Update macOS builds to use GCC 10, Ruby 2.7.1 and Xcode 12. _(Mihael Pranjić)_
- Increase wait time on builds to fix build timeout errors ("No output has been received in the last 10m0s"). _(Mihael Pranjić)_

## Website

The website is generated from the repository, so all information about
plugins, bindings and tools are always up to date. Furthermore, we changed:

- Renamed [website](/src/tools/website) and removed its backend. _(Markus Raab)_
- Use strict dependency injection for website modules. _(Marvin Mall)_
- Added `package-lock.json` to ensure repeatable builds. _(Marvin Mall)_

## Decisions

We are intensively working on Elektra 1.0.
We decide about the last details of the semantics of Elektra 1.0.
The decisions are [documented](/doc/decisions) but some of them
are still in discussion.

If you are interested in the discussion, please
[subscribe](https://issues.libelektra.org/3520).

## Outlook

We are currently working on following topics:

- Elektrify KDE _(Dardan Haxhimustafa)_, _(Felix Resch)_ and _(Mihael Pranjić)_
- Elektrify GNOME _(Mihael Pranjić)_
- Default TOML plugin _(Jakob Fischer)_
- Improve Plugin Framework _(Vid Leskovar)_
- Keyname Overhaul _(Klemens Böswirth)_
- Continious Releases _(Robert Sowula)_
- FUSE Integration _(Alexander Firbas)_
- Improve 3-way merge _(Dominic Jäger)_
- Shell completion _(Ulrike Schäfer)_
- Improve Elektra developer experience _(Hani Torabi)_
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
