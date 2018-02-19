# 0.8.<<VERSION>> Release

This release did not happen yet.

Please update this file within PRs accordingly.
For non-trivial changes, you can choose to be
part of the highlighted changes. Please make
sure to add some short tutorial, asciinema,
or how-to-use for highlighted items.

Please add your name to every contribution
syntax: ", thanks to <myname>".


<<`scripts/generate-news-entry`>>

We are proud to release Elektra 0.8.<<VERSION>>.

<<`scripts/git-release-stats 0.8.VERSION`>>

## What is Elektra?

Elektra serves as a universal and secure framework to access
configuration settings in a global, hierarchical key database.
For more information, visit [https://libelektra.org](https://libelektra.org).

For a small demo see here:

[![asciicast](https://asciinema.org/a/cantr04assr4jkv8v34uz9b8r.png)](https://asciinema.org/a/cantr04assr4jkv8v34uz9b8r)

You can also read the news [on our website](https://www.libelektra.org/news/0.8.<<VERSION>>-release)

You can also read the [FOSDEM interview](https://fosdem.org/2018/interviews/markus-raab/) given recently.

Elektra is now an official part of [Homebrew](http://formulae.brew.sh/formula/elektra) and
[Alpine Linux](https://pkgs.alpinelinux.org/package/edge/testing/x86_64/elektra).



## Highlights

- New Logo and Website Theme
- INI as new default configuration file format
- Notifications API and Bindings for Asynchronous I/O
- Plugin Processes
- Lookup with the Order Preserving Minimal Perfect Hash Map
- <<HIGHLIGHT6>>


### New Logo and Website Theme

We are proud to present our new logo.
It has a new shape and cooler colors.

<img src="https://cdn.rawgit.com/ElektraInitiative/libelektra/master/doc/images/logo/logo_color.svg" alt="Elektra" width="150" />

Thanks to Philipp Frei

We also gave the website a new look. It has the colors from the logo and new
fonts ([Lato](https://fonts.google.com/specimen/Lato) and
[Libre Franklin](https://fonts.google.com/specimen/Libre+Franklin)) that improve
readability and add to a unique look. The restructured start page contributes to
the new look as well.

We also updated asciinema-player to 2.6.0.

Thanks to Thomas Wahringer.

We also fixed security issues due to an old version of jquery,
thanks to Marvin Mall.

### INI as new Default Configuration File Format

As promised in the [previous release notes](https://www.libelektra.org/news/0.8.21-release.html) we switched to INI as default format.
The migration will be smoothly: The `dini` plugin makes sure that old dump files are still being read.
Only when writing out configuration files, configuration files are converted to INI.

TODO: write a bit about INI syntax+short guide


### Notification API and Bindings for Asynchronous I/O

This release contains an experimental implementation of Elektra's notification
feature.
This feature enables applications to get updates when configuration is
changed at run-time.
For more details see the preview tutorial at
[doc/tutorials/notifications.md](https://github.com/ElektraInitiative/libelektra/tree/master/doc/tutorials/notifications.md)

The
[Notification API](https://doc.libelektra.org/api/current/html/kdbnotification_8h.html)
is implemented by a new library called `elektra-notification`.
To use the library you need the new internalnotification plugin.
Since the plugin is experimental it needs to be enabled when building Elektra
from source (e.g. by passing
`-DPLUGINS="ALL;-EXPERIMENTAL;internalnotification"` to `cmake`).

New bindings for asynchronous I/O called "I/O bindings" also have been added.
These bindings allow Elektra's plugins and other parts to perform
asynchronous operations.
I/O bindings are opt-in for application developers.
New features of Elektra that take advantage of I/O bindings will have fallbacks
where viable.
These fallbacks will use synchronous I/O thus keeping the status quo.

This release includes an experimental I/O binding for [uv](http://libuv.org/).
The interface for I/O bindings is currently experimental and might change in the
future.

Elektra's notification feature is not complete yet.
So called "transport plugins" will be responsible for sending and receiving
notifications using different protocols or libraries (like ZeroMQ, D-Bus or
Redis).
These plugins will make use of the new I/O bindings.
We plan to introduce the first transport plugins with the next release of
Elektra.


### Plugin Processes

A new library called [pluginprocess](https://github.com/ElektraInitiative/libelektra/tree/master/src/libs/pluginprocess)
has been added. This library contains functions that aid in executing plugins in
a separate process. This child process is forked from Elektra's main process
each time such plugin is used and gets closed again afterwards. It uses a simple
communication protocol based on a KeySet that gets serialized through a pipe via
the dump plugin to orchestrate the processes.

Such a behavior is useful for plugins which cause memory leaks to be
isolated in an own process. Furthermore this is useful for runtimes or libraries
that cannot be reinitialized in the same process after they have been used.

### Lookup with the Order Preserving Minimal Perfect Hash Map

The `ksLookup (...)` has a new search algorithm, that acts as an alternative to the
binary search. The Order Preserving Minimal Perfect Hash Map (OPMPHM) is a non-dynamic,
randomized hash map and is very effective for mostly static configurations.
The OPMPHM can be enabled for a search by passing the in [kdbproposal.h](https://github.com/ElektraInitiative/libelektra/blob/master/src/include/kdbproposal.h)
defined option `KDB_O_OPMPHM` to the lookup.
Be aware that if the KeySet changes often using the OPMPHM might not be a good
idea, read more about the [OPMPHM](https://github.com/ElektraInitiative/libelektra/blob/master/doc/dev/data-structures.md#order-preserving-minimal-perfect-hash-map-aka-opmphm).



When you are not sure if the OPMPHM will speed up you searches, wait for the next release,
that one will include a hybrid search algorithm that combines the best properties of
both search algorithms.

### <<HIGHLIGHT6>>


## Other New Features

We added even more functionality, which could not make it to the highlights:

- The Web UI was greatly improved, thanks to Daniel Bugl
- Elektra is now part of the official [Homebrew repository](http://formulae.brew.sh/formula/elektra). We still provide a
  [tap](http://github.com/ElektraInitiative/homebrew-elektra), if you want to install Elektra together with plugins or bindings that require
  additional libraries.
- The building and linking of the haskell bindings and haskell plugins has been
[greatly improved](https://github.com/ElektraInitiative/libelektra/pull/1698).
- The invoke library can now [report errors](https://github.com/ElektraInitiative/libelektra/pull/1801) upon opening/closing a plugin,
  thanks to Armin Wurzinger.
- The [YAML CPP plugin](https://www.libelektra.org/plugins/yamlcpp) does not require [Boost](http://www.boost.org) anymore, if you
  installed [yaml-cpp 0.6](https://github.com/jbeder/yaml-cpp/releases/tag/yaml-cpp-0.6.0).
- Improved colored output in `kdb` tool.

## Documentation

We improved the documentation in the following ways:

- We've [documented how you can setup a build node for Jenkins using a Docker container](https://github.com/ElektraInitiative/libelektra/tree/master/doc/docker/jenkinsnode/README.md)
  We also provide an example Dockerfile based on Debian Stretch for that purpose,
  thanks to Armin Wurzinger.
- Document how `rlwrap` might be used for `kdb shell`
- Fixed docu in `hosts` plugin.
- Greatly improved the license documentation in `debian/copyright`
  in the `debian` branch, thanks to Thomas Wahringer.

## Compatibility

As always, the ABI and API of kdb.h is fully compatible, i.e. programs
compiled against an older 0.8 version of Elektra will continue to work
(ABI) and you will be able to recompile programs without errors (API).

Furthermore:

- Added public headerfiles `kdbnotification.h`, `kdbio.h`, `kdbiotest.h`.
- Added private headerfiles `kdbnotificationplugin.h`, `kdbioprivate.h`.

## Portability

- Fix bash shebang of bash scripts, thanks to Jakub Jirutka
- Remove unportable unneeded asm, thanks to Timo Teräs and Jakub Jirutka
- Fixed syntax in shell recorder, thanks to René Schwaiger

## Notes for Maintainer

These notes are of interest for people maintaining packages of Elektra:

- `dini` is no longer experimental.
- BINDINGS syntax is now similar to PLUGINS.
  By default now all MAINTAINED bindings except EXPERIMENTAL and DEPRECATED are included.
  For more details see
  [/doc/COMPILE.md](https://github.com/ElektraInitiative/libelektra/tree/master/doc/COMPILE.md).
  To include both intercept bindings, you now need to write `INTERCEPT`, to only include getenv
  interception `intercept_env`. `intercept` alone does not work anymore.
- CMake: `BINDINGS` now behaves like `PLUGINS`
- <<TODO>>

The following files are new:

- Libs: `libelektra-notification.so`, `libelektra-io.so`, `libelektra-io-uv.so`, `libelektra-pluginprocess.so`
- Headers: `kdbgetenv.h`, `kdbio.h`, `kdbpluginprocess.h`
- Plugins: `base64/Base64.pdf`
- Binaries: `getenv`, `test_context`, `test_fork`, `test_getenv`, `test_ks_opmphm`, `test_opmphm`
- Other: `elektra-io.pc`

The following files were removed:

- Tests: `testmod_semlock`

## Notes for Elektra's Developers

These notes are of interest for people developing Elektra:

- Build Agent v2 was added to speed up `jenkins build all please`,
  thanks to Armin Wurzinger.
- Travis maintenance (Qt 5 and other problems),
  thanks to René Schwaiger.
- `BINDINGS` was greatly improved and the CMake functions were simplified.
   Bindings now also have a `README.md` with meta data.
   A big thanks to Thomas Wahringer.

## Testing

- We added a crash test for the INI plugin that feeds the plugin with problematic input data we determined using [AFL](http://lcamtuf.coredump.cx/afl)

## Fixes

Many problems were resolved with the following fixes:

- We fixed [internal inconsistency](https://github.com/ElektraInitiative/libelektra/pull/1761) in the CMake code of the [Augeas plugin](https://www.libelektra.org/plugins/augeas)
- We fixed various small bugs that could potentially cause the INI plugin to crash
- The INI plugin now [converts a section to a normal key-value pair](https://github.com/ElektraInitiative/libelektra/issues/1793) if you store a value inside it. This has the advantage that you will not [lose data unexpectedly anymore](https://github.com/ElektraInitiative/libelektra/issues/1697).
- The [INI plugin](https://www.libelektra.org/plugins/ini) should now read most key-value pairs containing delimiter characters (`=`) properly.
- We fixed the [haskell bindings and plugins on Debian Stretch](https://github.com/ElektraInitiative/libelektra/pull/1787)
  and added a [new build server job](https://build.libelektra.org/job/elektra-haskell/) to test that in the future.

## Outlook

We are currently working on following topics:

- <<TODO>>

## Get It!

You can download the release from [here](https://www.libelektra.org/ftp/elektra/releases/elektra-0.8.<<VERSION>>.tar.gz)
or [GitHub](https://github.com/ElektraInitiative/ftp/blob/master/releases/elektra-0.8.<<VERSION>>.tar.gz?raw=true)


The [hashsums are:](https://github.com/ElektraInitiative/ftp/blob/master/releases/elektra-0.8.<<VERSION>>.tar.gz.hashsum?raw=true)

<<`scripts/generate-hashsums`>>

The release tarball is also available signed by me using GnuPG from
[here](https://www.libelektra.org/ftp/elektra/releases/elektra-0.8.<<VERSION>>.tar.gz.gpg) or
[GitHub](https://github.com/ElektraInitiative/ftp/blob/master/releases//elektra-0.8.<<VERSION>>.tar.gz.gpg?raw=true)

Already built API-Docu can be found [online](https://doc.libelektra.org/api/0.8.<<VERSION>>/html/)
or [GitHub](https://github.com/ElektraInitiative/doc/tree/master/api/0.8.<<VERSION>>).


## Stay tuned!

Subscribe to the
[RSS feed](https://www.libelektra.org/news/feed.rss)
to always get the release notifications.

For any questions and comments, please contact the
issue tracker [on GitHub](http://issues.libelektra.org)
or me by email using elektra@markus-raab.org.

[Permalink to this NEWS entry](https://www.libelektra.org/news/0.8.<<VERSION>>-release)

For more information, see [https://libelektra.org](https://libelektra.org)

Best regards,
[Elektra Initiative](https://www.libelektra.org/developers/authors)
