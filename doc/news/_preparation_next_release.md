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



## Highlights

- New Logo
- INI as new default configuration file format
- Bindings for Asynchronous I/O
- <<HIGHLIGHT2>>
- <<HIGHLIGHT3>>


### New Logo

We are proud to present our new Logo.
It has a new shape and cooler colors.

<img src="https://cdn.rawgit.com/ElektraInitiative/libelektra/master/doc/images/logo/logo_color.svg" alt="Elektra" width="150" />

Thanks to Philipp Frei

### INI as new Default Configuration File Format

As promised in the [previous release notes](https://www.libelektra.org/news/0.8.21-release.html) we switched to INI as default format.
The migration will be smoothly: The `dini` plugin makes sure that old dump files are still being read.
Only when writing out configuration files, configuration files are converted to INI.

TODO: write a bit about INI syntax+short guide


### Bindings for Asynchronous I/O

New bindings for asynchronous I/O called "I/O bindings" have been added.
These bindings allow Elektra's plugins and other parts to perform
asynchronous operations.

I/O bindings are opt-in for application developers.
New features of Elektra that take advantage of I/O bindings will have fallbacks
where viable.
These fallbacks will use synchronous I/O thus keeping the status quo.
For example we plan to add a notification system that facilitates I/O bindings.
Plugins that send notifications will have synchronous fallbacks while receiving
plugins are asynchronous only.

For more details see the preview tutorial
[doc/tutorials/notifications.md](https://github.com/ElektraInitiative/libelektra/tree/master/doc/tutorials/notifications.md)

This release includes an experimental I/O binding for [uv](http://libuv.org/).
The interface for I/O bindings is currently experimental.


### <<HIGHLIGHT2>>


### <<HIGHLIGHT2>>


## Other New Features

We added even more functionality, which could not make it to the highlights:

- <<TODO>>

## Documentation

We improved the documentation in the following ways:

- <<TODO>>

## Compatibility

As always, the ABI and API of kdb.h is fully compatible, i.e. programs
compiled against an older 0.8 version of Elektra will continue to work
(ABI) and you will be able to recompile programs without errors (API).

Futhermore:

- Added public headerfiles `kdbio.h`, `kdbiotest.h`.
- Added private headerfiles `kdbioprivate.h`.

## Notes for Maintainer

These notes are of interest for people maintaining packages of Elektra:

- `dini` is no longer experimental.
- BINDINGS syntax is now similar to PLUGINS.
  By default now all MAINTAINED bindings except EXPERIMENTAL and DEPRECATED are included.
  For more details see
  [/doc/COMPILE.md](https://github.com/ElektraInitiative/libelektra/tree/master/doc/COMPILE.md).
- <<TODO>>

The following files are new:

- Libs: `libelektra-io.so`, `libelektra-io-uv.so`
- <TOPIC>: <FILELIST>

## Notes for Elektra's Developers

These notes are of interest for people developing Elektra:

- <<TODO>>

## Testing

- We added a crash test for the INI plugin that feeds the plugin with problematic input data we determined using [AFL](http://lcamtuf.coredump.cx/afl)

## Fixes

Many problems were resolved with the following fixes:

- We fixed [internal inconsistency](https://github.com/ElektraInitiative/libelektra/pull/1761) in the CMake code of the [Augeas plugin](https://www.libelektra.org/plugins/augeas)
-  We fixed various small bugs that could potentially cause the INI plugin to crash

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
