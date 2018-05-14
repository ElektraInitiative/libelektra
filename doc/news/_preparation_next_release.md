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

<<`scripts/git-release-stats 0.8.<<VERSION>>`>>

## What is Elektra?

Elektra serves as a universal and secure framework to access
configuration settings in a global, hierarchical key database.
For more information, visit [https://libelektra.org](https://libelektra.org).

For a small demo see here:

[![asciicast](https://asciinema.org/a/cantr04assr4jkv8v34uz9b8r.png)](https://asciinema.org/a/cantr04assr4jkv8v34uz9b8r)

You can also read the news [on our website](https://www.libelektra.org/news/0.8.<<VERSION>>-release)



## Highlights

- <<HIGHLIGHT1>>
- <<HIGHLIGHT2>>
- <<HIGHLIGHT3>>


### <<HIGHLIGHT1>>


### <<HIGHLIGHT2>>


### <<HIGHLIGHT2>>


## Other New Features

We added even more functionality, which could not make it to the highlights:

- <<TODO>>
- <<TODO>>
- <<TODO>>


## New Plugins

- <<TODO>>
- <<TODO>>
- <<TODO>>


## Other News

- <<TODO>>
- <<TODO>>
- <<TODO>>


## Documentation

We improved the documentation in the following ways:

- <<TODO>>
- <<TODO>>
- <<TODO>>


## Compatibility

As always, the ABI and API of kdb.h is fully compatible, i.e. programs
compiled against an older 0.8 version of Elektra will continue to work
(ABI) and you will be able to recompile programs without errors (API).

- <<TODO>>
- <<TODO>>
- <<TODO>>


## Notes for Maintainer

These notes are of interest for people maintaining packages of Elektra:

- <<TODO>>
- <<TODO>>
- <<TODO>>


## Infrastructure

- <<TODO>>
- <<TODO>>
- <<TODO>>


## Website

The website is generated from the repository, so all information about
plugins, bindings and tools are always up to date. Furthermore, we changed:

- <<TODO>>
- <<TODO>>
- <<TODO>>


## Notes for Elektra's Developers

These notes are of interest for people developing Elektra:

- <<TODO>>
- <<TODO>>
- <<TODO>>
- The script [`check_formatting.sh`](https://master.libelektra.org/tests/shell/check_formatting.sh) now also checks the formatting of CMake
  code if you installed [`sponge`](https://joeyh.name/code/moreutils) and [`cmake-format`][].
- Our Travis build job now
  - builds all (applicable) bindings by default again, and
  - checks the formatting of CMake code via [`cmake-format`][]

  .

[`cmake-format`]: https://github.com/cheshirekow/cmake_format

## Fixes

Many problems were resolved with the following fixes:

- <<TODO>>
- <<TODO>>
- <<TODO>>


## Outlook

We are currently working on following topics:

- <<TODO>>
- <<TODO>>
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
or Markus Raab by email using elektra@markus-raab.org.

[Permalink to this NEWS entry](https://www.libelektra.org/news/0.8.<<VERSION>>-release)

For more information, see [https://libelektra.org](https://libelektra.org)

Best regards,
[Elektra Initiative](https://www.libelektra.org/developers/authors)


