# 0.8.<<VERSION>> Release

This release did not happen yet.
Please update this file within PRs accordingly.
For non-trivial changes, you can choose to be
part of the highlighted changes. Please make
sure to add some short tutorial or how-to-use
for highlighted items.
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

For this release we also have a demo showing Elektra's cryptographic abilities:

![asciicast](https://asciinema.org/a/153014.png)](https://asciinema.org/a/153014)


## Highlights

- Fosdem Talk about Elektra in Main Track
- CC-licensed book about vision of Elektra published
- Maturing of plugins
- Elektra with encryption: 
  ![asciicast](https://asciinema.org/a/153014.png)](https://asciinema.org/a/153014)
- Preparation for switch to INI as default storage

### Fosdem Talk about Elektra in Main Track

We are happy to announce that there will be a talk about
Elektra in the main track of [Fosdem 2018](https://fosdem.org/2018).

See you in Brussels at 3 and 4 February 2018!

Elektra will also be present in [Config Management Camp](http://cfgmgmtcamp.eu/).

### CC-licenced book about vision of Elektra published

I am proud to release a book describing:

- the last 13 years of Elektra (focus on last 4 years),
- the current state, and
- the long-term visions of Elektra.

The Fosdem talk will cover these highlights.

The LaTeX sources are available [here](https://github.com/ElektraInitiative/book)
and the compiled book can be downloaded from [here](https://github.com/ElektraInitiative/book/raw/master/book/book.pdf).

TODO: https://book.libelektra.org

### Maturing of plugins

- The new [Directory Value plugin](https://www.libelektra.org/plugins/directoryvalue) supports storage plugins such as [YAJL](https://www.libelektra.org/plugins/yajl) and [YAML CPP ](https://www.libelektra.org/plugins/yamlcpp). It adds extra leaf values for directories (keys with children) that store the data of their parents. This way plugins that normally are only able to store values in leaf keys are able to support arbitrary key sets.

### Elektra with encryption

The plugin `fcrypt` is now considered stable. It is no longer tagged as `experimental`.

For a short demo see here: [![asciicast](https://asciinema.org/a/153014.png)](https://asciinema.org/a/153014)

### Switch to INI

We plan to switch to INI as default storage instead of the infamous
Elektra's internal dump format.

As preparation work we implemented the `dini` plugin which transparently
converts all `dump` files to `ini` files on any write attempts.

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

## Notes for Maintainer

These notes are of interest for people maintaining packages of Elektra:

- <<TODO>>

## Notes for Elektra's Developers

These notes are of interest for people developing Elektra:

- <<TODO>>

## Fixes

Many problems were resolved with the following fixes:

- <<TODO>>

## Outlook

We are currently working on following topics:

- The Order Preserving Minimal Perfect Hash Map is ready to extend the ksLookup. The implementation of the randomized Las Vegas hash map
  algorithm is in a final stage and the heuristic functions that ensure time and space optimality are backed up by benchmarks.
  The next release will include it!
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

[Permalink to this NEWS entry](https://doc.libelektra.org/news/0.8.<<VERSION>>-release)

For more information, see [https://libelektra.org](https://libelektra.org)

Best regards,
[Elektra Initiative](https://www.libelektra.org/developers/authors)


