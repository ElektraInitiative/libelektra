# 0.8.<<VERSION>> Release

This release did not happen yet.

Please update this file within PRs accordingly.
For non-trivial changes, you can choose to be
part of the highlighted changes. Please make
sure to add some short tutorial (checked by
shell recorder) or asciinema for highlighted items.

Please add your name at the end of every contribution.
**Syntax:** _(your name)_

<<`scripts/generate-news-entry`>>

We are proud to release Elektra 0.8.<<VERSION>>.

## What is Elektra?

Elektra serves as a universal and secure framework to access
configuration settings in a global, hierarchical key database.
For more information, visit [https://libelektra.org](https://libelektra.org).

You can also read the news [on our website](https://www.libelektra.org/news/0.8.<<VERSION>>-release)

## Highlights

- <<HIGHLIGHT1>>
- <<HIGHLIGHT2>>
- <<HIGHLIGHT3>>

### <<HIGHLIGHT1>>

### <<HIGHLIGHT2>>

### <<HIGHLIGHT2>>

## Plugins

The following section lists news about the [modules](https://www.libelektra.org/plugins/readme) we updated in this release.

### YAMBi

- The plugin is now able detect multiple syntax errors in a file. _(René Schwaiger)_
- The error message now includes more information about the location of syntax errors. For example, for the incorrect YAML input `config.yaml`:

  ```yaml
  key 1: - element 1
   - element 2
  key 2: scalar
         - element 3
  ```

  , the plugin prints an error message that includes the following text:

  ```
  config.yaml:2:2: syntax error, unexpected start of sequence, expecting end of map or key
                    - element 2
                    ^
  config.yaml:4:8: syntax error, unexpected start of sequence, expecting end of map or key
                          - element 3
                          ^
  ```

  . _(René Schwaiger)_

### Yan LR

- The build system now disables the plugin, if you installed a version of ANTLR 4 that does not support ANTLR’s C++ runtime (like ANTLR
  `4.5.x` or earlier). _(René Schwaiger)_

### YAwn

- The plugin is now able to print error messages for multiple syntax errors. _(René Schwaiger)_
- We also improved the error messages of YAwn, which now also contain the input that caused a syntax error. For example, for the input

  ```yaml
  key: value
    - element
  ```

  the plugin prints an error message that contains the following text:

  ```
  config.yaml:2:3: Syntax error on input “start of sequence”
                     - element
                     ^
  ```

  . _(René Schwaiger)_

### YAy PEG

- The plugin now includes the input that could not be parsed in error messages. _(René Schwaiger)_
- We improved the error messages for certain errors slightly. For example, the error message for the input

  ```yaml
  "double quoted
  ```

  now includes the following text

  ```
  1:14: Missing closing double quote or incorrect value inside flow scalar
        "double quoted
                      ^
  ```

  . _(René Schwaiger)_

### Quickdump

- [quickdump](https://www.libelektra.org/plugins/quickdump) is a new storage plugin. It implements a more concise form of the
  [dump](https://www.libelektra.org/plugins/dump) format, which is also quicker too read. _(Klemens Böswirth)_

### Specload

- The [specload](https://www.libelektra.org/plugins/specload) pluign is a special storage plugin. Instead of using a storage file
  it calls an external application to request its specification. For the transfer it relies on the
  [quickdump](https://www.libelektra.org/plugins/quickdump) plugin. _(Klemens Böswirth)_
- Currently changing the specification is only allowed in a very limited way. However, in future the plugin should allow overriding a
  specification in all cases where this can be done safely. NOTE: While the plugin technically allows some modifications, because of a
  problem with the resolver this cannot be used right now (see [limitations](https://www.libelektra.org/plugins/specload)).
- We also export `elektraSpecloadSendSpec` to abstract over the `quickdump` dependency. _(Klemens Böswirth)_

## Libraries

The text below summarizes updates to the [C (and C++)-based libraries](https://www.libelektra.org/libraries/readme) of Elektra.

### Compatibility

As always, the ABI and API of kdb.h is fully compatible, i.e. programs
compiled against an older 0.8 version of Elektra will continue to work
(ABI) and you will be able to recompile programs without errors (API).

- <<TODO>>
- <<TODO>>
- <<TODO>>

### Core

- <<TODO>>
- <<TODO>>
- <<TODO>>

### Ease

- The functions for reference resolving used in the [reference plugin](https://www.libelektra.org/plugins/reference) have been extracted
  into libease. This lets other parts of Elektra easily use references and ensures a consistent syntax for them. _(Klemens Böswirth)_

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

- The [Markdown Link Converter](https://master.libelektra.org/doc/markdownlinkconverter) now uses the style

  ```
  filename:line:0
  ```

  instead of

  ```
  filename|line col 0|
  ```

  to show the location data for broken links. This is also the same style that Clang and GCC use when they display location information for
  compiler errors. This update has the advantage, that certain tools such as [TextMate](https://macromates.com) are able to convert the
  location data, providing additional features, such as clickable links to the error source. _(René Schwaiger)_

- We added a badge for [LGTM](https://lgtm.com) to the [main ReadMe file](https://master.libelektra.org/README.md). _(René Schwaiger)_
- Added [LCDproc](../../examples/spec/lcdproc) and [Cassandra](../../examples/spec/cassandra.ini) specification examples. These examples
  provide a good guideline for writing specifications for configurations. _(Michael Zronek)_
- Improved the documentation for the type plugin. _(Michael Zronek)_
- Updated the hello-elektra tutorial. _(Thomas Bretterbauer)_
- Add typo fix to the hello-elektra tutorial. _(Dmytro Moiseiuk)_
- <<TODO>>

## Tests

- We now check the source code of the repository with [LGTM](https://lgtm.com). _(René Schwaiger)_
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

- We disabled the tests:

  - `testmod_crypto_botan`,
  - `testmod_crypto_openssl`,
  - `testmod_dbus`,
  - `testmod_dbusrecv`,
  - `testmod_fcrypt`,
  - `testmod_gpgme`, and
  - `testmod_zeromqsend`

  , since they are [known to fail in high load scenarios](https://issues.libelektra.org/2439). _(René Schwaiger)_

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

Following authors made this release possible:

<<`scripts/git-release-stats 0.8.<<VERSION>>`>>

## Join the Initiative!

We welcome new contributors!
Read [here](https://www.libelektra.org/devgettingstarted/ideas) about how to get started.

As first step, you could give us feedback about these release notes.
Contact us via our [issue tracker](https://issues.libelektra.org).

## Get the Release!

You can download the release from [here](https://www.libelektra.org/ftp/elektra/releases/elektra-0.8.<<VERSION>>.tar.gz)
or [GitHub](https://github.com/ElektraInitiative/ftp/blob/master/releases/elektra-0.8.<<VERSION>>.tar.gz?raw=true)

The [hashsums are:](https://github.com/ElektraInitiative/ftp/blob/master/releases/elektra-0.8.<<VERSION>>.tar.gz.hashsum?raw=true)

<<`scripts/generate-hashsums`>>

The release tarball is also available signed by Markus Raab using GnuPG from
[here](https://www.libelektra.org/ftp/elektra/releases/elektra-0.8.<<VERSION>>.tar.gz.gpg) or on
[GitHub](https://github.com/ElektraInitiative/ftp/blob/master/releases//elektra-0.8.<<VERSION>>.tar.gz.gpg?raw=true)

Already built API-Docu can be found [here](https://doc.libelektra.org/api/0.8.<<VERSION>>/html/)
or on [GitHub](https://github.com/ElektraInitiative/doc/tree/master/api/0.8.<<VERSION>>).

## Stay tuned!

Subscribe to the
[RSS feed](https://www.libelektra.org/news/feed.rss)
to always get the release notifications.

If you also want to participate, or for any questions and comments
please contact us via our issue tracker [on GitHub](http://issues.libelektra.org).

[Permalink to this NEWS entry](https://www.libelektra.org/news/0.8.<<VERSION>>-release)

For more information, see [https://libelektra.org](https://libelektra.org)

Best regards,
[Elektra Initiative](https://www.libelektra.org/developers/authors)
