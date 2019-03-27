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

### Type (new version)

The `type` plugin was completely rewritten in C. The old version is now called `cpptype`. _(Klemens Böswirth)_

The new `type` plugin also provides the functionality of the `enum` and the `boolean` plugin. These plugins are now considered obsolete and
you should use `type` instead.

A few notes on compatibility:

- the new `type` does not support the full feature set of `enum` and `boolean`, but it supports the features we consider useful.
- the new `type` doesn't support `FSType` and `empty`. These have been deprecated for a long time and there are good alternatives available.
- the new `type` supports `enum`, `wchar` and `wstring` as types, whereas the old `cpptype` would throw an error for these. In most cases
  this won't be a problem, but you should be aware of this breaking change.
- the new `type` does not support `check/type/min` and `check/type/max`, please use the `range` plugin.

To switch from `enum` to the new `type`, you have to add either `check/type=enum` or `type=enum`. Without a `check/type` or `type` metakey,
the `type` plugin will ignore the key. We now also support converting enum values to and from integer values (see
[README](https://www.libelektra.org/plugins/type)).

To switch from `boolean` to the new `type`, you don't have to do anything, if you used the default config. If you used a custom configuration
please take a look at the [README](https://www.libelektra.org/plugins/type).

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
- We fixed an ambiguity in the [YAML grammar](https://master.libelektra.org/src/plugins/yanlr/YAML.g4). _(René Schwaiger)_
- The build system now regenerates the modified parsing code, every time we update the grammar file. _(René Schwaiger)_
- The plugin now reports the location of syntax errors correctly. _(René Schwaiger)_

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
  [dump](https://www.libelektra.org/plugins/dump) format, which is also quicker too read. Contrary to dump, quickdump only stores
  keynames relative to the parent key. This allows easy relocation of configurations. _(Klemens Böswirth)_

### Reference

- Fixed missing Metadata in README and METADATA.ini. _(Michael Zronek)_

### Specload

- The [specload](https://www.libelektra.org/plugins/specload) plugin is a special storage plugin. Instead of using a storage file
  it calls an external application to request its specification. For the transfer it relies on the
  [quickdump](https://www.libelektra.org/plugins/quickdump) plugin. _(Klemens Böswirth)_
- Currently changing the specification is only allowed in a very limited way. However, in future the plugin should allow overriding a
  specification in all cases where this can be done safely. NOTE: While the plugin technically allows some modifications, because of a
  problem with the resolver this cannot be used right now (see [limitations](https://www.libelektra.org/plugins/specload)).
- We also export `elektraSpecloadSendSpec` to abstract over the `quickdump` dependency. _(Klemens Böswirth)_

### Syslog

- We fixed an incorrect format specifier in a call to the `syslog` function. _(René Schwaiger)_

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

- JNA is now not experimental anymore. _(Markus Raab)_
- gsettings is not default anymore. _(Markus Raab)_

- <<TODO>>
- <<TODO>>
- <<TODO>>

### <<Binding2>>

### <<Binding3>>

## Tools

- `kdb get -v` now displays if the resulting value is a default-value defined by the metadata of the key. _(Thomas Bretterbauer)_
- <<TODO>>
- <<TODO>>
- <<TODO>>

## Scripts

- <<TODO>>
- <<TODO>>
- <<TODO>>

## Documentation

- We fixed some spelling mistakes in the documentation. _(René Schwaiger)_
- We added a (very) basic tutorial that tells you [how to write a (well behaved) storage plugin](../tutorials/storage-plugins.md). _(René Schwaiger)_
- The documentation now uses [fenced code blocks](https://help.github.com/en/articles/creating-and-highlighting-code-blocks#syntax-highlighting) to improved the syntax highlighting of code snippets. _(René Schwaiger)_
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
- Improved readability in README. _(Philipp Gackstatter)_
- Add typo fix to the hello-elektra tutorial. _(Dmytro Moiseiuk)_
- Add typo fix to the Java kdb tutorial. _(Dominik Hofmann)_
- We fixed the format specifiers in the [“Hello, Elektra” example](https://master.libelektra.org/examples/helloElektra.c). _(René Schwaiger)_

- Fixed capitalization of the initial letter in Readme. _(Miruna Orsa)_
- Improved the `checkconf` section in the plugin tutorial. _(Peter Nirschl)_
- Added a basic tutorial on [How-To: Write a Java Plugin](../tutorials/java-plugins.md) _(Miruna Orsa)_
- <<TODO>>

## Tests

- We now test the [Directory Value Plugin](https://www.libelektra.org/plugins/directoryvalue) with additional test data. _(René Schwaiger)_
- <<TODO>>
- The [CFramework](https://master.libelektra.org/tests/cframework) now also compares the names of meta keys. _(René Schwaiger)_

### Source Code Checks

- The formatting instructions printed by [`check_formatting`](https://master.libelektra.org/tests/shell/check_formatting.sh) now also work correctly, if

  - the `diff` output does not start with the test number added by CTest, and
  - you use a non-POSIX shell such as [`fish`](https://www.fishshell.com)

  . _(René Schwaiger)_

- We now check the source code of the repository with [LGTM][]. _(René Schwaiger)_
- We fixed various warnings about

  - missing or duplicated include guards,
  - undefined behavior,
  - incorrect format specifiers,
  - unnecessary statements,
  - short names for global variables, and
  - empty `if`-statements

  reported by [LGTM][]. _(René Schwaiger)_

- The `reformat-source` script now also formats `tests/shell/include_common.sh.in`. Additionally it ensures that the file is 1000 lines long,
  so that line numbers of files using it are easier to read. _(Klemens Böswirth)_

[lgtm]: https://lgtm.com

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
