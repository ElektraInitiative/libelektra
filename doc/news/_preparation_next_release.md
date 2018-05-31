# 0.8.<<VERSION>> Release

This release did not happen yet.

Please update this file within every PR:

- For non-trivial changes, you can choose to be
  part of the highlighted changes.
- Please make sure to add some short tutorial, asciinema,
  or how-to-use for highlighted items.
- Please add your name in parentheses and italics
  to every contribution,
  i.e., syntax: "*(<myname>)*".
  Note: No change is irrelevant but similar contributions might
  be summarized shortly before the release.


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

- Type system preview
- <<HIGHLIGHT2>>
- <<HIGHLIGHT3>>


### Type system preview

Elektra supports specifying the semantics of keys via metakeys in the `spec`
namespace. An example is the metakey `check/range` which can be used to specify
that a key only holds numbers in a given range. Another metakey is `check/enum`
which only allows specific keywords to be the content of a key. Up to now these
semantics are being checked at runtime. Therefore a type system was developed to
be able to check configuration specifications statically. As an example, it
would detect when one accidentally adds both a range and an enum check if their
possible contents are not compatible with each other.

The type system is available as a plugin that gets mounted along with a
configuration specification into the spec namespace. Furthermore we include a
set of type definitions for commonly used metakeys such as `check/range`,
`check/long`, `fallback` or `override`.

For more details see the
[typechecker readme](https://github.com/ElektraInitiative/libelektra/tree/master/src/plugins/typechecker/README.md)

Thanks to Armin Wurzinger.

### <<HIGHLIGHT2>>


### <<HIGHLIGHT2>>


## Other New Features

We added even more functionality, which could not make it to the highlights:

- A new I/O binding for [ev](https://www.libelektra.org/bindings/io_ev) has been
  added.
  It can be used to integrate the notification feature with applications based
  on [ev](http://libev.schmorp.de) main loops. *(Thomas Wahringer)*
- The logging plugins ["syslog"](https://www.libelektra.org/plugins/syslog),
  ["journald"](https://www.libelektra.org/plugins/journald) and
  ["logchange"](https://www.libelektra.org/plugins/logchange) now have a new
  option called "get" which can be enabled to log which configuration settings
  are loaded by applications.
  The new option can be used for logging application behavior when using
  [notifications](https://www.libelektra.org/tutorials/notifications). *(Thomas Wahringer)*
- The build system no longer installs haskell dependencies from hackage by itself, instead
  this has to be done beforehand like it is the case with all other dependencies. The main
  reason is that the build servers shouldn't compile the dependencies over and over again,
  only if something changes. See the [readme](https://github.com/ElektraInitiative/libelektra/blob/master/src/bindings/haskell/README.md). *(Armin Wurzinger)*
- <<TODO>>
- <<TODO>>
- The new tool `kdb find` lists keys of the database matching a certain regular expression. *(Markus Raab)*

## New Plugins

- The plugin [hexnumber](https://www.libelektra.org/plugins/hexnumber) has been added. It can be used
  to convert hexadecimal values into decimal when read, and back to hexadecimal when written. *(Klemens Böswirth)*
- <<TODO>>
- <<TODO>>


## Other News

- The `crypto` plugin now uses Elektra's `libinvoke` and the `base64` plugin in order to encode and decode Base64 strings. This improvement reduces code duplication between the two plugins. *(Peter Nirschl)*
- The completion for [fish](http://fishshell.com) now also suggest the `info/` meta attributes of the
  [file plugin](https://www.libelektra.org/plugins/file). *(René Schwaiger)*
- The [`list` plugin](http://libelektra.org/plugins/list) now allows us to pass
  common configuration for all plugins by using keys below the "config/" setting.
  The updated plugin documentation contains more information and an example. *(Thomas Wahringer)*
- <<TODO>>
- <<TODO>>
- <<TODO>>

## Documentation

We improved the documentation in the following ways:

- <<TODO>>
- <<TODO>>
- <<TODO>>
- We added new [Markdown Shell Recorder](https://master.libelektra.org/tests/shell/shell_recorder/tutorial_wrapper) tests for the
  - [`file`](https://www.libelektra.org/plugins/file),
  - [`iconv`](https://www.libelektra.org/plugins/iconv) and
  - [`uname`](https://www.libelektra.org/plugins/uname),
  plugin. *(René Schwaiger)*
- We improved the formatting of our [compilation guide](/doc/COMPILE.md). *(René Schwaiger)*

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
- The Jenkins build server now also compiles and tests Elektra with enabled address sanitizer. *(Lukas Winkler)*
- Travis now uses the latest version of GCC and Clang to translate Elektra on Linux. *(René Schwaiger)*


## Website

The website is generated from the repository, so all information about
plugins, bindings and tools are always up to date. Furthermore, we changed:

- <<TODO>>
- <<TODO>>
- <<TODO>>


## Notes for Elektra's Developers

These notes are of interest for people developing Elektra:

- A build job checks if PRs modify the release notes. *(Markus Raab)*
- <<TODO>>
- <<TODO>>
- Ported GCC ASAN build job to new build system *(René Schwaiger + Lukas Winkler)*
- `withDockerEnv` Jenkinsfile helper now no longer provides stages automatically. *(Lukas Winkler)*
- Docker artifacts are now cleaned up in our daily build job. *(Lukas Winkler)*
- `icheck` build server job has been ported to our new build system. *(Lukas Winkler)*
- The script [`check_formatting.sh`](https://master.libelektra.org/tests/shell/check_formatting.sh) now also checks the formatting of CMake
  code if you installed [`sponge`](https://joeyh.name/code/moreutils) and [`cmake-format`][]. *(René Schwaiger)*
- Our Travis build job now
  - builds all (applicable) bindings by default again, and
  - checks the formatting of CMake code via [`cmake-format`][]

  . *(René Schwaiger)*
- (Markdown) Shell Recorder tests now save test data below `/tests` (see issue [#1887][]). *(René Schwaiger)*
- The Markdown Shell Recorder checks `kdb set` commands to ensure we only add tests that store data below `/tests`. *(René Schwaiger)*
- We disabled the test `testlib_notification` on ASAN enabled builds, since Clang reports that the test leaks memory. *(René Schwaiger)*
- Docker Registry is cleaned up by our daily buildserver task. *(Lukas Winkler)*
- We now import the current version of [Google Test][] as external project at configuration time using
   [DownloadProject](https://github.com/Crascit/DownloadProject). If you want to use a local installation of
   [Google Test][] instead, please set the value of `GTEST_ROOT` to the path of you local copy of the
   [Google Test][] framework. *(René Schwaiger)*

[`cmake-format`]: https://github.com/cheshirekow/cmake_format
[#1887]: https://github.com/ElektraInitiative/libelektra/issues/1887
[Google Test]: https://github.com/google/googletest

## Fixes

Many problems were resolved with the following fixes:

- <<TODO>>
- <<TODO>>
- <<TODO>>
- We fixed a memory leak in the [mINI plugin](https://libelektra.org/plugins/mini) by requiring the plugin
  [`ccode`](https://libelektra.org/plugins/ccode) instead of the “provider” `code`. *(René Schwaiger)*
- The script [`check_bashisms.sh`](https://master.libelektra.org/tests/shell/check_bashisms.sh) should now work correctly again, if the
  system uses the GNU version `find`. *(René Schwaiger)*
- The Markdown Shell Recorder now supports indented code blocks. *(René Schwaiger)*
- We fixed some problems in the [Markdown Shell Recorder](https://master.libelektra.org/tests/shell/shell_recorder/tutorial_wrapper) test
  of [`kdb ls`](https://master.libelektra.org/doc/help/kdb-ls.md). *(René Schwaiger)*
- The script [`reformat-cmake`](https://master.libelektra.org/scripts/reformat-cmake) now checks if `cmake-format` works before it reformats CMake files. Thank you to Klemens Böswirth for the [detailed description of the problem](https://github.com/ElektraInitiative/libelektra/pull/1903#discussion_r189332987). *(René Schwaiger)*
- `scripts/run_icheck` now no longer leaves the base directory of the project
  when checking if the ABI changed. *(Lukas Winkler)*
- You can now build the [Qt-GUI](https://www.libelektra.org/tools/qt-gui) using Qt `5.11`. *(René Schwaiger)*
- We fixed various minor spelling mistakes in the documentation. *(René Schwaiger)*
- The [`list` plugin](http://libelektra.org/plugins/list) which is responsible
  for global mounting had a bug which prevented globally mounted plugins from
  being configurable. *(Thomas Wahringer)*

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
