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
- Elektra Web 1.5
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
[typechecker readme](https://www.libelektra.org/plugins/typechecker)

Thanks to Armin Wurzinger.

### Elektra Web 1.5

The new release of Elektra Web features many UX improvements from the usability test!

Try it out now on: http://webui.libelektra.org:33334/

- search completely reworked - it does not act as a filter on already opened keys anymore, and instead searches the whole key database - feedback from the search was also greatly improved (pulsating while searching, glowing blue when done)
- added "abort" buttons to dialogs to revert actions
- added "create array" button to easily create arrays
- removed confirmation dialog before deletion (undo can be used instead)
- created a docker image: `elektra/web`
- small fixes:
  - updated visibility levels
  - removed "done" button in main view
  - fixed issues with the opener click area
  - remove metakeys when they are set to the default value or empty/0
  - improved keyboard support
  - fixed many small issues (#2037)

### <<HIGHLIGHT2>>


## Plugins

### Crypto

- The `crypto` plugin now uses Elektra's `libinvoke` and the `base64` plugin in order to encode and decode Base64 strings. This improvement reduces code duplication between the two plugins. *(Peter Nirschl)*

### HexNumber

- The plugin [hexnumber](https://www.libelektra.org/plugins/hexnumber) has been added. It can be used
  to convert hexadecimal values into decimal when read, and back to hexadecimal when written. *(Klemens Böswirth)*

### List

- The [`list` plugin](http://libelektra.org/plugins/list) now allows us to pass
  common configuration for all plugins by using keys below the "config/" setting.
  The updated plugin documentation contains more information and an example. *(Thomas Wahringer)*
- The [`list` plugin](http://libelektra.org/plugins/list) which is responsible
  for global mounting had a bug which prevented globally mounted plugins from
  being configurable. *(Thomas Wahringer)*

### mINI

- We fixed a memory leak in the [mINI plugin](https://libelektra.org/plugins/mini) by requiring the plugin
  [`ccode`](https://libelektra.org/plugins/ccode) instead of the “provider” `code`. *(René Schwaiger)*

### Regex Dispatcher

- The plugin [regexdispatcher](https://www.libelektra.org/plugins/regexdispatcher) has been added. It calculates regex representations for
  commonly used specification keywords to be used with the [typechecker](https://www.libelektra.org/plugins/typechecker). Currently the
  keywords `check/range`, `check/enum` and `check/validation` are supported. *(Armin Wurzinger)*

### Misc

- The logging plugins ["syslog"](https://www.libelektra.org/plugins/syslog),
  ["journald"](https://www.libelektra.org/plugins/journald) and
  ["logchange"](https://www.libelektra.org/plugins/logchange) now have a new
  option called "get" which can be enabled to log which configuration settings
  are loaded by applications.
  The new option can be used for logging application behavior when using
  [notifications](https://www.libelektra.org/tutorials/notifications). *(Thomas Wahringer)*
- An issue when building Haskell plugins with a cached sandbox is fixed in case 
  a Haskell library bundled with elektra gets changed. *(Armin Wurzinger)*
- <<TODO>>

## Bindings

- A new I/O binding for [ev](https://www.libelektra.org/bindings/io_ev) has been
  added.
  It can be used to integrate the notification feature with applications based
  on [ev](http://libev.schmorp.de) main loops. *(Thomas Wahringer)*

## Tools

- The new tool `kdb find` lists keys of the database matching a certain regular expression. *(Markus Raab)*
- You can now build the [Qt-GUI](https://www.libelektra.org/tools/qt-gui) using Qt `5.11`. *(René Schwaiger)*

## Scripts

- The script [`check_formatting.sh`](https://master.libelektra.org/tests/shell/check_formatting.sh) now also checks the formatting of CMake
  code if you installed [`sponge`](https://joeyh.name/code/moreutils) and [`cmake-format`][]. *(René Schwaiger)*
- The script [`check_bashisms.sh`](https://master.libelektra.org/tests/shell/check_bashisms.sh) should now work correctly again, if the
  system uses the GNU version `find`. *(René Schwaiger)*
- The script [`reformat-cmake`](https://master.libelektra.org/scripts/reformat-cmake) now checks if `cmake-format` works before it reformats CMake files. Thank you to Klemens Böswirth for the [detailed description of the problem](https://github.com/ElektraInitiative/libelektra/pull/1903#discussion_r189332987). *(René Schwaiger)*
- `scripts/run_icheck` now no longer leaves the base directory of the project
  when checking if the ABI changed. *(Lukas Winkler)*
- The completion for [fish](http://fishshell.com) now also suggest the `info/` meta attributes of the
  [file plugin](https://www.libelektra.org/plugins/file). *(René Schwaiger)*

[`cmake-format`]: https://github.com/cheshirekow/cmake_format

## Documentation

- We improved the formatting of our [compilation guide](/doc/COMPILE.md). *(René Schwaiger)*
- We fixed various minor spelling mistakes in the documentation. *(René Schwaiger)*
- The man pages for [`kdb change-resolver-symlink`](https://www.libelektra.org/manpages/kdb-change-resolver-symlink) and
   [`kdb change-storage-symlink`](https://www.libelektra.org/manpages/kdb-change-storage-symlink) referenced the wrong command.
   *(Lukas Winkler, René Schwaiger)*

## Tests

- We added new [Markdown Shell Recorder](https://master.libelektra.org/tests/shell/shell_recorder/tutorial_wrapper) tests for the
  - [`file`](https://www.libelektra.org/plugins/file),
  - [`iconv`](https://www.libelektra.org/plugins/iconv),
  - [`ni`](https://www.libelektra.org/plugins/ni), and
  - [`uname`](https://www.libelektra.org/plugins/uname)
  plugin. *(René Schwaiger)*
- (Markdown) Shell Recorder tests now save test data below `/tests` (see issue [#1887][]). *(René Schwaiger)*
- The Markdown Shell Recorder checks `kdb set` commands to ensure we only add tests that store data below `/tests`. *(René Schwaiger)*
- The Markdown Shell Recorder now supports indented code blocks. *(René Schwaiger)*
- The Markdown Shell Recorder now also tests if a command prints nothing to `stdout` if you add the check `#>`. *(René Schwaiger)*
- We fixed some problems in the [Markdown Shell Recorder](https://master.libelektra.org/tests/shell/shell_recorder/tutorial_wrapper) test
  of [`kdb ls`](https://master.libelektra.org/doc/help/kdb-ls.md). *(René Schwaiger)*
- The documentation for `kdb` and `kdb set` now mention the `--` option to stop option processing. This is useful for setting negative values among other things. *(Klemens Böswirth)*
- Plugins added with the flag `SHARED_ONLY` no longer get tested in the script `check_kdb_internal_check.sh` if executed with kdb-full or kdb-static. *(Armin Wurzinger)*

## Compatibility

[#1887]: https://github.com/ElektraInitiative/libelektra/issues/1887

## Build

### CMake

- The build system no longer installs Haskell dependencies from hackage by itself, instead
  this has to be done beforehand like it is the case with all other dependencies. The main
  reason is that the build servers shouldn't compile the dependencies over and over again,
  only if something changes. See the [readme](https://www.libelektra.org/bindings/haskell). *(Armin Wurzinger)*
- Plugins can be specified to be only built for `BUILD_SHARED` builds, but to be excluded 
  from any `BUILD_FULL` or `BUILD_STATIC` builds using the new optional argument `ONLY_SHARED`
  for our cmake macro `add_plugin`. This way `BUILD_SHARED` can be combined with the other 
  options without excluding such plugins. The cmake messages about plugin inclusion have 
  been updated to indicate this behavior. This behavior has been applied for the Haskell 
  plugins- and bindings as they currently don't support full or static builds. *(Armin Wurzinger)*
- We now import the current version of [Google Test][] as external project at configuration time using
   [DownloadProject](https://github.com/Crascit/DownloadProject). If you want to use a local installation of
   [Google Test][] instead, please set the value of `GTEST_ROOT` to the path of you local copy of the
   [Google Test][] framework. *(René Schwaiger)*
- The cmake variable `GTEST_ROOT` now respects the environment variable
  `GTEST_ROOT` if it is set. *(Lukas Winkler)*
- We disabled the test `testlib_notification` on ASAN enabled builds, since Clang reports that the test leaks memory. *(René Schwaiger)*
- Disable Markdown Shell Recorder test `validation.md` for ASAN builds.
  It leaks memory and thus fails the test during spec mount. *(Lukas Winkler)*
- Haskell plugins and bindings are now correctly excluded when using BUILD_FULL or BUILD_STATIC
  as this is currently unsupported. Another issue when building Haskell plugins with a cached sandbox
  is fixed as well. *(Armin Wurzinger)*

[Google Test]: https://github.com/google/googletest

### Docker

- `clang-5.0` is now used for clang tests by the build system *(Lukas Winkler)*
- An additional build job on Ubuntu:xenial has been added *(Lukas Winkler)*
- `withDockerEnv` Jenkinsfile helper now no longer provides stages automatically. *(Lukas Winkler)*
- [Google Test][] is installed in Docker images used by the build system. *(Lukas Winkler)*

## Infrastructure

### Jenkins

- A build job checks if PRs modify the release notes. *(Markus Raab)*
- Several improvements to the build system have been implemented *(Lukas Winkler)*:
  - Better Docker image handling.
  - Abort of previously queued but unfinished runs on new commits.
  - Document how to locally replicate the Docker environment used for tests.
- The Jenkins build server now also compiles and tests Elektra with enabled address sanitizer. *(Lukas Winkler)*
- Ported GCC ASAN build job to new build system *(René Schwaiger + Lukas Winkler)*
- Docker artifacts are now cleaned up in our daily build job. *(Lukas Winkler)*
- `clang` tests have been ported to the new build system *(Lukas Winkler et al)*
- `icheck` build server job has been ported to our new build system. *(Lukas Winkler)*
- Port `elektra-gcc-configure-debian-optimizations` to new build system. *(Lukas Winkler)*
- Port `elektra-gcc-configure-mingw-w64` to new build system. *(Lukas Winkler)*
- Port `debian-multiconfig-gcc-stable` to new build system. *(Lukas Winkler)*
- Port `elektra-ini-mergerequests` to new build system. *(Lukas Winkler)*
- Port `elektra-gcc-configure-debian-nokdbtest` to new build system. *(Lukas Winkler)*
- Port `elektra-gcc-configure-xdg`to new build system. *(Lukas Winkler)*
- Port `elektra-gcc-i386` to new build system. *(Lukas Winkler)*
- Docker Registry is cleaned up by our daily buildserver task. *(Lukas Winkler)*


### Travis

- Travis now uses the latest version of GCC and Clang to translate Elektra on Linux. *(René Schwaiger)*
- Our Travis build job now
  - builds all (applicable) bindings by default again, and
  - checks the formatting of CMake code via [`cmake-format`][]
  . *(René Schwaiger)*
- Some cache issues on the Travis build job for cached haskell sandboxes have been resolved. *(Armin Wurzinger)*

## Compatibility

As always, the ABI and API of kdb.h is fully compatible, i.e. programs
compiled against an older 0.8 version of Elektra will continue to work
(ABI) and you will be able to recompile programs without errors (API).

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
