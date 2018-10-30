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

- Added a new, binary and fast storage plugin called [`mmapstorage`](https://libelektra.org/plugins/mmapstorage).
  It leverages the `mmap()` syscall and supports full Elektra semantics.
  We provide two compile variants: `mmapstorage` and `mmapstorage_crc`.
  The `mmapstorage_crc` variant enables CRC32 checksums for critical data,
  while the `mmapstorage` variant omits the checksum for maximum performance.

  We ran a synthetic benchmark with 257 iterations using 40k keys in a keyset,
  and compared the performance to the `dump` storage plugin.

  Median write time in microseconds:

  | Plugin | Time |
  | --- | --- |
  | `dump` | 71079 |
  | `mmapstorage` | 2964 |
  | `mmapstorage_crc` | 7644 |

  Median read time in microseconds:

  | Plugin | Time |
  | --- | --- |
  | `dump` | 82737 |
  | `mmapstorage` | 1145 |
  | `mmapstorage_crc` | 5744 |

  In our benchmark, the `mmapstorage` plugin writes more than 23x faster,
  and reads more than 72x faster than the `dump` storage plugin. *(Mihael Pranjić)*


- Hybrid Search Algorithm for `ksLookup (...)`

- <<HIGHLIGHT2>>
- <<HIGHLIGHT3>>


### Hybrid Search Algorithm for `ksLookup (...)`

The hybrid search algorithm is now implemented, this concludes the extension of the `ksLookup (...)` search with the
[order preserving minimal perfect hash map (OPMPHM)](https://master.libelektra.org/doc/dev/data-structures.md#order-preserving-minimal-perfect-hash-map-aka-opmphm).
The hybrid search combines the best properties of the binary search and the [OPMPHM](https://master.libelektra.org/doc/dev/data-structures.md#order-preserving-minimal-perfect-hash-map-aka-opmphm).
The hybrid search decides dynamically which search algorithm to use, the API user can overrule the hybrid search by passing
`KDB_O_OPMPHM` or `KDB_O_OPMPHM` to the `ksLookup (...)`. The constants are defined in [kdbproposal.h](https://master.libelektra.org/src/include/kdbproposal.h).*(Kurt Micheli)*

#### Results

The implemented randomized [OPMPHM](https://master.libelektra.org/doc/dev/data-structures.md#order-preserving-minimal-perfect-hash-map-aka-opmphm)
algorithm is in 99.5% of the measured random cases optimal. However the randomization property of the algorithm leaves an uncertainty.

The results made with random cases had shown that the hybrid search is except for small keyset sizes almost always faster
compared to the standalone binary search. The performance increase strongly depended on the measured hardware. In the random cases
where the hybrid search is faster, on average ~8.53% to ~20.92% of time was saved.
The implemented hybrid search works only above a keyset size of `599` to exclude the small keyset sizes.

### <<HIGHLIGHT2>>


### <<HIGHLIGHT2>>


## Plugins

The following section lists news about the [modules](https://www.libelektra.org/plugins/readme) we updated in this release.

### Process

-  There is also a new plugin called [process](https://libelektra.org/plugins/process).
   This plugin utilizes the pluginprocess library in order to execute arbitrary other
   plugins in an own process, acting as a proxy itself. Therefore it is not required
   to explicitly change a plugin's implementation if it shall be executed in an own
   process. This plugin is not completely finished yet, as currently there is no way
   for it to mimic the proxied plugin's contract in Elektra. It can be used with simple
   plugins like `dump` however, check the limitations in the readme for more details. *(Armin Wurzinger)*

### Directory Value

We improved the performance of the plugin a little bit. *(René Schwaiger)*

### FSTab

The detection of the `mntent` functions now also works correctly, if you use the compiler switch `-Werror`. *(René Schwaiger)*

### gpgme

- The `gpgme` plugin was brought into existence to provide cryptographic functions using GnuGP via the `libgpgme` library. See [#896] *(Peter Nirschl)*

### network

The `network` plugin now also allows for non-numerical hosts (i.e. "localhost") to be set and tries to
resolve it via DNS. *(Michael Zronek)*

### YAMBi

This new plugin parses a subset of YAML using a parser generated by [Bison](https://www.gnu.org/software/bison). *(René Schwaiger)*

### YAML CPP

The build system now disables the plugin automatically, if you use a GCC compiler (`6.x` or earlier) and enable the option `ENABLE_ASAN`.
We updated the behavior, since otherwise the plugin will report memory leaks at runtime. *(René Schwaiger)*

### Yan LR

- The plugin does not modify the (original) parent key any more. As a consequence, setting values at the root of a mountpoint:

  ```sh
  sudo kdb mount config.yaml user/tests/yambi yambi
  kdb set user/tests/yanlr 'Mount Point Value'
  kdb get user/tests/yanlr
  #> Mount Point Value
  ```

  now works correctly. *(René Schwaiger)*
- We now use C++ code to test the plugin. *(René Schwaiger)*
- The CMake code of the plugin now also recognizes `antlr` as ANTLR executable, if `antlr4` is not available. *(René Schwaiger)*
- The build system now disables the unit test for the plugin, if you use GCC (`6.x` or earlier) to translate Elektra. We introduced this
  behavior, since the code generated by ANTLR (`YAML.h`) seems to contain a double free that causes a segmentation fault on systems that
  use the GNU C library. *(René Schwaiger)*
- The build system now disables the plugin automatically, if you use a GCC compiler (`6.x` or earlier) and enable the option `ENABLE_ASAN`.
  *(René Schwaiger)*

### YAwn

This new plugin parses a subset of YAML using the Earley Parser library [YAEP](https://github.com/vnmakarov/yaep). *(René Schwaiger)*

### <<Plugin3>>

- <<TODO>>
- <<TODO>>
- <<TODO>>


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

### <<Library1>>

- <<TODO>>
- <<TODO>>
- <<TODO>>

### passwd

- We fixed an issue with the passwd plugin not properly setting compile flags.
    This resolves a problem with undefined functions when building with musl.
    *(Lukas Winkler)*

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

- Added benchmarks for storage plugins. The currently benchmarked plugins are `dump` and `mmapstorage`.  *(Mihael Pranjić)*
- Avoid raw pointers in KDB tools. *(Markus Raab)*
- Improved error text of KDB tool `cp`. *(Markus Raab)*
- Document hidden feature of KDB tool `mount`. *(Markus Raab)*
- <<TODO>>
- <<TODO>>


## Scripts

- We now require [`clang-format`](https://clang.llvm.org/docs/ClangFormat.html) 6.0 for formatting C and C++ code. *(René Schwaiger)*
- The command [`reformat-source`](https://master.libelektra.org/scripts/reformat-source) now displays information about the installed
  version of `clang-format`, if it is unable to locate a supported version of the tool. *(René Schwaiger)*
- We now also check the POSIX compatibility of our scripts with [`shfmt`][]. *(René Schwaiger)*
- The new command [`reformat-shfmt`][] reformats Shell scripts using the tool [`shfmt`](https://github.com/mvdan/sh). *(René Schwaiger)*

[`shfmt`]: https://github.com/mvdan/sh

## Documentation

- We fixed some minor spelling mistakes in the documentation. *(René Schwaiger)*
- The ReadMe now includes two badges that show the latest released version of Elektra and the status of the Travis build. *(René Schwaiger)*
- Fixed documenation error on Ruby plugin Readme. *(Bernhard Denner)*
- <<TODO>>
- Go into more detail in
    [BUILDSERVER.md](https://master.libelektra.org/doc/BUILDSERVER.md).
    *(Lukas Winkler)*


## Tests

- Fix potential parallel execution of maven tests, which write to KDB. *(Markus Raab)*
- The unit test for the [`dbus` plugin](https://www.libelektra.org/plugins/dbus) does not leak memory anymore, if it fails on macOS.
  *(Thomas Wahringer)*
- The tests `testkdb_allplugins` and `testscr_check_kdb_internal_check` do not test a plugin on an ASAN enabled build anymore, if you
  specify the status tag `memleak` in the plugin contract. *(René Schwaiger)*
- The [CFramework](https://master.libelektra.org/doc/TESTING.md) macro `compare_key` now also checks if the meta values of keys are equal.
  *(René Schwaiger)*
- The test `testscr_check_bashisms` does not print warnings about skipped files anymore. *(René Schwaiger)*
-  We added a [Markdown Shell Recorder][] test for the [`shell` plugin ](https://www.libelektra.org/plugins/shell). *(René Schwaiger)*
- Added many storage plugin tests. Most tests use the keyset, key name and value APIs.
  Currently, the tests are only active for `dump` and `mmapstorage`. *(Mihael Pranjić)*
- The test `testcpp_contextual_basic` now compiles without warnings, if we use Clang 7 as compiler. *(René Schwaiger)*
- crypto, fcrypt and gpgme properly shut down the gpg-agent after the unit test is done. See #1973 . *(Peter Nirschl)*
- minor refactoring of the unit tests for crypto, fcrypt, gpgme: moved shared code to separate module in order to avoid code duplication. *(Peter Nirschl)*
- The CMake targets for plugin tests (`testmod_[plugin]`) now depend on the respective CMake targets for the plugins themselves
  (`elektra-[plugin]`). *(Klemens Böswirth)*
- Fixed bug in CMake plugin tests, if only `BUILD_FULL` but not `BUILD_SHARED` is used. *(Klemens Böswirth)*
- The test [`testscr_check_formatting`](https://master.libelektra.org/tests/shell/check_formatting.sh) now also checks the formatting of
  Shell code. *(René Schwaiger)*
- We pumped version numbers in XML-test files. *(Markus Raab)*
- We fixed a crash in the unit test of the [JNA](https://www.libelektra.org/bindings/jna) binding. *(René Schwaiger)*

[Markdown Shell Recorder]: https://master.libelektra.org/tests/shell/shell_recorder/tutorial_wrapper

## Build

### CMake

- We improved the detection of Python 2 and Python 3 in the CMake code of the Python bindings/plugins. *(René Schwaiger)*
- We restructured the code of the [CMake module](https://master.libelektra.org/cmake/Modules/FindHaskell.cmake) we use to detect Haskell
  tools . *(René Schwaiger)*
- The CMake configuration step now displays less debug messages about found libraries. *(René Schwaiger)*
- Building the Haskell binding should now work again. *(René Schwaiger)*
- Provide a wrapper around `check_symbol_exists` that handles issues with
  `-Werror -Wpedantic`. *(Lukas Winkler)*
- The argument `INCLUDE_SYSTEM_DIRECTORIES` of the function `add_plugin` now supports multiple include directories. *(René Schwaiger)*
- We reformatted all CMake source files with cmake-format 0.4.3. *(René Schwaiger)*
- Generating coverage data (`ENABLE_COVERAGE=ON`) should now also work on macOS. *(René Schwaiger)*

### Docker

- The Docker image for Debian stretch now contains all (optional) dependencies for Elektra. *(René Schwaiger)*
- The docker images used by our build system are now available to download from
    our systems without authentication.
    Try it out and list available images via
        `docker run --rm anoxis/registry-cli -r https://hub-public.libelektra.org` .
    Afterwards pull your desired image as you would do from any public registry:
        `docker pull hub-public.libelektra.org/build-elektra-alpine:201809-791f9f388cbdff0db544e02277c882ad6e8220fe280cda67e6ea6358767a065e`.
    *(Lukas Winkler)*
- <<TODO>>

### Vagrant

- Added Vagrantfile for Ubuntu artful 32-bit. *(Mihael Pranjić)*

## Infrastructure

### Jenkins

- <<TODO>>
- <<TODO>>
- The build jobs now print less non-relevant output. *(René Schwaiger)*
- Enable `-Werror` in `debian-stable-full`. *(Lukas Winkler)
- We added the compiler switch `-Werror` to the build jobs:
  - `alpine`,
  - `debian-stable-full-i386`,
  - `debian-stable-full-mmap-asan`,
  - `debian-stable-full-mmap`,
  - `debian-stable-full-optimizations-off`,
  - `debian-stable-full-xdg`,
  - `debian-stable-minimal`,
  - `debian-stable-multiconf`,
  - `debian-unstable-clang-asan`,
  - `debian-unstable-full-clang`,
  - `debian-unstable-full`,
  - `ubuntu-xenial`, and
  - `debian-stable-asan`. *(René Schwaiger)*
- Build Artifacts for all PR's to detect issues before merging *(Lukas Winkler)*
- Stricter removal of temporary docker images on docker nodes *(Lukas Winkler)*
- Added jenkins build jobs `debian-stable-full-mmap` and `debian-stable-full-mmap-asan`
  with `mmapstorage` as the default storage. *(Mihael Pranjić)*
- We added basic support for coverage analysis via [Coveralls](http://coveralls.io). *(René Schwaiger)*


### Travis

- Travis now also checks the code for memory leaks in the build job `🍏 Clang ASAN`. *(René Schwaiger)*
- The Travis build jobs `🍏 Clang ASAN` and `🐧 GCC ASAN` now only translates a minimal set of plugins, since we had various timeout
  problems with these jobs before. We explicitly excluded plugins, to make sure that the build jobs still test newly added plugins.
  *(René Schwaiger)*
- Added travis build job `🍏 mmap` on macOS with `mmapstorage` as the default storage. *(Mihael Pranjić)*
- Travis now prints the CMake configuration for each build job. *(René Schwaiger)*
- We now test Elektra using the latest version of Xcode (`10.0`). *(René Schwaiger)*
- We added the build job `🍏 Check Source`, which only runs source code checks such as `testscr_check_oclint`. This update allows us to
  remove the source code checks from the jobs `🍏 MMap` and `🍏 Clang`, which sometimes hit the
  [timeout limit for public repositories](https://docs.travis-ci.com/user/customizing-the-build#build-timeouts) before. *(René Schwaiger)*
- All Travis build jobs now use the compiler switch `-Werror`. *(René Schwaiger)*
- The new job `🍏 FULL` and the build job `🐧 FULL` build Elektra using the CMake options `BUILD_FULL=ON` and `BUILD_SHARED=OFF`.
  *(René Schwaiger)*

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

## Finished Thesis

- Daniel Bugl finished his [thesis](https://www.libelektra.org/ftp/elektra/publications/bugl2018web.pdf)
- Thomas Wahringer finished his [thesis](https://www.libelektra.org/ftp/elektra/publications/wahringer2018notification.pdf)


## Get It!

You can download the release from [here](https://www.libelektra.org/ftp/elektra/releases/elektra-0.8.<<VERSION>>.tar.gz)
or [GitHub](https://github.com/ElektraInitiative/ftp/blob/master/releases/elektra-0.8.<<VERSION>>.tar.gz?raw=true)


The [hashsums are:](https://github.com/ElektraInitiative/ftp/blob/master/releases/elektra-0.8.<<VERSION>>.tar.gz.hashsum?raw=true)

<<`scripts/generate-hashsums`>>

The release tarball is also available signed by Markus Raab using GnuPG from
[here](https://www.libelektra.org/ftp/elektra/releases/elektra-0.8.<<VERSION>>.tar.gz.gpg) or
[GitHub](https://github.com/ElektraInitiative/ftp/blob/master/releases//elektra-0.8.<<VERSION>>.tar.gz.gpg?raw=true)

Already built API-Docu can be found [here](https://doc.libelektra.org/api/0.8.<<VERSION>>/html/)
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


