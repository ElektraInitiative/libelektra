# 0.8.<<VERSION>> Release

This release did not happen yet.

Please update this file within PRs accordingly.
For non-trivial changes, you can choose to be
part of the highlighted changes. Please make
sure to add some short tutorial, asciinema,
or how-to-use for highlighted items.

Please add your name at the end of every contribution.
**Syntax:** *(your name)*


<<`scripts/generate-news-entry`>>

We are proud to release Elektra 0.8.<<VERSION>>.

## What is Elektra?

Elektra serves as a universal and secure framework to access
configuration settings in a global, hierarchical key database.
For more information, visit [https://libelektra.org](https://libelektra.org).

For a small demo see here:

[![asciicast](https://asciinema.org/a/cantr04assr4jkv8v34uz9b8r.png)](https://asciinema.org/a/cantr04assr4jkv8v34uz9b8r)

You can also read the news [on our website](https://www.libelektra.org/news/0.8.<<VERSION>>-release)



## Highlights

- The new High-Level-API has been added. *(Klemens Böswirth)*
- <<HIGHLIGHT2>>
- <<HIGHLIGHT3>>


### High-Level API

The new high-level API provides an easier way to get started with Elektra.

To get started (including proper error handling) you now only need a few self-explanatory lines of code:

```c
ElektraError * error;
Elektra * elektra = elektraOpen ("/sw/org/myapp/#0/current", NULL, &error);
if (elektra == NULL)
{
	printf ("An error occurred: %s", elektraErrorDescription (error));
	elektraErrorReset (&error);
	return -1;
}

int myint = elektraGetLong (elektra, "myint");

elektraClose (elektra);
```

Once you have an instance of `Elektra` you simply call one of the typed `elektraGet*` functions to read a value:

```c
const char * mystring = elektraGetString (elektra, "mystring");
```

No need to specify the base path `/sw/org/myapp/#0/current` anymore, as the high-level API keeps track of that for you.
The API supports the CORBA types already used by some plugins. The high-level API should also be used in combination
with a specification (`spec-mount`). When used this way, the API is designed to be error and crash free while reading values.
Writing values, can of course still produce errors.

Another advantage of the new API is, that it will be much easier to write bindings for other languages now, because only a few simply
types and functions have to be mapped to provide the full functionality.

Take a look at the [README](/src/libs/highlevel/README.md) for more infos.

For examples on how build an application using this API take a look at our [example](/examples/highlevel). *(Klemens Böswirth)*


### <<HIGHLIGHT2>>


### <<HIGHLIGHT2>>


## Plugins

The following section lists news about the [modules](https://www.libelektra.org/plugins/readme) we updated in this release.

### Augeas

- We changed the default [Augeas](http://augeas.net) directory prefix for the lenses directory on macOS to the one used by
  [Homebrew](https://brew.sh): `/usr/local`. *(René Schwaiger)*

### network

- The `network` plugin also supports port declarations to check if a port number is valid
  or if the port is available to use. *(Michael Zronek)*
- We added a [Markdown Shell Recorder][] test to the [ReadMe of the plugin](https://www.libelektra.org/plugins/network). *(René Schwaiger)*

### YAMBi

- The build system does not print a warning about a deprecated directive any more, if you build the plugin with Bison `3.3` or later.
  *(René Schwaiger)*

### path

Enhanced the plugin to also check for concrete file or directory permissions such as `rwx`.
You can specify for example that a user can write to a certain directory or file which prevents applications of runtime failures
once they try to access the given path (such as a log directory or file).
Simply add `check/path/user <user>` and `check/path/mode <modes>` as metadata
and be assured that you can safely set a path value to the key. A more detailed explanation can be found
[here](/src/plugins/path/README.md) *(Michael Zronek)*

### YAwn

- The plugin now handles comments at the end of a file properly. *(René Schwaiger)*
- We improved the syntax error messages of the plugin. *(René Schwaiger)*

### YAy PEG

The new plugin [YAy PEG](https://www.libelektra.org/plugins/yaypeg) parses a subset of YAML using a parser based on
[PEGTL](https://github.com/taocpp/PEGTL). *(René Schwaiger)*

### Ruby

- Added some basic unit tests *(Bernhard Denner)*

### <<Plugin3>>

- <<TODO>>
- <<TODO>>
- <<TODO>>

### Misc

- We fixed some compiler warnings for the plugins

  - [`camel`](https://www.libelektra.org/plugins/camel),
  - [`line`](https://www.libelektra.org/plugins/line),
  - [`mini`](https://www.libelektra.org/plugins/mini) and
  - [`resolver`](https://www.libelektra.org/plugins/resolver)

  reported on FreeBSD. *(René Schwaiger)*
- The [`resolver` plugin](/src/plugins/resolver) and its tests now better support `KDB_DB_SYSTEM` and `KDB_DB_SPEC` paths 
  using `~` to refer to a home directory. *(Klemens Böswirth)*
- If `KDB_DB_SYSTEM` is set to a relative path, it is now treated as relative to `CMAKE_INSTALL_PREFIX`. This ensures that
  `KDB_DB_SYSTEM` actually points to the same location no matter the current working directory. *(Klemens Böswirth)*

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

- All plugins in the KDB now get a handle to a global keyset via `elektraPluginGetGlobalKeySet()`, for communication between plugins.
  See [Global KeySet Handle](/doc/decisions/global_keyset.md) for details. *(Mihael Pranjić)*
- `elektraWriteArrayNumber` now uses `kdb_long_long_t` for array indices to be compatible with the high level API.
  Similarly the value of `ELEKTRA_MAX_ARRAY_SIZE` was changed to match this. *(Klemens Böswirth)*
- <<TODO>>

### Libease

- The function `elektraArrayValidateBaseNameString` now returns the offset to the first digit of the array index, if the given string
  represents an array element containing an index. This update enhances the behavior of the function. Now it not only tells you if a name
  represents a valid array element, but also the start position of the array index.

  ```c
  elektraArrayValidateBaseNameString ("#_10");
  //                                     ~~^ Returns `2` (instead of `1`)

  elektraArrayValidateBaseNameString ("#___1337");
  //                                   ~~~~^ Returns `4` (instead of `1`)
  ```

  If your program already used `elektraArrayValidateBaseNameString` and you check for a valid array element using the equality operator
  (`== 1`), then please use (`>= 1`) instead. For example, if you code that looks like this:

  ```c
  if (elektraArrayValidateBaseNameString(baseName) == 1) …;
  ```

  , please update your code to check for a valid array element name like this:

  ```c
  if (elektraArrayValidateBaseNameString(baseName) >= 1) …;
  ```

  . *(René Schwaiger)*

- <<TODO>>
- <<TODO>>


### Libopts

- This is a new lib containing only the function `elektraGetOpts`. This function can be used to parse command line arguments and
  environment variables and add their values to keys in the proc namespace.

  You can use `opt`, `opt/long` and `env` to specify a short, a long option and an environment variable. For more information take
  a look at [the tutorial](/doc/tutorials/command-line-options.md) and the code documentation. *(Klemens Böswirth)*


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

- `kdb spec-mount` correctly includes type plugin to validate `type`. *(Markus Raab)*
- <<TODO>>
- <<TODO>>
- <<TODO>>


## Scripts

- <<TODO>>
- <<TODO>>
- <<TODO>>


## Documentation

- We fixed various spelling mistakes. *(René Schwaiger)*
- The documentation for `elektraMetaArrayToKS` was fixed. It now reflects the fact
  that the parent key is returned as well.  *(Klemens Böswirth)*
- <<TODO>>

## Tests

- The tests for the IO bindings and notification plugins now use increased timeout values to make sure the test suite fails less often on
  machines with high load. *(René Schwaiger)*
- We update most of the [Markdown Shell Recorder][] tests so they use an explicit namespace (like `system` or `user`). This has the
  advantage that the output of these tests [does not change depending on the user that executes them](https://issues.libelektra.org/1773).
  Before the update these tests used [cascading keys](https://www.libelektra.org/tutorials/namespaces). *(René Schwaiger)*
- The [Shell Recorder][] now also works correctly on FreeBSD. *(René Schwaiger)*
- Fix memcheck target to detect memory problems again and enabled parallel testing to speed it up. *(Mihael Pranjić)*
- Fix memleak in pluginprocess tests. *(Mihael Pranjić)*
- The test [`check-env-dep`](https://master.libelektra.org/scripts/check-env-dep) does not require Bash anymore. *(René Schwaiger)*

[Shell Recorder]: https://master.libelektra.org/tests/shell/shell_recorder
[Markdown Shell Recorder]: https://master.libelektra.org/tests/shell/shell_recorder/tutorial_wrapper

## Build

### CMake

- The CMake find module [`FindAugeas.cmake`](https://master.libelektra.org/cmake/Modules/FindAugeas.cmake) does not print an error
  message anymore, if it is unable to locate Augeas in the `pkg-config` search path. *(René Schwaiger)*
- The CMake find module [`FindLua.cmake`](https://master.libelektra.org/cmake/Modules/FindLua.cmake) does not print an error message
  anymore, if it is unable to locate a Lua executable. *(René Schwaiger)*
- We added code that makes sure you can compile [IO GLIB](https://www.libelektra.org/bindings/io_glib) on macOS, even if `pkg-config`
  erroneously reports that GLIB requires linking to the library `intl` (part of [GNU gettext](https://www.gnu.org/software/gettext)).
  *(René Schwaiger)*
- The plugin name is now provided as compiler definition `ELEKTRA_PLUGIN_NAME` via CMake.
  See [#1042](https://issues.libelektra.org/1042). *(Peter Nirschl)*
- We added a [CMake find module for GLib](https://master.libelektra.org/cmake/Modules/FindGLib.cmake). The module makes sure you can
  compile and link [IO GLib](https://www.libelektra.org/bindings/io_glib) on macOS. *(René Schwaiger)*
- The CMake find module [`FindLibOpenSSL.cmake`](https://master.libelektra.org/cmake/Modules/FindLibOpenSSL.cmake) does not require
  `pkg-config` anymore. The updated code also fixes some linker problems on macOS (and probably other operating systems too), where the
  build system is not able to link to OpenSSL using only the name of the OpenSSL libraries. *(René Schwaiger)*
- We simplified the CMake find module [`FindLibgcrypt.cmake`](https://master.libelektra.org/cmake/Modules/FindLibgcrypt.cmake).The update
  fixes problems on macOS, where the build system excluded the plugin `crypto_gcrypt`, although
  [Libgcrypt](https://gnupg.org/software/libgcrypt) was installed on the system. *(René Schwaiger)*
- We now use the [official CMake find module for `iconv`](https://github.com/Kitware/CMake/blob/master/Modules/FindIconv.cmake). This
  update fixes linker problems with the [`iconv`](http://libelektra.org/plugins/iconv) and
  [`filecheck`](http://libelektra.org/plugins/filecheck) plugin on FreeBSD 12. *(René Schwaiger)*
- The [CMake find module for Botan](https://master.libelektra.org/cmake/Modules/FindLibgcrypt.cmake) does not require `pkg-config` anymore.
  *(René Schwaiger)*
- The [CMake find module for libgit2](https://master.libelektra.org/cmake/Modules/FindLibGit2.cmake) now also exports the version number of
  libgit2. *(René Schwaiger)*
- We added a CMake find module for [libuv](https://libuv.org) and fixed a problem on macOS, where the build system was
  [unable to locate the header file of libuv](https://cirrus-ci.com/task/4852008365326336 ). *(René Schwaiger)*
- We added a CMake find module for [ZeroMQ](http://zeromq.org) to fix build problems on macOS. *(René Schwaiger)*

### Docker

- <<TODO>>
- <<TODO>>
- <<TODO>>


## Infrastructure

### Cirrus

- We now use [Cirrus CI](https://cirrus-ci.com) to [build and test Elektra](http://cirrus-ci.com/github/ElektraInitiative/libelektra) on

  - [FreeBSD 11.2](https://www.freebsd.org/releases/11.2R/announce.html) and
  - [FreeBSD 12.0](https://www.freebsd.org/releases/12.0R/announce.html)

  . *(René Schwaiger)*
- The new build job `🍎 Clang` tests Elektra on macOS. *(René Schwaiger)*
- We added the build job `🍎 Clang ASAN`, which uses Clang with enabled [AddressSanitizer](https://en.wikipedia.org/wiki/AddressSanitizer)
  to test Elektra on macOS. *(René Schwaiger)*
- The new build job `🍎 FULL` compiles and test Elektra using the CMake options `BUILD_SHARED=OFF` an `BUILD_FULL=ON`. *(René Schwaiger)*
- We added `🍎 MMap`, which tests Elektra using [`mmapstorage`](https://www.libelektra.org/plugins/mmapstorage) as default storage module.
  *(René Schwaiger)*

### Jenkins

- We added a badge displaying the current build status to the main [ReadMe](https://master.libelektra.org/README.md). *(René Schwaiger)*
- <<TODO>>
- <<TODO>>

### Travis

- We now test Elektra on [Ubuntu Xenial Xerus](https://docs.travis-ci.com/user/reference/xenial). *(René Schwaiger)*
- We removed the build jobs `🍏 Clang` and `🍏 Check Shell` in favor of the Cirrus build job `🍎 Clang`. *(René Schwaiger)*
- We removed the build jobs `🍏 Clang ASAN` in favor of the Cirrus build job `🍎 Clang ASAN`. *(René Schwaiger)*
- We removed the build jobs `🍏 FULL` in favor of the Cirrus build job `🍎 FULL`. *(René Schwaiger)*
- We removed the build jobs `🍏 MMap` in favor of the Cirrus build job `🍎 MMap`. *(René Schwaiger)*
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

We welcome new contributors!


## Get It!

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
please contact us via the issue tracker [on GitHub](http://issues.libelektra.org).

[Permalink to this NEWS entry](https://www.libelektra.org/news/0.8.<<VERSION>>-release)

For more information, see [https://libelektra.org](https://libelektra.org)

Best regards,
[Elektra Initiative](https://www.libelektra.org/developers/authors)


