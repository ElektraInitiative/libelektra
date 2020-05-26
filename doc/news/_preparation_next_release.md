# 0.9.2 Release

Not yet released.

- shortDesc: KDE and GNOME Integration,`elektrad` in Go

We are proud to release Elektra 0.9.2.

This release focuses on bugfixes and stability improvements, but also brings some new features. With the 0.9.x series of releases we shift our focus to KDE and GNOME integration.

## What is Elektra?

Elektra serves as a universal and secure framework to access
configuration settings in a global, hierarchical key database.
For more information, visit [https://libelektra.org](https://libelektra.org).

You can also read the news [on our website](https://www.libelektra.org/news/0.9.<<VERSION>>-release)

## Highlights

- KDE and GNOME Integration
- `elektrad` rewritten in Go

### KDE Integration

We created a [fork](https://github.com/ElektraInitiative/kconfig) of [KDE's](https://kde.org/) `KConfig` configuration system and patched it to use libelektra. We have done some initial testing and replaced the `KConfig` library for [Kate](https://kate-editor.org/) and [KDevelop](https://www.kdevelop.org/) successfully. Additionally, a new Elektra plugin called `kconfig` was added, which can read and write KDE's kconfig ini files to help migrate to Elektra's `KConfig` fork. _(Dardan Haxhimustafa)_ and _(Felix Resch)_

### GNOME Integration

We continued work on Elektra's bindings for [GNOME GSettings](https://developer.gnome.org/gio/stable/GSettings.html). Our implementation should be able to replace the widely used [dconf](https://wiki.gnome.org/Projects/dconf) backend. Elektra's `gsettings` bindings are not yet ready for production use, but they are already able to replace dconf for a complete GNOME session without problems. We are still lacking proper dbus integration for change notifications. _(Gabriel Rauter)_ and _(Mihael Pranjić)_

### `elektrad` rewritten in Go

[elektrad](https://www.libelektra.org/tools/elektrad) provides an HTTP API to access Elektra remotely. `elektrad` is now completely rewritten in Go, which drastically improves the performance by leveraging the new [go-elektra](https://github.com/ElektraInitiative/go-elektra/) bindings instead of calling the `kdb` command-line tool on every request. The new Elektrad creates a session per user to reuse the same kdb handle for better performance. _(Raphael Gruber)_

## Plugins

We removed the `maintained` status of the following [plugins](https://www.libelektra.org/plugins/readme):

- blockresolver
- csvstorage
- gitresolver
- list
- multifile
- spec

New maintainers are very much welcomed!

### Augeas

- Improved error message for augeas to show lensPath. _(Michael Zronek)_

### CCode

- The [Markdown Shell Recorder][] test of the plugin does not require Bash any more. _(René Schwaiger)_

[markdown shell recorder]: https://master.libelektra.org/tests/shell/shell_recorder/tutorial_wrapper

### Crypto

- The crypto plugin no longer supports Botan and OpenSSL as provider of cryptographic functions. The support has been removed to improve the maintainability of the code. _(Peter Nirschl)_
- The unit test of the crypto plugin attempts to kill the gpg-agent if a regular shutdown via `connect-gpg-agent` should fail for any reason during the clean-up phase. _(Peter Nirschl)_

### Directory Value

- The plugin now only interprets a key set as [array](../tutorials/arrays.md) if the parent contains the meta key `array`. _(René Schwaiger)_

### Fcrypt

- Improve handling of temporary files after encryption and decryption by trying to perform a manual copy if the call of `rename` fails. This problem might occur if another file system is mounted at `/tmp`. _(Peter Nirschl)_

### KConfig

- We implemented the methods that save a KeySet into a file with the KConfig Ini format. _(Dardan Haxhimustafa)_

### SWIG

- Configure line (-DBINDINGS="..") for SWIG based bindings have been changed from `swig_foo` to `foo`. _(Manuel Mausz)_
- Exclude SWIG bindings if SWIG Version is 4.0.1 and Python is >= 3.8 or Ruby is >= 2.7 due to incompatibility (#3378, #3379). _(Mihael Pranjić)_

### SWIG/python

- Added bindings for libelektratools. _(Manuel Mausz)_
- Add test for kdbEnsure. _(Mihael Pranjić)_

### SWIG/python2

- Removed. _(Manuel Mausz)_

### Tcl

- The [Markdown Shell Recorder][] test of the plugin now correctly requires the [`xmltool` plugin](../../src/plugins/xmltool). _(René Schwaiger)_

### YAMBi

- We removed the plugin in favor of [Yan LR](../../src/plugins/yanlr). _(René Schwaiger)_

### YAML CPP

- The plugin now always prints a newline at the end of the YAML output. _(René Schwaiger)_
- The plugin does not interpret a key set such as

  ```
  user/example
  user/example/#0
  user/example/#1
  user/example/#2
  ```

  as array unless the parent key `user/example` contains the meta key `array`. _(René Schwaiger)_

- YAML CPP now always sets and requires the metakey `type` with the value `boolean` for boolean data. _(René Schwaiger)_

- We limited the scope of a logging function of the module. This makes it possible to build Elektra again, if
  - you enabled the logger (`ENABLE_LOGGER=ON`),
  - build the “full” (`BUILD_FULL=ON`) version of Elektra, and
  - include both the Directory Value and YAML CPP plugin in your build configuration. _(René Schwaiger)_

### Yan LR

- The CMake code of the plugin does not print error messages produced by the tool `ldd` any more. _(René Schwaiger)_
- The plugin now also supports ANTLR 4.8. _(René Schwaiger)_
- We limited the scope of the logging code of the module. For more information, please take a look at the last news entry of the YAML CPP plugin. _(René Schwaiger)_

### GOpts

- The plugin now supports an offset into `argv` given by the `/offset` config key. When `/offset` is set, `gopts` will
  ignore a number of arguments at the start of `argv`. This can be used in e.g. python scripts to ignore the interpreter
  arguments. _(Klemens Böswirth)_
- `gopts` now also writes help message into the key `proc/elektra/gopts/help/message` in addition to setting
  `proc/elektra/gopts/help = 1`. This is also useful in non-C/C++ environments. _(Klemens Böswirth)_
- `gopts` is also affected by the changes and improvements to the `opts` library outlined below.

### Cache

- Respect `XDG_CACHE_HOME` when resolving the mmap cache directory. _(Mihael Pranjić)_

## Libraries

The text below summarizes updates to the [C (and C++)-based libraries](https://www.libelektra.org/libraries/readme) of Elektra.

### Compatibility

- We clarified compatibility requirements for Elektra and its plugins and bindings.
  Furthermore, we renamed `system/elektra/version/constants/KDB_VERSION_MICRO`
  to `system/elektra/version/constants/KDB_VERSION_PATCH` to be compatible
  with [Semantic Versioning 2.0.0](https://semver.org/). _(Markus Raab)_

### Opts

- The library function `elektraGetOpts` now supports sub-commands.
  Sub-commands are best explained by looking at an application that uses them, like `git`.
  For example `add` is a sub-command in `git add`, and interprets `-p` differently from `git`:
  `git -p add` is `git --paginate add`, but `git add -p` is `git add --patch`.
  `elektraGetOpts` now implements this notion of sub-commands.
  For more information take a look at the [tutorial for command-line-options](../tutorials/command-line-options.md).
  By extension this functionality is also available via the `gopts` plugin. _(Klemens Böswirth)_
- The generated help message was improved. It now also gives details about parameter arguments, sub-commands and
  environment variables in addition to the existing support for option arguments. This also means that it is no longer
  possible to have multiple keys with the `args=remaining` metadata (because their `opt/help` may not be the same).
  _(Klemens Böswirth)_

## Bindings

Bindings allow you to utilize Elektra using [various programming languages](https://www.libelektra.org/bindings/readme). This section keeps
you up to date with the multi-language support provided by Elektra.

### python2

- Removed. _(Manuel Mausz)_

### Rust

- Published `elektra` and `elektra-sys` versions `0.9.1` to crates.io. _(Philipp Gackstatter)_

## Tools

- Update `kdb cache` tool synopsis to reflect man page. _(Mihael Pranjić)_
- Pull elektrad, webui and webd out of shared web folder to allow fine grained selection of tools. _(Raphael Gruber)_

## Scripts

- The [fish completion script](../../scripts/completion/kdb.fish) now recognizes the new names of subcommands (e.g. `meta-set` instead of `setmeta` ) introduced with Elektra `0.9.1`. _(René Schwaiger)_
- The script [reformat-cmake](../../scripts/dev/reformat-cmake) now reformats the code with `cmake-format` 0.6.3. _(René Schwaiger)_
- The scripts

  - [reformat-c](../../scripts/dev/reformat-cmake), and
  - [reformat-java](../../scripts/dev/reformat-java)

  now uses `clang-format` 9 to reformat the code base. _(René Schwaiger)_

- The script [reformat-shell](../../scripts/dev/reformat-shell) now makes sure that you do not use `shfmt` 3, which formats parts of the code base slightly differently. _(René Schwaiger)_

## Documentation

- improved formatting of the [`validation tutorial`](../../doc/tutorials/validation.md) _(Anton Hößl)_
- We fixed some minor spelling mistakes. _(René Schwaiger)_
- We updated the man pages of the [`web`](../tutorials/install-webui.md) tool. _(René Schwaiger)_
- We now automatically close issues after one year of inactivity. _(Mihael Pranjić)_
- Updated documentation for Ubuntu-Bionic Packages. _(Djordje Bulatovic)_
- Fixed an old path of the reformatting script in the [`docker reformatting tutorial`](../tutorials/run_reformatting_script_with_docker.md) _(Jakob Fischer)_

## Tests

- We now use [Google Test](https://github.com/google/googletest) `1.10` to test Elektra. _(René Schwaiger)_
- The C++ test code does not produce warnings about a missing macro argument for `...` any more. _(René Schwaiger)_
- Whitelisted many broken links. _(Mihael Pranjić)_
- Enabled regex in link checker. _(Mihael Pranjić)_
- The [formatting check](../../tests/shell/check_formatting.sh) now also works correctly, if it is invoked multiple times. _(René Schwaiger)_
- `KDB_EXEC_PATH` is not being set globally to contain the build directory any longer. _(Peter Nirschl)_
- Rewrite gpg-agent shutdown logic to use `fork` and `execv` instead of `system`. _(Peter Nirschl)_
- Removed a broken link from the link checker. _(Djordje Bulatovic)_

## Build

### Compilation

- We do not use implicit typing in the code of the

  - `augeas`,
  - `base64`, and
  - `blockresolver`

  plugin any more. After this update, the code compiles without any warnings, even though we now use the compiler switch `-Wconversion`. _(René Schwaiger)_

### Support

- Debian 9 “stretch” (oldstable) is now the oldest supported platform. _(René Schwaiger)_

### CMake

- We fixed warnings about CMake policy [CMP0078](https://cmake.org/cmake/help/latest/policy/CMP0078.html) and [CMP0086](https://cmake.org/cmake/help/latest/policy/CMP0086.html). _(René Schwaiger)_
- The CMake functions `add_msr_test` and `add_msr_test_plugin` do not export the list of required plugins as environment variable any more. _(René Schwaiger)_
- The CMake code of the code generation does not print warnings about unknown regex operators any more. _(René Schwaiger)_
- Generating the build system now requires CMake `3.4` (released in November 2015). _(René Schwaiger)_

### Docker

- We updated some of the software in the [Dockerfile for Debian sid](../../scripts/docker/debian/sid/Dockerfile). _(René Schwaiger)_
- Building the [documentation Dockerfile for Debian Stretch](../../scripts/docker/debian/stretch/doc.Dockerfile) works again. _(René Schwaiger)_
- Use python 3, SWIG 4.0 and ruby 2.5 in the [Dockerfile for Debian sid](../../scripts/docker/debian/sid/Dockerfile). _(Mihael Pranjić)_
- Disable python binding on `debian-unstable-full-clang` due to upstream [issue](https://github.com/ElektraInitiative/libelektra/issues/3379). _(Mihael Pranjić)_
- Use current ruby-dev on debian sid image as ruby 2.5 has been dropped. _(Mihael Pranjić)_

## Infrastructure

### Cirrus

- We fixed a minor problem with the package install procedure on macOS build jobs. _(René Schwaiger)_
- We updated the startup command for D-Bus on macOS. _(René Schwaiger)_
- We removed python2 (EOL and removed from homebrew). _(Mihael Pranjić)_
- Use latest macOS Catalina Xcode stable. _(Mihael Pranjić)_
- Use newer FreeBSD images and use image family instead of concrete image names. _(Mihael Pranjić)_
- Disable tcl on FreeBSD images because of test failures (see #3353). _(Mihael Pranjić)_
- Disable curlget plugin for macOS jobs (see #3382). _(Mihael Pranjić)_
- Add more dependencies to Fedora image to cover many tests. _(Mihael Pranjić)_
- Installed ruby 2.6 to test the ruby bindings and plugins. _(Mihael Pranjić)_
- Upgraded Fedora image to current stable (version 32). _(Mihael Pranjić)_

### Jenkins

- Fixed [coveralls](https://coveralls.io/github/ElektraInitiative/libelektra) coverage report. _(Mihael Pranjić)_
- The build jobs `debian-unstable-clang-asan` and `debian-unstable-full-clang` now use Clang 9 to compile Elektra. _(René Schwaiger)_
- Added the Jenkins.monthly in the jenkins' scripts file. _(Djordje Bulatovic)_
- Temporarily disabled some problematic tests on debian unstable. _(Mihael Pranjić)_
- Enabled building packages for Bionic. _(Djordje Bulatovic)_
- Improve gpgme unit test stability. _(Peter Nirschl)_
- Publishing packages for Bionic to community. _(Djordje Bulatovic)_
- Added Fedora 32 image to main build stage, moved Fedora 31 to full build stage. _(Mihael Pranjić)_
- Method call correction. _(Djordje Bulatovic)_
- Fixed path for publishing in Jenkinsfile. _(Djordje Bulatovic)_
- Added Fedora 32 ASAN builds. _(Mihael Pranjić)_
- Reliably build the rust bindings based on the same version, by adding back the `Cargo.lock` file. _(Philipp Gackstatter)_

### Restyled

- Restyled now also checks the formatting of C, C++ and Java code in the repository. _(René Schwaiger)_

### Travis

- Use newer Xcode 11.4 and ruby 2.6.5 on macOS builds and use macOS 10.15. _(Mihael Pranjić)_
- Disable curlget plugin for macOS jobs (see #3382). _(Mihael Pranjić)_

## Website

The website is generated from the repository, so all information about
plugins, bindings and tools are always up to date. Furthermore, we changed:

- Fix and re-enable website auto-deployment. _(Mihael Pranjić)_
- Update docker images for website frontend and backend to debian buster. Update dependencies to newer versions. _(Mihael Pranjić)_
- Remove obsolete parts from the website. _(Mihael Pranjić)_

## Statistics

<<`scripts/git-release-stats 0.9.1 0.9.2`>>

## Finished Thesis

[René Schwaiger](https://github.com/sanssecours) finished [his thesis](https://github.com/sanssecours/Configuration-File-Parsing/releases) about parsing techniques and parsing tools for configuration files.

## Join the Initiative!

We welcome new contributors!
Read [here](https://www.libelektra.org/devgettingstarted/ideas) about how to get started.

As first step, you could give us feedback about these release notes.
Contact us via our [issue tracker](https://issues.libelektra.org).

## Get the Release!

You can download the release from [here](https://www.libelektra.org/ftp/elektra/releases/elektra-0.9.<<VERSION>>.tar.gz)
or [GitHub](https://github.com/ElektraInitiative/ftp/blob/master/releases/elektra-0.9.<<VERSION>>.tar.gz?raw=true)

The [hashsums are:](https://github.com/ElektraInitiative/ftp/blob/master/releases/elektra-0.9.<<VERSION>>.tar.gz.hashsum?raw=true)

<<`scripts/generate-hashsums elektra-0.9.2.tar.gz`>>

The release tarball is also available signed by Markus Raab using GnuPG from
[here](https://www.libelektra.org/ftp/elektra/releases/elektra-0.9.<<VERSION>>.tar.gz.gpg) or on
[GitHub](https://github.com/ElektraInitiative/ftp/blob/master/releases/elektra-0.9.<<VERSION>>.tar.gz.gpg?raw=true)

Already built API-Docu can be found [here](https://doc.libelektra.org/api/0.9.<<VERSION>>/html/)
or on [GitHub](https://github.com/ElektraInitiative/doc/tree/master/api/0.9.<<VERSION>>).

## Stay tuned!

Subscribe to the
[RSS feed](https://www.libelektra.org/news/feed.rss)
to always get the release notifications.

If you also want to participate, or for any questions and comments
please contact us via our issue tracker [on GitHub](http://issues.libelektra.org).

[Permalink to this NEWS entry](https://www.libelektra.org/news/0.9.<<VERSION>>-release)

For more information, see [https://libelektra.org](https://libelektra.org)

Best regards,
[Elektra Initiative](https://www.libelektra.org/developers/authors)
