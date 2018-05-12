# 0.8.<<VERSION>> Release

This release did not happen yet.

Please update this file within PRs accordingly.
For non-trivial changes, you can choose to be
part of the highlighted changes. Please make
sure to add some short tutorial, asciinema,
or how-to-use for highlighted items.

Please add your name to every contribution
syntax: ", thanks to <myname>".

TODO: look through git history
done till 45bfae3ad73333598f28af9c47e3b2a526162151 cmake: require list


<<`scripts/generate-news-entry`>>

We are proud to release Elektra 0.8.<<VERSION>>.

<<`scripts/git-release-stats 0.8.VERSION`>>

## What is Elektra?

Elektra serves as a universal and secure framework to access
configuration settings in a specified, global, hierarchical key database.
For more information, visit [https://libelektra.org](https://libelektra.org).

For a small demo see here:

[![asciicast](https://asciinema.org/a/cantr04assr4jkv8v34uz9b8r.png)](https://asciinema.org/a/cantr04assr4jkv8v34uz9b8r)

You can also read the news [on our website](https://www.libelektra.org/news/0.8.<<VERSION>>-release)



## Highlights

- Notification: New transport plugin
- Web UI
- Overhaul of Build System and Daily Stretch Repository


### Notification: New transport plugin

Elektra's notification feature has received its first transport plugin pair:
D-Bus.
Transport plugins provide a link between applications using Elektra.
These plugins send and receive notifications when a key is modified within the
key database.
The existing `dbus` plugin has been modified to use an asynchronous I/O binding
for sending messages, if available.
The new `dbusrecv` plugin is responsible for receiving messages sent from the
`dbus` plugin and other sources with the same
[message format](https://www.libelektra.org/plugins/dbus#notification-format).

For more details see the
[notification tutorial](https://github.com/ElektraInitiative/libelektra/tree/master/doc/tutorials/notifications.md)
or the
[example applications](https://github.com/ElektraInitiative/libelektra/tree/master/src/libs/notification/example)

Thanks to Thomas Wahringer.

### Web UI

The Web UI was greatly improved.

The goal of the Web UI is to have safe and unified access to all configuration settings of a system.
Different to all other UIs, it can dynamically read specifications of configuration settings and
generates its interface accordingly.

If a configuration setting only has some choices, you get exactly these choices
within the user interface.

To get outstanding usability, it now provides:

- undo functionality
- Using visiblility you are able to hide irrelevant configuration settings
- built-in validation for many types of configuration settings
- support for arrays
- description of configuration settings is embedded in the user interface

Furthermore:

- The Web-UI now is able to install itself via cmake.
- The API was updated for [elektrad](https://master.libelektra.org/doc/api_blueprints/elektrad.apib)
  and [webd](doc/api_blueprints/webd.apib) (former clusterd).

> Note that new version of the Web UI requires Elektra 0.8.23 or later.

Thanks to Daniel Bugl.


### Overhaul of Build System and Daily Stretch Repository

We started to overhaul our build system to improve build times and responsiveness.
It focuses heavily on containerisation to improve hardware utilization.

If you are interested in `#devops` have a look at our
[Jenkinsfile](https://github.com/ElektraInitiative/libelektra/blob/master/scripts/jenkins/Jenkinsfile).

Daily builds Debian packages for Stretch are available again in our
[stretch repository](https://debian-stretch-repo.libelektra.org).
Add it to your `sources.list`:

```
deb     [trusted=yes] https://debian-stretch-repo.libelektra.org/ stretch main
deb-src [trusted=yes] https://debian-stretch-repo.libelektra.org/ stretch main
```

Thanks to Lukas Winkler.

## Other New Features

We added even more functionality, which could not make it to the highlights:

- A new experimental [I/O binding for glib](https://www.libelektra.org/bindings/io_glib)
  has been added.
  It can be used to integrate the notification feature with applications based
  on glib.
- The Order Preserving Minimal Perfect Hash Map (OPMPHM), used to speed up the lookups, got optimized
  and a benchmark was added,
  thanks to Kurt Micheli
- Added a script that calculates the complexity of configuration settings based on their specification,
  thanks to Anton Hößl
- `kdb ls` now has `-0` option (needed for Web UI)
- The [csvstorage](https://www.libelektra.org/plugins/csvstorage) now can treat selected columns
  to be part of the key.
  thanks to Thomas Waser

## Other News

- We added a tutorial about securing the integrity and confidentiality of configuration values.
- Peter Nirschl finished his [thesis](https://www.libelektra.org/ftp/elektra/publications/nirschl2018cryptographic.pdf)
  ([signature](https://www.libelektra.org/ftp/elektra/publications/nirschl2018cryptographic.pdf.sig)).
  It includes a benchmark of different cryptographic providers.
- Markus Raab gave a [talk](https://cfp.linuxwochen.at/de/LWW18/public/events/798) at Linuxwochen Wien (in German).
  For similar talks in English, please refer to the [FOSDEM talks](https://fosdem.org/2018/schedule/speaker/markus_raab/).
- The code of conduct was changed to not have the word "project" in it (project has per definition an end date,
  Elektra has not, thus it is called an initiative)

## Documentation

We improved the documentation in the following ways:

- FAQ was extended by [Why do I need Elektra if I already use configuration management tools?](https://www.libelektra.org/docgettingstarted/faq)
- documented possible JSON corruption
- uniformly add `.` at end of short help
- Logo for Doc Set was added and logo for favicon was updated
  thanks to René Schwaiger
- template of design decisions was updated to use the words
  problem (instead of issue) and rationale (instead of argument).
- METADATA.ini:
  - added visibility (partially implemented in WebUI)
  - added type (only check/type existed)
  - plenty of metadata is now used by Web UI
- update docu for type plugin to say that `check/type/min` and  `check/type/max` are deprecated
  (not only METADATA.ini)
- Fixed various spelling mistakes
  thanks to René Schwaiger
- Document limitations of resolver (kdbOpen fails if no home directory found)
  and yaml plugin (intermixing of array and objects not detected, which is possible
  in Elektra but impossible in JSON)
- Required environment to run tests is documented.
- A decision about [deferred plugin calls](doc/decisions/deferred_plugin_calls.md) has been made and implemented.
  Thanks to Thomas Wahringer.
- <<TODO>>

## Compatibility

As always, the ABI and API of kdb.h is fully compatible, i.e. programs
compiled against an older 0.8 version of Elektra will continue to work
(ABI) and you will be able to recompile programs without errors (API).

We added:

- the private headerfiles `kdbnotificationinternal.h`, `kdbioplugin.h`.
- `kdb get`, `kdb mv` and `kdb cp` use error code `11` if keys are not found
- the constant `ENABLE_ASAN` in the constants plugin

We removed:

- not used error code `12` from `kdb mv` removed from docu
- cascading keys as arguments to `kdb cp` and `kdb mv` now fail instead
  of doing something unexpected, thanks to @sanssecours for reporting
  (see #1483)


Shell scripts:

- cp and mv no longer accept cascading keys.

## Notes for Maintainer

These notes are of interest for people maintaining packages of Elektra:

- Docu is updated that [cmake3](https://cmake.org/cmake/help/v3.0/) is required.
  thanks to Lukas Winkler for reporting.
- To run all tests successfully, the `spec` and `list` plugin is required.
  So if `ENABLE_TESTING` is checked, cmake checks the presence of a storage,
  a resolver, the list and the spec plugin.
  Thanks to René Schwaiger
- This will be the last release supporting Debian Wheezy
  (LTS support will stop in May)
  Directly after the release, Jessie (oldstable) with gcc 4.8.4 will
  be the oldest supported platform.
- `Base64.pdf` is not installed anymore.
- <<TODO>>

## Website

- Error page that is shown if no JavaScript is enabled now more clearly
  says that the Website only renders content from the repo and
  only contains free JavaScript.
- The [FAQ](https://www.libelektra.org/docgettingstarted/faq) is now
  more visible (added to "getting started").

## Notes for Elektra's Developers

These notes are of interest for people developing Elektra:

- `. run_dev_env` is a script to be sourced from the build directory.
  It sets environment variables, so that Elektra from the build
  directory is used (instead of the installed one).
- We now allow `clang-reformat-5.0`,  `clang-reformat-6.0`, and
  `clang-reformat-7.0` for formatting.
  Thanks to René Schwaiger.
- To make enums nicely formatting, make sure at least one member
  is documented.
- Tests no longer clear environment or reset locales.
  This fixes TMPDIR, DBUS_SESSION_BUS_ADDRESS problems but might
  cause problems with wrongly set HOME and USER.
  thanks to Lukas Winkler
- You can now add a [Markdown Shell Recorder][] test for a plugin
  via the CMake function `add_plugin` by adding `TEST_README`.
  Furthermore `TEST_REQUIRED_PLUGINS` allows us to specify which
  additional plugins are required.
  thanks to René Schwaiger
- `const` was added to exceptions in catch blocks
  thanks to René Schwaiger
- Mention to read [doc/DESIGN.md](https://master.libelektra.org/doc/DESIGN.md)
  in the contributing guidelines.
- The CMake functions

   - `add_plugin`
   - `add_msr_test`
   - `add_msr_test_plugin`, and the new
   - `add_shell_recorder_test`

    now allow you to specify a list of required plugins for [Shell Recorder][] and
   Markdown Shell Recorder tests.
- The [Markdown Shell Recorder][] now compares the whole output of `stderr` with the text following the directive `STDERR:`.
  thanks to René Schwaiger
- You can now leave the text following the directive `STDERR:` in a [Markdown Shell Recorder][] test empty:

   ```sh
   true # Print nothing to `stderr`
   # STDERR:
   ```

   . The Markdown Shell Recorder will then check if the command printed nothing to the standard error output.
- The [Shell Recorder][] now also prints the content of the protocol file if a test was unsuccessful or you used the command switch `-p`.
  thanks to René Schwaiger
- The [Shell Recorder][] now always removes the protocol file.
- All current versions of Clang-Format (6.0+) and the outdated Clang-Format 5 will now produce exactly the same output for the whole
  codebase.
  thanks to René Schwaiger
- We added an [Markdown Shell Recorder][] test for the [Constants](http://libelektra.org/plugins/constants) plugin.
- The [Markdown Shell Recorder][] now prints the path of the test file.
  thanks to René Schwaiger
- If any of the tests in `make run_memcheck` fail valgrind will now set an exit-code which will get picked up by make.
  thanks to René Schwaiger
- The haskell binding now explicitly requires GHC installed with a minimum version of 8.0.0 during cmake
  thanks to René Schwaiger and Lukas Winkler
- We introduced git reference repositories to save I/O on our build system,
  thanks to Lukas Winkler
- Set `LD_LIBRARY_PATH` in all tests removing the need to specify it for running ctest
  thanks to Lukas Winkler
- Provide the `RUN_SERIAL` property to all tests that can not be run in
  parallel
  thanks to Lukas Winkler
- Speeding up your test runs via ctest -j is now possible
  thanks to Lukas Winkler
- Documentation and debugging capabilities of [Markdown Shell Recorder][] were improved.

[Markdown Shell Recorder]: https://master.libelektra.org/tests/shell/shell_recorder/tutorial_wrapper
[Shell Recorder]: https://master.libelektra.org/tests/shell/shell_recorder

## Fixes

Many problems were resolved with the following fixes:

- [YAML CPP](http://libelektra.org/plugins/yamlcpp) now also saves key values directly below a mountpoint correctly.
  thanks to René Schwaiger
- If you use a minimal configuration ([`dump`](http://libelektra.org/plugins/dump), [`resolver`](https://www.libelektra.org/plugins/resolver), list, and spec), all test of the test suite now finish successfully again.
  thanks to René Schwaiger
- small refactoring in `kdb-test`
- The haskell plugin failed to build if the haskell bindings were not included explicitly by name.
- Fix invalid handling of keynames in the [spec](http://libelektra.org/plugins/spec) plugin.
- We now disable the [Xerces plugin](http://libelektra.org/plugins/xerces) if you use GCC with enabled ASAN to build Elektra. This update
  makes sure that you do not build the plugin with compilation settings that are known to
  [cause problems](https://github.com/ElektraInitiative/libelektra/issues/1895).
- The [Shell Recorder][] counts the number of executed tests properly again.
- CMake now fails if the required plugins [list](http://libelektra.org/plugins/list) or [spec](http://libelektra.org/plugins/spec) (on
   non-[MinGW](http://mingw.org) platforms) are missing from the current build configuration.
- The [Lua](http://libelektra.org/plugins/lua), [Python 2](http://libelektra.org/plugins/python2),
  [Python](http://libelektra.org/plugins/python), and [Ruby](http://libelektra.org/plugins/ruby) plugins now require SWIG bindings for
  the corresponding programming language.
  thanks to René Schwaiger
- type checker now also honors `type` next to `check/type`
- Fix various compiler warnings
- The detection of

   - Botan,
   - Libgcrypt,
   - LibGit2 and
   - OpenSSL

   now also works properly, if we treat warnings as errors (compiler switch `-Werror`).
- The [multifile plugin](http://libelektra.org/plugins/multifile) now passes the child config
  to the storage plugins too and also handles symlinks correctly.

## Outlook

We are currently working on following topics:

- [elektrifying LCDproc](https://www.libelektra.org/news/elektrify-lcdproc) by Klemens Böswirth
  After some setbacks (the two original developers who wanted to work on LCDproc resigned)
  LCDproc development restarted now successfully.
  The new plan is to have more intermediate stages.
  In particular the first integration will be a minimal invasive integration without high-level API.
- Continous integration by Lukas Winkler
- Type system by Armin Wurzinger
- Web UI by Daniel Bugl
- We created a proof of concept for a Chef resource and an Ansible module successfully setting Elektra's keys.
  They are not yet published.  If you are interested on this preliminary work, please contact us.

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
