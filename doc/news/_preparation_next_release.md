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

<<`scripts/git-release-stats 0.8.VERSION`>>

## What is Elektra?

Elektra serves as a universal and secure framework to access
configuration settings in a global, hierarchical key database.
For more information, visit [https://libelektra.org](https://libelektra.org).

For a small demo see here:

[![asciicast](https://asciinema.org/a/cantr04assr4jkv8v34uz9b8r.png)](https://asciinema.org/a/cantr04assr4jkv8v34uz9b8r)

You can also read the news [on our website](https://www.libelektra.org/news/0.8.<<VERSION>>-release)



## Highlights

- Notification: New transport plugin
- <<HIGHLIGHT2>>
- <<HIGHLIGHT3>>


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

### <<HIGHLIGHT2>>


### <<HIGHLIGHT2>>


## Other New Features

We added even more functionality, which could not make it to the highlights:

- A new experimental [I/O binding for glib](https://www.libelektra.org/bindings/io_glib)
  has been added.
  It can be used to integrate the notification feature with applications based
  on glib.
- The Order Preserving Minimal Perfect Hash Map (OPMPHM), used to speed up the lookups, got optimized.
- `kdb ls` now has `-0` option

## Documentation

We improved the documentation in the following ways:

- FAQ was extended by [Why do I need Elektra if I already use configuration management tools?](https://www.libelektra.org/docgettingstarted/faq)
- documented possible JSON corruption
- uniformly add `.` at end of short help
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

## Notes for Maintainer

These notes are of interest for people maintaining packages of Elektra:

- Docu is updated that cmake3 is required.
  thanks to Lukas Winkler for reporting.
- <<TODO>>

## Notes for Elektra's Developers

These notes are of interest for people developing Elektra:

- Tests no longer clear environment or reset locales.
  This fixes TMPDIR, DBUS_SESSION_BUS_ADDRESS problems but might
  cause problems with wrongly set HOME and USER.
- You can now add a [Markdown Shell Recorder][] test for a plugin
  via the CMake function `add_plugin`.
- The CMake functions

   - `add_plugin`
   - `add_msr_test`
   - `add_msr_test_plugin`, and the new
   - `add_shell_recorder_test`

    now allow you to specify a list of required plugins for [Shell Recorder](https://master.libelektra.org/tests/shell/shell_recorder) and
   Markdown Shell Recorder tests.
- All current versions of Clang-Format (6.0+) and the outdated Clang-Format 5 will now produce exactly the same output for the whole
  codebase.
- We added an [Markdown Shell Recorder][] test for the [Constants](http://libelektra.org/plugins/constants) plugin.
- The [Markdown Shell Recorder][] now prints the path of the test file.
- If any of the tests in `make run_memcheck` fail valgrind will now set an exit-code which will get picked up by make.
- The haskell binding now explicitly requires GHC installed with a minimum version of 8.0.0 during cmake
- We introduced git reference repositories to save io on our build system
- Set `LD_LIBRARY_PATH` in all tests removing the need to specify it during
  ctest runs
- Provide the `RUN_SERIAL` property to all tests that can not be run in
  parallel
- Speeding up your test runs via ctest -j is now possible

[Markdown Shell Recorder]: https://master.libelektra.org/tests/shell/shell_recorder/tutorial_wrapper

## Fixes

Many problems were resolved with the following fixes:

- [YAML CPP](http://libelektra.org/plugins/yamlcpp) now also saves key values directly below a mountpoint correctly.
- If you use a minimal configuration ([`dump`](http://libelektra.org/plugins/dump) and [`resolver`](https://www.libelektra.org/plugins/resolver) only), all test of the test suite now finish successfully again.
- We resolved undefined behavior in polymorphic classes that contained virtual functions, by explicitly adding a virtual destructor.
- small refactoring in `kdb-test`
- Fix invalid handling of keynames in the [spec](http://libelektra.org/plugins/spec) plugin.
- We now disable the [Xerces plugin](http://libelektra.org/plugins/xerces) if you use GCC with enabled ASAN to build Elektra. This update
  makes sure that you do not build the plugin with compilation settings that are known to
  [cause problems](https://github.com/ElektraInitiative/libelektra/issues/1895).

## Outlook

We are currently working on following topics:

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

[Permalink to this NEWS entry](https://www.libelektra.org/news/0.8.<<VERSION>>-release)

For more information, see [https://libelektra.org](https://libelektra.org)

Best regards,
[Elektra Initiative](https://www.libelektra.org/developers/authors)
