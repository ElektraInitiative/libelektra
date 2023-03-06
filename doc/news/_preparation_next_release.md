# 0.9.<<VERSION>> Release

This release did not happen yet.

Please always update this file within **every PR**:

1. write what changed
2. use links pointing to your change (See [Documentation Guidelines](/doc/contrib/documentation.md))
3. add your name at the end of the line **Syntax:** _(your name)_

For example, Max would write:

```
- Added a new [doc plugin](https://www.libelektra.org/plugins/doc) _(Max)_
```

Pick a random line to write your changes to minimize the chances of conflicts in this file.

For non-trivial changes, you can choose to be part of the highlighted changes.
Please write a highlight section in this case.

After the horizontal line the release notes for the next version starts.

---

<<`scripts/generate-news-entry`>>

We are proud to release Elektra 0.9.<<VERSION>>.

## What is Elektra?

Elektra serves as a universal and secure framework to access configuration settings in a global, hierarchical key database.
For more information, visit [https://libelektra.org](https://libelektra.org).

You can also read the news [on our website](https://www.libelektra.org/news/0.9.<<VERSION>>-release).

You can try out the latest Elektra release using our docker image [elektra/elektra](https://hub.docker.com/r/elektra/elektra).
This is the quickest way to get started with Elektra without compiling and other obstacles, simply run:

```sh
docker pull elektra/elektra
docker run -it elektra/elektra
```

## Highlights

- New Changetracking API
- New spec plugin
- <<HIGHLIGHT>>

### New Changetracking API

We've created a new KeySet diffing and changetracking API for both internal and external use.
Several plugins already had their own implementations to detect changes made to the key database.
Unfortunately, every implementation did something different and results varied.
With this new release every plugin now uses the same shared implementation!

To try the amazing new API yourself, take a look at [the tutorial](../tutorials/changetracking.md)!

Elektra's internal code now also uses this new API to detect which backends to execute.
This can lead to better performance in I/O bound cases, where previously certain keys would have been detected as changed when they weren't.

We ran some memory benchmarks and found a slightly increased memory usage on a stock instance of Elektra.
If you're using many plugins that do changetracking, the overhead will decrease.

| Number of Keys | Old Implementation (bytes) | New Implementation (bytes) | Memory Increase (%) |
| -------------: | -------------------------: | -------------------------: | ------------------: |
|             50 |               225792 bytes |               229580 bytes |              1,68 % |
|            500 |               383180 bytes |               411238 bytes |              7,32 % |
|           5000 |              1992294 bytes |              2306867 bytes |             15,79 % |
|          50000 |             18245222 bytes |             21181235 bytes |             16,09 % |
|         500000 |            178782208 bytes |            207827763 bytes |             16,25 % |

Apart from memory benchmark, we also ran some performance benchmarks.
As the benchmark is heavily I/O bound, the biggest bottleneck is the I/O performance of the system.
We could not reliably detect a real, reliably reproducible performance impact measured in seconds.
Alternatively, we have measured executed instructions.
There seems to be about 10 % overhead, but we don't expect it to be noticeable in real-world workloads.

| Number of Keys | Old Implementation (Instructions) | New Implementation (Instructions) | Performance Overhead (%) |
| -------------: | --------------------------------: | --------------------------------: | -----------------------: |
|             50 |                          18910449 |                          19583227 |                   3,56 % |
|            500 |                          63001911 |                          68948096 |                   9,44 % |
|           5000 |                         526801917 |                         586344210 |                  11,30 % |
|          50000 |                        5730261920 |                        6340292587 |                  10,65 % |
|         500000 |                      104614374974 |                      110702166761 |                   5,82 % |

### New spec plugin

The spec plugin was rewritten to use the standardized error handling in Elektra.
It is now strictly defined that the `spec` plugin throws a warning on `kdbGet` and on any other call an error.

Default values are now created in the `default` namespace.
The instantiated array specifications are now also created in the `default` namespace.

Keys with a require metakey and no default metakey do throw an error now.

Known limitations:

- `#` and `_` keys do not work on MINGW
- No defaults for `_` globbing character

For more information see [Spec Plugin](../../src/plugins/spec/README.md).

### <<HIGHLIGHT>>

## Plugins

The following text lists news about the [plugins](https://www.libelektra.org/plugins/readme) we updated in this release.

### General

- Updated target name of shared object files according to [#3486](https://issues.libelektra.org/3486)

### spec

- Rewrite spec plugin, fix bugs and use correct error handling _(Tomislav Makar @tmakar)_
- Remove info metakey from spec plugin _(Tomislav Makar @tmakar)_
- <<TODO>>

### <<Plugin>>

- <<TODO>>
- <<TODO>>
- <<TODO>>

### counter

- Move `static` variables into functions to avoid global variables _(@kodebach)_
- <<TODO>>
- <<TODO>>

### <<Plugin>>

- <<TODO>>
- <<TODO>>
- <<TODO>>

### logchange

- Utilize new changetracking API _(Maximilian Irlinger @atmaxinger)_
- Add Maximilian Irlinger as maintainer _(Maximilian Irlinger @atmaxinger)_

### yajl

- The `itKs` global variable workaround, which was used to replace the now removed internal `KeySet` cursor, was replaced with a custom context struct. _(@kodebach)_
- <<TODO>>
- <<TODO>>

### toml

- The `flex` lexer and `bison` parser are now fully reentrant and therefore thread-safe. _(@kodebach)_
- <<TODO>>
- <<TODO>>

### multifile

- Remove multifile plugin as it is unmaintained and conflicts with the new backend architecture _(Maximilian Irlinger @atmaxinger)_
- <<TODO>>
- <<TODO>>

### c

- Add Florian Lindner as maintainer _(Florian Lindner @flo91)_
- <<TODO>>
- <<TODO>>
- Use separate symbols for `set` and `commit` functions to satisfy `kdb plugin-check` _(@kodebach)_
- Use new `elektraPluginGetPhase()` instead of counting executions _(@kodebach)_

### mmapstorage

> **Note**: The plugin is currently disabled, because it is not yet compatible with the COW data structures.

TODO: remove above note, when COW support is added.

- The magic data structures are now fully compile-time constants.
  The magic number to detect endianness is generated in CMake instead of at runtime. _(@kodebach)_
- <<TODO>>
- <<TODO>>

### syslog

- Convert to hook plugin _(Maximilian Irlinger @atmaxinger)_
- Utilize new changetracking API _(Maximilian Irlinger @atmaxinger)_
- <<TODO>>

### lineendings

- Add Florian Lindner as maintainer _(Florian Lindner @flo91)_

### length

- Add Florian Lindner as maintainer _(Florian Lindner @flo91)_

### missing

- Add Florian Lindner as maintainer _(Florian Lindner @flo91)_

### unit

- Add Florian Lindner as maintainer _(Florian Lindner @flo91)_

### dbus

- Utilize new changetracking API _(Maximilian Irlinger @atmaxinger)_
- Add Maximilian Irlinger as maintainer _(Maximilian Irlinger @atmaxinger)_

### internalnotifications

- Utilize new changetracking API _(Maximilian Irlinger @atmaxinger)_
- Add Maximilian Irlinger as maintainer _(Maximilian Irlinger @atmaxinger)_

## Libraries

The text below summarizes updates to the [C (and C++)-based libraries](https://www.libelektra.org/libraries/readme) of Elektra.

### Compatibility

- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>

### Core

- The `syslog` logging code now calls `openlog` before every `syslog` to avoid the use of a global variable. _(@kodebach)_
- <<TODO>>
- Fix memleak in kdb.c, #4925 _(@hannes99)_
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>

### loader

- Adapt target rename with `-plugin-` in `dl.c` _(Tomislav Makar @tmakar)_
- <<TODO>>
- <<TODO>>

### <<Library>>

- <<TODO>>
- <<TODO>>
- <<TODO>>

### kdb

- Add new changetracking API _(Maximilian Irlinger @atmaxinger)_
- Fix unwanted removal of subkeys when using mv _(Hannes Laimer @hannes99)_
- Fix inconsistent return values in code, tests and man pages _(Hannes Laimer @hannes99)_
- Remove `smount` alias _(Hannes Laimer @hannes99)_
- <<TODO>>

### <<Library>>

- <<TODO>>
- <<TODO>>
- <<TODO>>

### <<Library>>

- <<TODO>>
- <<TODO>>
- <<TODO>>

## Bindings

Bindings allow you to utilize Elektra using [various programming languages](https://www.libelektra.org/bindings/readme).
This section keeps you up-to-date with the multi-language support provided by Elektra.

### jna

- Updated Java binding related dependencies. _(Michael Tucek @tucek)_
- Updated `KDBException` to only access error key at construction time. _(Michael Tucek @tucek)_
- Removed public naive resource release API. To migrate just remove calls to the affected methods `Key#release()`, `KeySet#release()` and `KDBException#releaseErrorKey()` _(Michael Tucek @tucek)_
- Enabled strict javadoc checking for Gradle build _(Michael Tucek @tucek)_
- add merging based on elektraMerge _(Maximilian Irlinger @atmaxinger)_
- Added support for `ksIncRef` for `KeySet` _(Michael Tucek @tucek)_
- Enabled `ReferenceCleaner` _(Michael Tucek @tucek)_

### go-elektra

- Move `go-elektra` binding from repository into bindings folder of `libelektra` _(Tomislav Makar @tmakar)_
- <<TODO>>
- <<TODO>>

### <<Binding>>

- <<TODO>>
- <<TODO>>
- <<TODO>>

### <<Binding>>

- <<TODO>>
- <<TODO>>
- <<TODO>>

### <<Binding>>

- <<TODO>>
- <<TODO>>
- <<TODO>>

### <<Binding>>

- <<TODO>>
- <<TODO>>
- <<TODO>>

## Tools

### elektrad

- Implemented new request to add multiple metakeys for one key _(Tomislav Makar @tmakar)_
- Adding bulk creation request for configuration keys _(Tomislav Makar @tmakar)_
- <<TODO>>
- <<TODO>>

### KDB

- Replace C++ of the CLI by a C version, C++ is fallback for not yet implemented commands _(@hannes99)_
- Add general error/warning output and formatting for whole CLI
- Add extra tests for CLI commands
- Disable cascading writes as described in #3742
- Fix inconsistent return values #1563
- Update documentation, tutorials and examples where they would not match the new C version

### webd

- Implemented new request to add multiple metakeys for one key _(Tomislav Makar @tmakar)_
- Adding bulk creation request for configuration keys _(Tomislav Makar @tmakar)_
- <<TODO>>
- <<TODO>>

### <<Tool>>

- <<TODO>>
- <<TODO>>
- <<TODO>>

### <<Tool>>

- <<TODO>>
- <<TODO>>
- <<TODO>>

## Scripts

- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>

## Documentation

- Adapt and remove outdated docs https://issues.libelektra.org/4882 _(Tomislav Makar @tmakar)_
- Added missing dependencies in COMPILE.md for APT-based systems _(Michael Tucek @tucek)_
- Add `default` namespace to [namespaces documentation](../tutorials/namespaces.md) _(Tomislav Makar @tmakar)_
- Move `go-elektra` binding into bindings folder of `libelektra` _(Tomislav Makar @tmakar)_
- <<TODO>>
- <<TODO>>
- Added Tomislav Makar to `AUTHORS.md` _(Tomislav Makar @tmakar)_
- <<TODO>>
- <<TODO>>
- Added Florian Lindner to `AUTHORS.md` _(Florian Lindner @flo91)_
- <<TODO>>
- .github rework _(Markus Raab)_
- Added `hook` to `placements` contract in [CONTRACT.ini](../CONTRACT.ini) _(Tomislav Makar @tmakar)_
- Added `hook` information to [hooks.md](../dev/hooks.md)
- Added Hannes Laimer to `AUTHORS.md` _(Hannes Laimer @hannes99)_
- Add README to tools/kdb _(Hannes Laimer @hannes99)_
- <<TODO>>
- Fixed shell-recorder test in [`install-webui.md`](../tutorials/install-webui.md) _(Tomislav Makar @tmakar)_
- Add new shell-recorder test in [`install-webui.md`](../tutorials/install-webui.md) for newly implemented request for adding multiple metakeys for one key _(Tomislav Makar @tmakar)_
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- Update AUTHORS.md info _(@kodebach)_
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- Add Stefan Hanreich to AUTHORS.md _(Stefan Hanreich @lawli3t)_
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>

### Use Cases

- Add specification use case for [array-specification](../usecases/specification/array-specification.md) _(Tomislav Makar @tmakar)_
- Add specification use case for [underline-specification](../usecases/specification/underline-specification.md) _(Tomislav Makar @tmakar)_
- Add specification use case for [simple-specification](../usecases/specification/simple-specification.md) _(Tomislav Makar @tmakar)_
- Add specification use case for [enum-specification](../usecases/specification/enum-specification.md) _(Tomislav Makar @tmakar)_
- Add complete specification for `dockerd` configuration file (`daemon.json`) _(Tomislav Makar @tmakar)_
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>

### Decisions

- Decide and implement [decision process](../decisions/5_partially_implemented/decision_process.md) _(Markus Raab)_
- Decided future [library split](../decisions/4_decided/library_split.md) _(@kodebach)_
- Decided [decision process](https://www.libelektra.org/decisions/decision-process) _(Markus Raab)_
- Draft for [man pages](../decisions/0_drafts/man_pages.md) _(Markus Raab)_
- <<TODO>>
- Add decision for [change tracking](../decisions/3_in_review/change_tracking.md) _(Maximilian Irlinger @atmaxinger)_
- <<TODO>>
- Create [decision](../decisions/0_drafts/operation_sequences.md) for allowed and prohibited operation seqences _(Maximilian Irlinger @atmaxinger)_
- <<TODO>>
- Add decisions about [location of headers](../decisions/4_decided/header_file_structure.md) and [use of `#include`](../decisions/4_decided/header_include.md) in the repo _(@kodebach)_
- <<TODO>>
- <<TODO>>
- Add decision about [metadata semantics](../decisions/0_drafts/metakey_semantics.md) _(@kodebach)_
- <<TODO>>
- <<TODO>>
- Many small fixes to adapt to documentation guidelines and new decision process. _(Markus Raab)_
- <<TODO>>
- Add decision for [read-only keynames](../decisions/0_drafts/readonly_keynames.md) _(Maximilian Irlinger @atmaxinger)_
- <<TODO>>
- <<TODO>>
- Revive [keyname decision](../decisions/4_decided/keyname.md) _(@kodebach)_
- <<TODO>>
- <<TODO>>
- Add decision for [copy-on-write](../decisions/6_implemented/copy_on_write.md) and provide implementation suggestions. _(Maximilian Irlinger @atmaxinger)_
- <<TODO>>
- Added explanation on why we wanted to migrate from Maven to [Gradle](../decisions/6_implemented/gradle.md) for Java-related build facilities. _(Michael Tucek @tucek)_
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>

### Tutorials

- <<TODO>>
- Add basic tutorial about changetracking _(Maximilian Irlinger @atmaxinger)_
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>

### Man Pages

- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>

## Tests

- Add shell test to verify correct behavior for https://github.com/ElektraInitiative/libelektra/issues/4061 _(Tomislav Makar @tmakar)_
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>

### C

- <<TODO>>
- <<TODO>>
- <<TODO>>

### Shell Recorder

- <<TODO>>
- <<TODO>>
- <<TODO>>

### C++

- <<TODO>>
- <<TODO>>
- <<TODO>>

## Packaging

- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>

## Build

### CMake

- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>

### Docker

- <<TODO>>
- <<TODO>>
- CentOS 8 Stream: manually install `config-manager` DNF plugin. _(Maximilian Irlinger @atmaxinger)_
- <<TODO>>
- <<TODO>>
- <<TODO>>

## Infrastructure

### Jenkins

- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>

### Cirrus

- Rename deprecated `d-bus` to `dbus` in `macOS.yml` and `.cirrus.yml` [Issue-#4900](https://github.com/ElektraInitiative/libelektra/issues/4900) _(Tomislav Makar @tmakar)_
- <<TODO>>
- <<TODO>>
- <<TODO>>
- Push FreeBSD 12.3 to 12.4 since 12.3 is end of life. _(Richard Stöckl @eiskasten)_
- <<TODO>>

### GitHub Actions

- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>

## Website

The website is generated from the repository, so all information about plugins, bindings and tools are always up-to-date. Furthermore, we changed:

- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>

## Miscellaneous

- Many global variables that where used as constants have been made fully `const` _(@kodebach)_

## Outlook

We are currently working on following topics:

- <<TODO>>
- Session recording and better Ansible integration _(Maximilian Irlinger @atmaxinger)_
- <<TODO>>
- <<TODO>>

## Statistics

We closed [<<NUMISSUES>> issues](https://github.com/ElektraInitiative/libelektra/milestone/<<MILESTONE>>?closed=1) for this release.

<<`scripts/git-release-stats 0.9.VER-1 0.9.<<VERSION>>`>>

Thanks to all authors for making this release possible!

## Join the Initiative!

We welcome new contributors!
Read [here](https://www.libelektra.org/devgettingstarted/ideas) about how to get started.

As first step, you could give us feedback about these release notes.
Contact us via our [issue tracker](https://issues.libelektra.org).

## Get the Release!

You can download the release from

- [here](https://www.libelektra.org/ftp/elektra/releases/elektra-0.9.<<VERSION>>.tar.gz) or
- [GitHub](https://github.com/ElektraInitiative/ftp/blob/master/releases/elektra-0.9.<<VERSION>>.tar.gz?raw=true)

The [hashsums are:](https://github.com/ElektraInitiative/ftp/blob/master/releases/elektra-0.9.<<VERSION>>.tar.gz.hashsum?raw=true)

<<`scripts/generate-hashsums elektra-0.9.<<VERSION>>.tar.gz`>>

The release tarball is also available signed using GnuPG from

- [here](https://www.libelektra.org/ftp/elektra/releases/elektra-0.9.<<VERSION>>.tar.gz.gpg) or
- [GitHub](https://github.com/ElektraInitiative/ftp/blob/master/releases/elektra-0.9.<<VERSION>>.tar.gz.gpg?raw=true)

The following GPG Key was used to sign this release: 12CC44541E1B8AD9B66AFAD55262E7353324914A

Already built API documentation can be found

- [here](https://doc.libelektra.org/api/0.9.<<VERSION>>/html/) or
- [GitHub](https://github.com/ElektraInitiative/doc/tree/master/api/0.9.<<VERSION>>).

## Stay tuned!

Subscribe to the [RSS feed](https://www.libelektra.org/news/feed.rss) to always get the release notifications.

If you also want to participate, or for any questions and comments, please contact us via our issue tracker [on GitHub](http://issues.libelektra.org).

[Permalink to this NEWS entry](https://www.libelektra.org/news/0.9.<<VERSION>>-release)

For more information, see [https://libelektra.org](https://libelektra.org).

Best regards,
[Elektra Initiative](https://www.libelektra.org/developers/authors)
