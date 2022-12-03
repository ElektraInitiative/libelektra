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

- <<HIGHLIGHT>>
- <<HIGHLIGHT>>
- <<HIGHLIGHT>>

### New Backend

The entire logic for backends has been rewritten.
Instead of calling most plugin directly, `libelektra-kdb` now only calls so-called backend plugins and special hook plugins.
There is a [contract](../dev/backend-plugins.md) between `libelektra-kdb` and the backend plugins.
All backend plugins must adhere to this contract.
To achieve this goal, most backend plugins will call other plugins (like `libelektra-kdb` did previously).

The logic previously implemented in `libelektra-kdb` was moved to the new default backend plugin `backend`.
It works like the old system, but now also allows an unlimited number of plugins in positions where that makes sense.
For example, you can have unlimited `postgetstorage` plugins, but only a single `getresolver`.

There have also been slight changes to `kdbGet` and `kdbSet`.
Please read their API docs to find out, if you rely on any behavior that has been altered.
You can also read the [new low-level docs](../dev/kdb-operations.md) to find out all the intricate details.

The structure of `system:/elektra/mountpoints` changed as well.
Take a look at the [new docs](../dev/mountpoints.md), if you need to know details.

<!-- TODO [new_backend]: finish release notes, explain new mount stuff -->

- Implement [hooks](../decisions/4_partially_implemented/hooks.md). _(Maximilian Irlinger @atmaxinger)_
- Removed old global plugins code. _(Maximilian Irlinger @atmaxinger)_
- New backend logic, based on PR #2969 by @vLesk _(@kodebach)_

### Copy-on-Write within `libelektra-core`

Thanks to _(Maximilian Irlinger @atmaxinger)_ our `Key` and `KeySet` datastructures are now fully copy-on-write!
This means noticeably reduced memory usage for cases where keys and keysets are copied and/or duplicated!

We run some very promising benchmarks, each were performed with 400,000 keys.
All benchmarks were executed using `valgrind --tool=massif --time-unit=B --max-snapshots=200 --threshold=0.1`.

| Benchmark      | Old Implementation | Copy-on-Write | Size Reduction | Remarks                    |
| :------------- | -----------------: | ------------: | -------------: | :------------------------- |
| `createkeys.c` |            5.3 MiB |       6.5 MiB |          -22 % |                            |
| `deepdup.c`    |           10.5 MiB |       8.2 MiB |           22 % |                            |
| `large.c`      |           18.9 MiB |      15.3 MiB |           19 % |                            |
| `kdb.c`        |           23.5 MiB |      17.8 MiB |           24 % |                            |
| `kdbget.c`     |           11.0 MiB |       8.8 MiB |           20 % |                            |
| `kdbmodify.c`  |           11.0 MiB |       8.8 MiB |           20 % | Same results as `kdbget.c` |

First, it should be noted that a single key, without counting payload, is about 50% larger with the copy-on-write implementation.
This explains why the `createkeys.c` benchmark yields a negative reduction result.
This benchmark only allocates keys, so not much improvement can be expected there.
Still, as other stuff also uses heap memory, the overall memory consumption only increased by 22%, which is far less than 50%.

All other benchmarks saw meaningful reductions of heap memory used.
One interesting observation is that `kdbget.c` and `kdbmodify.c` used exactly the same memory.
This can most likely be explained by internal caching within the memory allocator of `glibc`.

We also performed runtime tests on the same benchmarks using `perf stat --repeat 13` to ensure no major performance regressions occur.

| Benchmark      | Old Implementation | Deviation | Copy-on-Write | Deviation | Runtime Increase |
| :------------- | -----------------: | --------: | ------------: | --------: | ---------------: |
| `createkeys.c` |         0.209572 s |    0.36 % |     0.21987 s |    0.77 % |            4.9 % |
| `deepdup.c`    |          0.23025 s |    0.47 % |    0.231804 s |    0.32 % |            0.6 % |
| `large.c`      |          1.14038 s |    0.21 % |     1.14837 s |    0.21 % |            0.7 % |
| `kdb.c`        |           1.9270 s |    2.63 % |     1.93354 s |    0.17 % |            0.3 % |
| `kdbget.c`     |         0.145663 s |    0.17 % |     0.15763 s |    0.70 % |            8.2 % |
| `kdbmodify.c`  |         0.146506 s |    0.19 % |    0.156347 s |    0.15 % |            6.7 % |

Overall, the runtime performance hit is less than 10%.
The more a program does, the less the additional overhead of the copy-on-write algorithms matter.
One interesting detail is that `keyCopy` and `keyDup` have become quite a bit faster.
This can be seen by comparing the differences between `createkeys.c` and `deepdup.c`.
The differences are 21 ms for the old implementation and 12 ms for the copy-on-write implementation.

### <<HIGHLIGHT>>

### <<HIGHLIGHT>>

## Plugins

The following text lists news about the [plugins](https://www.libelektra.org/plugins/readme) we updated in this release.

### list

- Removed the `list` plugin. _(Maximilian Irlinger @atmaxinger)_

### logchange

- Made logchange a notification-send hook plugin _(Maximilian Irlinger @atmaxinger)_
- <<TODO>>
- <<TODO>>

### website

- Fix broken /pythongen link on homepage _(@stefnotch)_
- Fix redirect logic to not cause loops _(@stefnotch)_
- <<TODO>>

### uname

- Add error handling if uname call fails _(Richard Stöckl @Eiskasten)_
- <<TODO>>

### quickdump

- elektraQuickdumpSet: don't fclose if stdout _(@hannes99)_

### <<Plugin>>

- <<TODO>>
- <<TODO>>
- <<TODO>>

### mmapstorage

- Remove code duplication in the data block calculation _(Richard Stöckl @Eiskasten)_
- <<TODO>>
- <<TODO>>

### <<Plugin>>

- <<TODO>>
- <<TODO>>
- <<TODO>>

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

- <<TODO>>
- The Key and KeySet datastructures are now fully copy-on-write. _(Maximilian Irlinger @atmaxinger)_
- `keyCopy` now only allocates additional memory if `KEY_CP_META` or `KEY_CP_ALL` is used. _(Maximilian Irlinger @atmaxinger)_
- <<TODO>>
- <<TODO>>
- Check for circular links (overrides) _(@0x6178656c)_
- <<TODO>>
- <<TODO>>
- <<TODO>>

### io

- Check file flags for elektraIoFdSetFlags: file flags must be exactly one of: read only, write only or read write _(Richard Stöckl @Eiskasten)_
- <<TODO>>
- <<TODO>>

### <<Library>>

- <<TODO>>
- <<TODO>>
- <<TODO>>

### <<Library>>

- <<TODO>>
- <<TODO>>
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

### intercept/env

- Remove fallback code. _(Markus Raab)_
- Command-line functionality broken due to new-backend differences.
- <<TODO>>
- <<TODO>>

### intercept/fs

- Use `KDB_MAX_PATH_LENGTH` for better portability. _(Markus Raab)_
- <<TODO>>
- <<TODO>>

### jna

- Documentation: Improved build instructions _(@Bujuhu)_
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

### kdb

- Removed `global-mount` and `global-umount` commands. _(Maximilian Irlinger @atmaxinger)_
- <<TODO>>
- Fixed SIGSEGV when using find without argument _(Christian Jonak-Moechel @joni1993)_

### elektrad

- Removed leftover package-lock.json file _(stefnotch)_
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

- Added [automatic spelling corrections](https://master.libelektra.org/scripts/spelling.sed) for changeset. _(Maximilian Irlinger @atmaxinger)_
- Introduce shebang-conventions _(@0x6178656c)_
- Apply auto-fixes from shellcheck _(@0x6178656c)_
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- Sed: Add spelling correction for "key-value storage" _(@Bujuhu)_
- <<TODO>>
- Fix/extends some shell recorder tests _(@Joni1993)_
- <<TODO>>
- <<TODO>>
- Fix warning parsing in shell recorder _(@Joni1993)_
- <<TODO>>
- <<TODO>>
- <<TODO>>
- Add audit-dependencies script to check for vulnerabilities for npm depndencies _(Juri Schreib @Bujuhu)_ _(Nikola Prvulovic @Dynamichost96)_
- <<TODO>>
- <<TODO>>

## Documentation

- Improve page on compilation _(@0x6178656c)_
- Improve page for bindings _(@0x6178656c)_
- Improve page for getting started _(@stefnotch)_
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- Fix grammar for `elektra-granularity.md` _(@dtdirect)_
- Rephrase sections in doc/dev/error-\* _(@dtdirect)_
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- Fix internal links _(@0x6178656c)_
- <<TODO>>
- <<TODO>>
- Update AUR Link from `elektra` to `libelektra` package _(@Bujuhu)_
- <<TODO>>
- <<TODO>>
- Update example ansible playbook in VISION.md _(@Bujuhu)_
- <<TODO>>
- <<TODO>>
- <<TODO>>
- documentation: fix some minor mistakes in CONTRIBUTING.md _(@Joni1993)_
- <<TODO>>
- <<TODO>>
- <<TODO>>
- Denoted pacakage names & global variable names in [INSTALL.md](../INSTALL.md) as `Code` _(@janldeboer)_
- <<TODO>>
- Improve readability of doc/tutorials/highlevel.md _(@deoknats861)_
- Improve reference to Podman documentation _(@0x6178656c)_
- <<TODO>>
- <<TODO>>
- Unify spelling of documentation _(@Joni1993)_
- <<TODO>>
- Fix typo in dev/hooks.md _(@dtdirect)_
- <<TODO>>
- <<TODO>>
- <<TODO>>
- Fixed Coverage Badge Link _(@janldeboer)_
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- Move note in GETSTARTED.md _(@Joni1993)_
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

- Documented [decision process](../decisions/3_decided/decision_process.md) _(Markus Raab)_
- Decided future [library split](../decisions/3_decided/library_split.md) _(@kodebach)_
- decided [decision process](https://www.libelektra.org/decisions/decision-process) _(Markus Raab)_
- draft for [man pages](../decisions/0_drafts/man_pages.md) _(Markus Raab)_
- <<TODO>>
- Add decision for [change tracking](../decisions/0_drafts/change_tracking.md) _(Maximilian Irlinger @atmaxinger)_
- <<TODO>>
- Create [decision](../decisions/0_drafts/operation_sequences.md) for allowed and prohibited operation seqences _(Maximilian Irlinger @atmaxinger)_
- <<TODO>>
- Add decisions about [location of headers](../decisions/3_decided/header_file_structure.md) and [use of `#include`](../decisions/3_decided/header_include.md) in the repo _(@kodebach)_
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- Many small fixes to adapt to documentation guidelines and new decision process. _(Markus Raab)_
- <<TODO>>
- Add decision for [read-only keynames](../decisions/0_drafts/readonly_keynames.md) _(Maximilian Irlinger @atmaxinger)_
- <<TODO>>
- <<TODO>>
- <<TODO>>
- Add decision for [copy-on-write](../decisions/2_in_progress/copy_on_write.md) and provide implementation suggestions. _(Maximilian Irlinger @atmaxinger)_
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- Update [internal cache](../decisions/3_decided/internal_cache.md) _(Markus Raab)_
- Move [internal cache](../decisions/3_decided/internal_cache.md) back to draft _(@kodebach)_
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- Replace TOC-style [README.md](../decisions/README.md) with folders and generate HTML for website _(@kodebach)_
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- Added [Documentation Guidelines](https://master.libelektra.org/doc/contrib/documentation.md) _(Markus Raab)_
- Add links and formatting to documents affected by PR#4492 (Document Guidelines) and rephrase some parts. _(Florian Lindner @flo91)_
- Decisions for changes to `keyIsBelow` and new `keyGetNextPart` functions _(@kodebach)_
- Apply fix spelling to more files. _(Markus Raab)_

### Tutorials

- Add tutorial for manual installation from the AUR on Arch Linux _(@Bujuhu)_
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- Reinstate mounting tutorial _(@Bujuhu)_
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
- Added links to the website & webui after further reading. _(Philipp Nirnberger @nirnberger)_
- <<TODO>>

## Tests

- Fix an Issue where scripts/dev/fix-spelling does not work, if a resolved path contains whitespaces _(Juri Schreib @Bujuhu)_ _(Nikola Prvulovic @Dynamichost96)_
- Rename scripts/sed to [scripts/spelling.sed](https://master.libelektra.org/scripts/spelling.sed) _(Juri Schreib @Bujuhu)_ _(Nikola Prvulovic @Dynamichost96)_
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

- Fix warning for CMP0115 _(0x6178656c)_
- <<TODO>>
- Fix developer warning for package DISCOUNT _(Dennis Toth @dtdirect)_
- Pass `--stacktrace` to gradle for the JNA builds. _(Maximilian Irlinger @atmaxinger)_
- <<TODO>>
- <<TODO>>
- Adapt npm build flags to remove reproducability issues _(Juri Schreib @Bujuhu)_ _(Nikola Prvulovic @Dynamichost96)_
- <<TODO>>

### Docker

- Update packagename `libpcrec++-dev` to `libpcrecpp0v5` in Debian Sid. _(Richard Stöckl @Eiskasten)_
- <<TODO>>
- <<TODO>>
- Add shellcheck to Debian containers. _(@0x6178656c)_
- Use `openjdk-17-jdk` in Debian Sid. _(Maximilian Irlinger @atmaxinger)_
- <<TODO>>
- Update Alpine Linux to 3.16.3. _(Mihael Pranjić @mpranj)_
- Add Fedora 37 images. _(Mihael Pranjić @mpranj)_

## Gradle

- Use Gradle 7.5.1. _(Mihael Pranjić @mpranj)_

## Infrastructure

### Jenkins

- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- Add Fedora 37 builds, drop Fedora 35 builds. _(Mihael Pranjić @mpranj)_
- <<TODO>>
- <<TODO>>
- Run more tests also on Master. _(Markus Raab)_
- Move doc to main build stage. _(Markus Raab)_
- <<TODO>>
- <<TODO>>
- <<TODO>>
- Upgrade Jenkins node container to Debian bullseye _(@0x6178656c)_
- <<TODO>>
- <<TODO>>
- <<TODO>>

### Cirrus

- Use Fedora 37. _(Mihael Pranjić @mpranj)_
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>

### GitHub Actions

- Add auto-cancellation-running action. _(Tomislav Makar @tmakar)_
- <<TODO>>
- <<TODO>>
- <<TODO>>
- Change stale issue/PR checking to GitHub action. _(@0x6178656c)_
- <<TODO>>

## Website

The website is generated from the repository, so all information about plugins, bindings and tools are always up-to-date. Furthermore, we changed:

- <<TODO>>
- <<TODO>>
- Update npm packages. _(Mihael Pranjić @mpranj)_
- <<TODO>>
- <<TODO>>
- <<TODO>>

## Outlook

We are currently working on following topics:

- Session recording and better Ansible integration _(Maximilian Irlinger @atmaxinger)_
- Change tracking. _(Maximilian Irlinger @atmaxinger)_
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
