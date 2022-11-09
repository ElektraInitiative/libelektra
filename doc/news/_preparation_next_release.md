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

- Implement [hooks](../decisions/4_partially_implemented/hooks.md). _(Maximilian Irlinger @atmaxinger)_
- Removed old global plugins code. _(Maximilian Irlinger @atmaxinger)_

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

### <<Plugin>>

- <<TODO>>
- <<TODO>>
- <<TODO>>

### <<Plugin>>

- <<TODO>>
- <<TODO>>
- <<TODO>>

### <<Plugin>>

- <<TODO>>
- <<TODO>>
- <<TODO>>

### <<Plugin>>

- <<TODO>>
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
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
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

- Added [automatic spelling corrections](https://master.libelektra.org/scripts/sed) for changeset. _(Maximilian Irlinger @atmaxinger)_
- Introduce shebang-conventions _(@0x6178656c)_
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

- Improve page on compilation _(@0x6178656c)_
- Improve page for bindings _(@0x6178656c)_
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
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
- <<TODO>>
- <<TODO>>
- documentation: fix some minor mistakes in CONTRIBUTING.md _(@Joni1993)_
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- <<TODO>>
- Improve reference to Podman documentation _(@0x6178656c)_
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
- Update [internal cache](../decisions/0_drafts/internal_cache.md) _(Markus Raab)_
- Move [internal cache](../decisions/0_drafts/internal_cache.md) back to draft _(@kodebach)_
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
- Added links to the website & webui after further reading. _(Philipp Nirnberger @nirnberger)_
- <<TODO>>

## Tests

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
- <<TODO>>
- Pass `--stacktrace` to gradle for the JNA builds. _(Maximilian Irlinger @atmaxinger)_
- <<TODO>>
- <<TODO>>

### Docker

- Update packagename `libpcrec++-dev` to `libpcrecpp0v5` in Debian Sid. _(Richard Stöckl @Eiskasten)_
- <<TODO>>
- <<TODO>>
- <<TODO>>
- Use `openjdk-17-jdk` in Debian Sid. _(Maximilian Irlinger @atmaxinger)_
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

- <<TODO>>
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
- <<TODO>>
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

Already built API-Docu can be found

- [here](https://doc.libelektra.org/api/0.9.<<VERSION>>/html/) or
- [GitHub](https://github.com/ElektraInitiative/doc/tree/master/api/0.9.<<VERSION>>).

## Stay tuned!

Subscribe to the [RSS feed](https://www.libelektra.org/news/feed.rss) to always get the release notifications.

If you also want to participate, or for any questions and comments, please contact us via our issue tracker [on GitHub](http://issues.libelektra.org).

[Permalink to this NEWS entry](https://www.libelektra.org/news/0.9.<<VERSION>>-release)

For more information, see [https://libelektra.org](https://libelektra.org).

Best regards,
[Elektra Initiative](https://www.libelektra.org/developers/authors)
