# 0.9.<<VERSION>> Release

- guid: 9e1e0790-e7ae-4947-9906-2c176a4f8dd2
- author: Mihael Pranjić
- pubDate: Thu, 16 Mar 2023 14:43:04 +0100
- shortDesc: Bugfix Release

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

- The main purpose of this bugfix release is to fix regressions ([#4859](https://github.com/ElektraInitiative/libelektra/issues/4859)) introduced in Elektra 0.9.12 and 0.9.13.
- Please refer to the [Elektra 0.9.12](https://www.libelektra.org/news/0.9.12-release) release notes for a complete list of changes. Due to breaking changes since 0.9.11 we highly recommend to read the upgrade instructions.

## Plugins

The following text lists news about the [plugins](https://www.libelektra.org/plugins/readme) we updated in this release.

### timeofday

- Use separate symbols for `set` and `commit` functions to satisfy `kdb plugin-check` _(@kodebach)_
- Use new `elektraPluginGetPhase()` instead of counting executions _(@kodebach)_

### tracer

- Use separate symbols for `set` and `commit` functions to satisfy `kdb plugin-check` _(@kodebach)_

## Tests

- Enable more `kdb plugin-check` tests _(@kodebach)_

## Build

### Docker

- Fix conflicting Java versions in CentOS Stream 8 image _(@kodebach)_

## Outlook

We are currently working on following topics:

- 1.0 API _(Klemens Böswirth @kodebach)_ and _(Stefan Hanreich)_
- Session recording and better Ansible integration _(Maximilian Irlinger @atmaxinger)_
- Change tracking _(Maximilian Irlinger @atmaxinger)_
- Rewriting tools in C _(@hannes99)_
- Elektrify KDE and GNOME _(Mihael Pranjić @mpranj)_
- Elektrify XFCE _(Richard Stöckl @Eiskasten)_
- Mounting SQL databases _(Florian Lindner @flo91)_
- Recording Configuration _(Maximilian Irlinger)_
- Ansible-Elektra _(Lukas Hartl)_ and _(Maximilian Irlinger)_
- Configure Olimex Base Images _(Maximilian Irlinger)_
- Improving Build Server Infrastructure _(Lukas Hartl)_ and _(Maximilian Irlinger)_
- Improve Java Development Experience _(Michael Tucek)_

## Statistics

We closed [2 issues](https://github.com/ElektraInitiative/libelektra/milestone/35?closed=1) for this release.

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
