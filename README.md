# Elektra

[![Release](https://img.shields.io/github/release/ElektraInitiative/libelektra.svg)](https://github.com/ElektraInitiative/libelektra/releases/latest)
[![Jenkins Build Status](https://img.shields.io/jenkins/t/https/build.libelektra.org/job/libelektra/job/master.svg)](https://build.libelektra.org/job/libelektra/job/master/lastBuild)
[![macOS Build Status](https://github.com/ElektraInitiative/libelektra/actions/workflows/macOS.yml/badge.svg)](https://github.com/ElektraInitiative/libelektra/actions/workflows/macOS.yml)
[![Cirrus Build Status](https://api.cirrus-ci.com/github/ElektraInitiative/libelektra.svg)](https://cirrus-ci.com/github/ElektraInitiative/libelektra)
[![Coverage Status](https://coveralls.io/repos/github/ElektraInitiative/libelektra/badge.svg?branch=master)](https://coveralls.io/github/ElektraInitiative/libelektra?branch=master)

_Elektra serves as a universal and secure framework to access configuration
settings in a global, hierarchical key database._

<img src="https://cdn.rawgit.com/ElektraInitiative/libelektra/master/doc/images/logo/logo_color.svg" alt="Elektra" width="150" />

Elektra provides a mature, consistent and easily comprehensible API.
Its modularity effectively avoids code duplication across applications
and tools concerning their configuration tasks. Elektra abstracts from
cross-platform-related issues and enables applications to be aware of other
applications' configurations, leveraging easy application integration.

## Often Used Links

- If you are new, start reading [Get Started](doc/GETSTARTED.md)
- If you enjoy working with docker take a look at [our](/scripts/docker/README.md) docker images.
- [Build server](https://build.libelektra.org/)
- [Website](https://www.libelektra.org)
- [API documentation](https://doc.libelektra.org/api/master/html/)

## Overview

Elektra provides benefits for:

1. _Application Developers_ by making it easier to access configuration settings in a modular, reliable, and extensible way.
2. _System Administrators_ by making it possible to access configuration settings in the same way applications access them.
3. _Everyone_ by making application integration possible and less misconfiguration a reality.

Elektra consists of three parts:

1. _LibElektra_ is a modular configuration access toolkit to
   construct and integrate applications into a global,
   hierarchical key database. The building blocks are:
   - language bindings (inclusive high-level interfaces)
   - GenElektra, the code generator for type-safe bindings
   - plugins for configuration access behavior and validation
2. _SpecElektra_ is a configuration specification language
   that is easy to use and self-contained in the same key database (i.e.
   written in any of the configuration file formats Elektra supports).
3. Tools on top of LibElektra for system administrators, such as
   CLI tools, web UIs, and GUIs.

To highlight a few concrete things about Elektra, configuration settings can come from any
data source, but usually comes from configuration files that are [_mounted_](doc/help/elektra-mounting.md) into Elektra
similar to mounting a file system. Elektra is a plugin-based framework, for example,
plugins implement various configuration formats like INI, JSON, XML, etc.
There is a lot more to discover like executing scripts (`python`, `lua` or
`shell`) when a configuration value changes, or, enhanced validation plugins that will not
allow corrupted configuration settings to reach your application.

As an application developer you get instant access to various configuration formats and the ability
to fallback to default configuration settings without having to deal with this on your own. As an system administrator
you can choose your favorite configuration format and _mount_ this configuration for the application.
_Mounting_ enables easy application integration as any application using Elektra can access any _mounted_
configuration. You can even _mount_ `/etc` files such as `hosts` or `fstab`, so that there is no need to
configure the same values twice in different files.

In case you are worried about linking to such a powerful library. The core is a small library
implemented in C, works cross-platform, and does not need any external dependencies. There are
[bindings](src/bindings) for other languages in case C is too low-level for you.

## Contact

Do not hesitate to ask any question on
[GitHub issue tracker](https://issues.libelektra.org/)
or directly to one of the [authors](doc/AUTHORS.md).

## Quickstart

### Installation

The preferred way to install Elektra is by using packages provided for
your distribution, see [INSTALL](/doc/INSTALL.md) for available packages and alternative ways for installation.

> Note: It is preferable to use a recent version: They contain many bug fixes and usability improvements.

### Usage

Now that we have Elektra installed, we can start:

- using the [kdb command](/doc/help/kdb.md),
- using [qt-gui](/src/tools/qt-gui/) for people preferring graphical user interfaces, and
- using [web-ui](/src/tools/webui/) for people preferring web user interfaces.

### Documentation

To get an idea of Elektra, you can take a look at the
[presentation](https://www.libelektra.org/ftp/elektra/presentations/2016/FOSDEM/fosdem.odp).

In the GitHub repository the full documentation is available, including:

- [tutorials](/doc/tutorials/),
- [FAQ](/doc/help/elektra-faq.md),
- [glossary](/doc/help/elektra-glossary.md), and
- [concepts and man pages](/doc/help/elektra-introduction.md)

You can read the documentation for the kdb tool, either

- [on the Website](https://www.libelektra.org/man-pages/kdb)
- [in the API documentation](https://doc.libelektra.org/api/master/html/doc_help_kdb_md.html)
- by using `man kdb`
- by using `kdb --help` or `kdb help <command>`
- [on GitHub](https://master.libelektra.org/doc/help/kdb.md)

> Note: All these ways to read the documentation provide the same content,
> all generated from the GitHub repository.

## Facts and Features

- Elektra uses simple key-value pairs.
- Elektra uses the [BSD licence](LICENSE.md).
- Elektra implements an [API](https://doc.libelektra.org/api/master/html/) to fully access a global key database.
- Elektra can be thought of a [virtual file system for configuration](/doc/BIGPICTURE.md).
- Elektra supports mounting of existing configuration files into a global key database.
- Elektra has dozens of [Plugins](src/plugins/) that make it possible
  to have a tiny core, but still support many features, including:
  - Elektra can import and export configuration files in any [supported format](src/plugins/).
  - Elektra is able to log and notify other software on any configuration changes, for example,
    using [Dbus](src/plugins/dbus/) and [Journald](src/plugins/journald/).
  - Elektra can improve robustness by rejecting invalid configuration via [type checking](src/plugins/type/), [regex](src/plugins/validation/) and more.
  - Elektra provides different mechanisms to [locate configuration files](src/plugins/resolver/).
  - Elektra supports different ways to [escape](src/plugins/ccode/) and [encode](src/plugins/iconv/) content of configuration files.
- Elektra is multi-process safe and can be used in multi-threaded programs.
- Elektra (except for some [plugins](src/plugins/)) is portable and completely written in ANSI C99.
- Elektra (except for some [plugins](src/plugins/)) has no external dependency.
- Elektra is suitable for embedded systems and early boot stage programs.
- Elektra provides many powerful [Bindings](src/bindings) to avoid low-level access code.
- Elektra provides powerful [Code Generation Techniques](src/tools/pythongen) for high-level configuration access.

## News

Go to the [website](https://www.libelektra.org), see the [news](doc/news/), and its [RSS feed](https://www.libelektra.org/news/feed.rss).

## Download

Elektra uses a [Git repository at GitHub](https://github.com/ElektraInitiative/libelektra).

You can clone the latest version of Elektra by running:

```sh
git clone https://github.com/ElektraInitiative/libelektra.git
```

Releases can be downloaded from [here](https://www.libelektra.org/ftp/elektra/releases/).

## Build Server

The [build server](https://build.libelektra.org/) builds
Elektra for every pull request and on every commit in various ways and also produces [LCOV code
coverage report](https://doc.libelektra.org/coverage/master/debian-buster-full/).

## Contributing

Take a look at [how to start contributing](doc/IDEAS.md).

## Goals

- Make developer's life easier by proving a well-tested mature library
  instead of rolling your own configuration system for every application.
  This reduces rank growth of configuration systems (including but not limited
  to configuration file parsers) in our ecosystem and fosters well-maintained
  plugins instead.
- Postpone configuration decisions (such as which configuration files to use)
  from developers to system administrators and package maintainers to
  provide an overall more consistent and user-friendly system.
  (Default behavior of applications still is in control of developers,
  you can even roll your own plugins to provide exactly the same behavior
  as your application has now.)
- Make configuration storage more safe: avoid that applications
  receive wrong or unexpected values that could lead to undefined behavior.

And in terms of quality, we want:

1. Simplicity (make configuration tasks, like access of configuration settings, simple),
2. Robustness (no undefined behavior of applications), and
3. Extensibility (gain control over configuration access)

[Continue reading about the goals of Elektra](doc/GOALS.md)
