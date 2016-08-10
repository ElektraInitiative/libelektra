# libelektra #

_Elektra serves as a universal and secure framework to access configuration
parameters in a global, hierarchical key database._

<img src="https://cdn.rawgit.com/ElektraInitiative/libelektra/master/doc/images/circle.svg" alt="Elektra" width="150" />

Elektra provides a mature, consistent and easily comprehensible API.
Its modularity effectively avoids code duplication across applications
and tools regarding configuration tasks. Elektra abstracts from
cross-platform-related issues and allows applications to be aware of other
applications' configurations, leveraging easy application integration.

## Overview

Elektra consists of three parts:

1. *LibElektra* is a modular configuration access toolkit to
   construct and integrate applications into a global,
   hierarchical key database. The building blocks are:
   - language bindings (inclusive high-level interfaces)
   - GenElektra, the code generator for type-safe bindings
   - plugins for configuration access behaviour and validation
2. *SpecElektra* is a configuration specification language
   that is easy to use and self-contained in the same key database (i.e.
   written in any of the configuration file formats Elektra supports).
3. Tools on top of LibElektra for administrators, such as CLI tools and GUIs.

To highlight a few concrete things about Elektra, configuration data can come from any
data source, but usually comes from configuration files that are [_mounted_](doc/help/elektra-mounting.md) into Elektra
similar to mounting a file system. As Elektra is a plugin based framework, there are a
lot of _storage plugins_ that support various configuration formats like ini, json, xml,
etc. However, there's a lot more to discover like executing scripts (`python`, `lua` or
`shell`) when a configuration value changes, or, enhanced validation plugins that won't
allow corrupted configuration to reach your application.

As an application developer you get instant access to various configuration formats and the ability
to fallback to a default configuration without having to deal with this on your own. As an administrator
you can choose your favourite configuration format and _mount_ this configuration for the application.
This features easy application integration as any application using Elektra can access any _mounted_
configuration. You can even _mount_ `/etc` files such as `hosts` or `fstab`, so that there is no need to
configure the same data twice in different files.

In case you're worried about linking to such a powerful library. The core is a small library
implemented in C, works cross-platform, and does not need any external dependencies. There are
[bindings](src/bindings) for other languages in case C is too low-level for you.

[Why should I use Elektra?](doc/WHY.md)


## Contact ##

Do not hesitate to ask any question on
[GitHub issue tracker](https://github.com/ElektraInitiative/libelektra/issues),
[Mailing List](https://lists.sourceforge.net/lists/listinfo/registry-list)
or directly to one of the [authors](doc/AUTHORS).


## Quickstart ##

If you want to use Elektra for your application, [read the application integration tutorial](doc/tutorials/application-integration.md).

### Installation ###

The preferred way to install Elektra is by using packages provided for
your distribution. On Debian/Ubuntu, this can be done by running the following
command:

```bash
sudo apt-get install elektra-bin libelektra-dev
```

This will install the Elektra tools as well as everything needed to develop
with Elektra.

If you're not running Debian/Ubuntu, check out the [package list](#packages),
[download elektra directly](#download) or [compile it yourself](#compiling).

It is preferable to use a recent version: They contain many bug fixes and
additional features. See [INSTALL](doc/INSTALL.md) for other ways to install
Elektra.

### Usage ###

Now that we have Elektra installed, we can start using the [kdb command](/doc/help/kdb.md) and
the [qt-gui](/src/tools/qt-gui/).

Here a small demo:

[![asciicast](https://asciinema.org/a/cantr04assr4jkv8v34uz9b8r.png)](https://asciinema.org/a/cantr04assr4jkv8v34uz9b8r)

For import/export/mount formats see [Plugins](src/plugins/).
For information about elektrified environment variables, see
[/src/libgetenv/README.md](/src/libs/getenv/README.md)


### Documentation ###


To get an idea of Elektra, you can take a look at the
[presentation](http://www.libelektra.org/ftp/elektra/presentations/2016/FOSDEM/fosdem.odp).

The full documentation, including
[tutorials](http://libelektra.org/blob/master/doc/tutorials/),
[glossary](/doc/help/elektra-glossary.md), and
[concepts and man pages](/doc/help/elektra-introduction.md)
is available in the GitHub repository.

You can read the documentation for the kdb tool, either

- [on GitHub](http://libelektra.org/blob/master/doc/help/kdb.md)
- [in the API docu](http://doc.libelektra.org/api/latest/html/md_doc_help_kdb.html)
- by using `kdb --help` or `kdb help <command>`
- by using `man kdb`



## Goals ##

- Make it trivial for applications and administrators to access
  any configuration
- Postpone some decisions from programmers to
- Make configuration storage more safe: avoid that applications
  receive wrong or unexpected values that could lead to undefined behaviour.
- Allow software to be better integrated on configuration level
  maintainers/administrators, e.g. which syntax and the location of
  configuration files.
- Reduce rank growth of configuration parsers in our ecosystem, but
  foster well maintained plugins instead.

And in terms of quality, we want:

1. Simplicity (make configuration tasks simple),
2. Robustness (no undefined behaviour of applications), and
3. Extensibility (gain control over configuration access)

[Read more about the goals of Elektra](doc/GOALS.md)


## Facts and Features ##

 * Elektra uses the [BSD licence](doc/COPYING).
 * Elektra implements an [API](http://doc.libelektra.org/api/latest/html/) to fully access a global key database.
 * Elektra can be thought of a virtual file system for configuration.
 * Elektra supports mounting of existing configuration files into the global key database.
 * Elektra has dozens of [Plugins](src/plugins/) that make it possible
   to have a tiny core, but still support many features, including:
   * Elektra can import and export configuration files in any [supported format](src/plugins/).
   * Elektra is able to log and notify other software on any configuration changes, e.g., using [Dbus](src/plugins/dbus/) and [Journald](src/plugins/journald/).
   * Elektra can improve robustness by rejecting invalid configuration via [type checking](src/plugins/type/), [regex](src/plugins/validation/) and more.
   * Elektra provides different mechanisms to [locate configuration files](src/plugins/resolver/).
   * Elektra supports different ways to [escape](src/plugins/ccode/) and [encode](src/plugins/iconv/) content of configuration files.
 * Elektra is multi-process safe and can be used in multi-threaded programs.
 * Elektra (except for some [plugins](src/plugins/)) is portable and completely written in Ansi-C99.
 * Elektra (except for some [plugins](src/plugins/)) has no external dependency.
 * Elektra is suitable for embedded systems and early boot stage programs.
 * Elektra uses simple key/value pairs that include metadata for any other information.
 * Elektra provides many powerful [Bindings](src/bindings) to avoid low-level access code.
 * Elektra provides powerful [Code Generation Techniques](src/tools/gen) for high-level configuration access.


## News ##

 - [14 Jun 2016 0.8.17](http://doc.libelektra.org/news/e6153a39-c4bd-41c3-bc86-785d451eb6c5.html) survey, usability improvements
 - [29 Apr 2016 0.8.16](http://doc.libelektra.org/news/9c9247ee-ee9c-4f4a-a68e-76959def9b82.html) stability improvements
 - [16 Feb 2016 0.8.15](http://doc.libelektra.org/news/1ab4a560-c286-46d2-a058-1a8e7e208fe8.html) lib split, improved mount
 - [19 Nov 2015 0.8.14](http://doc.libelektra.org/news/519cbfac-6db5-4594-8a38-dec4c84b134f.html) adds docu and plugins
 - [17 Sep 2015 0.8.13](http://doc.libelektra.org/news/3c00a5f1-c017-4555-92b5-a2cf6e0803e3.html) adds elektrify-getenv
 - [12 Jul 2015 0.8.12](http://doc.libelektra.org/news/98770541-32a1-486a-98a1-d02f26afc81a.html) adds dir namespace
 - [03 Apr 2015 0.8.11](http://doc.libelektra.org/news/7d4647d4-4131-411e-9c2a-2aca39446e18.html) adds spec namespace
 - [02 Dec 2014 0.8.10](http://doc.libelektra.org/news/6ce57ecf-420a-4a31-821e-1c5fe5532eb4.html) adds XDG/OpenICC compatibility
 - [04 Nov 2014 0.8.9](http://doc.libelektra.org/news/38640673-3e07-4cff-9647-f6bdd89b1b08.html) adds qt-gui
 - [02 Sep 2014 0.8.8](http://doc.libelektra.org/news/eca69e19-5ddb-438c-ac06-57c20b1a9160.html) adds 3-way merging

Also see [News](doc/NEWS.md) and its [RSS feed](http://www.libelektra.org/news/feed.rss).


## Sources ##

### Packages ###

The preferred way to install Elektra is by using packages provided for
your distribution:
 - [Fedora](https://admin.fedoraproject.org/pkgdb/package/elektra/)
 - [Gentoo](http://packages.gentoo.org/package/app-admin/elektra)
 - [Arch Linux](https://aur.archlinux.org/packages/elektra/)
 - [Debian](https://packages.debian.org/de/jessie/libelektra4)
 - [Ubuntu](https://launchpad.net/ubuntu/+source/elektra)

Available, but not up-to-date (Version 0.7):
 - [Mageia](http://svnweb.mageia.org/packages/updates/1/elektra/)
 - [Linux Mint](http://community.linuxmint.com/software/view/elektra)

For [OpenSUSE, CentOS, Fedora, RHEL and SLE](https://build.opensuse.org/package/show/home:bekun:devel/elektra)
Kai-Uwe Behrmann kindly provides packages [for download](http://software.opensuse.org/download.html?project=home%3Abekun%3Adevel&package=libelektra4).
For Debian wheezy and jessie amd64 we provide latest builds. See build server below.

If there are no packages available for your distribution, see the
[installation document](doc/INSTALL.md).

### Download ###

Elektra's uses a [git repository at GitHub](https://github.com/ElektraInitiative/libelektra).

You can clone the latest version of Elektra by running:

         git clone https://github.com/ElektraInitiative/libelektra.git

Releases can be downloaded from [http](http://www.libelektra.org/ftp/elektra/releases/) and
`ftp://ftp.libelektra.org/elektra/releases/`

### Compiling ###

After downloading or cloning Elektra, `cd` to the directory and run the
following commands to compile it:

 * `mkdir -p build`
 * `cd build`
 * `cmake ..`
 * `make`

Then you can use `sudo make install` to install it.

You can also use the [`./configure`](configure) command to generate a `cmake`
command with special options.

For more information, especially how to set CMake Cache, see [here](doc/COMPILE.md).
Make sure to read how to add plugins, tools and bindings.


## Build Server ##

The [build server](http://build.libelektra.org:8080/) builds
Elektra on every commit in various ways and also produces [LCOV code
coverage report](http://doc.libelektra.org/coverage/latest).

To use the debian repository of the latest builds from master put following files in
/etc/apt/sources.list.
For jessie:

        deb     [trusted=yes] http://debian-stable.libelektra.org/elektra-stable/ jessie main
        deb-src [trusted=yes] http://debian-stable.libelektra.org/elektra-stable/ jessie main

For wheezy:

         deb     [trusted=yes] http://build.libelektra.org/debian/ wheezy main
         deb-src [trusted=yes] http://build.libelektra.org/debian/ wheezy main



## Contributing ##

To start development, just clone the repo and start hacking!
We prepared [beginner friendly tasks](https://github.com/ElektraInitiative/libelektra/issues?q=is%3Aissue+is%3Aopen+label%3A%22beginner+friendly%22)
for you.

- We encourage you to improve documentation, especially the README.md
  as if they were a webpage.
- In the source code, you should look into [libs](src/libs/) and [plugins](/src/plugins/).
- You can always peek into the [TODOs](doc/todo), if you don't know
  what to do.
- You should read the [CONTRIBUTING](.github/CONTRIBUTING.md) before you issue a
  pull request.
