# libelektra #

_Elektra provides a universal and secure framework to store configuration
parameters in a global, hierarchical key database._

<img src="https://cdn.rawgit.com/ElektraInitiative/libelektra/master/doc/images/circle.svg" alt="Elektra" width="50" />

The core is a small library implemented in C. The plugin-based framework fulfills many
configuration-related tasks to avoid any unnecessary code duplication
across applications while it still allows the core to stay without any
external dependency. Elektra abstracts from cross-platform-related issues
with an consistent API, and allows applications to be aware of other
applications' configurations, leveraging easy application integration.

[Why should I use Elektra?](#goals)


## Contact ##

Do not hesitate to ask any question on [github issue tracker](https://github.com/ElektraInitiative/libelektra/issues), [Mailing List](https://lists.sourceforge.net/lists/listinfo/registry-list)
or one of the [authors](doc/AUTHORS).


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
[download elektra directly](#download) or [compile it yourself](#compile).

### Usage ###

Now that we have Elektra installed, we can start using the `kdb` command and
the `qt-gui`.

The easiest way to use Elektra is running the `qt-gui`:

```bash
kdb qt-gui
```

Or you can use the `kdb` command to configure your applications:

```bash
kdb set user/env/override/HTTP_PROXY "http://my.proxy:8080"
```

This will set the `HTTP_PROXY` environment variable to `http://my.proxy:8080`.
Configuration can be retrieved with `kdb get`:

```bash
kdb get user/env/override/HTTP_PROXY
```

For more information about elektrified environment variables, see
[src/libgetenv/README.md](src/libgetenv/README.md)


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

1. Robustness (no undefined behaviour of applications),
2. Extensibility (gain control over configuration access) and
3. Simplicity (make configuration tasks simple)

[Read more about the goals of Elektra](doc/GOALS.md)


## Facts and Features ##

 * Elektra uses the [BSD licence](doc/COPYING).
 * Elektra implements an [API](http://doc.libelektra.org/api/latest/html/) to fully access a global key database.
 * Elektra can be thought of a virtual file system for configuration.
 * Elektra supports mounting of existing configuration files into the global key database.
 * Elektra has dozens of [Plugins](src/plugins#introduction) that make it possible
   to have a tiny core, but still support many features, including:
   * Elektra can import and export configuration files in any [supported format](src/plugins#storage).
   * Elektra is able to log and notify other software on any configuration changes, e.g., using [Dbus](src/plugins/dbus) and [Journald](src/plugins/journald).
   * Elektra can improve robustness by rejecting invalid configuration via [type checking](src/plugins/type), [regex](src/plugins/validation) and more.
   * Elektra provides different mechanisms to [locate configuration files](src/plugins/resolver).
   * Elektra supports different ways to [escape](src/plugins/ccode) and [encode](src/plugins/iconv) content of configuration files.
 * Elektra is multi-process safe and can be used in multi-threaded programs.
 * Elektra (except for some [plugins](src/plugins#introduction)) is portable and completely written in Ansi-C99.
 * Elektra (except for some [plugins](src/plugins#introduction)) has no external dependency.
 * Elektra is suitable for embedded systems and early boot stage programs.
 * Elektra uses simple key/value pairs that include metadata for any other information.
 * Elektra provides many powerful [Bindings](src/bindings) to avoid low-level access code.
 * Elektra provides powerful [Code Generation Techniques](src/tools/gen) for high-level configuration access.


## News ##

 - [17 Sep 2015 0.8.13](http://doc.libelektra.org/news/3c00a5f1-c017-4555-92b5-a2cf6e0803e3.html) adds elektrify-getenv
 - [12 Jul 2015 0.8.12](http://doc.libelektra.org/news/98770541-32a1-486a-98a1-d02f26afc81a.html) adds dir namespace
 - [03 Apr 2015 0.8.11](http://doc.libelektra.org/news/7d4647d4-4131-411e-9c2a-2aca39446e18.html) adds spec namespace
 - [02 Dec 2014 0.8.10](http://doc.libelektra.org/news/6ce57ecf-420a-4a31-821e-1c5fe5532eb4.html) adds XDG/OpenICC compatibility
 - [04 Nov 2014 0.8.9](http://doc.libelektra.org/news/38640673-3e07-4cff-9647-f6bdd89b1b08.html) adds qt-gui
 - [02 Sep 2014 0.8.8](http://doc.libelektra.org/news/eca69e19-5ddb-438c-ac06-57c20b1a9160.html) adds 3-way merging

Also see [News](doc/NEWS.md) and its [RSS feed](http://www.libelektra.org/news/feed.rss).


## Further Information ##

To get an introduction, it is best to take a look at the
[presentation](http://www.libelektra.org/ftp/elektra/presentations/2012/lgm.odp),
read our
[tutorials](doc/tutorials),
see the
[poster](http://www.libelektra.org/ftp/elektra/poster.pdf)
and read the
[abridgment](http://www.libelektra.org/ftp/elektra/abridgement.pdf).

The currently best information about Elektra is
[this thesis](http://www.libelektra.org/ftp/elektra/thesis.pdf).

The API documentation can be found
[here](http://doc.libelektra.org/api/current/html).


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
For Debian stable amd64 we provide latest builds. Just add following lines to
sources.list in wheezy:

        deb     [trusted=yes] ftp://markus-raab.org/wheezy wheezy main
        deb-src [trusted=yes] ftp://markus-raab.org/wheezy wheezy main

If there are no packages available for your distribution, see the
[installation document](doc/INSTALL.md).

### Download ###

Elektra's uses a [git repository at github](https://github.com/ElektraInitiative/libelektra).

You can clone the latest version of Elektra by running: `git clone https://github.com/ElektraInitiative/libelektra.git`

Releases can be downloaded from [http](http://www.libelektra.org/ftp/elektra/releases/) and

         ftp://ftp.libelektra.org/elektra/releases/

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


## Build Server ##

The [build server](http://build.libelektra.org:8080/) builds
Elektra on every commit in various ways and also produces [LCOV code
coverage report](http://doc.libelektra.org/coverage/latest).

To use the debian repository of the latest builds from master put following files in
/etc/apt/sources.list:

         deb     [trusted=yes] http://build.libelektra.org/debian/ wheezy main
         deb-src [trusted=yes] http://build.libelektra.org/debian/ wheezy main


## Develop ##

To start development, just clone the repo and start hacking!

- We encourage you to improve documentation, especially the README.md
  as if they were a webpage.
- You should read the [coding document](doc/CODING.md) before you issue a
  pull request.
- Make yourself familiar with the [KeySet](http://doc.libelektra.org/api/latest/html/group__keyset.html),
  the central data structure in Elektra.
- You should read the [design document](doc/DESIGN.md) before you make
  design relevant decisions.
- You can always peek into the [TODOs](doc/todo), if you don't know
  what to do.
