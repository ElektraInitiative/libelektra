Elektra provides a universal and secure framework to store configuration
parameters in a global, hierarchical key database.  The core is a small
library implemented in C. The plugin-based framework fulfills many
configuration-related tasks to avoid any unnecessary code duplication
across applications while it still allows the core to stay without any
external dependency. Elektra abstracts from cross-platform-related issues
with an consistent API, and allows applications to be aware of other
applications' configurations, leveraging easy application integration.

[Read more about Goals ..](doc/GOALS.md)

![Elektra](doc/images/circle.jpg)

## Facts and Features ##

 * Elektra uses the [BSD licence](doc/COPYING).
 * Elektra implements an [API](http://doc.libelektra.org/api/latest/html/) to fully access a global key database.
 * Elektra supports mounting of existing configuration files into the global key database.
 * Elektra has dozens of [Plugins](src/plugins#introduction) that make it possible
   to have a tiny core, but still support all features.
   * Elektra can import and export configuration files in any [supported format](src/plugins#storage).
   * Elektra is able to log and notify other software on any configuration changes using [Dbus](src/plugins/dbus) and [Journald](src/plugins/journald).
   * Elektra can improve robustness by rejecting invalid configuration via [type checking](src/plugins/type), [regex](src/plugins/regex) and more.
   * Elektra provides different mechanisms to [locate configuration files](src/plugins/resolver).
   * Elektra supports different ways to [escape](src/plugins/ccode) and [encode](src/plugins/iconv) content of configuration files.
 * Elektra is multi-process safe and can be used in multi-threaded programs.
 * Elektra (except for some [plugins](src/plugins#introduction)) is portable and completely written in Ansi-C99.
 * Elektra (except for some [plugins](src/plugins#introduction)) has no external dependency.
 * Elektra is suitable for embedded systems and early boot stage programs.
 * Elektra supports comments and other non-configuration information by meta data.
 * Elektra provides many powerful [Bindings](src/bindings) to avoid low-level access code.
 * Elektra provides powerful [Code Generation Techniques](src/tools/gen) for Configuration Access.


## Further Information ##

To get an introduction, it is best to take a look at the
[presentation](http://www.libelektra.org/ftp/elektra/presentations/2012/lgm.odp),
see the
[poster](http://www.libelektra.org/ftp/elektra/poster.pdf)
and read the
[abridgment](http://www.libelektra.org/ftp/elektra/abridgement.pdf).

See [News](doc/NEWS.md) what is happening at the moment.
During the summer you can read the [GSoC 2014 blog](http://community.libelektra.org/wp)!

The currently best information about Elektra is
[this thesis](http://www.libelektra.org/ftp/elektra/thesis.pdf).

The API documentation can be found
[here](http://doc.libelektra.org/api/current/html).



## Contact ##

Do not hesitate to ask any question on [Mailing List](https://lists.sourceforge.net/lists/listinfo/registry-list)
or one of the [authors](doc/AUTHORS).


# Get Started #

## Download ##

Elektra's uses a [git repository at github](https://github.com/ElektraInitiative/libelektra).

Releases can be downloaded from [http](http://www.libelektra.org/ftp/elektra/releases/) and

         ftp://ftp.libelektra.org/elektra/releases/

To use the debian repository of the releases put following files in
/etc/apt/sources.list:

         deb     http://build.libelektra.org/debian/ elektra-release-glue main
         deb-src http://build.libelektra.org/debian/ elektra-release-glue main

The [build server](http://build.libelektra.org:8080/) builds release
and master branches on every commit and also produces [LCOV code
coverage report](http://doc.libelektra.org/coverage/latest).


## Compile ##

See this [document](doc/COMPILE.md) for documentation how to compile the software.
You might find [configure](configure) useful: It will print the cmake
commando you need when you are used to `./configure`.


## Install ##

The preferred way to install Elektra is by using packages provided for
your distribution:
 - [Fedora](https://admin.fedoraproject.org/pkgdb/package/elektra/)
 - [Gentoo](http://packages.gentoo.org/package/app-admin/elektra)
 - [Arch Linux](https://aur.archlinux.org/packages/elektra/)

Available, but not up-to-date (Version 0.7):
 - [Mageia](http://svnweb.mageia.org/packages/updates/1/elektra/)
 - [Ubuntu](https://launchpad.net/ubuntu/+source/elektra)
 - [Debian](https://packages.debian.org/de/wheezy/libelektra3)
 - [Linux Mint](http://community.linuxmint.com/software/view/elektra)

For [CentOS, Fedora, OpenSUSE, RHEL and SLE](http://software.opensuse.org/download.html?project=home%3Abekun&package=elektra)
Kai-Uwe Behrmann kindly provides packages.
For Debian stable we provide latest builds. Just add following lines to
sources.list in wheezy:

        deb ftp://markus-raab.org/wheezy wheezy main
        deb-src ftp://markus-raab.org/wheezy wheezy main

If there are no packages available for your distribution, see the
[installation document](doc/INSTALL).

## Develop ##

To start development, just clone the repo and start hacking!

- We encourage you to improve documentation, especially the README.md
  as if they were a webpage.
- You should read the [coding document](doc/CODING.md) before you issue a
  pull request.
- Make yourself familiar with the [KeySet](http://doc.libelektra.org/api/latest/html/group__keyset.html).
- You should read the [design document](doc/DESIGN.md) before you make
  design relevant decisions.
- You can always peek into the [TODOs](doc/todo), if you don't know
  what to do.

