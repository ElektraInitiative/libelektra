# Install


## Linux

For the following Linux distributions and package managers 0.8 packages are available:

 - [Arch Linux](https://aur.archlinux.org/packages/elektra/)
 - [Openwrt](https://github.com/openwrt/packages/tree/master/libs/elektra)
 - [OpenSuse](https://software.opensuse.org/package/elektra)
 - [Debian](https://packages.debian.org/de/jessie/libelektra4)
 - [Ubuntu](https://launchpad.net/ubuntu/+source/elektra)
 - [Gentoo](http://packages.gentoo.org/package/app-admin/elektra)
 - [Linux Mint](https://community.linuxmint.com/software/view/elektra-bin)
 - [LinuxBrew](https://github.com/Linuxbrew/homebrew-core/blob/master/Formula/elektra.rb)
 - [Alpine Linux](https://pkgs.alpinelinux.org/package/edge/testing/x86_64/elektra)

Available, but not up-to-date:

 - [Mageia](http://svnweb.mageia.org/packages/updates/1/elektra/)
 - [Fedora](https://admin.fedoraproject.org/pkgdb/package/elektra/)

For [OpenSUSE, CentOS, Fedora, RHEL and SLE](https://build.opensuse.org/package/show/home:bekun:devel/elektra)
Kai-Uwe Behrmann kindly provides packages [for download](http://software.opensuse.org/download.html?project=home%3Abekun%3Adevel&package=libelektra4).



### Debian

To use the debian repository of the latest builds from master put following lines in
`/etc/apt/sources.list`:

For Stretch:

        deb     [trusted=yes] https://debian-stretch-repo.libelektra.org/ stretch main
        deb-src [trusted=yes] https://debian-stretch-repo.libelektra.org/ stretch main

Which can also be done using:

        echo "deb     [trusted=yes] https://debian-stretch-repo.libelektra.org/ stretch main" | sudo tee /etc/apt/sources.list.d/elektra.list

Or alternatively, you can use (if you do not mind many dependences just to add one line to a config file):

        sudo apt-get install software-properties-common apt-transport-https
        sudo add-apt-repository "deb     [trusted=yes] https://debian-stretch-repo.libelektra.org/ stretch main"

For Jessie:

        deb     [trusted=yes] https://debian-stable.libelektra.org/elektra-stable/ jessie main
        deb-src [trusted=yes] https://debian-stable.libelektra.org/elektra-stable/ jessie main

For Wheezy (not updated anymore, contains 0.8.19-8121 packages):

        deb     [trusted=yes] https://build.libelektra.org/debian/ wheezy main
        deb-src [trusted=yes] https://build.libelektra.org/debian/ wheezy main

To get all packaged plugins, bindings and tools install:

	apt-get install libelektra4-all

For a small installation with command-line tools available use:

	apt-get install elektra-bin

If you want to rebuild Elektra from Debian unstable or
our repositories, add a `deb-src` entry to `/etc/apt/sources.list`
and then run:

	apt-get source -b elektra

To build Debian Packages from the source you might want to use:

	dpkg-buildpackage -us -uc -sa

(You need to be in the Debian branch, see [GIT](GIT.md))

## macOS

You can install Elektra using [Homebrew](http://brew.sh) via the shell command:

```sh
brew install elektra
```

. We also provide a tap containing a more elaborate formula [here](http://github.com/ElektraInitiative/homebrew-elektra).

## Generic

First follow the steps in [COMPILE](COMPILE.md).

To install Elektra use:

	sudo make install
	sudo ldconfig  # See troubleshooting below

To uninstall Elektra use (will not be very clean,
e.g. it will not remove directories and `*.pyc` files):

	sudo make uninstall
	sudo ldconfig

or in the build directory (will not honor DESTDIR!):

	xargs rm < install_manifest.txt

## CPack

First follow the steps in [COMPILE](COMPILE.md).

Then use:

	cpack

which should create a package for distributions where a Generator is
implemented. See [this cmake file](/cmake/ElektraPackaging.cmake) for available Generators
and send a merge request for your system.


## Troubleshooting

### Error Loading Libraries

If you encounter the problem that the library can not be found (output like this)

	kdb: error while loading shared libraries:
	     libelektra-core.so.4: cannot open shared object file: No such file or directory

or:

	kdb: error while loading shared libraries:
	     libelektratools.so.2: cannot open shared object file: No such file or directory

you need to place a configuration file at `/etc/ld.so.conf.d/` (e.g. `/etc/ld.so.conf.d/elektra.conf`).

Add the path where the library has been installed

	/usr/lib/local/

and run `ldconfig` as root.

## Installation Manuals

For some of the plugins and tools that ship with Elektra,
additional installation manuals have been written.
You can find them in the [tutorial overview](tutorials/README.md).

## See also

- [COMPILE](COMPILE.md).
- [TESTING](TESTING.md).
