# Install

## Status

The graph below shows an (incomplete) list of available packages for Elektra.

[![Packaging status](https://repology.org/badge/vertical-allrepos/elektra.svg)](https://repology.org/metapackage/elektra/versions)

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

For [OpenSUSE, CentOS, Fedora, RHEL and SLE](https://build.opensuse.org/package/show/home:bekun:devel/elektra)
Kai-Uwe Behrmann kindly provides packages [for download](http://software.opensuse.org/download.html?project=home%3Abekun%3Adevel&package=libelektra4).

### Ubuntu-Bionic

---

To use the Ubuntu Bionic repository of the latest builds from master following steps need to be made:

1. Run `sudo apt-key adv --keyserver keyserver.ubuntu.com --recv-keys D919CE8B27A64C16656FCA9FF1532673651F9C6C` to obtain the key.

2. Add `deb https://ubuntu-bionic-repo.libelektra.org/ bionic main` into `/etc/apt/sources.list`

   Which can also be done using:

   ```sh
   apt-get install software-properties-common apt-transport-https
   echo "deb https://ubuntu-bionic-repo.libelektra.org/ bionic main" | sudo tee /etc/apt/sources.list.d/elektra.list
   ```

   Or alternatively, you can use (if you do not mind many dependences just to add one line to a config file):

   ```sh
   sudo apt-get install software-properties-common apt-transport-https
   sudo add-apt-repository "deb https://ubuntu-bionic-repo.libelektra.org/ bionic main"
   ```

### Debian-Buster

To use the Debian Buster repository of the latest builds from master put following lines in
`/etc/apt/sources.list`:

```
deb     [trusted=yes] https://debian-buster-repo.libelektra.org/ buster main
deb-src [trusted=yes] https://debian-buster-repo.libelektra.org/ buster main
```

Which can also be done using:

```sh
sudo apt-get install software-properties-common apt-transport-https
echo "deb     [trusted=yes] https://debian-buster-repo.libelektra.org/ buster main" | sudo tee /etc/apt/sources.list.d/elektra.list
```

Or alternatively, you can use (if you do not mind many dependences just to add one line to a config file):

```sh
sudo apt-get install software-properties-common apt-transport-https
sudo add-apt-repository "deb     [trusted=yes] https://debian-buster-repo.libelektra.org/ buster main"
```

If you want to rebuild Elektra from Debian unstable or
our repositories, add a `deb-src` entry to `/etc/apt/sources.list`
and then run:

```sh
apt-get source -b elektra
```

### Install

To get all packaged plugins, bindings and tools install:

```sh
apt-get install libelektra4-all
```

For a small installation with command-line tools available use:

```sh
apt-get install elektra-bin
```

To build Debian/Ubuntu Packages from the source you might want to use:

```sh
make package # See CPack below
```

## macOS

You can install Elektra using [Homebrew](http://brew.sh) via the shell command:

```sh
brew install elektra
```

. We also provide a tap containing a more elaborate formula [here](http://github.com/ElektraInitiative/homebrew-elektra).

## Windows

Please refer to the section OS Independent below.

## OS Independent

First follow the steps in [COMPILE](COMPILE.md).

After you completed building Elektra on your own, there are multiple options how to install it. For example, with make or CPack tools.
We recommend that you generate your own packages with CPack so ensure compatibility with future releases.

### CPack

The current supported systems are: Debian, Ubuntu and Fedora.

Then use:

```sh
make package
```

which will create packages for distributions where a Generator is implemented.

You can find the generated packages in the `package` directory of the build directory.

> NOTE: If all plugins/bindings/tools a package includes are excluded, the package will be not generated.

#### Debian/Ubuntu

On Debian based distributions you will need to set LD_LIBRARY_PATH before generating the package.
Simply `cd` into the build directory and run following command:

```sh
LD_LIBRARY_PATH=$(pwd)/lib:${LD_LIBRARY_PATH} make package
```

To install the packages run this in the `package` directory:

```sh
dpkg -i *
```

If any dependency problems appear, run following command to install the missing dependencies:

```sh
apt-get -f install
```

#### Fedora

To install RPM packages we recommend using `yum localinstall` since installing with `rpm` doesn't resolve missing dependencies.

Run following command in the `package` directory:

```sh
yum localinstall *
```

### make

```sh
sudo make install
sudo ldconfig  # See troubleshooting below
```

To uninstall Elektra use (will not be very clean,
e.g. it will not remove directories and `*.pyc` files):

```sh
sudo make uninstall
sudo ldconfig
```

or in the build directory (will not honor `DESTDIR`!):

```sh
xargs rm < install_manifest.txt
```

## Troubleshooting

### Error Loading Libraries

If you encounter the problem that the library can not be found (output like this)

```
kdb: error while loading shared libraries:
     libelektra-core.so.4: cannot open shared object file: No such file or directory
```

or:

```
kdb: error while loading shared libraries:
     libelektratools.so.2: cannot open shared object file: No such file or directory
```

you need to place a configuration file at `/etc/ld.so.conf.d/` (e.g. `/etc/ld.so.conf.d/elektra.conf`). Note that under Alpine Linux this file is called `/etc/ld-musl-x86_64.path` or similar, depending on your architecture.

Add the path where the library has been installed (on Alpine Linux this had to be `usr/lib/elektra` for it to work)

```
/usr/lib/local/
```

and run `ldconfig` as root.

## Installation Manuals

For some of the plugins and tools that ship with Elektra,
additional installation manuals have been written.
You can find them in the [tutorial overview](tutorials/README.md).

## See Also

- [COMPILE](COMPILE.md).
- [TESTING](TESTING.md).
