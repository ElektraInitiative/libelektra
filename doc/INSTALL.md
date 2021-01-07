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

We also provide more recent versions and bleeding-edge versions in our repositories:

### Debian/Ubuntu

We provide stable and bleeding-edge repositories for following Debian-based distributions:

- Debian Buster
- Ubuntu Focal
- Ubuntu Bionic

To use the our stable repositories with our recent releases, following steps need to be made:

1. Run `sudo apt-key adv --keyserver keys.gnupg.net --recv-keys F26BBE02F3C315A19BF1F791A9A25CC1CC83E839` to obtain the key.

2. Add `deb https://debs.libelektra.org/<DISTRIBUTION> <DISTRIBUTION> main` into `/etc/apt/sources.list`
   where `<DISTRIBUTION>` is the Codename of your distributions e.g.`focal`,`bionic`,`buster`, etc.

This can also be done using (e.g. for Ubuntu Focal):

```sh
apt-get install software-properties-common apt-transport-https
echo "deb https://debs.libelektra.org/focal focal main" | sudo tee /etc/apt/sources.list.d/elektra.list
```

Or alternatively, you can use (if you do not mind many dependences just to add one line to a config file):

```sh
sudo apt-get install software-properties-common apt-transport-https
sudo add-apt-repository "deb https://debs.libelektra.org/focal focal main"
```

If you would like to use our bleeding-edge builds (master builds), append `-unstable` to `<DISTRIBUTION>`.

The `etc/apt/source.list` entry must look like following: `deb https://debs.libelektra.org/<DISTRIBUTION> <DISTRIBUTION>-unstable main`

E.g. `deb https://debs.libelektra.org/focal focal-unstable main`

> NOTE: for Ubuntu Bionic the yamlcpp plugin is excluded due to missing dependencies and therefore the package `libelektra4-yamlcpp` is not available.

### Fedora

We also provide stable and bleeding-edge packages for Fedora 33.

For the stable repository:

```sh
wget https://rpms.libelektra.org/fedora-33/libelektra.repo -O libelektra.repo;
sudo mv libelektra.repo /etc/yum.repos.d/;
sudo yum update
```

Or alternatively you can use dnf to add this repo:

```sh
dnf config-manager --add-repo https://rpms.libelektra.org/fedora-33/libelektra.repo
```

For our bleeding-edge builds append `-unstable` to the distribution name:

```sh
wget https://rpms.libelektra.org/fedora-33-unstable/libelektra.repo -O libelektra.repo;
sudo mv libelektra.repo /etc/yum.repos.d/;
sudo yum update
```

Or alternatively you can use dnf to add this repo:

```sh
dnf config-manager --add-repo https://rpms.libelektra.org/fedora-33-unstable/libelektra.repo
```

### Install

> TODO: add how-to for debugsym packages (different for rpm and deb)

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
We recommend to use the packages from our build server or that you generate your own packages with CPack.

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
apt-get install ./*
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
