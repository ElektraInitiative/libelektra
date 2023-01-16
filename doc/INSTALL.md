# Install

## Status

The graph below shows an (incomplete) list of available packages for Elektra.

[![Packaging status](https://repology.org/badge/vertical-allrepos/elektra.svg)](https://repology.org/metapackage/elektra/versions)

## Linux

For the following Linux distributions and package managers 0.9 packages are available:

- [Arch Linux (AUR)](https://aur.archlinux.org/packages/libelektra/)
- [Openwrt](https://github.com/openwrt/packages/tree/master/libs/elektra)
- [LinuxBrew](https://github.com/Linuxbrew/homebrew-core/blob/master/Formula/elektra.rb)

### Debian/Ubuntu

We provide repositories for latest releases and latest builds from master (suite postfixed with `-unstable`) for following Debian-based distributions:

- Debian Bullseye
- Debian Buster
- Ubuntu Focal
- Ubuntu Bionic

To use our stable repositories with our latest releases, following steps need to be made:

1. Run `sudo apt-key adv --keyserver keyserver.ubuntu.com --recv-keys F26BBE02F3C315A19BF1F791A9A25CC1CC83E839` to obtain the key.

2. Add `deb https://debs.libelektra.org/<DISTRIBUTION> <SUITE> main` into `/etc/apt/sources.list`
   where `<DISTRIBUTION>` and `<SUITE>` is the codename of your distributions e.g.`focal`,`bionic`,`buster`, etc.

   This can also be done using:

   ```sh
   # Example for Ubuntu Focal
   sudo apt-get install software-properties-common apt-transport-https
   echo "deb https://debs.libelektra.org/focal focal main" | sudo tee /etc/apt/sources.list.d/elektra.list
   ```

   Or alternatively, you can use (if you do not mind many dependences just to add one line to a config file):

   ```sh
   # Example for Ubuntu Focal
   sudo apt-get install software-properties-common apt-transport-https
   sudo add-apt-repository "deb https://debs.libelektra.org/focal focal main"
   ```

   If you would like to use the latest builds of master, append `-unstable` to `<SUITE>`.

   The `etc/apt/source.list` entry must look like following: `deb https://debs.libelektra.org/<DISTRIBUTION> <SUITE>-unstable main`

   E.g. `deb https://debs.libelektra.org/focal focal-unstable main`

3. Run `sudo apt-get update`.

> NOTE: for Ubuntu Bionic the yamlcpp plugin is excluded due to missing dependencies and therefore the package `libelektra5-yamlcpp` is not available.

### Fedora

We provide repositories for latest releases and latest builds from master (suite postfixed with `-unstable`) for Fedora 33 and Fedora 34

For our stable repository with our latest releases:

```sh
wget https://rpms.libelektra.org/fedora-34/libelektra.repo -O libelektra.repo;
sudo mv libelektra.repo /etc/yum.repos.d/;
sudo yum update
```

Or alternatively you can use dnf to add this repo:

```sh
sudo dnf config-manager --add-repo https://rpms.libelektra.org/fedora-34/libelektra.repo
```

For our latest builds from master append `-unstable` to the suite name:

```sh
wget https://rpms.libelektra.org/fedora-34-unstable/libelektra.repo -O libelektra.repo;
sudo mv libelektra.repo /etc/yum.repos.d/;
sudo yum update
```

Or alternatively you can use dnf to add this repo:

```sh
sudo dnf config-manager --add-repo https://rpms.libelektra.org/fedora-34-unstable/libelektra.repo
```

### openSUSE

We provide repositories for latest releases and latest builds from master (suite postfixed with `-unstable`) for openSUSE Leap 15.3

For our stable repository with our latest releases:

> NOTE: stable packages will be available as of the `0.9.8` release.

```sh
sudo zypper ar -f https://rpms.libelektra.org/opensuse-leap-15.3 libelektra
sudo zypper update
```

For our latest builds from master append `-unstable` to the suite name:

```sh
sudo zypper ar -f https://rpms.libelektra.org/opensuse-leap-15.3-unstable libelektra-unstable
sudo zypper update
```

### Arch Linux

We provide an package for Arch Linux through the [Arch User Repository](https://wiki.archlinux.org/title/Arch_User_Repository)

You can either install it using any AUR helper or manually using makepkg

#### Manual install

Make you have `base-devel` and `git` installed (`sudo pacman -S base-devel git`)

Then you can install it using the following commands:

```sh
cd /tmp
git clone https://aur.archlinux.org/libelektra.git
cd libelektra
makepkg -si
```

### Install

To get all packaged plugins, bindings and tools install:

```sh
# For Debian based distributions
apt-get install libelektra5-all
# For Fedora based distributions
dnf install libelektra5-all
# For openSUSE
zypper install libelektra5-all
```

For a small installation with command-line tools available use:

```sh
# For Debian based distributions
apt-get install elektra-bin
# For Fedora based distributions
dnf install elektra-bin
# For openSUSE
zypper install elektra-bin
```

To install all debugsym/debuginfo packages:

```sh
# For Debian based distributions
apt-get install elektra-dbg
# For Fedora based distributions
dnf install elektra-dbg
# For openSUSE
zypper install elektra-dbg
```

If you want to install individual debugsym/debuginfo packages:

```sh
# For Debian based distributions
apt-get install <packagename>-dbgsym # e.g. apt-get install libelektra5-dbgsym
# For Fedora based distributions
dnf debuginfo-install <packagename> # e.g. dnf debuginfo-install libelektra5
# For openSUSE
zypper install <packagename>-debuginfo # e.g. zypper install libelektra5-debuginfo
```

To build Debian/Ubuntu Packages from the source you might want to use:

```sh
make package # See CPack below
```

## macOS

You can install Elektra using [Homebrew](https://brew.sh) via the shell command:

```sh
brew install elektra
```

. We also provide a tap containing a more elaborate formula [here](https://github.com/ElektraInitiative/homebrew-elektra).

## Windows

Installation for WSL is described [here](tutorials/contributing-windows.md).

If you prefer native but in functionality limited version you can download MinGW
[32-bit](https://build.libelektra.org/job/libelektra/job/master/lastSuccessfulBuild/artifact/artifacts/debian-bullseye-mingw-w64-i686/elektra.zip)
and [64-bit](https://build.libelektra.org/job/libelektra/job/master/lastSuccessfulBuild/artifact/artifacts/debian-bullseye-mingw-w64-x86_64/elektra.zip) builds.

Otherwise please refer to the section `OS Independent` below.

## OS Independent

First follow the steps in [COMPILE](COMPILE.md).

After you completed building Elektra on your own, there are multiple options how to install it. For example, with make or CPack tools.
We recommend using the packages from our build server or that you generate your own packages with CPack.

### CPack

The current supported systems are: Debian, Ubuntu and Fedora.

Then use:

```sh
make package
```

which will create packages for distributions where a Generator is implemented.

You can find the generated packages in the `package` directory of the build directory.

> NOTE: If all plugins/bindings/tools a package includes are excluded, the package will not be generated.

#### Debian/Ubuntu

First make sure you have `debhelper` and `d-shlibs` installed:

```sh
apt-get install debhelper d-shlibs
```

(Otherwise you'll see an error file utility is not available, breaking `CPACK_DEBIAN_PACKAGE_SHLIBDEPS` and `CPACK_DEBIAN_PACKAGE_GENERATE_SHLIBS`.)

On Debian-based distributions you will need to set `LD_LIBRARY_PATH` before generating the package.
Simply `cd` into the build directory and run following command:

```sh
LD_LIBRARY_PATH=$(pwd)/lib:${LD_LIBRARY_PATH} make package -j2
```

To install the packages run this in the `package` directory:

```sh
sudo apt-get install ./*
```

If any dependency problems appear, run following command to install the missing dependencies:

```sh
sudo apt-get -f install
```

#### Fedora

For packaging Fedora has `rpmdevtools` as an additional dependency:

```sh
sudo yum install -y rpmdevtools
```

When configuring CMake it is also required to set the `CMAKE_BUILD_TYPE` to `Release`.
For instance, a minimal configuration is

```sh
cmake .. -DCMAKE_BUILD_TYPE="Release"
make package
```

To install RPM packages we recommend using `yum localinstall` since installing with `rpm` doesn't resolve missing dependencies.

Run following command in the `build/package` directory:

```sh
sudo yum localinstall *
```

Depending on the system configuration one might also need to configure and run `ldconfig`, for instance

```sh
sudo echo "/usr/local/lib" >> /etc/ld.so.conf
sudo ldconfig
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

For some plugins and tools that ship with Elektra,
additional installation manuals have been written.
You can find them in the [tutorial overview](tutorials/README.md).

## See Also

- [COMPILE](COMPILE.md).
- [TESTING](TESTING.md).
