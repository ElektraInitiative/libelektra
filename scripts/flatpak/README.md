# Elektra Flatpak Artifacts

This folder contains all Flatpak-related artifacts.

## Building the Flatpak package Locally

To build the Elektra Flatpak package, one must do the following:

```bash
# install flatpak and flatpak-builder, using the package manager of your distro
$ apt install flatpak flatpak-builder

# some distributions may require the package elfutils as well
$ apt install elfutils

# add the flathub remote
$ flatpak remote-add --if-not-exists flathub https://flathub.org/repo/flathub.flatpakrepo

# download the runtime and sdk
$ flatpak install flathub org.kde.Platform org.kde.Sdk

# change to the path where you cloned libelektra
$ cd /path/to/elektra/source

# build and install the package for the current user
$ flatpak-builder .flatpak-app scripts/flatpak/org.libelektra.kdb.yaml  --force-clean --user --install

# try running the kdb binary
# elektra is given permissions to modify the host filesystem
# for more info check scripts/flatpak/org.libelektra.kdb.yaml
$ flatpak run org.libelektra.kdb help

# try running the Qt GUI through the cli
# there should also be a desktop entry available
$ flatpak run org.libelektra.kdb qt-gui

# add an alias to your shell
$ echo 'alias kdb="flatpak run org.libelektra.kdb"' >> ~/.bashrc && source ~/.bashrc

# you can then run kdb directly
$ kdb help
```

## Installing prebuilt packages

Currently, prebuilt Flatpak packages are not yet available.
