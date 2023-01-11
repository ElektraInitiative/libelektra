- infos = Information about the desktop plugin is in keys below
- infos/author = Markus Raab <elektra@libelektra.org>
- infos/licence = BSD
- infos/needs =
- infos/provides = storage/info
- infos/recommends =
- infos/placements = getstorage setstorage
- infos/status = recommended maintained nodep experimental concept
- infos/features/storage = read limited limited
- infos/metadata =
- infos/description = reads desktop information

## Introduction

The plugin is informational and mainly be used to provide context for
other configuration. See [elektrify-getenv](/src/bindings/intercept/env/README.md).

## Installation

See [installation](/doc/INSTALL.md).
The package is called `libelektra5-extra`.

## Usage

To mount the plugin please use:

```sh
sudo kdb mount --resolver noresolver none system:/info/desktop desktop
```

or it is already included if you already mounted the info plugins with:

```
kdb mount-info
```

Then you can get desktop information via:

```
kdb get system:/info/desktop
```

You either get a _lower-case_ string (supported desktops see below)
or no key if no desktop was detected.

## Supported Desktops

Currently supported desktops are:

- GNOME
- KDE
- TDE
- Unity
- XDG conformant desktops (`XDG_CURRENT_DESKTOP`)

Currently the detection relies on environment variables,
which will not work in setuid or otherwise secured binaries.
Please open a bug report if the detection does not work for you:
https://issues.libelektra.org

## Unmount the plugin

To unmount the plugin you can run

```sh
sudo kdb umount system:/info/desktop
```
