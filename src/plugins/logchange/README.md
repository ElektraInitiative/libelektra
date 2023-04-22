- infos = Information about the logchange plugin is in keys below
- infos/author = Markus Raab <elektra@libelektra.org>
- infos/maintainer = Maximilian Irlinger <max@maxirlinger.at>
- infos/licence = BSD
- infos/needs =
- infos/provides = tracing
- infos/placements = hook pregetstorage postgetstorage postcommit
- infos/status = maintained nodep global nodoc
- infos/description = demonstrates notification of key changes

## Purpose

The purpose of this plugin is to demonstrate how one can
be notified of every removed, added or changed key easily.

## Installation

See [installation](/doc/INSTALL.md).
The package is called `libelektra5-extra`.

## Usage

Prints every added, changed or deleted key on the console.
To use it, add it during mounting:

```sh
sudo kdb mount logchange.dump user:/tests/logchange dump logchange
# And to unmount it
sudo kdb umount user:/tests/logchange
```

Configure the plugin with `log/get=1` to enable printing when configuration is
loaded. For example, `kdb gmount logchange log/get=1`.
