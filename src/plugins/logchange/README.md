- infos = Information about the logchange plugin is in keys below
- infos/author = Markus Raab <elektra@libelektra.org>
- infos/licence = BSD
- infos/needs =
- infos/provides = tracing
- infos/placements = pregetstorage postgetstorage postcommit
- infos/status = maintained nodep global nodoc
- infos/description = demonstrates notification of key changes

## Purpose

The purpose of this plugin is to demonstrate how one can
be notified of every removed, added or changed key easily.

## Usage

Prints every added, changed or deleted key on the console.
To use it, add it during mounting:

```sh
kdb mount logchange.dump user/logchange dump logchange
```

Configure the plugin with `log/get=1` to enable printing when configuration is
loaded. For example, `kdb gmount logchange log/get=1`.
