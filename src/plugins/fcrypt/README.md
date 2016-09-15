- infos = Information about fcrypt plugin is in keys below
- infos/author = Peter Nirschl <peter.nirschl@gmail.com>
- infos/licence = BSD
- infos/provides = filefilter crypto
- infos/needs =
- infos/recommends =
- infos/placements = pregetstorage postgetstorage precommit
- infos/status = experimental unfinished discouraged
- infos/metadata =
- infos/description = File Encryption

# fcrypt Plugin #

## Introduction ##

This plugin enables file based encryption and decryption using GPG.

This plugin encrypts backend files before the commit is executed (thus `precommit`).
The plugin decrypts the backend files before the getstorage opens it (thus `pregetstorage`).
After the getstorage plugin has read the backend file, the plugin decrypts the backend file again (thus `postgetstorage`).

## Dependencies ##

This plugin uses parts of the `crypto` plugin.

### GnuPG (GPG) ###

Please refer to [crypto](../crypto/).

## Restrictions ##

Please refer to [crypto](../crypto/).

## Examples ##

TODO

## Configuration ##

### GPG Configuration ###

The GPG Configuration is described in [crypto](../crypto/).
