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

TODO

## Dependencies ##

### GnuPG (GPG) ###

GPG is a runtime dependency for the fcrypt plugin.
Either the `gpg` or the `gpg2` binary should be installed when using the plugin.
Note that `gpg2` will be prefered if both versions are available.
The GPG binary can be configured in the plugin configuration as `/gpg/bin` (see _GPG Configuration_ below).
If no such configuration is provided, the plugin will look at the PATH environment variable to find the GPG binaries.

## Restrictions ##

At the moment the plugin will only run on UNIX/Linux-like systems, that provide implementations for `fork ()` and `execv ()`.

## Examples ##

TODO

## Configuration ##

### GPG Configuration ###

The path to the gpg binary can be specified in

	/gpg/bin

The GPG recipient keys can be specified as `/gpg/key` directly.
If you want to use more than one key, just enumerate like:

	/gpg/key/#0
	/gpg/key/#1

If more than one key is defined, every owner of the corresponding private key can decrypt the values of the backend.
This might be useful if applications run with their own user but the administrator has to update the configuration.
The administrator then only needs the public key of the application user in her keyring, set the values and the application will be able to decrypt the values.
