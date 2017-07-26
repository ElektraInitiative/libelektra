- infos = Information about fcrypt plugin is in keys below
- infos/author = Peter Nirschl <peter.nirschl@gmail.com>
- infos/licence = BSD
- infos/provides = sync filefilter crypto
- infos/needs =
- infos/recommends =
- infos/placements = pregetstorage postgetstorage precommit
- infos/ordering = sync
- infos/status = unittest nodep configurable experimental unfinished discouraged
- infos/metadata =
- infos/description = File Encryption

# fcrypt Plugin

## Introduction

This plugin enables file based encryption and decryption using GPG.
Also an option for signing and verifying files using GPG is provided.

This plugin encrypts backend files before the commit is executed (thus `precommit`).
The plugin decrypts the backend files before the getstorage opens it (thus `pregetstorage`).
After the getstorage plugin has read the backend file, the plugin decrypts the backend file again (thus `postgetstorage`).

## Security Considerations

During decryption the plugin temporarily writes the decrypted plain text to the same directory as the original (encrypted) file.
This is a vulnerability as an attacker might have access to the plain text for a short period of time (the time between pregetstorage and postgetstorage calls).
The plugin shreds, i.e. overwrites, the temporary file with zeroes to reduce the risk of leakage.

If the application crashes parts of the decrypted data may leak.

## Known Issues

If you encounter the following error at `kdb mount`:

	The command kdb mount terminated unsuccessfully with the info:
	Too many plugins!
	The plugin sync can't be positioned to position precommit anymore.
	Try to reduce the number of plugins!

	Failed because precommit with 7 is larger than 6
	Please report the issue at https://issues.libelektra.org/

you might want to consider disabling the sync plugin by entering:

	kdb set /sw/elektra/kdb/#0/current/plugins ""
	kdb set system/sw/elektra/kdb/#0/current/plugins ""

Please note that this is a workaround until a more sustainable solution is found.

## Dependencies

### Crypto Plugin

This plugin uses parts of the `crypto` plugin.

### GnuPG (GPG)

Please refer to [crypto](../crypto/).

## Restrictions

Please refer to [crypto](../crypto/).

## Examples

You can mount the plugin with encryption enabled like this:

	kdb mount test.ecf /t fcrypt encrypt/key=DDEBEF9EE2DC931701338212DAF635B17F230E8D

If you only want to sign the configuration file, you can mount the plugin like this:

	kdb mount test.ecf /t fcrypt sign/key=DDEBEF9EE2DC931701338212DAF635B17F230E8D

Both options `encrypt/key` and `sign/key` can be combined:

	kdb mount test.ecf /t fcrypt encrypt/key=DDEBEF9EE2DC931701338212DAF635B17F230E8D sign/key=DDEBEF9EE2DC931701338212DAF635B17F230E8D

If you create a key under `/t`

	kdb set /t/a "hello world"

you will notice that you can not read the plain text of `test.ecf` because it has been encrypted by GPG.

But you can still access `/t/a` with `kdb get`:

	kdb get /t/a

## Configuration

### Signatures

The GPG signature keys can be specified as `sign/key` directly.
If you want to use more than one key for signing, just enumerate like:

    sign/key/#0
    sign/key/#1

If more than one key is defined, every private key is used to sign the file of the backend.

If a signature is attached to a file, `fcrypt` automatically verifies its content whenever the file is being read.

Note that the signed file is stored in the internal format of GPG.
So you only see binary data when opening the signed configuration file directly.
However, you can simply display the plain text content of the file by using GPG:

	gpg2 -d signed.ecf


### GPG Configuration

The GPG Configuration is described in [crypto](../crypto/).
