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

## Security Considerations ##

During decryption the plugin temporarily writes the decrypted plain text to the same directory as the original (encrypted) file.
This is a vulnerability as an attacker might have access to the plain text for a short period of time (the time between pregetstorage and postgetstorage calls).
Furthermore it is important to notice that removing the temporary file does not mean that the content gets overwritten.
Most operating systems simply delete the index to the file, so residues of the plain text might remain on the drive.

## Dependencies ##

This plugin uses parts of the `crypto` plugin.

### GnuPG (GPG) ###

Please refer to [crypto](../crypto/).

## Restrictions ##

Please refer to [crypto](../crypto/).

## Examples ##

You can mount the plugin like this:

	kdb mount test.ecf /t fcrypt /gpg/key=DDEBEF9EE2DC931701338212DAF635B17F230E8D

If you create a key under `/t`

	kdb set /t/a "hello world"

you will notice that you can not read the plain text of `test.ecf` because it has been encrypted by GPG.

But you can still access `/t/a` with `kdb get`:

	kdb get /t/a

## Configuration ##

### GPG Configuration ###

The GPG Configuration is described in [crypto](../crypto/).
