- infos = Information about gpgme plugin is in keys below
- infos/author = Peter Nirschl <peter.nirschl@gmail.com>
- infos/licence = BSD
- infos/provides = crypto
- infos/needs =
- infos/recommends =
- infos/placements = postgetstorage presetstorage
- infos/status = unittest configurable memleak experimental unfinished
- infos/metadata = crypto/encrypt gpg/binary
- infos/description = Cryptographic operations wit GnuPG Made Easy (GPGME)

## Introduction

The `gpgme` plugin is a filter plugin that enables users to encrypt values before they are
persisted and to decrypt values after they have been read from a backend.
The encryption and decryption is designed to work transparently.

The cryptographic operations are performed by GnuPG via the `libgpgme` library.

## Installation

See [installation](/doc/INSTALL.md).
The package is called `libelektra5-experimental`.

## Dependencies

- `libgpgme11` version 1.10 or later

## Build Information

The plugin has been tested on Ubuntu 18.04 with `libgpgme` version 1.10.

## Examples

You can mount the plugin like this:

```sh
kdb mount test.ecf /t gpgme "encrypt/key=DDEBEF9EE2DC931701338212DAF635B17F230E8D"
```

Now you can specify a key `user:/t/a` and protect its content by using:

```sh
kdb set user:/t/a
kdb meta set user:/t/a crypt/encrypt 1
kdb set user:/t/a "secret"
```

The value of `user:/t/a` (for this example: "secret") will be stored encrypted.
You can still access the original value by using `kdb get`:

```sh
kdb get user:/t/a
```

## Configuration

### GnuPG Keys

The GPG recipient keys can be specified in two ways:

1. The GPG recipient key can be specified as `encrypt/key` directly.
2. If you want to specify multiple keys, you can enumerate them under `encrypt/key`.

The following example illustrates how multiple GPG recipient keys can be specified:

```
encrypt/key/#0
encrypt/key/#1
```

### Textmode

`gpgme` operates in textmode per default. In textmode the output of GPG is ASCII armored.

Textmode can be disabled by setting `/gpgme/textmode` to `0` in the plugin configuration.

## Technical Details

### Message Format

The encrypted values are valid PGP messages, that can be decrypted and read solely by the GnuPG binary without Elektra.
