- infos = Information about crypto plugin is in keys below
- infos/author = Peter Nirschl <peter.nirschl@gmail.com>
- infos/licence = BSD
- infos/provides = crypto
- infos/needs =
- infos/recommends =
- infos/placements = postgetstorage presetstorage
- infos/status = tested/unit configurable memleak experimental discouraged
- infos/metadata = crypto/encrypt
- infos/description = Cryptographic operations

## Introduction

This plugin is a filter plugin allowing Elektra to encrypt values before they are
persisted and to decrypt values after they have been read from a backend.

The idea is to provide protection of sensible values before they are persisted.
This means the value of a key needs to be encrypted before it is written to a file or a database.
It also needs to be decrypted whenever an admissible access (read) is being performed.

The users of Elektra should not be bothered too much with the internals of the cryptographic operations.
Also the cryptographic keys must never be exposed to the outside of the crypto module.

The crypto plugin uses libgcrypt as provider of cryptographic operations.

## Installation

See [installation](/doc/INSTALL.md).
The package is called `libelektra5-crypto`.

## Dependencies

- `libgcrypt20-dev` or `libgcrypt-devel`

### GnuPG (GPG)

GPG is a run-time dependency of the crypto plugin.
Either the `gpg` or the `gpg2` binary must be installed when using the plugin.
Note that `gpg2` will be preferred if both versions are available.
The GPG binary can be configured in the plugin configuration as `/gpg/bin` (see _GPG Configuration_ below).
If no such configuration is provided, the plugin will look at the PATH environment variable to find the GPG binaries.

## How to compile

Add "crypto" to the `PLUGINS` variable in `CMakeCache.txt` and re-run `cmake`.

An example `CMakeCache.txt` may contain the following variable:

```cmake
PLUGINS=crypto
```

### macOS

The crypto plugin works under macOS Sierra (Version 10.12.3 (16D32)).

To set up the build environment on macOS Sierra we recommend using [Homebrew](http://brew.sh/).
Follow these steps to get everything up and running:

```
brew install libgcrypt pkg-config cmake
```

Also a GPG installation is required. The [GPG Tools](https://gpgtools.org) work fine for us.

## Restrictions

At the moment the plugin will only run on Unix/Linux-like systems, that provide implementations for `fork ()` and `execv ()`.

## Examples

To mount a backend with the crypto plugin that uses the GPG key 9CCC3B514E196C6308CCD230666260C14A525406, use:

```sh
sudo kdb mount test.ecf user:/tests/t crypto "crypto/key=9CCC3B514E196C6308CCD230666260C14A525406"
```

Now you can specify a key `user:/t/a` and protect its content by using:

```sh
kdb set user:/tests/t/a
kdb meta-set user:/tests/t/a crypto/encrypt 1
kdb set user:/tests/t/a "secret"
```

The value of `user:/t/a` will be stored encrypted.
But you can still access the original value using `kdb get`:

```sh
kdb get user:/tests/t/a
```

To unmount it you can run:

```sh
sudo kdb umount user:/tests/t
```

## Configuration

### GPG Configuration

The path to the gpg binary can be specified in

```
/gpg/bin
```

The GPG recipient keys can be specified as `encrypt/key` directly.
If you want to use more than one key, just enumerate like:

```
encrypt/key/#0
encrypt/key/#1
```

If more than one key is defined, every owner of the corresponding private key can decrypt the values of the backend.
This might be useful if applications run with their own user but the administrator has to update the configuration.
The administrator then only needs the public key of the application user in her keyring, set the values and the application will be able to decrypt the values.

If you are not sure which keys are available to you, the `kdb` program will give you suggestions in the error description.
For example you can type:

```sh
sudo kdb mount test.ecf user:/tests/t crypto
# RET: 7
```

In the error description you should see something like:

```
The command ./bin/kdb mount terminated unsuccessfully with the info:
The provided plugin configuration is not valid!
Errors/Warnings during the check were:
Sorry, module crypto issued the error C01310:
Failed to create handle. Reason: Missing GPG key (specified as encrypt/key) in plugin configuration. Available key IDs are: B815F1334CF4F830187A784256CFA3A5C54DF8E4,847378ABCF0A552B48082A80C52E8E92F785163F
Please report the issue at https://issues.libelektra.org/
```

This means that the following keys are available:

- B815F1334CF4F830187A784256CFA3A5C54DF8E4
- 847378ABCF0A552B48082A80C52E8E92F785163F

So the full mount command could look like this:

```sh
sudo kdb mount test.ecf user:/tests/t crypto "crypto/key=847378ABCF0A552B48082A80C52E8E92F785163F"
```

To unmount it you can run:

```sh
sudo kdb umount user:/tests/t
```

### Cryptographic Operations

Please note that these options are meant for experts only.
If you do not provide these configuration options, secure defaults are being used.

The length of the master password that protects all the other keys can be set in:

```
/crypto/masterpasswordlength
```

The number of iterations that are to be performed in the PBKDF2 call can be set in:

```
/crypto/iterations
```

### Library Shutdown

The following key must be set to `"1"` within the plugin configuration,
if the plugin should shut down the crypto library:

```
/shutdown
```

Per default shutdown is disabled to prevent applications like the qt-gui from crashing.
Shutdown is enabled in the unit tests to prevent memory leaks.

## Technical Details

### Ciphers and Mode of Operation

The crypto plugin uses the Advanced Encryption Standard (AES) in Cipher Block Chaining Mode (CBC) with a key size of 256 bit.
