- infos = Information about crypto plugin is in keys below
- infos/author = Peter Nirschl <peter.nirschl@gmail.com>
- infos/licence = BSD
- infos/provides = crypto
- infos/needs =
- infos/recommends =
- infos/placements = postgetstorage presetstorage
#ifdef ELEKTRA_CRYPTO_API_GCRYPT
- infos/status = unittest configurable memleak experimental unfinished discouraged
#else
- infos/status = unittest configurable memleak experimental unfinished discouraged
#endif
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

The crypto plugin supports different libraries as provider for the cryptographic operations.
At the moment the following crypto APIs are supported:

- OpenSSL (`libcrypto`)
- libgcrypt
- Botan

## Dependencies

#ifdef ELEKTRA_CRYPTO_API_GCRYPT
- `libgcrypt20-dev` or `libgcrypt-devel`
#endif
#ifdef ELEKTRA_CRYPTO_API_OPENSSL
- `libssl-dev` or `openssl-devel`
#endif
#ifdef ELEKTRA_CRYPTO_API_BOTAN
- `libbotan1.10-dev` or `botan-devel`
#endif

### GnuPG (GPG)

GPG is a runtime dependency for all crypto plugin variants.
Either the `gpg` or the `gpg2` binary should be installed when using the plugin.
Note that `gpg2` will be preferred if both versions are available.
The GPG binary can be configured in the plugin configuration as `/gpg/bin` (see _GPG Configuration_ below).
If no such configuration is provided, the plugin will look at the PATH environment variable to find the GPG binaries.

## How to compile

The following compile variants are available:

1. crypto_gcrypt
2. crypto_openssl
3. crypto_botan

Add "crypto" and the variants, that you want (you can add one of them or all), to the `PLUGINS` variable in `CMakeCache.txt` and re-run `cmake`.

In order to add all compile variants you can add "CRYPTO" to the `PLUGINS` variable.

An example `CMakeCache.txt` may contain the following variable:

    PLUGINS=crypto;crypto_gcrypt;crypto_openssl;crypto_botan

or it may look like:

    PLUGINS=CRYPTO

### Manual Library Setup

If you have a custom built OpenSSL or libgcrypt on your system, you can tell CMake to use those by setting the following CMake variables.

For a custom OpenSSL location set:

- *OPENSSL_INCLUDE_DIR* to the library's header files
- *OPENSSL_LIBRARIES* to the library's binary file

For a custom libgcrypt location set:

- *LIBGCRYPT_INCLUDE_DIR* to the library's header files
- *LIBGCRYPT_LIBRARIES* to the library's binary file

For a custom Botan development file location set:

- *BOTAN_INCLUDE_DIRS* to Botan's header files (includes)
- *BOTAN_LIBRARIES* to Botan's binary file

### Mac OS X

Both variants of the plugin compile under Mac OS X "El Capitan" (Version 10.11.3 (15D21)).

For the `crypto_gcrypt` variant download and install either [MacPorts](https://www.macports.org/) or [Homebrew](http://brew.sh/).
Use one of those tools to download and install `libgcrypt`. If you choose MacPorts, you can set the CMake variables like this:

- *LIBGCRYPT_INCLUDE_DIR* to `/opt/local/include/`
- *LIBGCRYPT_LIBRARIES* to `/opt/local/lib/libgcrypt.dylib`

The CMake command might look something like:

    cmake -DLIBGCRYPT_INCLUDE_DIR="/opt/local/include/" -DLIBGCRYPT_LIBRARIES="/opt/local/lib/libgcrypt.dylib" -DPLUGINS="crypto;crypto_gcrypt;" /path_to_elektra_src

For the `crypto_openssl` variant a custom-built OpenSSL library is necessary as the MacPorts or Homebrew variants do not seem to work.
Download the latest version of the OpenSSL library from the [project homepage](https://www.openssl.org/source/) and compile it.
Copy the header files and the binary files to a location where all users can access them.

Set the CMake variables `OPENSSL_INCLUDE_DIR` and `OPENSSL_LIBRARIES` to your desired location.

## Restrictions

At the moment the plugin will only run on UNIX/Linux-like systems, that provide implementations for `fork ()` and `execv ()`.

## Examples

To mount a backend with the gcrypt plugin variant that uses the GPG key 9CCC3B514E196C6308CCD230666260C14A525406, use:

    kdb mount test.ecf user/t crypto_gcrypt "crypto/key=9CCC3B514E196C6308CCD230666260C14A525406"

Now you can specify a key `user/t/a` and protect its content by using:

    kdb set user/t/a
    kdb setmeta user/t/a crypto/encrypt 1
    kdb set user/t/a "secret"

The value of `user/t/a` will be stored encrypted.
But you can still access the original value using `kdb get`:

    kdb get user/t/a

## Configuration

### GPG Configuration

The path to the gpg binary can be specified in

    /gpg/bin

The GPG recipient keys can be specified as `/gpg/key` directly.
If you want to use more than one key, just enumerate like:

    /gpg/key/#0
    /gpg/key/#1

If more than one key is defined, every owner of the corresponding private key can decrypt the values of the backend.
This might be useful if applications run with their own user but the administrator has to update the configuration.
The administrator then only needs the public key of the application user in her keyring, set the values and the application will be able to decrypt the values.

### Cryptographic Operations

The length of the master password that protects all the other keys can be set in:

    /crypto/masterpasswordlength

The number of iterations that are to be performed in the PBKDF2 call can be set in:

    /crypto/iterations

### Library Shutdown

The following key must be set to `"1"` within the plugin configuration,
if the plugin should shut down the crypto library:

    /shutdown

Per default shutdown is disabled to prevent applications like the qt-gui from crashing.
Shutdown is enabled in the unit tests to prevent memory leaks.

