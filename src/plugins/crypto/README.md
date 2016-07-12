- infos = Information about crypto plugin is in keys below
- infos/author = Peter Nirschl <peter.nirschl@gmail.com>
- infos/licence = BSD
- infos/provides = filefilter crypto
- infos/needs =
- infos/recommends =
- infos/placements = postgetstorage presetstorage
#ifdef ELEKTRA_CRYPTO_API_GCRYPT
- infos/status = experimental unfinished memleak discouraged
#else
- infos/status = experimental unfinished discouraged
#endif
- infos/metadata = crypto/encrypt
- infos/description = Cryptographic operations

# Crypto Plugin #

## Introduction ##

This plugin is a filter plugin allowing Elektra to encrypt values before they are
persisted and to decrypt values after they have been read from a backend.

The idea is to provide protection of sensible values before they are persisted.
This means the value of a key needs to be encrypted before it is written to a file or a database.
It also needs to be decrypted whenever an admissible access (read) is being performed.

The users of Elektra should not be bothered too much with the internals of the cryptographic operations.
Also the cryptographic keys must never be exposed to the outside of the crypto module.

The crypto plugin supports different libraries as provider for the cryptographic operations.
At the moment the following crypto APIs are supported:

- OpenSSL (libcrypto)
- libgcrypt
- Botan

## Dependencies ##

#ifdef ELEKTRA_CRYPTO_API_GCRYPT
- `libgcrypt20-dev` or `libgcrypt-devel`
#endif
#ifdef ELEKTRA_CRYPTO_API_OPENSSL
- `libssl-dev` or `openssl-devel`
#endif
#ifdef ELEKTRA_CRYPTO_API_BOTAN
- `libbotan1.10-dev` or `botan-devel`
#endif

## How to compile ##

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

### Manual Library Setup ###

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

### Mac OS X ###

Both variants of the plugin compile under Mac OS X "El Capitan" (Version 10.11.3 (15D21)).

For the `crypto_gcrypt` variant download and install either [MacPorts](https://www.macports.org/) or [Homebrew](http://brew.sh/).
Use one of those tools to download and install `libgcrypt`. If you choose MacPorts, you can set the CMake variables like this:

- *LIBGCRYPT_INCLUDE_DIR* to `/opt/local/include/`
- *LIBGCRYPT_LIBRARIES* to `/opt/local/lib/libgcrypt.dylib`

The CMake command might look something like:

	cmake -DLIBGCRYPT_INCLUDE_DIR="/opt/local/include/" -DLIBGCRYPT_LIBRARIES="/opt/local/lib/libgcrypt.dylib" -DPLUGINS="crypto;crypto_gcrypt;" /path_to_elektra_src

For the `crypto_openssl` variant a custom-built OpenSSL library is neccessary as the MacPorts or Homebrew variants do not seem to work.
Download the latest version of the OpenSSL library from the [project homepage](https://www.openssl.org/source/) and compile it.
Copy the header files and the binary files to a location where all users can access them.

Set the CMake variables `OPENSSL_INCLUDE_DIR` and `OPENSSL_LIBRARIES` to your desired location.

## Restrictions ##

The crypto plugin will encrypt and decrypt values using AES-256 in CBC mode.

The key derivation is still WIP.

### Planned Features ###

- Encryption of values
- Decryption of values
- Key derivation by metadata (password provided within a meta-key)
- Key derivation by using a specified key-file (like the SSH client does)
- Key derivation by utilizing the pgp-agent

The encryption and decryption of values is a straight forward process, once the key and IV are supplied.
The key-derivation process in a library is a bit tricky.

The crypto plugin itself can hold configuration data about the order of the applied key derivation functions.
For example, if a password is provided by a meta-key, it will be used, otherwise we look for a key file.
If no key file has been configured, we try to trigger the PGP agent, etc.

The following example configuration illustrates this concept:

	system/elektra/crypto/config/key-derivation/#0 = meta
	system/elektra/crypto/config/key-derivation/#1 = file
	system/elektra/crypto/config/key-derivation/#2 = agent
	system/elektra/crypto/config/key-file/path/#0 = ~/.elektra/id_aes
	system/elektra/crypto/config/key-file/path/#1 = /etc/elektra/id_aes

Only keys marked with a certain meta-key will be considered for encryption/decryption.

## Configuration ##

The following key must be set to `"1"` within the plugin configuration,
if the plugin should shut down the crypto library:

	/shutdown

Per default shutdown is disabled to prevent applications like the qt-gui from crashing.
Shutdown is enabled in the unit tests to prevent memory leaks.

## Examples ##

### Metadata based encyption ###

You specify the parameters of the cryptographic operations in a KeySet together with the keys to be encrypted.
The following parameters are required:

- **Key** - the symmetric cryptographic key for encryption
- **IV** - the initialization vector (IV) that is required by the CBC mode

The following keys are required for metadata based encryption:

	/elektra/modules/crypto/key
	/elektra/modules/crypto/iv

You can use the following meta-key to mark a key for encryption:

	crypto/encrypt

If this meta-key has a value with a string-length greater than 0 (``strlen() > 0``) then
the crypto-plugin will try to encrypt it.

