- infos = Information about crypto plugin is in keys below
- infos/author = Peter Nirschl <peter.nirschl@gmail.com>
- infos/licence = BSD
- infos/needs = 
- infos/provides = filefilter
- infos/placements = postgetstorage presetstorage
- infos/description = Cryptographic operations

## Introduction ##

This plugin is a filter plugin allowing Elektra to encrypt values before they are
persisted and to decrypt values after they have been read from a backend.

The idea is to provide protection of sensible values before they are persisted.
This means the value of a key needs to be encrypted before it is written to a file or a database.
It also needs to be decrypted whenever an admissible access (read) is being performed.

The users of Elektra should not be bothered too much with the internals of the cryptographic operations.
Also the cryptographic keys must never be exposed to the outside of the crypto module.

## Dependencies ##

- `libgcrypt20-dev` or `libgcrypt-devel`

## Restrictions ##

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

## Examples ##

TBD

