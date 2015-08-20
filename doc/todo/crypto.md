# Concept for symmetric cryptography

## Idea

The idea is to provide protection of sensible values before they are persisted.
This means the value of a key needs to be encrypted before it is written to a file or a database.
It also needs to be decrypted whenever an admissible access (read) is being performed.

Other features (like encrypting whole files) may be added later.

## Requirements

The users of libelektra should not be bothered too much with the internals of the cryptographic operations.
Also the cryptographic keys should not be exposed to the user, but rather be kept within a single crypto module.

The provided functionality will be exposed within one or several filter-plugins.

## Features

- Encryption of values (Key)
- Decryption of values (Key)
- Key derivation by metadata (password provided within a meta-key)
- Key derivation by utilizing the pgp-agent
- Key derivation by using a specified key-file (like the SSH client does)

The encryption and decryption of values is a straight forward process, once the key and IV are supplied.
The key-derivation process in a library is a bit tricky.

The crypto plugin itself can hold configurtaion data about the order of the applied key derivation functions.
For example, if a password is provided by a meta-key, it will be used, otherwise we look for a key file. 
If no key file has been configured, we try to trigger the PGP agent, etc.

The following example configuration illustrates this concept:

	/system/elektra/crypto/config/key-derivation/#0#meta
	/system/elektra/crypto/config/key-derivation/#1#file
	/system/elektra/crypto/config/key-derivation/#2#agent
	/system/elektra/crypto/config/key-file/#0#path=~/.elektra/id_aes
	/system/elektra/crypto/config/key-file/#1#path=/etc/elektra/id_aes

Since it does not make sense to encrypt every key in a KeySet, only keys marked with a certain
meta-key will be considered for encryption/decryption.

## Adaptions

- Introduction of a new crypto module to Elektra, containing the filter-plugins for encryption and decryption
- Adding the libgcrypt library as a dependency to the crypto module
- Unit tests for basic cryptographic operations
- Adding support for the PGP agent
- Adding support for password-based key derivation
- Adding support for file-system driven key derivation

