# Concept for symmetric cryptography

## Idea

The idea is to provide protection of sensible values before they are persisted.
This means the value of a key needs to be encrypted before it is written to a file or a database.
It also needs to be decrypted whenever an admissible access (read) is being performed.

Other features (like encrypting whole files) may be added later.

## Requirements

The users of libelektra should not be bothered too much with the internals of the cryptographic operations.
Also the cryptographic keys should not be exposed to the user, but rather be kept within a single crypto module.

## Features

This can be realised in two ways:

1. We provide a "KEY_CRYPTO_PASSWORD" key-flag which expects a string-password as following argument.
   By applying the PBKDF2 to the password, the required keys and initialization vectors can be derived.
   
2. We provide a "KEY_CRYPTO_HANDLE" flag, which expects an identifier to a crypto-handle, 
   which will be internally represented as a struct, holding the key and other parameters, that are required
   for the cryptographic operations. However the contents of these structs will never be exposed to the user.
   The Elektra user just gets a random identifier. Everything else is kept inside the crypto module.

The crypto module would expose the following functions to the API:
   
- **handle_create** - for creating and initializing a crypto handle. It takes
  the key, the IV and all the other relevant information as input parameters and returns
  a random identifier of the created crypto handle
- **handle_release** - safely releases a given crypto handle (including zeroing of the memory, etc.)
- **release_all** - tells the crypto module to clean up its memory and release all
  crypto handles, close all open streams, etc.
     
## Adaptions

1. Introduction of a new crypto module to Elektra, which exposes several functions
   (let's call it *crypto.c* for now)
2. Adding new key-flags (*key.c* ??)
3. Processing the new key-flags (*key.c* ??)
4. The backend should trigger the encryption of the values (and also the decryption after a key has been loaded?)

## Open Issues

- **Error handling**
	- at runtime (e.g. wrong key given, data was not encrypted at all, ...)
	- at compile-time (e.g. libgcrypt is not available)

