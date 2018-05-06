# Cryptographic Methods in Elektra

Elektra can protect the following aspects of your configuration:

1. confidentiality, and
2. integrity.

Elektra provides two plugins to achieve this protection:

1. `crypto`, and
2. `fcrypt`.

## Configuration File Encryption/Decryption

The `fcrypt` plugin enables the encryption and decryption of entire configuration files, thus protecting the confidentiality of the configuration values.
`fcrypt` utilizes GnuPG (GPG) for all cryptographic operations.
The GPG key, which is used for encryption and decryption, is specified in the backend configuration under `encrypt/key`.
You MUST be in possesion of the private key, otherwise `fcrypt` will deny all operations.

Let's assume your GPG private key has the ID `DDEBEF9EE2DC931701338212DAF635B17F230E8D`.
If you want to encrypt your configuration file `test.ini` you can mount the backend under `user/test` like this:

	kdb mount test.ini user/test fcrypt "encrypt/key=DDEBEF9EE2DC931701338212DAF635B17F230E8D" ini

As a result the file `test.ini` is encrypted using GnuPG.
`fcrypt` will call the `gpg2` or `gpg` binary as follows:

	gpg2 -o test.ini -a -r DDEBEF9EE2DC931701338212DAF635B17F230E8D -e test.ini.tmp

Note that `test.ini` can not only be decrypted by Elektra, but it is also possible to decrypt it with GnuPG directly, as long as you are in possesion of the private key.

## Configuration File Signatures

`fcrypt` also offers the option to sign and verify configuration files, thus protecting the integrity of the configuration values.
If `sign/key` is specified in the backend configuration, `fcrypt` will forward the key ID to be used for signing the configuration file.

An example backend configuration is given as follows:

	kdb mount test.ini user/test fcrypt "sign/key=DDEBEF9EE2DC931701338212DAF635B17F230E8D" ini
	
As a result the file `test.ini` will be signed using GPG.
`fcrypt` will call the `gpg2` or `gpg` binary as follows:

	gpg2 -o test.ini -a -u DDEBEF9EE2DC931701338212DAF635B17F230E8D -r DDEBEF9EE2DC931701338212DAF635B17F230E8D -s test.ini.tmp

If `test.ini` is modified, all following calls of `kdb get` will fail with an error message stating that the signature of the file could not be verified.

### Combining Signatures and Encryption

The options `sign/key` and `encrypt/key` can be combined together, resulting in configuration files, that are signed and encrypted.

## Configuration Value Encryption/Decryption

The compilation variants of the `crypto` plugin:

1. `crypto_gcrypt`,
2. `crypto_openssl`, and
3. `crypto_botan`

provide the option to encrypt and decrypt single configuration values (Keys) in a Keyset.
`crypto` is using GPG for key-handling.

Let's assume you want to store an encrypted password in `test.ini` but you do not want to encrypt the entire configuration file.
You can use the `crypto` plugin to solve this problem.
An example backend configuration is given as follows:

	kdb mount test.ini user/test crypto_gcrypt "crypto/key=DDEBEF9EE2DC931701338212DAF635B17F230E8D" base64 ini
	
We recommend adding the `base64` plugin to the backend, because `crypto` will output binary data.
Having binary data in configuration files is hardly ever feasible.
`base64` encodes all binary values within a configuration file and transforms them into Base64 strings.

### Marking Keys For Encryption

To tell the `crypto` plugin which Keys it should process, the meta-key `crypto/encrypt` is used.
The `crypto` plugin searches for the meta-key `crypto/encrypt`.
If the value is equal to `1`, the value of the Key will be encrypted.

Let's demonstrate this using an example.
We want to protect the password, that is stored under `user/test/password`.
So we set the meta-key as follows:

	kdb setmeta user/test/password crypto/encrypt 1
	
Now we are safe to set the actual password:

	kdb set user/test/password "1234"
	
The resulting INI-file contains the following data:

	#@META crypto/encrypt = 1
	password = @BASE64IyFjcnlwdG8wMBEAAADwPI+lqp+X2b6BIfLdRYgwxmAhVUPurqkQVAI78Pn4OYONbei4NfykMPvx9C9w91KT

You can access the password as usual with `kdb get`:

	kdb get user/test/password
	
As a result you get "1234".

### Disabling Encryption

You can disable the encryption by setting `crypto/encrypt` to a value other than `1`, for example:

	kdb setmeta user/test/password crypto/encrypt 0


