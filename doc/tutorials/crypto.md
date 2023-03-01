# Cryptographic Methods in Elektra

Elektra can protect the following aspects of your configuration:

1. confidentiality (i.e. protection against unauthorized access), and
2. integrity (i.e. protection against unauthorized modification).

Elektra provides two plugins to achieve this protection:

1. `crypto`, and
2. `fcrypt`.

## Prerequisites - GnuPG

For the rest of this tutorial we assume that you are somewhat familiar with GnuPG (GPG).
The documentation of GnuPG can be found [here](https://gnupg.org/documentation/index.html).

In order to find your GPG private key(s) you can use:

```bash
gpg2 --list-secret-keys
```

If GPG private keys are available, you see an output, that looks similar to this:

```
sec   rsa1024 2016-08-20 [SC]
	  DDEBEF9EE2DC931701338212DAF635B17F230E8D
uid           [ultimate] Elektra Unit Tests (DO NOT USE IN PRODUCTION) <unit-tests@libelektra.org>
ssb   rsa1024 2016-08-20 [E]
```

The GPG key we use in this tutorial has the ID `DDEBEF9EE2DC931701338212DAF635B17F230E8D`.

A GPG private key is mandatory for the plugins to work.
If you have no GPG private key available, you can generate one by entering the following command:

```bash
gpg2 --generate-key
```

The `fcrypt` plugin and the `crypto` plugin support both versions (version 1 and version 2) of GPG.

## Introduction

In this tutorial we explain the use of the `crypto` plugin and the `fcrypt` plugin by a simple example:
We want to protect a password that is contained in an INI-file.

The following example demonstrates how the INI-file is mounted without encryption enabled.
We create the password at `user:/tests/password` and display the contents of `test.ini`.

_Step 1:_ Mount `test.ini`

```sh
kdb mount test.ini user:/tests ini
```

_Step 2:_ Set the password at `user:/tests/password` and display the contents of `test.ini`

```sh
kdb set user:/tests/password 1234
kdb file user:/tests/password | xargs cat | tail -n1
#> password = 1234
```

_Step 3:_ (Optional) Cleanup

```sh
kdb rm user:/tests/password
kdb umount user:/tests
```

As you can see the password is stored in plain text.
In this tutorial we demonstrate two different approaches towards confidentiality:

1. with the `fcrypt` plugin, which encrypts the entire INI-file, and
2. with the `crypto` plugin, which allows the encryption of specific key values only.

We also show how to approach integrity with the signature features of the `fcrypt` plugin.

## Configuration File Encryption/Decryption

The `fcrypt` plugin enables the encryption and decryption of entire configuration files, thus protecting the confidentiality of the configuration keys and values.
`fcrypt` utilizes GPG for all cryptographic operations.
The GPG key, which is used for encryption and decryption, is specified in the backend configuration under `encrypt/key`.

```bash
sudo kdb mount test.ini user:/tests fcrypt "encrypt/key=DDEBEF9EE2DC931701338212DAF635B17F230E8D" ini
```

If the above command fails, please take a look at the
[ReadMe of the `fcrypt` plugin](https://master.libelektra.org/src/plugins/fcrypt/README.md#known-issues).

As a result the file `test.ini` is encrypted using GnuPG.
`fcrypt` will call the `gpg2` or `gpg` binary as follows:

```bash
gpg2 -o test.ini -a -r DDEBEF9EE2DC931701338212DAF635B17F230E8D -e test.ini.tmp
```

Note that `test.ini` can not only be decrypted by Elektra, but it is also possible to decrypt it with GnuPG directly.
You can try to decrypt `test.ini` with GPG:

```bash
gpg2 -d test.ini
```

The complete procedure looks like this:

```sh
kdb mount test.ini user:/tests fcrypt "encrypt/key=$(kdb gen-gpg-testkey)" ini
kdb set user:/tests/password 1234
kdb file user:/tests/password | xargs cat
```

To clean up the environment we run:

```sh
kdb rm user:/tests/password
kdb umount user:/tests
```

## Configuration File Signatures

`fcrypt` also offers the option to sign and verify configuration files, thus protecting the integrity of the configuration values.
If `sign/key` is specified in the backend configuration, `fcrypt` will forward the key ID for signing the configuration file.

An example backend configuration is given as follows:

```bash
sudo kdb mount test.ini user:/tests fcrypt "sign/key=DDEBEF9EE2DC931701338212DAF635B17F230E8D" ini
```

As a result the file `test.ini` will be signed using GPG.
`fcrypt` will call the `gpg2` or `gpg` binary as follows:

```bash
gpg2 -o test.ini -a -u DDEBEF9EE2DC931701338212DAF635B17F230E8D -r DDEBEF9EE2DC931701338212DAF635B17F230E8D -s test.ini.tmp
```

If `test.ini` is modified, all following calls of `kdb get` will fail with an error message stating that the signature of the file could not be verified.

The complete example looks like this:

```sh
kdb mount test.ini user:/tests fcrypt "sign/key=$(kdb gen-gpg-testkey)" ini
kdb set user:/tests/password 1234
kdb file user:/tests/password | xargs cat
```

To clean up the environment we run:

```sh
kdb rm user:/tests/password
kdb umount user:/tests
```

### Combining Signatures and Encryption

The options `sign/key` and `encrypt/key` can be combined, resulting in configuration files, that are signed and encrypted.

Mounting `test.ini` with signatures and encryption enabled can be done like this:

```bash
sudo kdb mount test.ini user:/tests fcrypt "sign/key=DDEBEF9EE2DC931701338212DAF635B17F230E8D,encrypt/key=DDEBEF9EE2DC931701338212DAF635B17F230E8D" ini
```

The complete example looks like this:

```sh
kdb mount test.ini user:/tests fcrypt "sign/key=$(kdb gen-gpg-testkey),encrypt/key=$(kdb gen-gpg-testkey)" ini
kdb set user:/tests/password 1234
kdb file user:/tests/password | xargs cat
```

To clean up the environment we run:

```sh
kdb rm user:/tests/password
kdb umount user:/tests
```

## Configuration Value Encryption/Decryption

So far we learned how to encrypt and decrypt entry configuration files.
Sometimes we only want to protect a smaller subset of configuration values in a bigger configuration setting.
For this reason the `crypto` plugin was developed.

The `crypto` plugin uses `libgcrypt` as provider of cryptographic functions.

The `crypto` plugin provides the option to encrypt and decrypt single configuration values (Keys) in a Keyset.
GPG is required for the key-handling.

To follow our example of an encrypted password in `test.ini`, we first mount the INI-file with the `crypto` plugin enabled, like this:

```bash
sudo kdb mount test.ini user:/tests crypto "crypto/key=DDEBEF9EE2DC931701338212DAF635B17F230E8D" base64 ini
```

We recommend adding the `base64` plugin to the backend, because `crypto` will output binary data.
Having binary data in configuration files is hardly ever feasible.
`base64` encodes all binary values within a configuration file and transforms them into Base64 strings.

### Marking Keys For Encryption

To tell the `crypto` plugin which Keys it should process, the metakey `crypto/encrypt` is used.
The `crypto` plugin searches for the metakey `crypto/encrypt`.
If the value is equal to `1`, the value of the Key will be encrypted.

We want to protect the password, that is stored under `user:/test/password`.
So we set the metakey as follows:

```bash
kdb meta set user:/tests/password crypto/encrypt 1
```

Now we are safe to set the actual password:

```bash
kdb set user:/tests/password "1234"
```

The resulting INI-file contains the following data:

```ini
#@META crypto/encrypt = 1
password=@BASE64IyFjcnlwdG8wMBEAAADwPI+lqp+X2b6BIfLdRYgwxmAhVUPurqkQVAI78Pn4OYONbei4NfykMPvx9C9w91KT
```

You can access the password as usual with `kdb get`:

```bash
kdb get user:/tests/password
```

As a result you get "1234".

### Disabling Encryption

You can disable the encryption by setting `crypto/encrypt` to a value other than `1`, for example:

```bash
kdb meta set user:/tests/password crypto/encrypt 0
```

### Complete Example

The complete example looks like this:

```sh
kdb mount test.ini user:/tests crypto "crypto/key=$(kdb gen-gpg-testkey)" base64 ini
kdb meta set user:/tests/password crypto/encrypt 1
kdb set user:/tests/password 1234
kdb set user:/tests/unencrypted "I am not encrypted"
kdb file user:/tests/password | xargs cat
```

To disable encryption on `user:/tests/password`, we can run:

```sh
kdb meta set user:/tests/password crypto/encrypt 0
kdb file user:/tests/password | xargs cat
```

To clean up the environment we run:

```sh
kdb rm user:/tests/unencrypted
kdb rm user:/tests/password
kdb umount user:/tests
```

To shut down the `gpg-agent` we run:

```sh
gpg-connect-agent --quiet KILLAGENT /bye
```

The shutdown of `gpg-agent` is optional.
