# Cryptographic Key Handling

## Issue

The crypto plugin applies cryptographic operations to Keys and KeySets.
In order to do that it needs keys and initialization vectors (IV).
The problem is how to retrieve or derivate those keys in a safe way and how 
to pass them on to the underlying crypto libraries (OpenSSL and libgcrypt 
at the time of writing).

## Constraints

The solution must be reasonable to the user (not just experimental setups).

The key handling should follow best practises when dealing with cryptographic
operations, thus:

- the combination of key and IV must not be used twice for symmetric encryption
- the key and IV material must be as random as possible (using an SNRG)
- the key is not exposed to other Elektra modules or the user

## Assumptions

- The user knows or is willing to learn how to use GPG.
- The user is willing to configure `gpg` and `pinentry` on her system, as it does not always run out of the box (depending on the distribution)

## Considered Alternatives

We considered passing the key and IV in form of plugin configuration and meta-keys, but this approach violates our constraints.

## Decision

### General Approach

The introduction of a KDF will enable the user to provide a single password, which is used to derive the key and the IV for the cryptographic operations. Both OpenSSL and libgcrypt have built-in support for the PBKDF2 (see RFC 2898).

Instead of having the keys `crypto/iv` and `crypto/key` within the plugin configuration, we will have `crypto/password` for the moment. In the future there will be other ways of requesting this password (like the pgp-agent but this will be another issue).

The Key `crypto/password` (as part of the plugin config) will be used to derive the cryptographic key and the IV.

The PBKDF2 needs an iteration number and a salt in order to work. Those values will be stored per Key as MetaKey.

### Implementation Details

During the **mount phase** a random master password _r_ is being generated. _r_ is sent to the gpg binary for encryption. The resulting encrypted master password _m_ is stored in the plugin configuration at `config/masterChallenge`.

During the **set phase** the master password _m_ is sent to the gpg binary for decryption in order to retrieve _r_. The following steps will be repeated for every Key _k_, that is supposed to be encrypted. A random salt _s(k)_ is generated. By applying the PBKDF2 (mentioned earlier) with _r_ and _s(k)_, the cryptographic key _e(k)_ and the initialization vector _i(k)_ is being derived. The value of _k_ will be encrypted using _e(k)_ and _i(k)_. The seed _s(k)_ will be encoded as prefix into the encrypted value.

During the **get phase** the master password _m_ is sent to the gpg binary for decryption in order to retrieve _r_. The following steps will be repeated for every Key _k_, that is supposed to be decrypted. The salt _s(k)_ is read from the encrypted message. By applying the PBKDF2 with _r_ and _s(k)_ the values of _e(k)_ and _i(k)_ are restored. Then the encrypted message can be decrypted.

## Argument

The solution is reasonable to all users who are in favor of GPG.
Also the solution might lead to a decline in dependencies (i.e. all cryptographic operations could be handled by the gpg binary).

The constraints considering the key handling are met.

## Implications

To manipulate the plugin configuration during the **mount phase** a hook is required within the `BackendBuilder`.
See pull request [#733](https://github.com/ElektraInitiative/libelektra/pull/733) for full discussion.

## Related decisions

## Notes
