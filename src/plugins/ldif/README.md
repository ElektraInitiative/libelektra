# LDIF Storage Plugin

LDIF is the file format which is used by LDAP to exchange data. Mainly, this format is used to exchange user records for services with a large amount of users. Furthermore, this format is used by OpenLDAP in order to configure it.

As no storage plugin for LDIF already exists, we want to introduce it. Our approach is to use the `libldap` which includes an API which is able to read and write the LDIF file format.

The LDIF format is defined [here](https://datatracker.ietf.org/doc/html/rfc2849).

The plugin itself is implemented in the C programming languages.

## Scope

- Reading LDIF files
- Writing LDIF files

### Additional Keys

Supporting "holes" does not seem to be possible in this case, as in LDIF, every parent key of a key is always a valid key too and at least contains the `dn` attribute.

### Differentiation Between Empty Keys and Keys Containing an Empty String

As beforementioned, "holes" are not possible, but every key has at least the `dn` attribute which makes the differention obsolete.

### Support Values Inside Non-Leaf Keys

Supporting values inside non-leaf keys is mandatory as this is the essence of directory services an respectively the LDIF format.

### Support Array And Non-Array Data Properly

As the LDIF format is multi valued per key, supporting arrays is mandatory

### Storing Comments

Storing comments cannot be easily achieved with `libldap` as it does not provide an interface to deserialize them.
Besides from the order and comments, there exist no further meta data in ldif which answers the question about support additional meta data.

### Ordering of Elements

The ordering of the elements is preserved.
This is not only a nice to have but mandatory as LDIF files are order sensitive in general.

## Consideration of Alternatives

While there exist the `ldaptive` library for java and the `ldap3` crate for rust, the crate has not received a stable release yet and does not seem to be widely used.
The `libldap` is developed within OpenLDAP which exists since 1998 and is used in basically every client application on Linux which requires LDIF support.
So using it allows us using C and a library which is installed on the vast majority on Linux systems.

## Team

The team consists of the members @bhampl and @Eiskasten.
