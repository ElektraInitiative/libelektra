elektra-backends(7) -- the backend concept
==========================================

Elektra has introduced *backends** to support the storage of
key databases in different formats.
Elektra abstracts configuration so that applications can receive
and store settings without carrying information
about how and where these are actually stored.
It is the purpose of the backends to implement these details.

Since Elektra 0.8 a backend is composed of many plugins.

## SEE ALSO

- More information about [elektra-plugins(7)](elektra-plugins.md)
- The tool for mounting a backend is [kdb-mount(1)](kdb-mount.md)
- The plugins are ordered as described in [elektra-plugins-ordering(7)](elektra-plugins-ordering.md)
