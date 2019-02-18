# elektra-mounting(7) -- mounting

**Mounting** is the process of integrating a backend that reads and writes
a specific configuration file into the global key database.
Mounting allows you to use different configuration files but also
allows you to change the behavior of writing/reading keys
to/from the global key database. For example, you need to mount if you want to:

- change the syntax of a configuration file,
- log every change of a configuration file,
- validate a configuration file on every access,
- change the representation (e.g. the date-format or booleans), and
- everything else [plugins](/src/plugins/README.md) can do.

**Mounting** allegorises a common technique for
[virtual file systems](/doc/BIGPICTURE.md).
File systems on different partitions or devices can be added to the
currently accessible file system. Mounting is typically used to access
data from external media. A more advanced use case presents mounting
a file system that is optimised for specific purposes, for example,
one that can handle many small files well. Mounting also allows us to
access data via network storage. As a result, mounting of file systems
has proved to be extremely successful.

Mounting in Elektra specifically allows us to map a part of the global
key database to be handled by a different storage. A difference to
file systems is that key names express what file names express in a file
system. And instead of file systems writing to block devices, backends
writing to key databases are mounted into the global key database.
Mounting allows multiple backends to deal with configuration at the
same time. Each of them is responsible for its own subtree of the global
key database.

Mounting works for file systems only if the file system below is
accessible and a directory exists at the mount point. Elektra does not
enforce such restrictions.

Note, that you cannot mount the same configuration file multiple times.
You can, however, use the specification to link between configuration
items which gives an impression of a _bind mount_, i.e. having the
same configuration values on multiple places.

## SEE ALSO

- See [elektra-glossary(7)](elektra-glossary.md)
- More information about [elektra-backends(7)](elektra-backends.md)
- The tool for mounting plugins is [kdb-mount(1)](kdb-mount.md)
- [Back to main page](/README.md) if you started from there
