elektra-mounting(7) -- mounting
===============================

**Mounting** allegorises a common technique for file systems.
File systems on different partitions or devices can be added to the
currently accessible file system.  Mounting is typically used to access
data from external media.  A more advanced use case presents mounting
a file system that is optimised for specific purposes, for example,
one that can handle many small files well.  Mounting also allows us to
access data via network storage.  As a result, mounting of file systems
has proved to be extremely successful.

Mounting in Elektra specifically allows us to map a part of the global
key database to be handled by a different storage.  A difference to
file systems is that key names express what file names express in a file
system.  And instead of file systems writing to block devices, backends
writing to key databases are mounted into the global key database.
Mounting allows multiple backends to deal with configuration at the
same time. Each of them is responsible for its own subtree of the global
key database.

Mounting works for file systems only if the file system below is
accessible and a directory exists at the mount point.  Elektra does not
enforce such restrictions.
