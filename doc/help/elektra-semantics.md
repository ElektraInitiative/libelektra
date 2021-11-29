# elektra-semantics(7) -- Semantics of KDB

The use of arbitrary metadata has extensive effects in Elektra’s
semantics. They become simpler and more suited to carry key value pairs.
The semantics now gives us independence of the underlying file system.
So none of the file system's restrictions apply anymore. No constraints
on the length of a key name disturbs the user anymore. Additionally,
key names can be arbitrarily deep nested. Depth is the number
of unescaped `/` in the key name.

The directory concept is enforced by default. Keys can be created
everywhere. Keys always can have a value. The only constraint is
that key names are unique and occur in one of the
[namespaces](elektra-namespaces.md).
Every Key has an absolute name. There is no concept of relative names in
Elektra’s Keys except for metakeys belonging to a key. Every other Key is independent
of each other. We just do not care if there is another key below or
above the accessed one in the storage or not.

Some applications need specific structure in the keys. Plugins can
introduce and enforce relationships between keys. They can implement
a type system, check if holes are present and check the structure
and interrelations. They may propagate the metadata and introduce
inheritance. We see that plugins are able to add more semantics to
Elektra.

There are no symbolic links, hard links, device files or anything else
different from key value pairs. Again, most of these behaviors can be
mimicked using metadata. Especially, links are available using the metadata
`override` and `fallback`.

Hidden keys are not useful for Elektra. Instead comments or other
metadata contain information about keys that is not considered belonging
to the configuration. If hidden keys are desired, we can still write
a plugin to filter specific keys out.

## Problems

This section explains why using file system semantics for configuration
is not a good idea.

### filesys

`filesys` was the first backend. It implemented the principle that
every key is represented by a single file. The key name was actually
mapped to a file name and the value and the comment was written to
that file.

If the backend `filesys` was the ideal solution, Elektra’s
API (application programming interface) would be of limited
use. E.g.cascading, type checking and
optional cross-cutting features would be missing. The storage problem
itself and the location of a key in a key database would be solved.
because well-established APIs for accessing files are available in every
applicable programming language.

Elektra 0.7 already supported more than one backend, but `filesys`
was the only backend implementing the full semantics.

### Limitations of File Systems

Here we will discuss, why the file system's semantics are not
well suited for configuration at all.

One file per key turned out to be inefficient because of the file system's
practical limitations. In most file systems, a file needs about four
kilobytes (Depends on the block size, four kilobytes is a common
value often used as default.) of space, no matter how little content is
in it. Thus the file system wastes 99.9% of the space if keys have a
payload of four bytes. Additionally, every file allocates a file node,
which might be limited, too. We can argue, however, that we can use a file
system which does not have this problem.

Many additional restrictions occur for portable access. The file name
length in POSIX is limited to fourteen characters. Additionally, issues
with case sensitivity are likely. The common denominator for all file
systems is a surprisingly small one. If, for example, the traditional
FAT file system should be supported, file names are limited to eight
characters and case insensitivity.

On the one hand, there are many file system features that are
not needed for configuration. File systems have a strict hierarchy.
It is not possible to create a
file in a non-existing directory. We will refer to such a missing object
as **hole**. File systems do not support such holes.

A single **root directory** is not a useful
concept for configuration. Instead, the system configuration and each
user configuration has its own root. These root keys themselves are
typically not needed.

There is additional
metadata of files which is typically not needed for configuration: atime,
mtime, ctime, uid, gid and mode just to name a few. Additional file
types, for example, device files, links, fifos and sockets,
are not needed either. Features like sparse files are ridiculous for
the small strings, that key values typically are.

On the other hand, there are many _features missing_ in file systems
that we need in a serious key database. Creating a whole hierarchy
of files at once atomically is not possible. Ways to achieve this are
currently academic and not portable. Directories cannot
have any content next to the files below. Swap semantics are missing:
it is not possible to rename a file without removing the target first.

To sum up, file systems are not suitable to
hold configuration with one entry per file. Instead, they are perfectly
suitable to hold larger pieces of information like configuration files.
