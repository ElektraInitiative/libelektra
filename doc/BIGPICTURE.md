# Big Picture

Elektra solves a non-trivial issue: how to abstract configuration
in a way that software can be integrated and reconfiguration can
be automated. Elektra solves this problem in a holistic way.
Read [why Elektra](WHY.md) for an explanation of why such a
solution is necessary.

## Virtual File System Analogy

If you know virtual file systems, you already know a very similar solution
to a very similar problem (otherwise first read about what a virtual
file system is [here](https://en.wikipedia.org/wiki/Virtual_file_system)).

Before file systems (or for devices without operating system) software
simply wrote at discs (think of `dd of=/dev/sda`) to persistently store
data. This obviously does not work with multiple applications. To allow
multiple applications to access data in `/dev/sda`, a file system structures
them in a way that every application knows where its bits are. In analogy
configuration files are application-specific initialization that cannot
be shared with other applications. So as first steps we need to have a
"file system" that describes the content of a configuration file in a
uniform way. In Elektra [plugins](/src/plugins/README.md) represent
file systems: they know how the data in configuration files should
be interpreted.

For file systems, the API is `open`, `read`, `write`, and then `close`.
For configuration key-value access is more suitable because values are
so small that a single read/write always suffices. Thus Elektra [has a
key-value API](https://doc.libelektra.org/api/latest/html) with `kdbOpen`,
`kdbGet`, `kdbSet` and `kdbClose`. Not every application is written in C,
thus many `bindings` were written to access file systems. For example,
in C++ you have `fstream`, and in Java `FileReader`. Also Elektra provides
different [bindings](/src/bindings/README.md): In C++ you have a class
`KDB` and can use `kdb.get` or `kdb.set`. Furthermore, every language
has native support for the language's iterators which make Elektra easier
to use.

Furthermore, command-line tools like `cat` and `ls` provide an additional
interface to the content of file systems for users and administrators.
In Elektra also [command-line tools](/doc/help/kdb.md)
for the analog purpose exist: With `kdb cp`, `kdb mv`, and `kdb ls`
some command-line tools operating on file systems are mimicked.

There is not one file system satisfying every need, e.g., `proc`, `tmpfs`,
`nfs` and `ext4` have quite different use cases and are needed at the
same time in parallel. In analogy, different configuration file format
(parsers) have different advantages and disadvantages. For example,
`/etc/passwd` or `/etc/hosts` are structured, while
many other configuration files are semi-structured.

To have multiple file systems present at the same time a virtual
file system is able to `mount (2)` concrete file systems and thus give
applications a way to uniformly access them. Similarly, Elektra also
implements a [mount](/doc/help/elektra-mounting.md) functionality in
its core. In Elektra a [contract](/doc/CONTRACT.ini) specifies the
obligations between plugins.

Sometimes, it is even useful to have multiple file systems at
the same mount point, so-called `stacked` file systems. They allow
you to combine features of different file systems. For example,
[eCryptfs](https://wiki.archlinux.org/index.php/ECryptfs) allows you
to encrypt directories or files. In Elektra, stacking plugins is a core
feature and heavily used to avoid feature-bloated plugins. For example,
the [crypto plugin](/src/plugins/crypto/README.md) allows you to encrypt
individual keys or the [iconv plugin](/src/plugins/iconv/README.md)
to change the character encoding.

In file systems metadata describes information about files, e.g.
when they were last accessed and who they are owned by (`ls -l`). In the
same way Elektra has metadata that describe individual key-value pairs.
In Elektra metadata is [defined globally](/doc/METADATA.ini) but implemented
in many [plugins](/src/plugins/README.md).

Implementations of file systems is not an easy task. The idea of
FUSE (Filesystem in Userspace) is to make file system development
easier by having the conveniences of userspace together with a
helper library `libfuse`. In particular this allowed developers to
use any programming language and easier abstractions. Elektra also
tries hard to make plugin development simple. For example, special
[interpreter plugins](/src/plugins/README.md) enable developers
to also write plugins in different languages. Furthermore,
[other libraries](/src/libs/README.md) also assist in creating plugins.

Of course not every feature of virtual file systems or Elektra has
an analogy in the other system. If they would solve the same problem,
one of them would be useless. The main differences are:

- API (get/set vs. read/write)
- commit semantics: one `kdbSet` can change many configuration files atomically.
  This is important if you want, e.g., a new host in `/etc/hosts` and use this
  host in some other configuration files.
- [namespaces](/doc/help/elektra-namespaces.md) there are many places
  where the same configuration is stored. All of these configuration
  files have the same semantics and they override each other (think of
  command-line arguments, `/etc`, `$HOME/.config`, ...)
- Elektra interacts closely with the program execution environment
  such as command-line parsing. The namespace `proc` is specifically
  reserved for this purpose.
- in Elektra it is possible to create holes (files without directories
  above them) which are needed because of these override semantics:
  we want to be able to override a single value without duplicating the
  whole skeleton.
- validation: in Elektra you can describe how valid configuration should
  look like and reject invalid configuration.
- and much more...

## Further Readings

- [Compile](COMPILE.md) and [Install](INSTALL.md) Elektra
- Then continue reading the [tutorials](/doc/tutorials)
- Read about [bindings](/src/bindings/)
- Read about [tools](/src/tools/README.md)
- Look into [the glossary](/doc/help/elektra-glossary.md).
- Start reading about [command-line tools](/doc/help/kdb.md)
