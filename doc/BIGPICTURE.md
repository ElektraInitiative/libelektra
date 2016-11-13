# Big Picture #

Elektra solves a non-trivial issue: how to abstract configuration
in a way that software can be integrated and reconfiguration can
be automated.  Elektra solves this problem in a holistic way.
Read [why Elektra](WHY.md) for an explanation of why such an
solution is necessary.


## Virtual File System Analogy ##

If you know virtual file systems, you already know a very similar
solution to a very similar problem (otherwise skip this section,
as it won't help you or you can first read about what a virtual
file system is: https://en.wikipedia.org/wiki/Virtual_file_system.

Before file systems (or devices without operating system) software
simply wrote at discs (think of `dd of=/dev/sda`) to persistently store
data. This obviously does not work with multiple programs. To allow
multiple programs access data in `/dev/sda`, a file system structures
them in a way that every program knows where its bits are. In analogy
configuration files are a program-specific initialization that cannot
be shared with other programs. So as first steps we need to have a
"file system" that describes the content of a configuration file in a
uniform way. In Elektra [plugins](/src/plugins/) represent file systems:
they know how the data in configuration files should be interpreted.

For file systems, the API is `open`, `read`, `write`, and then `close`.
For configuration key/value access is more suitable because values are
so small that a single read/write always suffices. Thus Elektra [has
its own API](http://doc.libelektra.org/api/current/html).  Not every
program is written in C, thus many `bindings` where written to access
file systems. Also Elektra provides different [bindings](/src/bindings/).

Furthermore, command-line tools like `cat` and `ls` provide an additional
interface to the content of file systems for users and administrators.
In Elektra also [command-line tools](/doc/help/kdb-introduction.md)
for the analog purpose exist.

In the same way that there is not one file system satisfying
everybody's needs, different configuration file format parsers
have different advantages and disadvantages. To have multiple file
systems present at the same time.  A virtual file system is able to
[mount](doc/help/elektra-mounting.md) concrete file systems and thus
give applications a way to uniformly access them. Elektra in its core
exactly implements this. A [contract](/doc/CONTRACT.ini) specifies the
interface between plugins and the core.

In file systems metadata describes information about files, e.g.
when they were last accessed and who they are owned by (`ls -l`). In the
same way Elektra has metadata that describe individual key/value pairs.
In Elektra metadata is defined [here](/doc/METADATA.ini) and implemented
in [plugins](/src/plugins/).

Of course not every feature of virtual file systems or Elektra has
its analogy in the other system. If they would solve the same problem,
one of them would be useless.  Main differences are:

- API (get/set vs. read/write)
- [namespaces](/doc/help/elektra-namespaces.md) there are many places where the same configuration is stored which
  have the same semantics and override each other (think of command-line arguments, `/etc`, `$HOME/.config`, ...)
- in Elektra it is possible to create holes (files without directories above it)
  because of these override semantics: we want to be able to override a single value without duplicating the
  whole skeleton.
- validation: in Elektra you can describe how valid configuration should look like and reject
  invalid configuration.
- and much more...



## Further pointers ##

- Read the [tutorials](/doc/tutorials)
- Look into [the glossary](elektra-glossary.md).
- Start reading about [command-line tools](/doc/help/kdb-introduction.md)
