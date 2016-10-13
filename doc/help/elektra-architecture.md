elektra-architecture(7) -- architecture of elektra
==================================================

In this document we start to explain the implementation of Elektra.
There are several follow-up documents which explain all details of:

- [error handling](elektra-error-handling.md),
- [data structures](elektra-data-structures.md), and
- finally the [core algorithm](elektra-algorithm.md).

We discuss problems and the solution space so that the reader
can understand the rationale of how problems were solved.

To help readers to understand the algorithm that glues together the
plugins, we first describe some details of the
[data structures](elektra-data-structures.md). Full
knowledge of the [algorithm](elektra-algorithm.md) is not presumed to be able to develop
most plugins (with the exception of [the resolver](/src/plugins/resolver/)).

Further important concepts are explained in:

- [bootstrapping](elektra-bootstrapping.md)
- [granularity](elektra-granularity.md)
- [sync-flag](elektra-sync-flag.md)


## API

The aim of the Elektra Project is to design and implement a powerful
API for configuration.  When the project started, we assumed that this
goal was easy to achieve, but dealing with the semantics turned out to
be a difficult problem.  For the implementation, an ambitious solution
is required because of the necessary modularity to implement flexible
backends as introduced in Elektra.  But also the design of a good
API has proved to be much more difficult than expected.

### Changes in the APIs

From Elektra 0.7 to Elektra 0.8, we changed
the API of Elektra as little as possible.  It should be mentioned
that `KeySet` is now always sorted by name.  The function `ksSort()`
is now depreciated and was removed.  The handling of removed keys was
modified.  Additionally, the API for metadata has fundamentally changed,
but the old interface still works.  These changes will be described in
[implementation of meta data](elektra-meta-data.md). However, the implementation of
Elektra changed radically as discussed in [algorithm](elektra-algorithm.md).

### API Design

API Design presents a critical craft every programmer should be aware
of. We will shortly present some of the main design
issues that matter and show how Elektra has solved them.

A design goal is to detect errors early.  As easy as it sounds, as
difficult it is to actually achieve this goal.  Elektra tries to avoid
the problem by checking data being inserted into `Key` and `KeySet`.
Elektra catches many errors like invalid key names soon.
Elektra allows plugins to check
the configuration before it is written into the key database so that
problematic values are never stored.

"Hard to use it wrong" tends to be a more important design objective
than "Easy to use it right".  Searching for a stupid bug costs
more time than falling into some standard traps which are explained
in documentation.  In Elektra, the data structures are robust and some
efforts were taken to make misuse unlikely.

Another fundamental principle is that the API must hide implementation
details and should not be optimised towards speed.  In Elektra, the
actual process of making configuration permanent is completely hidden.

"Off-by-one confusion" is a topic of its own.  The best is to stick to
the conventions the programming language gives. For returning sizes of
strings, it must be clear whether a terminating `'\0'` is included or not.
All such decisions must be consistent.  In Elektra the terminating null
is always included in the size.

The interface must be as small as possible to tackle problems addressed
by the library.  Internal and external APIs must be separated. Internal
APIs in libraries shall be declared as `static` to prevent its export.
In Elektra, internal names start with `elektra` opposed to the external
names starting with `key`, `ks` or `kdb`.

Elektra always passes user context pointers, but never passes or receives
a full data structure by value.  It is impossible to be
ABI compatible otherwise.  Elektra
is restrictive in what it returns (strong postconditions), but as liberal
as possible for what comes in (preconditions are avoided where possible).
In Elektra even null pointers are accepted for any argument.

"Free everything you allocate" is a difficult topic in some cases.
If Elektra cannot free space or other resources after every call, it
provides a `close()` function.  Everything will be freed. The
tool **Valgrind** with **Memcheck** helps us locate problems. The
whole test suite runs without any memory problems. The user is
responsible for deleting all created `Key` and `KeySet` objects
and closing the `KDB` handle.

As a final statement, we note that the UNIX philosophy should always
be considered: "Do only one thing, but do it in the best way. Write it
that way that programs work together well."





## Modules

Elektra's core can be compiled with a C compiler conforming to the
ISO/IEC 9899:1999 standard:

- One line comments,
- inline functions,
- `snprintf()`
- inttypes.h and
- variable declaration at any place

are used in addition to what is already defined in the
standard ISO/IEC 9899:1990, called **C99** in the following text.
Functions not conforming to C99 are considered to be not portable
enough for Elektra and are separated into plugins.  But there is
one notable exception: it must be the core's task to load plugins.
Unfortunately, C99 does not know anything about modules.
**POSIX** (Portable Operating System Interface) provides
`dlopen()`, but other operating systems have dissimilar APIs for that
purpose.  They sometimes behave differently, use other names for the
libraries and have incompatible error reporting systems.  Because of
these requirements Elektra provides a small internal API to load such
modules independently from the operating system.  This API also hides the
fact that modules must be loaded dynamically if they are not available
statically.

Plugins are usually realised with modules.  Modules and libraries are
technically the same in most systems. (One exception is OS X.)
After the module is loaded, the special function plugin factory
is searched for.  This function returns a new plugin.  With the plugin
factory the actual plugins are created.

### Static loading

For the static loading of modules, the modules must be built-in.
With `dlopen(const` `char*` `file)` POSIX provides a solution to look
up such symbols by passing a null pointer for the parameter `file`.
Non-POSIX operating systems may not support this kind of static loading.
Therefore, Elektra provides a C99 conforming solution for that problem: a
data structure stores the pointers to the plugin factory of every plugin.
The build system generates the source file of this data structure because
it depends on built-in plugins.

Elektra distinguishes internally between modules and plugins. Several
plugins can be created out of a single module.  During the creation
process of the plugin, dynamic information - like the configuration or
the data handle - is added.

### API

The API of **libloader** consists
of the following functions:

Interface of Module System:

	elektraModulesInit (KeySet *modules, Key *error); elektraPluginFactory
	elektraModulesLoad (KeySet *modules,
			const char *name, Key *error);
	int elektraModulesClose (KeySet *modules, Key *error);

`elektraModulesInit()` initialises the module
cache and calls necessary operating system facilities if
needed. `elektraModulesLoad()` does the main work by either
returning a pointer to the plugin factory from cache or loading it from
the operating system. The plugin factory creates plugins that do not have
references to the module anymore. `elektraModulesClose()` cleans
up the cache and finalises all connections with the operating system.

Not every plugin is loaded by `libloader`.  For example, the
*version plugin*, which exports version information, is implemented
internally.

## Mount Point Configuration

`kdb mount` creates a **mountpoint configuration** as shown
in the example below.  `fstab` is a unique name
within the mountpoint configuration provided by the administrator.

Example for a mountpoint configuration:

	system/elektra/mountpoints system/elektra/mountpoints/fstab
	system/elektra/mountpoints/fstab/config
	system/elektra/mountpoints/fstab/config/path=fstab
	system/elektra/mountpoints/fstab/config/struct=list FStab
	system/elektra/mountpoints/fstab/config/struct/FStab
	system/elektra/mountpoints/fstab/config/struct/FStab/device
	system/elektra/mountpoints/fstab/config/struct/FStab/dumpfreq
	system/elektra/mountpoints/fstab/config/struct/FStab/mpoint
	system/elektra/mountpoints/fstab/config/struct/FStab/options
	system/elektra/mountpoints/fstab/config/struct/FStab/passno
	system/elektra/mountpoints/fstab/config/struct/FStab/type
	system/elektra/mountpoints/fstab/errorplugins
	system/elektra/mountpoints/fstab/errorplugins/#5#resolver#resolver#
	system/elektra/mountpoints/fstab/getplugins
	system/elektra/mountpoints/fstab/getplugins/#0#resolver
	system/elektra/mountpoints/fstab/getplugins/#5#fstab#fstab#
	system/elektra/mountpoints/fstab/mountpoint /fstab
	system/elektra/mountpoints/fstab/setplugins
	system/elektra/mountpoints/fstab/setplugins/#0#resolver
	system/elektra/mountpoints/fstab/setplugins/#1#struct#struct#
	system/elektra/mountpoints/fstab/setplugins/#2#type#type#
	system/elektra/mountpoints/fstab/setplugins/#3#path#path#
	system/elektra/mountpoints/fstab/setplugins/#3#path#path#/config
	system/elektra/mountpoints/fstab/setplugins/#3#path#path#/config/path/allow=proc tmpfs none
	system/elektra/mountpoints/fstab/setplugins/#5#fstab
	system/elektra/mountpoints/fstab/setplugins/#7#resolver

Let us look at the subkeys below the key
`system/elektra/mountpoints/fstab`:

- **config**:
Everything below `config` is the system's
configuration of the backend. Every plugin within the backend will find
this configuration directly below `system` in its
**plugin configuration**.  For example,

	system/elektra/mountpoints/fstab/config/struct/FStab/mpoint

will be translated to

	system/struct/FStab/mpoint

and inserted into the plugin configuration for all plugins in the
`fstab` backend.

It is the place where configuration can be provided for every plugin
of a backend.  The contract checker deduces this configuration to
satisfy the contract for a plugin.  Fstab, for example, claims in
a contract that it needs "struct".  But the struct plugin needs a
configuration to work properly.  Fstab will provide this configuration.
The **contract checker** writes out the configuration looking like
the one in this example.

- **config/path**:
is a common setting needed by the resolver plugin. It
is the relative path to a filename that is used by this backend.  On UNIX
systems, the resolver would determine the name `/etc/fstab`
for system configuration.

- **mountpoint**:
is a key that represents the mountpoint. Its value is
the location where the backend is mounted.  If a mountpoint has an entry
for both the user and the system hierarchy, it is called
**cascading mountpoint**.  A cascading mountpoint differs from two separate mount
points because internally only one backend is created.	In the example,
the mountpoint `/fstab` means that the backend handles both
`user/fstab` and `system/fstab`.  If the mountpoint
is `/`, the backend will be mounted to all namespaces except `spec`,
including both `user` and `system`.


- **errorplugins**:
presents a list of all plugins to be executed in
the error case of `kdbSet()` which will be explained in
**error situation**.

- **getplugins**:
is a list of all plugins used when reading the
configuration from the key database.  They are executed in `kdbGet()`.

- **setplugins**:
contains a list of all plugins used when storing
configuration.	They are executed in `kdbSet()`.


Each of the plugins inside the three lists may have the subkey
`config`.  The configuration below this subkey provides plugin specific
configuration.	This configuration appears in the user's configuration
of the plugin.	Configuration is renamed properly.  For example, the key

	system/elektra/mountpoints/fstab/setplugins/#3#path#path#/config/path/allow

is transformed to

	user/path/allow

and appears in the plugin configuration of the path plugin
inside the fstab backend.

### Referencing

The same plugin often must occur in more than one place within a
backend. The most common use case is a plugin that has to be executed
for both `kdbGet()` and `kdbSet()`.  It must be the same plugin if it
preserves state between the executions.

Other plugins additionally have to handle error or success situations.
One example of exceptional intensive use is the resolver plugin. It is
executed twice in `kdbSet()`.  In `kdbGet()` it is also used as shown
above.

* `#n<name>`:
  introduces a new plugin from the module
  `name` which cannot be referenced later.  The cypher `n` appoints the
  actual placement of the plugin.
* `#n#<name>#<label>#`:
  also introduces a new plugin from the module `name` and gives it the
  name `label`.  The last `#` shows that a new name is being introduced.
* `#n#<ref>`:
  references back to
  a label which was introduced before. This configuration does not create
  a new plugin.

`kdb mount` already implements the generation of these names as described above.


### Changing Mount Point Configuration

When the user changes the mountpoint configuration, without
countermeasures, applications already started will continue to run with
the old configuration.	This could lead to a problem if backends in use
are changed or removed.  It is necessary to restart all such programs.
Notification is the best way to deal with the situation.  Changes of
the mountpoint configuration, however, do not occur often.  For some
systems, the manual restart may also be appropriate.

In this situation, applications can receive warning or error information
if the configuration files are moved or removed.  The most adverse
situation occurs if the sequence of locking multiple files produces a
*dead lock*.  Under normal circumstances, the sequence of locking
the files is deterministic, so either all locks can be requested or
another program will be served first. But several programs with different
mountpoint configurations running at the same time can cause a disaster.
The problem gets even worse, because `kdb mount` is unable to detect
such situations.  Every specific mountpoint configuration for itself
is trouble-free.

But still a dead lock can arise when multiple programs run with different
mountpoint configurations.  Suppose we have a program `A` which uses the
backends `B1` and `B2` that requests locks for the files `F1` and `F2`.
Then the mountpoint configuration is changed.	The user removes `B1`
and introduces `B3`.  `B3` is in a different path mounted after `B2`, but
also accesses the same file `F1`.  The program `B` starts after the mount
point configuration is changed.  So it uses the backends `B2` and `B3`.
If the scheduler decides that first `A` and then `B` both successfully
lock the files `F1` and `F2`, a dead lock situation happens because in
the afterwards the applications `A` and `B` try to lock `F2` and `F1`.

A manual solution for this problem is to enable `kdb` to output a
list of processes that still use old mountpoint configuration. The
administrator can restart these processes.  The preferred solution is
to use notification for mountpoint configuration changes or simply to
use a lock-free resolver.


Continue reading [with the data structures](elektra-data-structures.md).
