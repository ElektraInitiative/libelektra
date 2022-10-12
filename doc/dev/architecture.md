# Architecture

In this document we start to explain the implementation of Elektra.
There are several follow-up documents which explain all details of:

- [error handling](error-handling.md),
- [data structures](data-structures.md), and
- finally the [core algorithm](algorithm.md).

We discuss problems and the solution space so that the reader
can understand the rationale of how problems were solved.

To help readers to understand the algorithm that glues together the
plugins, we first describe some details of the
[data structures](data-structures.md). Full
knowledge of the [algorithm](algorithm.md) is not presumed to be able to develop
most plugins.

Further important concepts are explained in:

- [bootstrapping](/doc/help/elektra-bootstrapping.md)
- [granularity](/doc/help/elektra-granularity.md)
- [sync-flag](/doc/help/elektra-sync-flag.md)

## Outdated

<!-- TODO [new_backend]: Update the text below using the docs listed in the warning. -->

> **Warning** Many of the things described below (especially in relation to backends and mountpoints) are outdated.
> See [`kdb-operations.md`](kdb-operations.md), [`backend-plugins.md`](backend-plugins.md) and [`mountpoints.md`](mountpoints.md) for more up-to-date information.

## API

The aim of the Elektra Initiative is to design and implement a powerful
API for configuration. When the project started, we assumed that this
goal was easy to achieve, but dealing with the semantics turned out to
be a difficult problem. For the implementation, an ambitious solution
is required because of the necessary modularity to implement flexible
backends as introduced in Elektra. But also the design of a good
API has proved to be much more difficult than expected.

### Changes in the APIs

From Elektra 0.7 to Elektra 0.8, we changed
the API of Elektra as little as possible. It should be mentioned
that `KeySet` is now always sorted by name. The function `ksSort()`
is now depreciated and was removed. The handling of removed keys was
modified. Additionally, the API for metadata has fundamentally changed,
but the old interface still works. These changes will be described in
[implementation of metadata](metadata.md). However, the implementation of
Elektra changed radically as discussed in [algorithm](algorithm.md).

### API Design

API Design presents a critical craft every programmer should be aware
of. We will shortly present some of the main design
issues that matter and show how Elektra has solved them.

A design goal is to detect errors early. As easy as it sounds, as
difficult it is to actually achieve this goal. Elektra tries to avoid
the problem by checking data being inserted into `Key` and `KeySet`.
Elektra catches many errors like invalid key names soon.
Elektra allows plugins to check
the configuration before it is written into the key database so that
problematic values are never stored.

"Hard to use it wrong" tends to be a more important design objective
than "Easy to use it right". Searching for a stupid bug costs
more time than falling into some standard traps which are explained
in documentation. In Elektra, the data structures are robust and some
efforts were taken to make misuse unlikely.

Another fundamental principle is that the API must hide implementation
details and should not be optimized towards speed. In Elektra, the
actual process of making configuration permanent is completely hidden.

"Off-by-one confusion" is a topic of its own. The best is to stick to
the conventions the programming language gives. For returning sizes of
strings, it must be clear whether a terminating `'\0'` is included or not.
All such decisions must be consistent. In Elektra the terminating null
is always included in the size.

The interface must be as small as possible to tackle problems addressed
by the library. Internal and external APIs must be separated. Internal
APIs in libraries shall be declared as `static` to prevent its export.
In Elektra, internal names start with `elektra` opposed to the external
names starting with `key`, `ks` or `kdb`.

Elektra always passes user context pointers, but never passes or receives
a full data structure by value. It is impossible to be
ABI compatible otherwise. Elektra
is restrictive in what it returns (strong postconditions), but as liberal
as possible for what comes in (preconditions are avoided where possible).
In Elektra even null pointers are accepted for any argument.

"Free everything you allocate" is a difficult topic in some cases.
If Elektra cannot free space or other resources after every call, it
provides a `close()` function. Everything will be freed. The
tool **Valgrind** with **Memcheck** helps us locate problems. The
whole test suite runs without any memory problems. The user is
responsible for deleting all created `Key` and `KeySet` objects
and closing the `KDB` handle.

As a final statement, we note that the Unix philosophy should always
be considered: "Do only one thing, but do it in the best way. Write it
that way that programs work together well."

## Modules

Elektra’s core can be compiled with a C compiler conforming to the
ISO/IEC 9899:1999 standard, called C99 henceforth.
Functions not conforming to C99 are considered to be not portable
enough for Elektra and are separated into plugins. But there is
one notable exception: it must be `libelektra-kdb`'s task to load plugins.
Unfortunately, C99 does not know anything about modules.
**POSIX** (Portable Operating System Interface) provides
`dlopen()`, but other operating systems have dissimilar APIs for that
purpose. They sometimes behave differently, use other names for the
libraries and have incompatible error reporting systems. Because of
these requirements Elektra provides a small internal API to load such
modules independently from the operating system. This API also hides the
fact that modules must be loaded dynamically if they are not available
statically.

Plugins are usually realized with modules. Modules and libraries are
technically the same in most systems. (One exception is macOS.)
After the module is loaded, the special function plugin factory
is searched for. This function returns a new plugin. With the plugin
factory the actual plugins are created.

### Static Loading

For the static loading of modules, the modules must be built-in.
With `dlopen(const char* file)` POSIX provides a solution to look
up such symbols by passing a null pointer for the parameter `file`.
Non-POSIX operating systems may not support this kind of static loading.
Therefore, Elektra provides a C99 conforming solution for that problem: a
data structure stores the pointers to the plugin factory of every plugin.
The build system generates the source file of this data structure because
it depends on built-in plugins.

Elektra distinguishes internally between modules and plugins. Several
plugins can be created out of a single module. During the creation
process of the plugin, dynamic information - like the configuration or
the data handle - is added.

### API

The API of **libloader** consists
of the following functions:

Interface of Module System:

```c
int elektraModulesInit (KeySet *modules, Key *error);
elektraPluginFactory elektraModulesLoad (KeySet *modules,
		const char *name, Key *error);
int elektraModulesClose (KeySet *modules, Key *error);
```

`elektraModulesInit()` initializes the module
cache and calls necessary operating system facilities if
needed. `elektraModulesLoad()` does the main work by either
returning a pointer to the plugin factory from cache or loading it from
the operating system. The plugin factory creates plugins that do not have
references to the module anymore. `elektraModulesClose()` cleans
up the cache and finalises all connections with the operating system.

Not every plugin is loaded by `libloader`. For example, the
_version plugin_, which exports version information, is implemented
internally.

## Mount Point Configuration

`kdb mount` creates a **mount point configuration** as shown
in the example below. `/hosts` is a unique name
within the mount point configuration provided by the administrator.
To escape the `/` character, the name is changed to `\/hosts` in the
configuration.

Example for a mount point configuration:

```
system:/elektra/mountpoints/\/hosts
system:/elektra/mountpoints/\/hosts/config
system:/elektra/mountpoints/\/hosts/config/glob/set/#0
system:/elektra/mountpoints/\/hosts/config/glob/set/#1
system:/elektra/mountpoints/\/hosts/config/glob/set/#2
system:/elektra/mountpoints/\/hosts/config/glob/set/#3
system:/elektra/mountpoints/\/hosts/config/glob/set/#4
system:/elektra/mountpoints/\/hosts/config/glob/set/#4/flags
system:/elektra/mountpoints/\/hosts/config/mountpoint
system:/elektra/mountpoints/\/hosts/config/path
system:/elektra/mountpoints/\/hosts/error
system:/elektra/mountpoints/\/hosts/error/rollback
system:/elektra/mountpoints/\/hosts/error/rollback/#0
system:/elektra/mountpoints/\/hosts/error/rollback/#0/label (="resolver")
system:/elektra/mountpoints/\/hosts/error/rollback/#0/name (="resolver_fm_hpu_b")
system:/elektra/mountpoints/\/hosts/get
system:/elektra/mountpoints/\/hosts/get/postgetstorage
system:/elektra/mountpoints/\/hosts/get/postgetstorage/#0
system:/elektra/mountpoints/\/hosts/get/postgetstorage/#0/label (="glob")
system:/elektra/mountpoints/\/hosts/get/postgetstorage/#0/name (="glob")
system:/elektra/mountpoints/\/hosts/get/getresolver
system:/elektra/mountpoints/\/hosts/get/getresolver/#0
system:/elektra/mountpoints/\/hosts/get/getresolver/#0/reference (="resolver")
system:/elektra/mountpoints/\/hosts/get/getstorage
system:/elektra/mountpoints/\/hosts/get/getstorage/#0
system:/elektra/mountpoints/\/hosts/get/getstorage/#0/label (="hosts")
system:/elektra/mountpoints/\/hosts/get/getstorage/#0/name (="hosts")
system:/elektra/mountpoints/\/hosts/set
system:/elektra/mountpoints/\/hosts/set/commit
system:/elektra/mountpoints/\/hosts/set/commit/#0
system:/elektra/mountpoints/\/hosts/set/commit/#0/reference (="resolver")
system:/elektra/mountpoints/\/hosts/set/precommit
system:/elektra/mountpoints/\/hosts/set/precommit/#0
system:/elektra/mountpoints/\/hosts/set/precommit/#0/label (="sync")
system:/elektra/mountpoints/\/hosts/set/precommit/#0/name (="sync")
system:/elektra/mountpoints/\/hosts/set/presetstorage
system:/elektra/mountpoints/\/hosts/set/presetstorage/#0
system:/elektra/mountpoints/\/hosts/set/presetstorage/#0/reference (="glob")
system:/elektra/mountpoints/\/hosts/set/presetstorage/#1
system:/elektra/mountpoints/\/hosts/set/presetstorage/#1/label (="error")
system:/elektra/mountpoints/\/hosts/set/presetstorage/#1/name (="error")
system:/elektra/mountpoints/\/hosts/set/presetstorage/#2
system:/elektra/mountpoints/\/hosts/set/presetstorage/#2/label (="network")
system:/elektra/mountpoints/\/hosts/set/presetstorage/#2/name (="network")
system:/elektra/mountpoints/\/hosts/set/setresolver
system:/elektra/mountpoints/\/hosts/set/setresolver/#0
system:/elektra/mountpoints/\/hosts/set/setresolver/#0/reference (="resolver")
system:/elektra/mountpoints/\/hosts/set/setstorage
system:/elektra/mountpoints/\/hosts/set/setstorage/#0
system:/elektra/mountpoints/\/hosts/set/setstorage/#0/reference (="hosts")

```

Let us look at the subkeys below the key
`system:/elektra/mountpoints/\/hosts`:

- **config**:
  Everything below `config` is the system's
  configuration of the backend. Every plugin within the backend will find
  this configuration directly below `system` in its
  **plugin configuration**. For example,

  ```
  system:/elektra/mountpoints/\/hosts/config/glob/set/#4/flags
  ```

  will be translated to

  ```
  system:/glob/set/#4/flags
  ```

  and inserted into the plugin configuration for all plugins in the
  `/hosts` backend.

It is the place where configuration can be provided for every plugin
of a backend. The contract checker deduces this configuration to
satisfy the contract for a plugin.

For example, `/hosts` recommends the use of the `glob` plugin. In this case, it is included.
To work properly, the `glob` plugin needs to receive certain configuration, which is provided
by `/hosts`:

```
system:/elektra/mountpoints/\/hosts/config/glob/set/#0
system:/elektra/mountpoints/\/hosts/config/glob/set/#1
system:/elektra/mountpoints/\/hosts/config/glob/set/#2
system:/elektra/mountpoints/\/hosts/config/glob/set/#3
system:/elektra/mountpoints/\/hosts/config/glob/set/#4
system:/elektra/mountpoints/\/hosts/config/glob/set/#4/flags
```

The **contract checker** writes out the configuration looking like
the one in this example.

- **config/path**:
  is a common setting needed by the resolver plugin. It
  is the relative path to a filename that is used by this backend. On Unix
  systems, the resolver would determine the name `/etc/hosts`
  for system configuration.

- **config/mountpoint**:
  is a key that represents the mount point. Its value is
  the location where the backend is mounted. If a mount point has an entry
  for both the user and the system hierarchy, it is called
  **cascading mount point**. A cascading mount point differs from two separate mount
  points because internally only one backend is created. In the example,
  the mount point `/hosts` means that the backend handles both
  `user:/hosts` and `system:/hosts`. If the mount point
  is `/`, the backend will be mounted to `dir:/`, `user:/` and `system:/`.

* **error**:
  presents a list of all plugins to be executed in
  the error case of `kdbSet()` which will be explained in
  **error situation**.

* **get**:
  is a list of all plugins used when reading the
  configuration from the key database. They are executed in `kdbGet()`.

* **set**:
  contains a list of all plugins used when storing
  configuration. They are executed in `kdbSet()`.

Each of the plugins inside the three lists may itself have the subkey
`config`. The configuration below this subkey provides plugin specific
configuration. This configuration appears in the user's configuration
of the plugin. Configuration is renamed properly. For a fictional backend named
`backendname` and a plugin named `pluginname` containing configuration
named `property`, the key

```
system:/elektra/mountpoints/backendname/set/storage/config/pluginname/property
```

is transformed to

```
user:/pluginname/property
```

and appears in the plugin configuration of the `pluginname` plugin
inside the `backendname` backend.

### Roles and Placements

Expressions after `get`, `set` or `error` such as `poststorage`, `storage` or `resolver`
describe the role that the plugin fulfills. For example, the key

```
system:/elektra/mountpoints/\/hosts/set/precommit/#0/name
```

belongs to a plugin fulfilling the `precommit` role.

The cypher `#0` describes the placement of the plugin in relation to potential other plugins
fulfilling the same role. More on roles and placements [here](plugins-ordering.md).

### Referencing

The same plugin often must occur in more than one place within a
backend. The most common use case is a plugin that has to be executed
for both `kdbGet()` and `kdbSet()`. It must be the same plugin if it
preserves state between the executions.

Other plugins additionally have to handle error or success situations.
One example of exceptional intensive use is the resolver plugin. It is
executed twice in `kdbSet()`. In `kdbGet()` it is also used as shown
above. Plugins can be defined in the following ways:

- **Single use**:

  Assuming that the following key contains the value `"pluginname"`,

  ```
  system:/elektra/mountpoints/backendname/get/poststorage/#0/name
  ```

  introduces a new plugin from the module
  `pluginname` which cannot be referenced later.

- **Labelling**:

  Assuming that the key ending with `label` contains the value `"pluginlabel"`
  and the key ending with `name` stays the same,

  ```
  system:/elektra/mountpoints/backendname/get/poststorage/#0/label
  system:/elektra/mountpoints/backendname/get/poststorage/#0/name
  ```

  also introduces a new plugin from the module `pluginname` and gives it the
  name `pluginlabel`. By using the introduced label, the plugin can be used later.

- **Referencing**:

  Assuming that the following key contains the value `"pluginlabel"`,

  ```
  system:/elektra/mountpoints/backendname/get/poststorage/#0/reference
  ```

  references back to the label which was introduced before. That way,
  the same plugin which was created previously is used.

`kdb mount` implements the generation of these names as described above.

### Changing Mount Point Configuration

When the user changes the mount point configuration, without
countermeasures, applications already started will continue to run with
the old configuration. This could lead to a problem if backends in use
are changed or removed. It is necessary to restart all such programs.
Notification is the best way to deal with the situation. Changes of
the mount point configuration, however, do not occur often. For some
systems, the manual restart may also be appropriate.

In this situation, applications can receive warning or error information
if the configuration files are moved or removed. The most adverse
situation occurs if the sequence of locking multiple files produces a
_dead lock_. Under normal circumstances, the sequence of locking
the files is deterministic, so either all locks can be requested or
another program will be served first. But several programs with different
mountpoint configurations running at the same time can cause a disaster.
The problem gets even worse, because `kdb mount` is unable to detect
such situations. Every specific mount point configuration for itself
is trouble-free.

But still a dead lock can arise when multiple programs run with different
mountpoint configurations. Suppose we have a program `A` which uses the
backends `B1` and `B2` that requests locks for the files `F1` and `F2`.
Then the mount point configuration is changed. The user removes `B1`
and introduces `B3`. `B3` is in a different path mounted after `B2`, but
also accesses the same file `F1`. The program `B` starts after the mount
point configuration is changed. So it uses the backends `B2` and `B3`.
If the scheduler decides that first `A` and then `B` both successfully
lock the files `F1` and `F2`, a dead lock situation happens because in
the afterwards the applications `A` and `B` try to lock `F2` and `F1`.

A manual solution for this problem is to enable `kdb` to output a
list of processes that still use old mount point configuration. The
administrator can restart these processes. The preferred solution is
to use notification for mount point configuration changes or simply to
use a lock-free resolver.

Continue reading [with the data structures](data-structures.md).
