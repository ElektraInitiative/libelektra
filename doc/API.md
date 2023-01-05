## Elektra Initiative Overview

Elektra serves as a universal and secure framework to access configuration
parameters in a global, hierarchical key database and provides a mature,
consistent and easily comprehensible API. Its modularity effectively
avoids code duplication across applications and tools regarding
configuration tasks. Elektra abstracts from cross-platform-related issues
and allows applications to be aware of other applications' configurations,
leveraging easy application integration.

See the [readme](/README.md) for more introduction.
See the [glossary](/doc/help/elektra-glossary.md) for the used
terminology.

## API Docu

This document's main goal is to describe the API.
It covers:

- external C-API (see Modules above), which are the essential core parts
- C++-API (see Data Structures above) from a direct binding to high-level
  functionality, such as mounting functionality
- plugins API, see @ref Plugins
- all other documentation of Elektra (see Related Pages next to Main Page)

On the one hand it gives an overview and an introduction for
developers using Elektra, on the
other hand it gives an informal description what methods must and may provide
to allow an alternative implementation of the API.

The latest released version (for stable releases) of this document can be found at
https://doc.libelektra.org/api/latest/html

The Git master version of this document can be found at
https://doc.libelektra.org/api/master/html

**Important:** On GitHub links to API functions are broken, so it is recommended that you continue
reading in one of these links above.

## Using the Elektra Library

A C or C++ source file that wants to use Elektra should include:

```c
#include <kdb.h>
```

To link an executable with the Elektra library, one way is to
use the `pkg-config` tool:

```sh
gcc -o application `pkg-config --cflags --libs elektra` application.c
```

Another way is to use CMake:

```cmake
find_package(Elektra REQUIRED)
include_directories (${ELEKTRA_INCLUDE_DIR})
target_link_libraries (application ${ELEKTRA_LIBRARIES})
```

Read about [compiling elektra](/doc/COMPILE.md).

### Tutorials

- [Application Integration](/doc/tutorials/application-integration.md)
- [Compilation Variants](/doc/tutorials/compilation-variants.md)
- [Export](/doc/tutorials/export.md)
- [Import](/doc/tutorials/import.md)
- [Merge](/doc/tutorials/merge.md)
- [Namespaces](/doc/tutorials/namespaces.md)
- [Plugins](/doc/tutorials/plugins.md)
- [Java Plugins](/doc/tutorials/java-plugins.md)

[List of all available Plugins](/src/plugins/) and get started by developing
your own plugins @ref plugin.

## Elektra API

The API was written in pure C because Elektra was designed to be useful
even for the most basic system programs.

The API follows an object-oriented design, and there are 3 main classes
as shown by the figure:

![Elektra Classes](/doc/images/classes.png)

Some general things you can do with each class are:

[KDB (Key Database)](@ref kdb)

- [Open](@ref kdbOpen) and [Close](@ref kdbClose) the Key Database
- [Get](@ref kdbGet) and [Set](@ref kdbSet)
  [KeySet](@ref keyset) in the Key Database
- See [class documentation](@ref kdb) for more

[Key](@ref key)

- [Create](@ref keyNew) and [Delete](@ref keyDel)
- Get and Set key the [name](@ref keySetName)
- Get and Set [string](@ref keySetString) or [binary](@ref keySetBinary) values
- Get and Set [Metadata](@ref keymeta)
- See [class documentation](@ref key) for more

[KeySet](@ref keyset)

- [Create](@ref ksNew) and [Delete](@ref ksDel)
- Append [a single key](@ref ksAppendKey) or an
  entire [KeySet](@ref ksAppend)
- [Lookup keys](@ref ksLookup)
- Pop [the last key](@ref ksPop), [a key by name](@ref ksLookup),
  or [every key](@ref ksCopy)
- Get a key at a [specific position](@ref ksAtCursor)
- Get the [number of elements](@ref ksGetSize) in a KeySet
- See [class documentation](@ref keyset) for more

[More background information about the classes](/doc/dev/classes.md)

## Namespaces

There are 5 trees (=namespaces) of keys: `spec`, `proc`, `dir`, `user` and `system`
that are all unified (in the given order) in one cascading tree starting with `/`.

The cascading tree is the logical tree to be used in applications.
The other trees are the physical ones that stem from configuration sources.
When using cascading key the best key will be searched at run-time,
which appears like a tree on its own.
See @ref cascading in the documentation of ksLookupByName() on how the selection
of keys works.

- The `spec` tree

  This tree specifies how the lookup should take place and also allows us to
  define defaults or document a key.
  The metadata of a key contains this information:

  - `override/#`: use these keys _in favor_ of the key itself (note that
    `#` is the syntax for arrays, e.g. `#0` for the first element, `#10` for the 11th and so on)
  - `namespace/#`: instead of using all namespaces in the predefined order,
    one can specify which namespaces should be searched in which order
  - `fallback/#`: when no key was found in any of the (specified) namespaces
    the `fallback`-keys will be searched
  - `default`: this value will be used if nothing else was found

- The `proc` tree

  Is the only read-only tree. The configuration does not stem from the
  [KDB (Key Database)](@ref kdb), but any other source, e.g. command-line arguments or environment.

- The `dir` tree

  Allows us to have a per-directory overwrite of configuration files, e.g.
  for project specific settings.

- The `user` tree

  Used to store user-specific configurations, like the personal settings
  of a user to certain programs. The user subtree will always be favored
  if present (except for security concerns the user subtree may not be considered).

- The `system` tree

  It is provided to store system-wide configuration keys, that is,
  the last fallback for applications but the only resort for
  daemons and system services.

Read more about [namespaces](/doc/help/elektra-namespaces.md)
and a tutorial for [namespaces](/doc/tutorials/namespaces.md).

## Rules for Key Names

When using Elektra to store your application's configuration and state,
please keep in mind the following rules:

- You are not allowed to create keys right under the root.
  They are reserved for more generic purposes.
- The keys for your application, called say _myapp_, should be created under
  `/sw/org/myapp/#0/current`
  - sw is for software
  - org is the organization. For uniqueness a full reverse url encoded with '/' instead of '.' is useful.
  - `#0` is the major version of the configuration
  - current is the default configuration profile.
  - That means you just need to kdbGet() `/sw/org/myapp/#0/profile`
    and then ksLookupByName() in `/sw/org/myapp/#0/profile/key` where
    profile is from command-line arguments and defaults to current.

Read more about [key names](/doc/help/elektra-key-names.md)

## Backend Overview

The core of Elektra does not store configuration itself to the
hard disk. Instead this work is delegated to backends.

If you want to develop a backend, you should already have some experience
with Elektra from the user point of view. You should be familiar with
the data structures: [Key](@ref key) and [KeySet](@ref keyset)
Then you can start reading about Backends that are composed out of
[Plugins](@ref plugin).
To get started with writing plugins, first read our [plugin tutorial](/doc/tutorials/plugins.md)
and then lookup details in the API description in @ref plugin.

Read more about [mounting](/doc/help/elektra-mounting.md)

## See Also

- See [elektra-glossary(7)](/doc/help/elektra-glossary.md)
- More information about [elektra-backends(7)](/doc/help/elektra-backends.md)
- More information about [plugins-framework](/doc/dev/plugins-framework.md)
