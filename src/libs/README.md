elektra-libs(7) -- libs overview
================================

## DESCRIPTION

Since version **[0.8.15](/doc/decisions/library_split.md)** **[libelektra](elektra/)**
is split into following libraries:

![Overview of Libraries](/doc/images/overview_libs.png)

### Loader

**[loader](loader/)** contains source files that implement the plugin
loader functionality. The files are linked to **[libelektra](elektra/)**.

### Libease

    libelektra-ease.so

**[libease](ease/)** contains data-structure operations on top of libcore which do not depend on internals.
Applications and plugins can choose to not link against it if they want to stay minimal.

### Libplugin

    libelektra-plugin.so

**[libplugin](plugin/)** contains `elektraPlugin*` symbols and plugins should link against it.

### Libpluginprocess

    libelektra-pluginprocess.so

**[libpluginprocess](pluginprocess/)** contains functions aiding in executing plugins in a separate
process and communicating with those child processes. This child process is forked from Elektra's
main process each time such plugin is used and gets closed again afterwards. It uses a simple
communication protocol based on a KeySet that gets serialized through a pipe via the dump plugin to
orchestrate the processes.

This is useful for plugins which cause memory leaks to be isolated in an own process. Furthermore
this is useful for runtimes or libraries that cannot be reinitialized in the same process after they
have been used.

### Libproposal

    libelektra-proposal.so

**[libproposal](proposal/)** contains functions that are proposed for libcore. Depends on internas of libcore and as
such must always fit to the exact same version.

### Libmeta

    libelektra-meta.so

**[libmeta](meta/meta.c)** contains metadata operations as described in **[METADATA.ini](/doc/METADATA.ini)**.
Will be code-generated in the future, so methods should be mechanical reflections
of the contents in **[METADATA.ini](/doc/METADATA.ini)**.

### Libcore

    libelektra-core.so
    <kdbhelper.h>
    <kdb.h> (key* and ks*)

Contains the fundamental data-structures every participant of Elektra needs
to link against. It should be the only part that access the internal
data structures.

### Libtools

**[libtools](tools/)** is a high-level C++ shared-code for tools. It includes:

- plugin interface
- backend interface
- 3-way merge

### Utility

**[libutility](utility/)** provides utility functions to be used in plugins.

### Libinvoke

    libelektra-invoke.so

**[libinvoke](invoke/)** provides a simple API allowing us to call functions exported by plugins.

### IO

    libelektra-io.so

**[io](io/)** provides the common [I/O binding API](https://doc.libelektra.org/api/current/html/group__kdbio.html).

### Notification

    libelektra-notification.so

**[notification](notification/)** provides the [notification API](https://doc.libelektra.org/api/current/html/group__kdbnotification.html).
