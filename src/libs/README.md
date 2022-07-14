# elektra-libs(7) -- libs overview

> **Note:** Some information in this document is outdated and will change before the release of Elektra 1.0.

## Highlevel APIs

### Highlevel

```
libelektra-highlevel.so
```

Contains the **[highlevel API](highlevel)**.
See [also examples](/examples/highlevel).

### Notification

```
libelektra-notification.so
```

**[notification](notification/)** provides the [notification API](https://doc.libelektra.org/api/latest/html/group__kdbnotification.html).
Usage examples:

- [Basic notifications using polling](https://www.libelektra.org/examples/notificationpolling)
- [Using asynchronous I/O bindings](https://www.libelektra.org/examples/notificationasync)
- [Reload KDB when Elektra's configuration has changed](https://www.libelektra.org/examples/notificationreload)

## Base Elektra Libraries

Since version **0.8.15** **[libelektra](elektra/)**
is split into following libraries:

![Overview of Libraries](/doc/images/overview_libs.png)

### Libkdb

```
libelektra-kdb.so
```

Accesses the configuration files by orchestrating the plugins.
The implementation lives in [elektra](elektra).

It coordinates the interactions between the applications and the plugins.

**[loader](loader/)** contains source files that implement the plugin
loader functionality as used by `libelektra-kdb`.

### Libcore

```
libelektra-core.so
<kdbhelper.h>
<kdb.h> (key* and ks*)
```

Contains the fundamental data-structures every participant of Elektra needs
to link against. It should be the only part that access the internal
data structures.
The implementation lives in [elektra](elektra).

### Libease

```
libelektra-ease.so
```

**[libease](ease/)** contains data-structure operations on top of libcore which do not depend on internals.
Applications and plugins can choose to not link against it if they want to stay minimal.
Its main goal is to make programming with Elektra easier if some extra kB are not an issue.

### Libplugin

```
libelektra-plugin.so
```

**[libplugin](plugin/)** contains `elektraPlugin*` symbols to be used by plugins.

### Libmeta

```
libelektra-meta.so
```

**[libmeta](meta/meta.c)** contains metadata operations as described in **[METADATA.ini](/doc/METADATA.ini)**.
Currently mainly contains legacy code and some generic metadata operations.

### Libelektra

Is a legacy library that provides the same functionality as `libelektra-kdb` and `libelektra-core`.
The sources can be found in **[libelektra](elektra/)**.

## Other Libraries

### Libpluginprocess

```
libelektra-pluginprocess.so
```

**[libpluginprocess](pluginprocess/)** contains functions aiding in executing plugins in a separate
process and communicating with those child processes. This child process is forked from Elektra's
main process each time such plugin is used and gets closed again afterwards. It uses a simple
communication protocol based on a KeySet that gets serialized through a pipe via the dump plugin to
orchestrate the processes.

This is useful for plugins which cause memory leaks to be isolated in an own process. Furthermore
this is useful for runtimes or libraries that cannot be reinitialized in the same process after they
have been used.

### Libtools

**[libtools](tools/)** is a high-level C++ shared-code for tools. It includes:

- plugin interface
- backend interface
- 3-way merge

### Utility

**[libutility](utility/)** provides utility functions to be used in plugins.

### Libinvoke

```
libelektra-invoke.so
```

**[libinvoke](invoke/)** provides a simple API allowing us to call functions exported by plugins.

### IO

```
libelektra-io.so
```

**[io](io/)** provides the
[common API](https://doc.libelektra.org/api/latest/html/group__kdbio.html) for
using asynchronous I/O bindings.

### Globbing

```
libelektra-globbing.so
```

**[globbing](globbing/)** provides globbing functionality for Elektra.

The supported syntax is a superset of the syntax used by `glob(7)`. The following extensions are supported:

- `#`, when used as `/#/` (or `/#"` at the end of the pattern), matches a valid array item
- `_` is the exact opposite; it matches anything but a valid array item
- if the pattern ends with `/__`, matching key names may contain arbitrary suffixes

For more info take a look a the documentation of `elektraKeyGlob()` and `elektraKsGlob()`.
