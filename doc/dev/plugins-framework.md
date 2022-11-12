# Plugins Framework

<!-- TODO [new_backend]: Incomplete -->

The key database of Elektra basically passes a `KeySet` from plugin to plugin.
Plugins can be chained or nested.
The library `libelektra-kdb` only interacts with specially designed hook plugins as well as one special type of plugin called a _backend plugin_, usually with other plugins nested below.
All other plugins must be called by these backend plugins.
Backend plugins then usually implement some kind of plugin chaining.

## Contract

In theory a plugin could do pretty much anything.
In practice, however, plugins must meet certain expectations, described by the plugin's contract.

Every plugin should provide a full contract to give information how it
will work with other plugins. Most parts of the contract are obligatory.
Plugins cannot be loaded without this information. For example, plugins
must provide the clause `infos/version`.

<!-- TODO: explain contract in general:
	- explain open, init, get, set, commit, error, close operation basics
	- explain plugin factory function: return type KeySet*
	- explain /exports feature for additional functions, add conventions for documenting functions
	- explain /constants feature (used by resolver) for static data
	- explain /infos
	- add /version to allow detection of contract change, possibly rely on hashing
	- add /type = backend to mark plugins that act as backends
	- add /backend/# to list backends known to be compatible with this plugin
-->

## Plugin Instances

<!-- TODO: explain that
	- plugin instances are always associated with a KDB* instance
	- the open operation is always called lazily
	- the close operation is only called during kdbClose()
	- a plugin instance can request the handle for another plugin instance (of the same mountpoint) via the name defined in the mountpoint definition with
	  ```c
	  Plugin * elektraPluginGetPlugin (Plugin * self, const char * instanceName);
	  ```
	  used in backend plugins, but can also be used by others when needed
-->

## Operations

<!-- TODO: explain open, init, set, commit, error, close in detail -->

## Plugin providers

Next to the backend plugins, already introduced before, there a few other common types of plugins that will be explained below.

### Backend Plugins

Backend plugins are a special type of plugin.
Compared to other plugins they have a more strict contract for what they may and may not do in certain situations.
This is because they are the only plugins that are invoked by `libelektra-kdb`.
Other plugins are normally called by a backend plugin, although any plugin may call another.

To `libelektra-kdb` it doesn't matter what other plugins do, as long as the backend plugins hide unexpected behavior.
In other words backend plugins must ensure that to the outside everything behaves according to the contract.

While any plugin could be used as backend plugin, only a select few will behave according to the contract for backend plugins.
Therefore, it is important to know which plugins can and cannot be used as backend plugins.

For more information on the contract for backend plugins, take a look at the [relevant documentation](backend-plugins.md).

It is also important to note, that not every plugin will be compatible with every backend plugin.
Some backend plugins may not support calling other plugins at all, while others may also have their own contracts for the plugins they call.
For example, the default backend plugin called `backend` requires a _resolver plugin_ and a _storage plugin_ to function.
Because these are very important types of plugins, they are also explained below.

### Resolver Plugins

<!-- TODO: explain that resolver plugins, refer to backend-plugins.md for more info -->

### Storage Plugins

<!-- TODO: explain storage plugins, refer to backend-plugins.md for more info -->

### Validation Plugins

<!-- TODO: explain validation plugins -->
