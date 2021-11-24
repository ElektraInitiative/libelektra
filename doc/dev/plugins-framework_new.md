# Plugins Framework

TODO: rewrite intro

## Contract

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

## Types of Plugins

In theory a plugin could do pretty much anything.
However, `libelektra-kdb` only interacts with one special type of plugin called a _backend plugin_.
All other plugins must be called by these backend plugins.

In addition to backend plugins there a few other common types of plugins that will be explained below.

### Backend Plugins

Backend plugins are a very special type of plugin.
Compared to other plugins they have a much more strict contract for what they may and may not do in certain situations.
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
