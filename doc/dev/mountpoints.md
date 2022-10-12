# New mountpoint config structure

A mountpoint is defined by adding keys below `system:/elekta/mountpoints/<mountpoint>`, where `<mountpoint>` is the key name for the parent key of the mountpoint with slashes properly escaped.
For example for the mountpoint `user:/mymountpoint` you have to add keys below `system:/elekta/mountpoints/user:\/mymountpoint`.

Every mountpoint consists of exactly three parts:

1. A set of plugins required for the mountpoint.
2. A single _backend plugin_ within the set of plugins.
3. The _mountpoint definition_ specific to the backend plugin.
4. The _mountpoint config_ available to all plugins of this mountpoint.

The set of plugins is defined below `system:/elektra/mountpoints/<mountpoint>/plugins`.
The keys below `system:/elektra/mountpoints/<mountpoint>/plugins/<ref>` define a single instance of a plugin.
The part `<ref>` will be used as name by which this plugin instance will be referenced later.
The name may be arbitrary, but it must not be `backend`. The name `backend` is reserved for the backend plugin (see below).

To define a plugin instance, `system:/elektra/mountpoints/<mountpoint>/plugins/<ref>/name` must be set to the name of the plugin, i.e., it must be possible to open the plugin with `elektraPluginOpen` via this value.
Optionally, the configuration keyset can be defined by the keys below `system:/elektra/mountpoints/<mountpoint>/plugins/<ref>/config`.

The single _backend plugin_ is the only one that the `libelektra-kdb` library communicates with.
It is responsible for calling other plugins when needed.
A _backend contract_ exists between this plugin and `libelektra-kdb`.
The backend plugin alone is responsible for upholding this contract and enforcing it upon other plugins it calls.

The backend plugin is defined (like the other plugins) below `system:/elektra/mountpoints/<mountpoint>/plugins`.
Specifically, it uses `backend` as `<ref>` and is therefore defined by the keys below `system:/elektra/mountpoints/<mountpoint>/plugins/backend`.

The _mountpoint definition_ for the backend plugin is defined by the keys below `system:/elektra/mountpoints/<mountpoint>/definition`.

<!-- TODO [new_backend]: implement correct config handling as described below -->

The _mountpoint config_ is defined by the keys below `system:/elektra/mountpoints/<mountpoint>/config`.
It is merged into the configuration keyset of every plugin of this mountpoint (including the backend plugin).
Specifically, the mountpoint config is put into the `user:/` namespace, while the config from `system:/elektra/mountpoints/<mountpoint>/plugins/<ref>/config` is put into the `dir:/` namespace of the keyset passed to a plugin.

Additionally, the `config/needs` config from plugins' contract is moved to `system:/`.
This config is not stored in `system:/elektra/mountpoints` and instead loaded at runtime by `libelektra-kdb` during the `open` operation.

This leaves the `default:/` namespace for plugins to add their own defaults before calling `ksLookup` to access the config keyset.

> **Note**: This _mountpoint definition_ is separate from the normal configuration keyset passed to a plugin.
> The backend plugin may still have such a keyset below its `system:/elektra/mountpoints/<mountpoint>/plugins/<ref>/config` key.
> The difference between these two keyset is that the configuration keyset should be used to define the general operation of the plugin, while the mountpoint definition should specify a mountpoint.
>
> For example, a backend plugin could support a logging mode, in which it produces a log entry every time it calls another plugin.
> This should be configured in the configuration keyset not in the mountpoint definition, since it doesn't change how the mountpoint works.
>
> Additionally, the config of a plugin is _always_ processed for _all_ mountpoints, no matter if the mountpoint is actually accessed or the plugin actually used.
> This is because config is processed during the `open` operation, which doesn't know what parts of the whole KDB are needed for the current application.
> The mountpoint definition meanwhile is only processed for the mountpoints that are actually accessed.
> In this case the processing happens during the first `get` operation (within an application) that uses this mountpoint.

All other keys below `system:/elektra/mountpoints/<mountpoint>` are ignored.

To illustrate this structure, here is an example configuration:

```
# List of plugins with their config
system:/elektra/mountpoints/\/hosts/plugins/resolver/name (="resolver_fm_hpu_b")
system:/elektra/mountpoints/\/hosts/plugins/myglob/name (="glob")
system:/elektra/mountpoints/\/hosts/plugins/myglob/config/set/#0
system:/elektra/mountpoints/\/hosts/plugins/myglob/config/set/#1
system:/elektra/mountpoints/\/hosts/plugins/myglob/config/set/#2
system:/elektra/mountpoints/\/hosts/plugins/myglob/config/set/#3
system:/elektra/mountpoints/\/hosts/plugins/myglob/config/set/#4/flags
system:/elektra/mountpoints/\/hosts/plugins/store/name (="hosts")
system:/elektra/mountpoints/\/hosts/plugins/sync/name (="sync")
system:/elektra/mountpoints/\/hosts/plugins/error/name (="error")
system:/elektra/mountpoints/\/hosts/plugins/network/name (="network")

## part of the list of plugins, but also defines backend plugin
system:/elektra/mountpoints/\/hosts/plugins/backend/name (="backend")

# Configuration for backend plugin
system:/elektra/mountpoints/\/hosts/definition/path (="myhosts")

system:/elektra/mountpoints/\/hosts/definition/positions/get/resolver (="resolver")
system:/elektra/mountpoints/\/hosts/definition/positions/get/storage (="store")
system:/elektra/mountpoints/\/hosts/definition/positions/get/poststorage/#0 (="myglob")

system:/elektra/mountpoints/\/hosts/definition/positions/set/resolver (="resolver")
system:/elektra/mountpoints/\/hosts/definition/positions/set/prestorage/#0 (="myglob")
system:/elektra/mountpoints/\/hosts/definition/positions/set/prestorage/#1 (="error")
system:/elektra/mountpoints/\/hosts/definition/positions/set/prestorage/#2 (="network")
system:/elektra/mountpoints/\/hosts/definition/positions/set/storage (="store")
system:/elektra/mountpoints/\/hosts/definition/positions/set/precommit/#0 (="sync")
system:/elektra/mountpoints/\/hosts/definition/positions/set/commit (="resolver")
system:/elektra/mountpoints/\/hosts/definition/positions/set/rollback (="resolver")

# Everything else below system:/elektra/mountpoints/\/hosts is ignored
```

Explanation:

- This configures a mountpoint `/hosts` that loads the plugins `resolver_fm_hpu_b`, `glob`, `hosts`, `sync`, `error`, `network` and `backend`.
  The plugin `glob` also has some configuration keys defined.
- The `backend` plugin is set as the backend plugin.
- The rest is the mountpoint definition specific to the `backend` plugin.

As you can see from this example, the "positions" are part of the plugin-specific mountpoint definition.
This allows different backend plugins to use different "positions".

For example, there is the `version` backend plugin that always just provides a few keys with version information for Elektra.
It doesn't need to call any other plugins and requires no configuration at all.
A mountpoint with this plugin could look like this:

```
system:/elektra/mountpoints/\/version/plugins/backend/name (="version")
```

> **Note**: The above example, is also the minimal setup for a mountpoint.
> The key `system:/elektra/mountpoints/<mountpoint>/plugins/backend/name` is the only one that must be defined.

## Operations and Phases

See [KDB Operations Documentation](./kdb-operations.md) for a description of operations and phases.

In each of the phases of a `get` or `set` operation, the corresponding function of the backend plugin is called.
For a description of how this works exactly read the [Backend Plugins Documentation](./backend-plugins.md).

In the first example above, the phases were mapped one-to-one to what the plugin `backend` called "positions".
The different terms are very much intentional, since this not a requirement.

## Further Examples

We already had two examples above.
Here we will look at a few more.

```
# List of plugins with their config
system:/elektra/mountpoints/\/hosts/plugins/resolver/name (="resolver_fm_hpu_b")
system:/elektra/mountpoints/\/hosts/plugins/glob/name (="glob")
system:/elektra/mountpoints/\/hosts/plugins/glob/config/set/#0
system:/elektra/mountpoints/\/hosts/plugins/glob/config/set/#1
system:/elektra/mountpoints/\/hosts/plugins/glob/config/set/#2
system:/elektra/mountpoints/\/hosts/plugins/glob/config/set/#3
system:/elektra/mountpoints/\/hosts/plugins/glob/config/set/#4/flags
system:/elektra/mountpoints/\/hosts/plugins/hosts/name (="hosts")
system:/elektra/mountpoints/\/hosts/plugins/sync/name (="sync")
system:/elektra/mountpoints/\/hosts/plugins/network/name (="network")

# Define backend plugin
system:/elektra/mountpoints/\/hosts/backend/name (="other_backend")

# Configuration for backend plugin
system:/elektra/mountpoints/\/hosts/definition/path (="myhosts")

system:/elektra/mountpoints/\/hosts/definition/positions/get/resolver = (="resolver")
system:/elektra/mountpoints/\/hosts/definition/positions/get/storage = (="hosts")
system:/elektra/mountpoints/\/hosts/definition/positions/get/validation/#0 (="glob")

system:/elektra/mountpoints/\/hosts/definition/positions/set/resolver = (="resolver")
system:/elektra/mountpoints/\/hosts/definition/positions/set/validation/#0 = (="glob")
system:/elektra/mountpoints/\/hosts/definition/positions/set/validation/#1 = (="network")
system:/elektra/mountpoints/\/hosts/definition/positions/set/storage = (="hosts")
system:/elektra/mountpoints/\/hosts/definition/positions/set/precommit/#0 = (="sync")
system:/elektra/mountpoints/\/hosts/definition/positions/set/commit = (="resolver")
system:/elektra/mountpoints/\/hosts/definition/positions/set/rollback (="resolver")
```

This example is very similar to the first one, but the plugin `other_backend` doesn't use the `postorage` and `prestorage` "positions".
Instead, there is a `validation` position that is (presumably) called in the appropriate phase.
The plugin `other_backend` may also impose its own restrictions on plugins configured for the `validation` position.
For example, it may define that such plugins must not generate, remove or modify keys and provide a different position for plugins that do so.

```
system:/elektra/mountpoints/\/hosts/plugins/network/name (="network")

system:/elektra/mountpoints/\/hosts/plugins/backend/name (="db_backend")

system:/elektra/mountpoints/\/hosts/definition/db/driver (="postgres")
system:/elektra/mountpoints/\/hosts/definition/db/server (="127.0.0.1")
system:/elektra/mountpoints/\/hosts/definition/db/port (="5432")
system:/elektra/mountpoints/\/hosts/definition/db/user (="admin")
system:/elektra/mountpoints/\/hosts/definition/db/password (="supersecret")

system:/elektra/mountpoints/\/hosts/definition/phases/set/prestorage/#0 (="network")
```

This example shows an entirely different type of backend.
The hypothetical `db_backend` is backed by a database.
In this case it is configured for a PostgreSQL database running on `127.0.0.1:5432` to which we connect as user `admin`.

We also configured the `network` plugin to run in the `prestorage` phase of the `set` operation.
Which phases can be used and how they must be configured of course depends on `db_backend`.

```
system:/elektra/mountpoints/\/hosts/plugins/yajl/name (="yajl")

system:/elektra/mountpoints/\/hosts/plugins/backend/name (="http_backend")

system:/elektra/mountpoints/\/hosts/url (="https://api.ipify.org/?format=JSON")

system:/elektra/mountpoints/\/hosts/decoder (="yajl")
```

The hypothetical `http_backend` plugin is a read-only backend plugin.
In the example above, it is configured load the URL `https://api.ipify.org/?format=JSON` and use the `yajl` plugin to parse the result into a keyset.
Both the HTTP request and the decoding would likely happen in the `storage` phase of the `get` operation.
The `resolver` phase could perform an HTTP cache check, for example.
