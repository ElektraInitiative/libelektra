# Proposal: New mountpoint config structure

A mountpoint is defined by adding keys below `system:/elekta/mountpoints/<mountpoint>`, where `<mountpoint>` is the key name for the parent key of the mountpoint with slashes properly escaped.
For example for the mountpoint `user:/mymountpoint` you have to add keys below `system:/elekta/mountpoints/user:\/mymountpoint`.

Every mountpoint consists of exactly three parts:

1. A list of plugins required for the mountpoint.
2. A single _backend plugin_ chosen from the list of plugins.
3. The _mountpoint definition_ specific to the backend plugin.

The list of plugins is defined as the array `system:/elektra/mountpoints/<mountpoint>/plugins/#`.
Specifically, `system:/elektra/mountpoints/<mountpoint>/plugins/#/name` contains the name of the plugin and `system:/elektra/mountpoints/<mountpoint>/plugins/#/config` the configuration keyset passed to the plugin.

The single _backend plugin_ is the only one that the `libelektra-kdb` library communicates with.
It is responsible for calling other plugins when needed.
A _backend contract_ exists between this plugin and `libelektra-kdb`.
The backend plugin alone is responsible for upholding this contract.

The backend plugin must be one of the ones defined in `system:/elektra/plugins/#`.
The tell `libelekta-kdb` which of the plugins to use as the _backend plugin_, you set `system:/elektra/mountpoints/<mountpoint>/backend` to its array index.

All other keys below `system:/elektra/mountpoints/<mountpoint>` are the _mountpoint definition_ for the backend plugin.

> **Note**: This _mountpoint definition_ is separate from the normal configuration keyset passed to a plugin.
> The backend plugin may still have such a keyset below its `system:/elektra/mountpoints/<mountpoint>/plugins/#/config` key.
> The difference between these two keyset is that the configuration keyset should be used to define the general operation of the plugin, while the mountpoint definition should specify a mountpoint.
>
> For example, a backend plugin could support a logging mode, in which it produces a log entry every time it calls another plugin.
> This should be configured in the configuration keyset not in the mountpoint definition, since it doesn't change how the mountpoint works.

To illustrate this structure, here is an example configuration:

```
# List of plugins with their config
system:/elektra/mountpoints/\/hosts/plugins/#0/name (="resolver_fm_hpu_b")
system:/elektra/mountpoints/\/hosts/plugins/#1/name (="glob")
system:/elektra/mountpoints/\/hosts/plugins/#1/config/set/#0
system:/elektra/mountpoints/\/hosts/plugins/#1/config/set/#1
system:/elektra/mountpoints/\/hosts/plugins/#1/config/set/#2
system:/elektra/mountpoints/\/hosts/plugins/#1/config/set/#3
system:/elektra/mountpoints/\/hosts/plugins/#1/config/set/#4/flags
system:/elektra/mountpoints/\/hosts/plugins/#2/name (="hosts")
system:/elektra/mountpoints/\/hosts/plugins/#3/name (="sync")
system:/elektra/mountpoints/\/hosts/plugins/#4/name (="error")
system:/elektra/mountpoints/\/hosts/plugins/#5/name (="network")
system:/elektra/mountpoints/\/hosts/plugins/#6/name (="backend")

# Define backend plugin
system:/elektra/mountpoints/\/hosts/backend (="#6")

# Configuration for backend plugin
system:/elektra/mountpoints/\/hosts/path (="myhosts")

system:/elektra/mountpoints/\/hosts/positions/get/resolver = (="#0")
system:/elektra/mountpoints/\/hosts/positions/get/storage = (="#2")
system:/elektra/mountpoints/\/hosts/positions/get/poststorage/#0 (="#1")

system:/elektra/mountpoints/\/hosts/positions/set/resolver = (="#0")
system:/elektra/mountpoints/\/hosts/positions/set/prestorage/#0 = (="#1")
system:/elektra/mountpoints/\/hosts/positions/set/prestorage/#1 = (="#4")
system:/elektra/mountpoints/\/hosts/positions/set/prestorage/#2 = (="#5")
system:/elektra/mountpoints/\/hosts/positions/set/storage = (="#2")
system:/elektra/mountpoints/\/hosts/positions/set/precommit/#0 = (="#3")
system:/elektra/mountpoints/\/hosts/positions/set/commit = (="#0")
system:/elektra/mountpoints/\/hosts/positions/set/rollback (="#0")
```

Explanation:

- This configures a mountpoint `/hosts` that loads the plugins `resolver_fm_hpu_b`, `glob`, `hosts`, `sync`, `error`, `network` and `backend`.
  The plugin `glob` also has some configuration keys defined.
- It then selects the `backend` plugin as the backend plugin.
- The rest is the mountpoint definition specific to the `backend` plugin.

As you can see from this example, the classic "positions" are part of the plugin-specific mountpoint definition.
This allows different backend plugins to use different "positions".

For example, imagine a `version` backend plugin that always just provides a few keys with version information for Elektra.
It doesn't need to call any other plugins and requires no configuration at all.
A mountpoint with this plugin could look like this:

```
system:/elektra/mountpoints/\/version/plugins/#0/name (="version")
system:/elektra/mountpoints/\/version/backend (="#0")
```

## Operations and Phases

See [KDB Operations Documentation](./kdb-operations.md) for a description of operations and phases.

In each of the phases of a `get` or `set` operation, the corresponding function of the backend plugin is called.
For a description of how this work exactly read the [Backend Plugins Documentation](./backend-plugins.md).

In the first example above, the phases were mapped one-to-one to what the plugin `backend` called "positions".
The different terms are very much intentional, since this not a requirement.

## Further Examples

We already had two examples above.
Here we will look at a few more.

```
# List of plugins with their config
system:/elektra/mountpoints/\/hosts/plugins/#0/name (="resolver_fm_hpu_b")
system:/elektra/mountpoints/\/hosts/plugins/#1/name (="glob")
system:/elektra/mountpoints/\/hosts/plugins/#1/config/set/#0
system:/elektra/mountpoints/\/hosts/plugins/#1/config/set/#1
system:/elektra/mountpoints/\/hosts/plugins/#1/config/set/#2
system:/elektra/mountpoints/\/hosts/plugins/#1/config/set/#3
system:/elektra/mountpoints/\/hosts/plugins/#1/config/set/#4/flags
system:/elektra/mountpoints/\/hosts/plugins/#2/name (="hosts")
system:/elektra/mountpoints/\/hosts/plugins/#3/name (="sync")
system:/elektra/mountpoints/\/hosts/plugins/#4/name (="network")
system:/elektra/mountpoints/\/hosts/plugins/#5/name (="other_backend")

# Define backend plugin
system:/elektra/mountpoints/\/hosts/backend (="#5")

# Configuration for backend plugin
system:/elektra/mountpoints/\/hosts/path (="myhosts")

system:/elektra/mountpoints/\/hosts/positions/get/resolver = (="#0")
system:/elektra/mountpoints/\/hosts/positions/get/storage = (="#2")
system:/elektra/mountpoints/\/hosts/positions/get/validation/#0 (="#1")

system:/elektra/mountpoints/\/hosts/positions/set/resolver = (="#0")
system:/elektra/mountpoints/\/hosts/positions/set/validation/#0 = (="#1")
system:/elektra/mountpoints/\/hosts/positions/set/validation/#1 = (="#4")
system:/elektra/mountpoints/\/hosts/positions/set/storage = (="#2")
system:/elektra/mountpoints/\/hosts/positions/set/precommit/#0 = (="#3")
system:/elektra/mountpoints/\/hosts/positions/set/commit = (="#0")
system:/elektra/mountpoints/\/hosts/positions/set/rollback (="#0")
```

This example is very similar to the first one, but the plugin `other_backend` doesn't use the `postorage` and `prestorage` "positions".
Instead, there is a `validation` position that is (presumably) called in the appropriate phase.
The plugin `other_backend` may also impose its own restrictions on plugins configured for the `validation` position.
For example, it may define that such plugins must not generate, remove or modify keys and provide a different position for such plugins.

```
system:/elektra/mountpoints/\/hosts/plugins/#0/name (="db_backend")
system:/elektra/mountpoints/\/hosts/plugins/#1/name (="network")

system:/elektra/mountpoints/\/hosts/backend (="#0")

system:/elektra/mountpoints/\/hosts/db/driver (="postgres")
system:/elektra/mountpoints/\/hosts/db/server (="127.0.0.1")
system:/elektra/mountpoints/\/hosts/db/port (="5432")
system:/elektra/mountpoints/\/hosts/db/user (="admin")
system:/elektra/mountpoints/\/hosts/db/password (="supersecret")

system:/elektra/mountpoints/\/hosts/phases/set/prestorage/#0 (="#1")
```

This example shows an entirely different type of backend.
The hypothetical `db_backend` is backed by a database.
In this case it is configured for a PostgreSQL database running on `127.0.0.1:5432` to which we connect as user `admin`.

We also configured the `network` plugin to run in the `prestorage` phase of the `set` operation.
Which phases can be used and how the must be configured of course depends on `db_backend`.

One might also imagine there could be a (probably quite complicated) part of the mountpoint definition that defines how the relational tables of the database are mapped into the KDB.

```
system:/elektra/mountpoints/\/hosts/plugins/#0/name (="http_backend")
system:/elektra/mountpoints/\/hosts/plugins/#1/name (="yajl")

system:/elektra/mountpoints/\/hosts/backend (="#0")

system:/elektra/mountpoints/\/hosts/url (="https://api.ipify.org/?format=json")

system:/elektra/mountpoints/\/hosts/decoder (="yajl")
```

The hypothetical `http_backend` plugin is a read-only backend plugin.
In the example above, it is configured load the URL `https://api.ipify.org/?format=json` and use the `yajl` plugin to parse the result into a keyset.
Both the HTTP request and the decoding would happen in the `storage` phase of the `get` operation.
The `resolver` phase could perform an HTTP cache check, for example.
