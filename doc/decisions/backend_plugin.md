# Backend Plugin

## Problem

- Backends store plugins in arrays which have a fixed number of slots for each plugin role. The number of plugins which can be assigned is limited,
  making it easy to reach the limit if many plugins are in use.
- As structs, backends are separate from the plugin interface and integrated into the core of Elektra. This makes it difficult to perform operations
  such as nesting plugins, or to develop other implementations for backends.

## Constraints

- It should be possible for all existing plugins to run normally

## Assumptions

## Considered Alternatives

- Multiple storage plugins within a single backend
- Plugin containing more plugin slots

## Decision

- The current backend implementation will be redeveloped into a backend plugin. That way, the core of Elektra will only access backends through
  the standard plugin interface.
- The `getplugins`, `setplugins` and `errorplugins` arrays will be changed into arrays of linked lists. Each time a plugin is added to a specific
  slot, it will be added at the end of the linked list.

## Rationale

- Making backends plugins themselves detaches their implementation from the core of Elektra, making it possible to develop new kinds of backends
  without major changes to the core itself.
- As plugins, backends can contain further backends, making it possible to nest plugins and enabling new kinds of plugin combinations such as
  fallback storage options.

## Implications

To take the changed structure of the plugin arrays into account, the mountpoint configuration needs to be modified accordingly. An example
of the new configuration:

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
system:/elektra/mountpoints/\/hosts/get/poststorage
system:/elektra/mountpoints/\/hosts/get/poststorage/#0
system:/elektra/mountpoints/\/hosts/get/poststorage/#0/label (="glob")
system:/elektra/mountpoints/\/hosts/get/poststorage/#0/name (="glob")
system:/elektra/mountpoints/\/hosts/get/resolver
system:/elektra/mountpoints/\/hosts/get/resolver/#0
system:/elektra/mountpoints/\/hosts/get/resolver/#0/reference (="resolver")
system:/elektra/mountpoints/\/hosts/get/storage
system:/elektra/mountpoints/\/hosts/get/storage/#0
system:/elektra/mountpoints/\/hosts/get/storage/#0/label (="hosts")
system:/elektra/mountpoints/\/hosts/get/storage/#0/name (="hosts")
system:/elektra/mountpoints/\/hosts/set
system:/elektra/mountpoints/\/hosts/set/commit
system:/elektra/mountpoints/\/hosts/set/commit/#0
system:/elektra/mountpoints/\/hosts/set/commit/#0/reference (="resolver")
system:/elektra/mountpoints/\/hosts/set/precommit
system:/elektra/mountpoints/\/hosts/set/precommit/#0
system:/elektra/mountpoints/\/hosts/set/precommit/#0/label (="sync")
system:/elektra/mountpoints/\/hosts/set/precommit/#0/name (="sync")
system:/elektra/mountpoints/\/hosts/set/prestorage
system:/elektra/mountpoints/\/hosts/set/prestorage/#0
system:/elektra/mountpoints/\/hosts/set/prestorage/#0/reference (="glob")
system:/elektra/mountpoints/\/hosts/set/prestorage/#1
system:/elektra/mountpoints/\/hosts/set/prestorage/#1/label (="error")
system:/elektra/mountpoints/\/hosts/set/prestorage/#1/name (="error")
system:/elektra/mountpoints/\/hosts/set/prestorage/#2
system:/elektra/mountpoints/\/hosts/set/prestorage/#2/label (="network")
system:/elektra/mountpoints/\/hosts/set/prestorage/#2/name (="network")
system:/elektra/mountpoints/\/hosts/set/resolver
system:/elektra/mountpoints/\/hosts/set/resolver/#0
system:/elektra/mountpoints/\/hosts/set/resolver/#0/reference (="resolver")
system:/elektra/mountpoints/\/hosts/set/storage
system:/elektra/mountpoints/\/hosts/set/storage/#0
system:/elektra/mountpoints/\/hosts/set/storage/#0/reference (="hosts")
```

The following changes have been made:

- The mountpoint has been moved from `system:/elektra/mountpoints/backendname/mountpoint`
  to `system:/elektra/mountpoints/backendname/config/mountpoint`. That way, the mountpoint of
  the backend can still be read out in the core from the backend plugin's plugin configuration.
- Plugin roles are no longer displayed as array slots, but actually by their names. In addition,
  the names of the roles and the plugin arrays have been shortened for redundancy. For example,
  `system:/elektra/mountpoints/\/hosts/getplugins/#0` is now
  `system:/elektra/mountpoints/\/hosts/get/resolver`.
- Each plugin role consists of a linked list containing the plugins fulfilling this role. In the
  configuration, the position of the plugin in the linked list is shown using an array. For example,
  the key `system:/elektra/mountpoints/\/hosts/set/prestorage/#1` means that the plugin (in this
  case `error`) belongs to the second position of the linked list belonging to the `prestorage` role
  in the `set` array.
- The name, reference name and the label of a plugin are now stored in separate keys to avoid using
  the `#` symbol for something other than arrays. An example from the `error` plugin:

```
system:/elektra/mountpoints/\/hosts/set/prestorage/#1
system:/elektra/mountpoints/\/hosts/set/prestorage/#1/label (="error")
system:/elektra/mountpoints/\/hosts/set/prestorage/#1/name (="error")
```

That way, the `error` plugin is opened and stored for later use with the defined label. If it were to
be used later, it would be referenced by adding `reference` instead of `label` and `name`.

Another change that had to be made is adding the `modules` KeySet to the `Plugin` structure so that it
can be accessed from within the `backend` plugin.

## Related Decisions

This decision builds upon the development of `kdbCommit()`, discussed in:

https://issues.libelektra.org/2798

## Notes

https://issues.libelektra.org/2963

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

There are four _operations_ in `libelektra-kdb`: `open`, `get`, `set` and `close`.
For each of these there is a `kdb*` function the user calls to trigger the operation and plugins implement a `elektra<Plugin>*` function for each of the operations they support (at least `get`).

> **Note**: There is a general plugin contract for these operations, but this it is not explained here.

TODO: cache, notifications

The operations `get` and `set` also have different _phases_:

- The `get` operation has: `init`, `resolver`, `prestorage`, `storage` and `poststorage`.
- The `set` operation has: `resolver`, `prestorage`, `storage` and `poststorage` followed by `precommit`, `commit` and `postcommit` if the previous phases where successful or by `prerollback`, `rollback` and `postrollback` if the previous phases failed.

These phases are implemented by a backend plugin.
In the first example above, the phases were mapped one-to-one to what the plugin `backend` called "positions".
The different terms are very much intentional, since this not a requirement.
(More at the very end)

## Backend Contract

There exists a _backend contract_ between `libelektra-kdb` and any plugin acting as a backend plugin.
This contract sets, the order of the phases described above and defines the interaction between a backend plugin and `libelektra-kdb`.

![Sequence of phases in `get` operation](kdbGet.svg)

![Sequence of phases in `set` operation](kdbSet.svg)

The diagrams above show the possible sequences of phases during a `get` and a `set` operation.
For each of the phases of a `get` operation `libelektra-kdb` calls the backend plugin's `elektra<Plugin>Get` function once.
Similarly, for the phases of a `set` operation `elektra<Plugin>Set` is called.

The current phase is communicated to the backend plugin via the global keyset.
The value of the key `system:/elektra/kdb/backend/phase` is always set to the name of the current phase.

### Operation `get`

The `get` operation is mandatory and all backend plugins must implement it.

#### Initialization Phase

During the `init` phase the backend plugin is called with:

- A key `parentKey` whose name is the parent key of the mountpoint and whose value is an empty string.
  The key name and value of this key are read-only.
- A keyset `ks` containing the mountpoint definition.

The backend plugin then:

- **MUST** parse the mountpoint definition and store the necessary information for later phases internally.
- **SHOULD** validate that the mountpoint definition _can be_ valid.
- **SHOULD NOT** do other validation.
  For example a file-based backend, _should not_ check whether the file(s) or path(s) referenced in the mountpoint definition exist.
  Such a check should be done in the `resolver` phase.
- **MAY** decided that the mountpoint should be read-only.
  If so, this must be indicated to `libelektra-kdb` via the return value.

This phase exists purely for the backend plugin to initialize and configure itself for the mountpoint.

> **Note**: This phase is only executed _once per instance of `KDB`_.
> Only the first `kdbGet()` call will result in `libelektra-kdb` executing this phase, all future calls to `kdbGet()` (and `kdbSet()`) start with the `resolver` phase.
> The backend plugin must store the information contain in the mountpoint definition internally to accommodate this.

#### Resolver Phase

During the `resolver` phase the backend plugin is called with:

- A key `parentKey` whose name is the parent key of the mountpoint and whose value is an empty string.
  The key name of this key is read-only.
- An empty keyset `ks`.

The backend plugin then:

- **MUST** set the value of the `parentKey` to a value identifying the storage unit that contains the data of the mountpoint.
  For file-based backend plugins, this means setting the value of `parentKey` to an absolute filename.
- **MAY** set metadata on `parentKey`, if encoding the information required for the following phases is too hard to encode in a single string value.

> **Note**: The backend plugin may also modify the keyset `ks`, but `libelektra-kdb` will discard this keyset after this phase, so these modifications won't have any effects.

During a `set` operation the backend plugin must also ensure that errors in the storage phases can be safely rolled back and that the `set` operation does not affect concurrent operations.
For file-based backends, this means creating a temporary storage file and returning its absolute filename instead of the name of the actual storage file.
In other words, in a `set` operation the `resolver` phase is also about preparing a transaction in addition to resolving the storage unit.

#### Storage Phases

These phases are responsible for reading and validating the actual data stored in the KDB.

In the `prestorage` phase the backend plugin is called with:

- The exact `parentKey` that was returned by the `resolver` phase of this `get` operation.
  The key name and value of this key are read-only.
- An empty keyset `ks`.

There are no restrictions on what the backend plugin may do in this phase, but just like in the `resolver` phase, change to `ks` will be discarded.
This phase is useful for file-level manipulations, like file-based encryption, line ending conversion or verifying file signatures.
In this sense, it is the counter-part of the `precommit` phase of the `set` operation.

The `storage` phase is the where the actual data is read.
In this phase the backend plugin is called with:

- The exact `parentKey` that was returned by the `prestorage` phase of this `get` operation.
  The key name and value of this key are read-only.
- An empty keyset `ks`.

The backend plugin then:

- **MUST** read the data for the mountpoint from the storage unit that was selected in the `resolver` phase.
- **MUST** parse that data and insert it below `parentKey` into `ks`.

The last of the storage phases is the `poststorage` phase.
In this phase the backend plugin is called with:

- The exact `parentKey` that was returned by the `storage` phase of this `get` operation.
  The key name and value of this key are read-only.
- The exact `ks` that was returned by the `storage` phase of this `get` operation.

Again there are no restrictions on what the backend plugin may do in this phase.
However, unlike the `prestorage` phase, this phase is a very important one.
It is where validation, generation of implicit values and similar tasks happen.

Finally, `libelektra-kdb` merges the keyset returned by the `poststorage` phase with the ones returned by other backend plugins for different mountpoints and then returns it to the user.

TODO: Are modifications to `parentKey` visible to the user?

### Operation `set`

The `set` operation is optional.
A mountpoint is automatically read-only and doesn't support the `set` operation, if the backend plugin does not define a `elektra<Plugin>Set` function.

Alternatively, the read-only nature of the mountpoint may also be indicated by the backend plugin during the `init` phase of the `get` operation.

#### Resolver Phase

During the `resolver` phase the backend plugin is called with:

- The exact `parentKey` that was returned by the `resolver` phase of the last `get` or `set` operation.
  The key name of this key is read-only.
- An empty keyset `ks`.

The backend plugin then:

- **MUST** set the value of the `parentKey` to a value identifying the storage unit that contains the data of the mountpoint.
  For file-based backend plugins, this means setting the value of `parentKey` to an absolute filename.
- **MAY** set metadata on `parentKey`, if encoding the information required for the following phases is too hard to encode in a single string value.
- **MUST** check whether the data was changed since the last `get` operation.
  The result of this check is given to `libelektra-kdb` via the return value of the `get` function.
- **MUST** ensure that errors in the storage phases can be safely rolled back and that the `set` operation does not affect concurrent operations.
  For file-based backends, this means creating a temporary storage file and returning its absolute filename instead of the name of the actual storage file.

> **Note**: The backend plugin may also modify the keyset `ks`, but `libelektra-kdb` will discard this keyset after this phase, so these modifications won't have any effects.

#### Storage Phases

These phases are responsible for validating and writing data to the KDB.

In the `prestorage` phase the backend plugin is called with:

- The exact `parentKey` that was returned by the `resolver` phase of this `set` operation.
  The key name and value of this key are read-only.
- The exact subset below `parentKey` of the keyset `ks` that was provided by the user.

There are no restrictions on what the backend plugin may do in this phase.
This phase can be used for validation to avoid storing invalid configuration.
However, it should not be used for generating keys or values implicitly defined by other keys.
Such keys should be generated during the `poststorage` phase of a `get` operation and should actually be _removed_ again in this phase.
That way there cannot be conflicts, if a key that implies another keys value changes.

> **Note**: Just in case there is actually a use case, where keys have to be generated, removed or modified during this phase, we do _not_ discard changes to `ks` (like we would do in a `get` operation).

The `storage` phase is the where the actual data is written.
In this phase the backend plugin is called with:

- The exact `parentKey` that was returned by the `prestorage` phase of this `set` operation.
  The key name and value of this key are read-only.
- The exact keyset `ks` that was returned by the `prestorage` phase of this `set` operation.
  All keys in this keyset and the keyset itself are fully read-only.

The backend plugin then:

- **MUST** serialize the data and below `parentKey` in `ks`.
- **MUST** write the data for the mountpoint into the storage unit that was selected in the `resolver` phase.
- **MUST** ensure that the data is written in such a way that

  - it can be read, if the storage unit is mounted at another mountpoint
  - that reading such a mountpoint will result in the same data just below a different parent key

  An important implication here is that all names inside storage units should be relative to the parent key.

The last of the storage phases is the `poststorage` phase.
In this phase the backend plugin is called with:

- The exact `parentKey` that was returned by the `storage` phase of this `set` operation.
  The key name and value of this key are read-only.
- The exact keyset `ks` that was returned by the `prestorage` phase of this `set` operation.
  All keys in this keyset and the keyset itself are fully read-only.

There are no formal restrictions, other than those enforced by `parentKey` and `ks` being (partially) read-only.
But the `poststorage` phase should not be used as a counter-part to the `prestorage` phase in the `get` operation.
Use the `precommit` phase instead.
Therefore, the `poststorage` phase has very little use cases other than logging and exists mostly because of symmetry.

#### Commit Phases (`set` only)

If all storage phases completed successfully, `libelektra-kdb` will continue with calling the `commit` phases.

All the `commit` phases (`precommit`, `commit`, `postcommit`) are called with:

- The exact `parentKey` that was returned by the previous phase (`poststorage`, `precommit` or `commit`) of this `set` operation.
  The key name and value of this key are read-only.
- The exact keyset `ks` that was returned by the `poststorage` phase of this `set` operation (which is the same one that was returned by the `prestorage` phase).
  All keys in this keyset and the keyset itself are fully read-only.

There are no restrictions on the `precommit` phase, other than those enforced by `parentKey` and `ks` being (partially) read-only.
This phase can be used for file-level manipulations, like file-based encryption, line ending conversion or adding file signatures.
In this sense, it is the counter-part of the `prestorage` phase of the `set` operation.

In the `commit` phase the backend plugin:

- **MUST** make the changes done during the `storage` phase of this `set` operation permanent in such a way that a following `get` operation will be able to read them (assuming there is no other `set` operation in between).

There are no restrictions on what the backend plugin may do in the `postcommit` phase.
However, it is important to keep in mind that an error in the `postcommit` phase will **not** make the `set` operation fail.
Once the `commit` phase completes successfully, the `set` operation is also deemed successful, since the changes were made permanent.
If an error does occur in the `postcommit` phase, it is reported as warning.
This makes the `postcommit` phase mostly useful for logging.

Finally, `libelektra-kdb` merges the keyset returned by the `postcommit` phase (which is still the same one that was returned by the `prestorage` phase) with the ones returned by other backend plugins for different mountpoints and then returns it to the user.

TODO: Are modifications to `parentKey` visible to the user?

#### Rollback Phases (`set` only)

If any of the phases `prestorage`, `storage`, `poststorage`, `precommit` or `commit` fail, `libelektra-kdb` will continue with the rollback phases.

Similar to the commit phases, the rollback phases (`prerollback`, `rollback` and `postrollback`) are called with:

- The exact `parentKey` that was returned by the phase of this `set` operation that reported an error.
  The key name and value of this key are read-only.
- The exact keyset `ks` that was returned by the phase of this `set` operation that reported an error.
  All keys in this keyset and the keyset itself are fully read-only.

Additionally, the phase that reported an error is communicated to the backend plugin via the global keyset (together with the current phase).
The value of the key `system:/elektra/kdb/backend/failedphase` is set to the name of the failed phase.

The `prerollback` and `postrollback` phases are mostly useful for logging.
There are no restrictions on these phase, other than those enforced by `parentKey` and `ks` being (partially) read-only.
However, they are similar to the `postcommit` phase, in that any errors they report will be ignored and reported as warnings.
In particular, even if the `prerollback` phase fails, we will `libelektra-kdb` will continue with the `rollback` phase as if `prerollback` succeeded.

In the `rollback` phase the backend plugin:

- **MUST** revert all changes made to the state of the storage unit chosen in the `resolver` phase of this `set` operation.
  For file-based backends, this means removing the temporary file.
- **MUST** ensure that a following `get` or `set` operation will act as if the failed `set` operation never happened.
- **MAY** act differently depending on which phase failed.

Finally, `libelektra-kdb` will restore `ks` to the state in which the user provided it and return.

TODO: Are modifications to `parentKey` visible to the user?

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
