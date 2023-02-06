# Hooks

Hooks are central points in the KDB lifecycle, where specialized plugins are called.

## Selecting which Plugin will be Used for a Specific Hook

The names of the plugins are hard coded.
This [decision](../decisions/5_partially_implemented/hooks.md) was made, because these plugins are meant to fulfil very specific purposes.
A symlink replacing the shared library file of the plugin could be used to change the implementation.

## Interface of the hooks

If a plugin should be able to act upon a hook, it must export all the functions that the hook requires.
These exports are of the form `system:/elektra/modules/<plugin name>/exports/hook/<hook name>/<hook function>`.

For example, the `gopts` hook only requires the `get` function. A plugin that wants to act upon the `gopts` hook therefore has to export `system:/elektra/modules/<plugin name>/exports/hook/gopts/get`.

Other hooks (e.g. `spec`) require multiple exported functions.

### `gopts` hook

Hard coded to search for a plugin named `gopts`.

The following function **must** be exported:

- `get`
  - Signature: `(Plugin * handle, KeySet * returned, Key * parentKey)`
  - Called in `kdbGet` after the storage phase, after `notification/send` hook but before the `spec` hook.
  - TODO: Describe what the function should do

### `spec` hook

Hard coded to search for a plugin named `spec`.

The following functions **must** be exported:

- `copy`
  - Signature: `(Plugin * handle, KeySet * returned, Key * parentKey, bool isKdbGet)`
  - Called in:
    - `kdbGet`: after the storage phase, after `notification/send` and `gopts` hook.
    - `kdbSet`: right after the backends are initialized
  - Should copy all the spec meta keys into the keyset
- `remove`
  - Signature: `(Plugin * handle, KeySet * returned, Key * parentKey)`
  - Called in `kdbSet` right after the prestorage phase
  - Should remove all the spec meta keys from the keyset

### `notification/send` hook

We look within the array `system:/elektra/hook/notification/send/plugins` for the plugins that shall be loaded.
The name of the plugin **must** be the value of the keys directly below this,
e.g. `system:/elektra/hook/notification/send/plugins/#0 (= dbus)`.

The following functions **may** be exported (optional):

- `get`:

  - Signature: `(Plugin * handle, KeySet * returned, Key * parentKey)`
  - Called in `kdbGet` after the storage phase.

- `set`:
  - Signature: `(Plugin * handle, KeySet * returned, Key * parentKey)`
  - Called in `kdbSet` after the storage phase.

## Lifecycle

1. Hooks are initialized within `kdbOpen` after the contract has been processed. This includes loading the plugins.
2. The appropriate hooks are called within each `kdbGet` and `kdbSet` call.
3. Hooks are deinitialized within `kdbClose`. This includes unloading the plugins.
