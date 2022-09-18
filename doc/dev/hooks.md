# Hooks

Hooks are central points in the KDB lifecycle, where specialized plugins are called. 
Hooks are the new mechanism that will replace the old global plugins, [as specified in this decision](../decisions/global_plugins.md).

## Selecting which Plugin will be used for a specific hook

Currently, the names of the plugins are hard-coded.

## Interface of the hooks

If a plugin should be able to act upon a hook, it must export all the functions that the hook requires.
These exports are of the form `system:/elektra/modules/<plugin name>/exports/hooks/<hook name>/<hook function>`.

For example, the `gopts` hook only requires the `get` function. A plugin that wants to act as a `gopts` hook therefor has to export `system:/elektra/modules/<plugin name>/exports/hooks/gopts/get`.

Other hooks might require multiple exported functions.

## Lifecycle

1. Hooks are initilized within `kdbOpen` after the contract has been processed. This includes loading the plugins.
2. The appropriate hooks are called within each `kdbGet` and `kdbSet` call.
3. Hooks are deinitialized within `kdbClose`. This includes unloading the plugins.

