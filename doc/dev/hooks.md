# Hooks

Hooks are central points in the KDB lifecycle, where specialized plugins are called.

## Selecting which Plugin will be Used for a Specific Hook

Currently, the names of the plugins are hard-coded.
This [decision](../decisions/hooks.md) was made, because these plugins are meant to fulfil very specific purposes.
A symlink replacing the shared library file of the plugin could be used to change the implementation.

## Interface of the hooks

If a plugin should be able to act upon a hook, it must export all the functions that the hook requires.
These exports are of the form `system:/elektra/modules/<plugin name>/exports/hook/<hook name>/<hook function>`.

For example, the `gopts` hook only requires the `get` function. A plugin that wants to act as a `gopts` hook therefore has to export `system:/elektra/modules/<plugin name>/exports/hook/gopts/get`.

Other hooks (e.g. `spec`) require multiple exported functions.

## Lifecycle

1. Hooks are initilized within `kdbOpen` after the contract has been processed. This includes loading the plugins.
2. The appropriate hooks are called within each `kdbGet` and `kdbSet` call.
3. Hooks are deinitialized within `kdbClose`. This includes unloading the plugins.
