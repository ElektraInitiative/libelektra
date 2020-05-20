- infos = Information about the list plugin is in keys below
- infos/author = Thomas Waser <thomas.waser@libelektra.org>
- infos/licence = BSD
- infos/needs =
- infos/provides =
- infos/placements = pregetstorage procgetstorage postgetstorage postgetcleanup presetstorage presetcleanup precommit postcommit prerollback postrollback
- infos/status = unittest nodep libc configurable global
- infos/description = delegates work to a list of plugins

## Introduction

The List plugin can be used everywhere a list of plugins is required. It takes a list of plugins + configurations
for every placement it's placed in and loads them.

## Configuration

`placements/set`

Specifies the set-placements for the list plugin, e.g. "presetstorage precommit"

`placements/get`

Specifies the get-placements for the list plugin.

`placements/error`

Specifies the error-placements for the list plugin.

`config/`

Common config for all plugins.

`plugins/#`

The name of the plugin to load.

`plugins/#/handle`

Internal handle of already loaded plugin. Useful when loading plugins at
run-time with `elektraPluginOpen()`. Do not set permanently (e.g. with
`kdb set`).

`plugins/#/placements/set`

A list of set-placements for the plugin. Same for "get" and "error"

`plugins/#/config/`

Plugin specific config.

## Exported Functions

The plugin exports a few useful functions:

```c
int elektraListMountPlugin (Plugin * handle, const char * pluginName, KeySet * pluginConfig, Key * errorKey)
int elektraListUnmountPlugin (Plugin * handle, const char * pluginName, Key * errorKey)
Plugin * elektraListFindPlugin (Plugin * handle, const char * pluginName)
```

`elektraListMountPlugin` can be used to add a new plugin to the config. The placement will be queried from the plugin itself (from its
`infos/placements` metadata). If the plugin is added already nothing happens, otherwise `pluginConfig` is used to open the plugin.

`elektraListUnmountPlugin` is the opposite, it is used to remove a plugin from the config.

Finally, `elektraListFindPlugin` looks for a plugin in the config, and if found returns its handle.

## Example

```
placements/get = "postgetstorage"
config/cut = "will/be/overridden/for/plugin/#0"
plugins/#0 = "rename"
plugins/#0/placements   plugin placements
plugins/#0/placements/get = "postgetstorage"
plugins/#0/config   pluginconfig goes here
plugins/#0/config/cut = "will/be/stripped"
plugins/#1 = "keytometa"
plugins/#1/placements
plugins/#1/placements/get = "postgetstorage"
plugins/#2 = "enum"
plugins/#2/placements
plugins/#2/placements/get = "postgetstorage"
```

would have the callstack:

1. `list->kdbGet`
   1. `rename->kdbGet`
   2. `keytometa->kdbGet`
   3. `enum->kdbGet`
