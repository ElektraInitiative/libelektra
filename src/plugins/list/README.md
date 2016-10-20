- infos = Information about the list plugin is in keys below
- infos/author = Thomas Waser <thomas.waser@libelektra.org>
- infos/licence = BSD
- infos/needs =
- infos/provides =
- infos/placements = presetstorage pregetstorage postgetstorage precommit postcommit prerollback postrollback
- infos/status = maintained unittest nodep libc global
- infos/description = loads a list of plugins

## Introduction ##

The List plugin can be used everywhere a list of plugins is required. It takes a list of plugins + configurations 
for every placement it's placed in and loads them.

## Configuration ##

`placements/set`

Specifies the set-placements for the list plugin, e.g. "presetstorage precommit"

`placements/get`

Specifies the get-placements for the list plugin.

`placements/error`

Specifies the error-placements for the list plugin.

`plugins/#`

The name of the plugin to load.

`plugins/#/placements/set`

A list of set-placements for the plugin. Same for "get" and "error"

`plugins/#/config/`

Plugin specific config.

## Example ##

    placements/get = "postgetstorage"
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

would have the callstack:

1. `list->kdbGet`
  1. `rename->kdbGet`
  2. `keytometa>kdbGet`
  3. `enum->kdbGet`

