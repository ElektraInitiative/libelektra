- infos = Information about the lua plugin is in keys below
- infos/author = Manuel Mausz <manuel-elektra@mausz.at>
- infos/licence = BSD
- infos/provides =
- infos/needs =
- infos/placements =
- infos/status = maintained unittest configurable global memleak
- infos/description = proxy that calls other plugins (scripts) written in lua

## Introduction

The plugin uses Lua to do magic things. It basically allows to call plugins written in Lua.

What a Lua script can do is not really limited by design, so any kind of plugin may be
implemented. The lua plugin is especially useful to write filter and logging scripts.

## Usage

The lua plugin accepts only the **script** configuration parameter holding the path to a Lua
script. The mount command would look like

    kdb mount file.ini /lua ini lua script=/path/to/filter_script.lua

if the **ini** plugin should be used for storage and the lua plugin only serves to invoke the
filter script.

For a Lua script that serves as (json) storage plugin itself, one could also use

    kdb mount file.json /lua lua script=/path/to/json_plugin.lua

### Lua Scripts

Lua scripts can implement the following functions

- elektraOpen(config, errorKey)
- elektraGet(returned, parentKey)
- elektraSet(returned, parentKey)
- elektraError(returned, parentKey)
- elektraClose(errorKey)

where *config* & *returned* are KeySets and *errorKey* & *parentKey* are Keys.
For the return codes of the functions, the same rules as for normal plugins apply.

If a function is not available, it simply is not called. A script does not have to
implement all functions therefore.

Access to **kdb** can be retrieved using the Lua import

    require("kdb")

## Example

An example script that prints some information for each method call would be:

    function elektraOpen(config, errorKey)
	    print("Lua script method 'elektraOpen' called")
	    return 0
    end

    function elektraGet(returned, parentKey)
	    print("Lua script method 'elektraGet' called")
	    return 1
    end

    function elektraSet(returned, parentKey)
	    print("Lua script method 'elektraSet' called")
	    return 1
    end

    function elektraError(returned, parentKey)
	    print("Lua script method 'elektraError' called")
	    return 1
    end

    function elektraClose(errorKey)
	    print("Lua script method 'elektraClose' called")
	    return 0
    end

Further examples can be found in the [lua](lua/) directory.

## Disclaimer

Note, this is a technical preview. It might have severe bugs
and the API might change in the future.

Be also aware that a Lua script will never be as performant as a native C/C++ plugin.
Spinning up the interpreter takes additional time and resources.

