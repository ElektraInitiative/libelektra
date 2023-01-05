- infos = Information about Lua bindings below
- infos/author = Manuel Mausz <manuel-elektra@mausz.at>
- infos/status = maintained
- infos/provides = swig
- infos/description =

# Lua

Lua bindings for Elektra.

## Installation

See [installation](/doc/INSTALL.md).
The package is called `lua-elektra`.

## Version

Should work with Lua 5.1 or later. Iterators need Lua 5.2.
Please make sure to set `TARGET_LUA_CMOD_FOLDER` and `TARGET_LUA_LMOD_FOLDER` if you use lua != 5.2.

## Building

Note that CMake does _not_ automatically rebuild SWIG bindings
when header files are changed. Remove the build directory
in that case.

## Iteration

Use external iterators the following way:

```lua
size = kdb.ksSize(keySet);
if size > 0 then
	for i = 1, size do
		key = kdb.ksAt(keySet, i - 1)
		-- ...
	end
end
```
