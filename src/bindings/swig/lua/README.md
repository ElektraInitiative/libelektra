- infos = Information about elektrify getenv below
- infos/author = Manuel Mausz <manuel-elektra@mausz.at>
- infos/status =
- infos/provides = swig
- infos/description =

# Lua

Lua bindings for Elektra.

## Version

Should work with Lua 5.1 or later. Iterators need Lua 5.2.
Please make sure to set `TARGET_LUA_CMOD_FOLDER` and `TARGET_LUA_LMOD_FOLDER` if you use lua != 5.2.

## Building

Note that cmake does _not_ automatically rebuild SWIG bindings
when header files are changed. Remove the build directory
in that case.
