# CMake Plugins

## Issue

- Plugin names and plugin folders must exactly match (resolver_*, crypto_*)
- Non-existing plugin variants are not checked
- Default plugin collection is too small
- Duplicate of all plugin names in different folder
- Only one category per plugin
- On some OS some plugins won't work (simpleini)

## Constraints

## Assumptions

- Many plugins work without dependencies and variants, this should be simple

## Considered Alternatives

- have one more mapping that describes folder <-> plugins (bad coherence)
- directly use README.md to also describe cmake deps (too limited in expression)
- split up AddPlugin.cmake and CMakeLists.txt (does not work well with variants)
- simply adding all directories in src/plugins and decide within the `add_plugin`
  if we should drop the plugin (see below in Argument)

Names for flag:

- `DEPENDENCIES` -> might be used for `add_plugin`
- `ASSEMBLE_DEPS`
- `FIND_DEPS` -> not only finding happens
- `PROCESS_DEPS`
- `HANDLE_DEPS`
- `DEPS_MODE` -> too generic, new terminology
- `CHECK_DEPS` -> not only checking happens
- `DO_DEPS` -> sounds funny
- `FIND_PACKAGES` -> not only packages are subject to this phase
- `COLLECT` -> needs NOT
- `FIND_PHASE` -> needs NOT
- `DEPENDENCY` -> ambiguous

## Decision

Introduce a cmake process where all plugins are processed two times:

1.) collection phase (`DEPENDENCY_PHASE` is `OFF`),
  with:
  - `ADDED_PLUGINS`
  - `REMOVED_PLUGINS`
  - `ADDED_DIRECTORIES`
  - add_plugin, but without actually adding internally
2.) assemble dependency phase (`DEPENDENCY_PHASE` is `ON`, only considering `ADDED_DIRECTORIES`),
  with:
  - add_libraries, actually search for libraries on the system
  - add_plugin, with *actually adding* the plugins

Use following cmake variables for the phases:

- `COLLECTION_PHASE` .. collect all `add_plugins`
- `DEPENDENCY_PHASE` .. resolve all dependencies, do `add_plugins` again
- `ADDTESTING_PHASE` .. (reserve for potential 3rd phase)

## Argument

Solves all the issues without adding too much complexity.
Can be extended to three phases if we need that for tests
(they need knowledge about which plugins actually were added)

Maintaining additional mappings is very time-consuming.
Simply adding all plugins directories is problematic. It would:

- clutter the cmake output (especially in the case of errors)
- introduce more variables into the CMakeCache which are irrelevant for the user
- maybe even find libraries in wrong versions which are incompatible to what other
  plugins need


## Implications

- need to adopt all CMakeLists.txt to use `find_package` only in the second phase

## Related decisions

## Notes
