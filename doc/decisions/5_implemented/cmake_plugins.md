# CMake Plugins

## Problem

- plugin names and plugin folders not always exactly match (`resolver_*`, `crypto_*`)
- plugin should be able to register new variants
- there should be only one place to define a new plugin
- multiple categories should be possible per plugin, defined in README.md
- on some OS some plugins won't work (simpleini)
- some unit tests depend on bindings

## Constraints

- should work with supported CMake
- It should be easy to add trivial plugins (without dependency and variants)
- It should be possible to add complex plugins (with dependencies on plugins/unit tests and many variants)

## Assumptions

## Considered Alternatives

- have one more mapping that describes folder <-> plugins (bad coherence)
- directly use README.md to also describe cmake deps (too limited in expression)
- split up AddPlugin.cmake and CMakeLists.txt (does not work well with variants)
- simply adding all directories in src/plugins and decide within the `add_plugin`
  if we should drop the plugin (see below in Rationale)

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

Introduce a CMake process where all plugins are processed three times.
Following CMake variables are used for the phases:

- `COLLECTION_PHASE` .. collect all `add_plugins`
- `DEPENDENCY_PHASE` .. resolve all dependencies, do `add_plugins` again
- `ADDTESTING_PHASE` .. (reserve for potential 3rd phase)

1. Collection phase (`COLLECTION_PHASE` is `ON`), add_plugin internally builds up:
   - `ADDED_PLUGINS`
   - `REMOVED_PLUGINS`
   - `ADDED_DIRECTORIES`
2. assemble dependency phase (`DEPENDENCY_PHASE` is `ON`, only considering `ADDED_DIRECTORIES`), with:
   - `find_libraries`, actually search for libraries on the system
     (only relevant libraries of plugins that are considered for inclusion)
   - `add_plugin`, with _actually adding_ the plugins
3. assemble all unit tests (`ADDTESTING_PHASE` is `ON`), either
   - with `ADD_TEST` in `add_plugin`, or
   - with `add_plugintest` (for unittests that have dependencies to bindings)

## Rationale

Solves all the issues without adding too much complexity for actually adding plugins.

Maintaining additional mappings is very time-consuming.
Simply adding all plugins directories is problematic.
It would:

- clutter the cmake output (especially in the case of errors)
- introduce more variables into the CMakeCache which are irrelevant for the user
- maybe even find libraries in wrong versions which are incompatible to what other plugins need

## Implications

- need to adopt all CMakeLists.txt

## Related Decisions

- [Plugin Variants](../0a_delayed/plugin_variants.md)

## Notes

## Limitations

- `ADDED_DIRECTORIES` of variants will be kept.
- Typos in plugin names are currently not checked, strings that are not plugin names are simply ignored.
