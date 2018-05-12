- infos = Information about the haskell plugin is in keys below
- infos/author = e1528532 <e1528532@libelektra.org>
- infos/licence = BSD
- infos/needs =
- infos/provides =
- infos/recommends =
- infos/placements = getstorage setstorage
- infos/status = maintained memleak experimental
- infos/metadata =
- infos/description = base for haskell plugins

## Introduction

A plugin which takes care about initializing the haskell run-time.

## Usage

This plugin on its own provides a minimalistic implementation to test
the basic functionality, but acts as a base for developing further haskell plugins.
To be precise, the following files are required and should use the following
naming scheme, with `<PLUGIN_NAME>` being the name of this plugin (`haskell` in this case):

- CMakeLists.txt which includes and calls the LibAddHaskellPlugin macro
- `<PLUGIN_NAME>.cabal.in` which is the cabal build file for the plugin, ensure that:
  - `hs-source-dirs:` is set to `"@CMAKE_CURRENT_SOURCE_DIR@"`
  - `build-depends:` includes `libelektra-haskell` for the haskell bindings
  - `build-type:` is set to `Custom`
  - `@CABAL_CUSTOM_SETUP@` is included somewhere in case the default Setup.hs is
    used
- `testmod_<PLUGIN_NAME>.c` which includes the tests for the plugin

Use the cmake command add_haskell_plugin which can be used by including LibAddHaskellPlugin.
This command will take care about the proper linking of your haskell plugin. Furthermore it
uses the c wrapper provided by this plugin so this doesn't have to be done again.

By default plugins get built in a cabal sandbox shared with all other Haskell plugins-
and the Haskell bindings to speed up compilation by compiling commonly used dependencies
just once. Dependencies are automatically resolved according to the cabal build
configuration.

## Dependencies

- Elektra's haskell bindings
- ghc, tested with 8.1.2, may work with older versions as well
- ghc-pkg, usually bundled with ghc
- cabal, the haskell build system, usually bundled with ghc

## Limitations

Currently the Haskell plugin only executes tests written in C and not directly in Haskell.

## Other

Unfortunately the haskell runtime itself leaks some memory, but as haskell plugins are generally
executed in an isolated process it should not matter too much.
