- infos = Information about the haskell plugin is in keys below
- infos/author = e1528532 <e1528532@libelektra.org>
- infos/licence = BSD
- infos/needs =
- infos/provides =
- infos/recommends =
- infos/placements = postgetstorage presetstorage
- infos/status = maintained experimental global
- infos/metadata =
- infos/description = base for haskell plugins

## Introduction

A plugin which typechecks specifications before setting keys and after getting keys from
a mounted specification. 

## Usage

This plugin on its own provides a minimalistic implementation to test
the basic functionality, but acts as a base for developing further haskell plugins.

Use the cmake command add_haskell_plugin which can used by including LibAddHaskellPlugin.
This command will take care about the proper linking of your haskell plugin. Furthermore it 
uses the c wrapper provided by this plugin so this doesn't have to be done again.

## Dependencies

* Elektra Haskell bindings installed
* ghc, tested with 8.1.2, may work with older versions as well
* ghc-pkg, usually bundled with ghc
* cabal, the haskell build system, usually bundled with ghc

## Limitations

None.
