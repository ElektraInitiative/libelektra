- infos = Information about the typechecker plugin is in keys below
- infos/author = e1528532 <e1528532@libelektra.org>
- infos/licence = BSD
- infos/needs = 
- infos/provides = typechecker
- infos/recommends =
- infos/placements = postgetstorage presetstorage
- infos/status = maintained experimental global
- infos/metadata =
- infos/description = a plugin which typechecks configuration specifications

## Introduction

A plugin which typechecks specifications before setting keys and after getting keys from
a mounted specification. 

## Usage

To see its functionality there will be a test specification installed into 
`/usr/local/share/elektra/specifications/simpleTestSpecification.ini`. This
path may vary slightly depending on the build configuration. The file can be
typechecked by mounting it along with the typechecker plugin by calling

`kdb mount simpleTestSpecification.ini spec/examples ini typechecker`

This test specification has no errors by default and will thus report nothing,
but if you alter it you can experiment with the typechecker. If Elektra is compiled
with the ENABLE_LOGGER flag, it will log the inferred types in all cases so the
type behavior can be observed when getting/setting a key in a specification.

## Dependencies

* ghc, tested with 8.1.2, may work with older versions as well
* ghc-pkg, usually bundled with ghc
* cabal, the haskell build system, usually bundled with ghc

## Limitations

Very experimental. Typechecking currently only happens when getting or setting
a key in a mounted specification. Errors are currently very raw and haskell-focused.
