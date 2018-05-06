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

The typesystem is currently based on regular expressions. Each key is assigned with a regex
that describes its contents. Links between keys can only be done if the regexes describing 
the linked keys are compatible with each other.

## Usage

In order to use the type checker, mount a configuration specification along with this
plugin. 

`kdb mount <specification> spec/<path> <storage plugin to read the specification> typechecker`

Currently there is no way to check the specification automatically upon mounting. Simply 
retrieve a random key from the specification using `kdb get <path>` will cause
the typechecking to happen, issuing a warning if it detects any problem with it.

## Examples

Loading a configuration specification which is invalid.

```sh
# Backup-and-Restore:spec/examples/simplespecification

sudo kdb mount simplespecification.ini spec/examples/simplespecification ini typechecker

kdb get spec/examples/simplespecification/key2
#> bar

sudo kdb rm -r spec/examples/simplespecification
sudo kdb umount spec/examples/simplespecification
```

## Debugging

This test specification has no errors by default and will thus report nothing,
but if you alter it you can experiment with the typechecker. If Elektra is compiled
with the ENABLE_LOGGER flag, it will log the inferred types in all cases so the
type behavior can be observed when getting/setting a key in a specification.

## Dependencies

* ghc >= 8.0.1 and <= 8.2
* ghc-pkg, usually bundled with ghc
* cabal, the haskell build system, usually bundled with ghc
* augeas, which provides libfa utilized by this plugin

## Limitations

- Rather experimental
- Typechecking only happens when getting or setting
a key in a mounted specification
- Errors are currently raw and haskell-focused
