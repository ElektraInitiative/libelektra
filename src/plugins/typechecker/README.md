- infos = Information about the typechecker plugin is in keys below
- infos/author = Armin Wurzinger <e1528532@libelektra.org>
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

The typesystem is based on regular expressions. Each key is assigned with a regex
that describes its contents. Links between keys can only be done if the regexes describing 
the linked keys are compatible with each other, i.e. the regex of the linked key is equal
or a subset of the regex of the linking key. This concept is not only useful for defining
links. It can be used to restrict the input types of transformations for 
instance as well.

## Usage

In order to use the type checker, mount a configuration specification along with this
plugin. 

`kdb mount <specification> spec/<path> <storage plugin to read the specification> typechecker`

Retrieving a random key from the specification using `kdb get <path>` will cause
the typechecking to happen. The typechecker will issue a warning if it detects any
problems with it. In case a key covered by a configuration specification is accessed
programmatically, the typechecking will also happen if the typechecker is mounted.

When altering a mounted specification using `kdb set <path>` or `kdb setmeta <path> <value>`,
the typechecker will issue an error if the newly added key or metakey will lead to an 
inconsistent configuration specification according to the type system specification.

It is often necessary to set multiple specification keywords at once in order to transform a
valid configuration specification to another valid one. Therefore some kind of transaction
is required so the typechecking only happens when the whole changes have been made. The
easiest way to do this is to use the command `kdb shell`. An example of its usage is shown
below.

## Examples

Create a sample configuration specification with three keys. As it is valid,
there will be no error issued. Currently we include the type signatures of the
various keywords manually in a specification. We use kdb shell to delay the 
typechecking until we have finished writing the whole specification.

```sh
# Backup-and-Restore:spec/examples/simplespecification

sudo kdb mount simplespecification.ini spec/examples/simplespecification ini typechecker

echo 'kdbGet spec/examples/simplespecification \
keySetName spec/examples/simplespecification/elektra/spec/fallback/# \
keySetMeta elektra/spec/type RegexContains b a => Key b :: . -> Key a -> Key a \
keySetMeta elektra/spec/impl fallback a (Key P.Nothing) = a \n fallback _ a = a \
ksAppendKey \
keyClear \
keySetName spec/examples/simplespecification/elektra/spec/override/# \
keySetMeta elektra/spec/type RegexContains b a => Key b :: . -> Key a -> Key a \
keySetMeta elektra/spec/impl override (Key P.Nothing) b = b \n override a _ = a \
ksAppendKey \
keyClear \
keySetName spec/examples/simplespecification/elektra/spec/check/range \
keySetMeta elektra/spec/type RegexIntersects a b => P.Proxy b :: Range . -> Key a -> Key (RegexIntersection a b) \
keySetMeta elektra/spec/impl checkrange _ a = a \
ksAppendKey \
keyClear \
keySetName spec/examples/simplespecification/elektra/spec/check/long \
keySetMeta elektra/spec/type RegexIntersects a "-[1-9]|-214748364[0-8]|-?[1-9][0-9]|-?[1-9][0-9][0-9]|-?[1-9][0-9][0-9][0-9]|-?[1-9][0-9][0-9][0-9][0-9]|-?[1-9][0-9][0-9][0-9][0-9][0-9]|-?[1-9][0-9][0-9][0-9][0-9][0-9][0-9]|-?[1-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9]|-?[1-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9]|-?1[0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9]|-?20[0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9]|-?21[0-3][0-9][0-9][0-9][0-9][0-9][0-9][0-9]|-?214[0-6][0-9][0-9][0-9][0-9][0-9][0-9]|-?2147[0-3][0-9][0-9][0-9][0-9][0-9]|-?21474[0-7][0-9][0-9][0-9][0-9]|-?214748[0-2][0-9][0-9][0-9]|-?2147483[0-5][0-9][0-9]|-?21474836[0-3][0-9]|[0-9]|214748364[0-7]" => Key a -> Key (RegexIntersection a "-[1-9]|-214748364[0-8]|-?[1-9][0-9]|-?[1-9][0-9][0-9]|-?[1-9][0-9][0-9][0-9]|-?[1-9][0-9][0-9][0-9][0-9]|-?[1-9][0-9][0-9][0-9][0-9][0-9]|-?[1-9][0-9][0-9][0-9][0-9][0-9][0-9]|-?[1-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9]|-?[1-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9]|-?1[0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9]|-?20[0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9]|-?21[0-3][0-9][0-9][0-9][0-9][0-9][0-9][0-9]|-?214[0-6][0-9][0-9][0-9][0-9][0-9][0-9]|-?2147[0-3][0-9][0-9][0-9][0-9][0-9]|-?21474[0-7][0-9][0-9][0-9][0-9]|-?214748[0-2][0-9][0-9][0-9]|-?2147483[0-5][0-9][0-9]|-?21474836[0-3][0-9]|[0-9]|214748364[0-7]") \
keySetMeta elektra/spec/impl checklong a = a \
ksAppendKey \
keyClear \
keySetName spec/examples/simplespecification/key1 \
keySetMeta check/range 0-5000 \
ksAppendKey \
keyClear \
keySetName spec/examples/simplespecification/key2 \
keySetMeta check/range 7200-10000 \
ksAppendKey \
keyClear \
keySetName spec/examples/simplespecification/key3 \
keySetMeta check/long \
keySetMeta fallback/#1 spec/examples/simplespecification/key1 \
ksAppendKey \
keyClear \
kdbSet spec/examples/simplespecification' | kdb shell

kdb get spec/examples/simplespecification/key1
```

Add an invalid link and see how it refuses the specification, showing the erroneous
parts instead.

```sh
kdb setmeta spec/examples/simplespecification/key2 fallback/#1 spec/examples/simplespecification/key1
# RET: 5
# STDERR-REGEX: .*Couldn't match type.*

kdb rm -r spec/examples/simplespecification
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
