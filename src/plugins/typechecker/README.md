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

There are basically two ways to use typechecker plugin. First we describe how the plugin
is used in general for configuration specification developers and users. Afterwards we 
describe how the typechecker plugin can be extended with the semantics of additional 
configuration specification keywords.

### General Usage

In order to use the type checker, mount a configuration specification along with this
plugin. There are already a lot of commonly used keywords supported out of the box specified 
in the file [prelude.ini](/src/plugins/typechecker/typechecker/prelude.ini).
Usually one wants to mount that file as well beforehand so the standard configuration.

`kdb mount prelude.ini spec/<path>/spec/elektra ini`
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

Please keep in mind that its better to unmount in the reverse way, so first unmount your
specification and then prelude.

### Usage for custom configuration specification keywords

In case a new plugin has been written which can be described using regular expressions, a
plugin developer may want to teach the type system about this plugin. In order to do that,
each configuration specification may contain a section prefixed with the path `elektra/spec`
that contains information about how the type system should treat a specification keyword by
formalizing its semantics as a function's type signature. Let's take the example of the 
fallback keyword, which would be formalized in the ini format as follows:

```
#@META elektra/spec/type = RegexContains b a => Key b :: . -> Key a -> Key a
#@META elektra/spec/impl = fallback a (Key P.Nothing) = a ; fallback _ b = b
[/elektra/spec/fallback/#]
```

A type signature, inspired by Haskell's type signatures, typically has two parts. The first 
part describes its constraints followed by `=>`. Then its parameter's types follow separated
by a `->`, where the last parameter type is the result type.
A constraint can bei either a containment constraint or a intersection constraint, written
`RegexContains` or `RegexIntersects` followed by two type variables. Type variables may as well
be a string constant describing a regex as we will see in the next example. In our case 
`RegexContains b a` means that the regex stored in the type variable a is either equal to or
a subset of the regex stored in the type variable b. The type variables arise from the type
parameters. A type parameter may either refer to a `Key` or to a `Regex`. A `Key` may either
refer to the path of another Key which is given relatively to the current key after the `::` 
symbol, or to the annotated key itself, in which case no extra annotation is necessary. 
A `Regex` refers directly to a regex which can then be used as a type variable.

Additionally, a small haskell implementation has to be given. Currently our type system does not 
support any kind of operations other than simple pattern matching on Keys, that may either hold
an arbitrary string as a default value matched as `Key (P.Just a)` or no default value, 
described as `Key P.Nothing`. If you want to pattern match over several possibilities like its
done for fallback, use `;` to separate between lines.

Summing up the above example can be read as "If the regex of the Key b, which path is obtained
by taking the value of the current key, relatively referred to via `.` is equal to or a 
subset of the regex describing the current key, this link is allowed. In that case the type
of the current key will not be changed and remain to be the regex stored in the type variable a.

Let us look at another example which makes use of `RegexIntersects`. Suppose you want to make a
specification keyword that checks whether the key's content lies with the boundaries of a 16 bit
unsigned short value, corresponding to the range 0-65535, or rewritten as a regex, 
`([0-9]|[1-8][0-9]|9[0-9]|[1-8][0-9]{2}|9[0-8][0-9]|99[0-9]|[1-8][0-9]{3}|9[0-8][0-9]{2}|99[0-8][0-9]|999[0-9]|[1-5][0-9]{4}|6[0-4][0-9]{3}|65[0-4][0-9]{2}|655[0-2][0-9]|6553[0-5])`

```
#@META elektra/spec/type = RegexIntersects a "([0-9]|[1-8][0-9]|9[0-9]|[1-8][0-9]{2}|9[0-8][0-9]|99[0-9]|[1-8][0-9]{3}|9[0-8][0-9]{2}|99[0-8][0-9]|999[0-9]|[1-5][0-9]{4}|6[0-4][0-9]{3}|65[0-4][0-9]{2}|655[0-2][0-9]|6553[0-5])" => Key a -> Key (RegexIntersection a "([0-9]|[1-8][0-9]|9[0-9]|[1-8][0-9]{2}|9[0-8][0-9]|99[0-9]|[1-8][0-9]{3}|9[0-8][0-9]{2}|99[0-8][0-9]|999[0-9]|[1-5][0-9]{4}|6[0-4][0-9]{3}|65[0-4][0-9]{2}|655[0-2][0-9]|6553[0-5])")"
#@META elektra/spec/impl = unsignedshort a = a
[/elektra/spec/check/unsignedshort]
```

This signature means "If the regex represented by the type variable a, which in that case refers
to the current key's regex, can be intersected with the regex depicting the range, the check
passes successfully and the resulting type is the intersection between the two regexes." This
approach is typically used with checker plugins which further restrict what kind of contents
a key may contain, and each intersection limits the type more and more. The constraints prevents
that two incompatible checks can be used for a single key.

## Examples

Create a sample configuration specification with three keys. As it is valid, there will be no error 
issued. We rely on the existence of [prelude.ini](/src/plugins/typechecker/typechecker/prelude.ini), which already
contains the type definitions for `check/range`, `check/long` and `fallback/#` and mount it along.
We use kdb shell to delay the typechecking until we have finished writing the whole specification.

```sh
# Backup-and-Restore:spec/examples/simplespecification

# When elektra is installed the first variant will work
# the following kdb get only decides whether this example is executed as a shell recorder test 
# during a build, then the second variant points to the correct location
(sudo kdb mount prelude.ini spec/examples/simplespecification/elektra/spec ini && \
	kdb get spec/examples/simplespecification/elektra/spec/fallback/#) || \
	(sudo kdb umount spec/examples/simplespecification/elektra/spec && \
		sudo kdb mount "$PWD/src/plugins/typechecker/typechecker/prelude.ini" spec/examples/simplespecification/elektra/spec ini)
# so in a normal use case one would simply call
# sudo kdb mount prelude.ini spec/examples/simplespecification/elektra/spec ini
sudo kdb mount simplespecification.ini spec/examples/simplespecification ini typechecker

echo 'kdbGet spec/examples/simplespecification \
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

Add an invalid link now and see how it refuses the specification, showing the erroneous
parts instead. As the two keys represent the ranges 0-5000 and 7200-10000, they 
obviously cannot be linked together.

```sh
kdb setmeta spec/examples/simplespecification/key2 fallback/#1 spec/examples/simplespecification/key1
# RET: 5
# STDERR-REGEX: .*Couldn't match type.*

sudo kdb umount spec/examples/simplespecification
sudo kdb umount spec/examples/simplespecification/elektra/spec
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
- The small implementations are not yet auto generated

