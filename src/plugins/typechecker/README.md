- infos = Information about the typechecker plugin is in keys below
- infos/author = Armin Wurzinger <e1528532@libelektra.org>
- infos/licence = BSD
- infos/needs =
- infos/needs = regexdispatcher
- infos/provides = typechecker
- infos/recommends =
- infos/placements = postgetstorage presetstorage
- infos/status = maintained global experimental
- infos/metadata =
- infos/description = a plugin that type checks configuration specifications

## Introduction

A plugin that type checks specifications before setting keys and after getting keys from
a mounted specification.

The type checker uses regular expressions as the foundation. It assigns a regex to each key
describing its possible contents, thus defining a key's type. Links between keys will fail
the type check if the regexes describing the linked keys are not compatible with each other,
i.e. the regex of the linked key is equal or a subset of the regex of the linking key.
This concept is not only useful for defining links. It can be used to restrict the input
types of transformations for instance as well.

## Usage

There are basically two ways to use type checker plugin. First we describe how the plugin
is used in general for users. Afterwards we describe how the type checker plugin can be
extended with the semantics of additional configuration specification keywords. This is
interesting for plugin developers who want to make use of the type system for their work
as well.

### General Usage

In order to use the type checker, mount a configuration specification along with this
plugin. There are already a lot of commonly used keywords supported out of the box specified
in the file [prelude.ini](/src/plugins/typechecker/typechecker/prelude.ini).
Usually one wants to mount that file as well beforehand for the standard configuration.

`kdb mount prelude.ini spec/<path>/spec/elektra ini`
`kdb mount <specification> spec/<path> <storage plugin to read the specification> typechecker`

Retrieving any key from the specification using `kdb get <path>` will cause
the type checking to happen. The type checker will issue a warning if it detects any
problems with it. In case a key covered by a configuration specification is accessed
programmatically, the type checking will also happen if the type checker is mounted.

When altering a mounted specification using `kdb set <path>` or `kdb setmeta <path> <value>`,
the type checker will issue an error if the newly added key or metakey will lead to an
inconsistent configuration specification according to the type system specification.

It is often necessary to set multiple specification keywords at once in order to transform a
valid configuration specification to another valid one. Therefore some kind of transaction
is required so the type checking only happens when the whole changes have been made. The
easiest way to do this is to use the command `kdb shell`. An example of its usage is shown
below. In case you prefer a graphical tool to edit configuration specifications you can use the
qt-gui for Elektra.

Please keep in mind that its better to unmount in the reverse way, so first unmount your
specification and then prelude.

### Usage for custom configuration specification keywords

In case a new plugin has been written which can be described using regular expressions, a
plugin developer may want to teach the type system about this plugin. In order to do that,
each configuration specification may contain a section describing the effects of keywords.
There are three metakeys that are interpreted by the typechecker, `elektra/spec/order`,
`elektra/spec/type` and `elektra/spec/impl`. These metakeys get explained and introduced
along the following examples. Let's take the example of the fallback keyword,
that would be formalized in the ini format as follows:

```
#@META elektra/spec/order = 10
#@META elektra/spec/impl = fallback a b = link a b
[fallback/#]
```

In the implementation `elektra/spec/impl` we are limited to the two functions `link` and `intersect`
forming the basic building blocks for describing more complex keywords. They respective type signatures
are `RegexContains b a => Key b :: . -> Key a -> Key a` for `link` and
`Intersectable (RegexIntersection a b) => Key a -> Regex b -> Key (RegexIntersection a b)`
for `intersect`.
A type signature, inspired by Haskell's type signatures, typically has two parts. The first
part describes its constraints followed by `=>`. Then its parameter's types follow separated
by a `->`, where the last parameter type is the result type.
A constraint can bei either a containment constraint or an intersection constraint, written
`RegexContains` or `Intersectable` followed by two type variables. Type variables may as well
be a string constant describing a regex as we will see in the next example. In our case
`RegexContains b a` means that the regex stored in the type variable a is either equal to or
a subset of the regex stored in the type variable b. The type variables arise from the type
parameters. A type parameter may either refer to a `Key` or to a `Regex`. A `Key` may either
refer to the path of another key which is given relatively to the current key after the `::`
symbol, or to the annotated key itself, in which case no extra annotation is necessary.
A `Regex` refers directly to a regex which can then be used as a type variable. Last, we see
the type `RegexIntersection` that represents the result of the intersection of two regexes.
The `Intersectable` constraints checks whether such an intersection is possible. This is used
to prevent two incompatible checks from being applied to a single key. For instance, consider
a check for letters and another check for numbers, there would be nothing that could be stored
in such a key. The metakey `elektra/spec/order` is used to specify the order in which checks
or links are applied. This is interesting because links should happen when all the checks on a
single key have already been applied so all the limitations of a key are already in effect.

Summing up the above example can be read as "If the regex of the key b, which path is obtained
by taking the value of the current key, relatively referred to via `.` is equal to or a
subset of the regex describing the current key, this link is allowed. In that case the type
of the current key will not be changed and remain to be the regex stored in the type variable a.

Let us look at another example which makes use of `intersect`. Suppose you want to make a
specification keyword that checks whether the key's content lies with the boundaries of a 16 bit
unsigned short value, corresponding to the range 0-65535, or rewritten as a regex,
`([0-9]|[1-8][0-9]|9[0-9]|[1-8][0-9]{2}|9[0-8][0-9]|99[0-9]|[1-8][0-9]{3}|9[0-8][0-9]{2}|99[0-8][0-9]|999[0-9]|[1-5][0-9]{4}|6[0-4][0-9]{3}|65[0-4][0-9]{2}|655[0-2][0-9]|6553[0-5])`

```
#@META elektra/spec/impl = checkunsignedshort a = intersect a (Key :: Regex "([0-9]|[1-8][0-9]|9[0-9]|[1-8][0-9]{2}|9[0-8][0-9]|99[0-9]|[1-8][0-9]{3}|9[0-8][0-9]{2}|99[0-8][0-9]|999[0-9]|[1-5][0-9]{4}|6[0-4][0-9]{3}|65[0-4][0-9]{2}|655[0-2][0-9]|6553[0-5])")
[check/unsignedshort]
```

This function means "If the regex represented by the type variable a, which in that case refers
to the current key's regex, can be intersected with the regex depicting the range, the check
passes successfully and the resulting type is the intersection between the two regexes." This
approach is typically used with checker plugins which further restrict what kind of contents
a key may contain, and each intersection limits the type more and more. The constraints prevents
that two incompatible checks can be used for a single key.

It is also possible to explicitly state the type signature for a metakey in case its effect cannot
be specified and inferred using `link` or `intersect`. In that case we can use `undefined` as the
result of the function. For instance the following example defines a transformation
that can be used on keys representing letters, and outputs a number:

```
#@META elektra/spec/order = 0
#@META elektra/spec/type = RegexContains a "[a-zA-Z]*" => Key a -> Key "[1-9][0-9]*|0"
#@META elektra/spec/impl = transformcountletters a = undefined
[transform/countLetters]
```

## Examples

Create a sample configuration specification with three keys. As it is valid, there will be no error
issued. We rely on the existence of [prelude.ini](/src/plugins/typechecker/typechecker/prelude.ini), which already
contains the type definitions for `check/range`, `check/long` and `fallback/#` and mount it along.
Please note not to mount prelude below the specification. We use kdb shell to delay the type checking
until we have finished writing the whole specification.

```sh
# Backup-and-Restore:spec/tests/typechecker
sudo kdb mount simplespecification.ini spec/tests/typechecker ini regexdispatcher typechecker

echo 'kdbGet spec/tests/typechecker \
keySetName spec/tests/typechecker/key1 \
keySetMeta check/range 0-5000 \
ksAppendKey \
keyClear \
keySetName spec/tests/typechecker/key2 \
keySetMeta check/range 7200-10000 \
ksAppendKey \
keyClear \
keySetName spec/tests/typechecker/key3 \
keySetMeta check/long \
keySetMeta fallback/#1 spec/tests/typechecker/key1 \
ksAppendKey \
keyClear \
keySetName spec/tests/typechecker/key4 \
keySetMeta check/validation a[0-9]+ \
ksAppendKey \
keyClear \
keySetName spec/tests/typechecker/key5 \
keySetMeta check/validation [a-z][0-9]+ \
keySetMeta fallback/#1 spec/tests/typechecker/key4 \
ksAppendKey \
keyClear \
kdbSet spec/tests/typechecker' | kdb shell

kdb get spec/tests/typechecker/key1
```

Add an invalid link now and see how it refuses the specification, showing the erroneous
parts instead. As the two keys represent the ranges 0-5000 and 7200-10000, they
obviously cannot be linked together.

```sh
kdb setmeta spec/tests/typechecker/key2 fallback/#1 spec/tests/typechecker/key1
# RET: 5
# STDERR: .*Could not deduce:.*

sudo kdb umount spec/tests/typechecker
```

As indicated in the limitations, error messages are currently rather raw and haskell-focused:

```
Type checking or compilation failed:
 /tmp/testSpecification80690-1.hs:15:8: error:
     • Could not deduce: RegexContains
                           "[0-9]|[1-9][0-9]|[1-9][0-9][0-9]|[1-4][0-9][0-9][0-9]|5000"
                           "[7-9][2-9][0-9][0-9]|10000"
         arising from a use of ‘fallback’
     • In the expression:
         fallback
           key1
           (checkvalidation
              (Key :: Regex "[7-9][2-9][0-9][0-9]|10000") (Key :: Regex ".*"))
       In an equation for ‘key2’:
           key2
             = fallback
                 key1
                 (checkvalidation
                    (Key :: Regex "[7-9][2-9][0-9][0-9]|10000") (Key :: Regex ".*"))
```

It is still fairly easy to map the error message back to the configuration
specification. The above error message shows that there was an error when type checking the
specification of `key2` as indicated by the line "In an equation for ...". The line
"In the expression: ..." then shows the exact part where its happen. The incompatibility arises
during checking the fallback from `key2` to `key1`. The regexes describing the contents of the
two keys are not compatible with each other. The first key contains numbers between 0 and 5000,
while the secondkey is restricted to numbers between 7200 and 10000.

## Debugging

This test specification has no errors by default and will thus report nothing,
but if you alter it you can experiment with the type checker. If Elektra is compiled
with the ENABLE_LOGGER flag, it will log the inferred types in all cases so the
type behavior can be observed when getting/setting a key in a specification.

## Dependencies

- ghc >= 8.0.1 and <= 8.2
- ghc-pkg, usually bundled with ghc
- cabal, the haskell build system, usually bundled with ghc
- augeas, which provides libfa utilized by this plugin

Furthermore the following haskell dependencies need to be installed to the sandbox
as explained in the [bindings readme](/src/bindings/haskell/README.md):

```
cabal install 'base >=4.9 && <4.12' 'containers >=0.5 && <0.6' \
	'directory >=1.2 && <1.4' 'process >=1.4 && <1.7' 'binary >=0.8 && <0.9' \
	'haskell-src-exts-any' 'pretty -any' 'hint >=0.7.0 && <0.8.0' 'temporary -any' \
	'exceptions -any' 'text -any' 'simple-logger -any' 'megaparsec -any' \
	'hspec -any' 'QuickCheck-any' --avoid-reinstalls
```

## Limitations

- Type checking only happens when getting or setting a key in a mounted specification.
  This is also the case if cascading keys are used.
- Errors are currently raw and haskell-focused.
