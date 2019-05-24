- infos = Information about type plugin is in keys below
- infos/author = Markus Raab <elektra@libelektra.org>
- infos/licence = BSD
- infos/provides = check
- infos/needs =
- infos/placements = presetstorage
- infos/status = nodep memleak unfinished old obsolete
- infos/metadata = check/type type check/type/min check/type/max
- infos/description = type checker using COBRA data types

**This plugin is obsolete:** Please use the `newtype` plugin instead.

## Introduction

This plugin is a type checker plugin using the `CORBA` data types.

A common and successful type system happens to be CORBA.
The system is well suited because of the many well-defined
mappings it provides to other programming languages.

The type checker plugin supports all basic CORBA types:
`short`, `unsigned_short`, `long`, `unsigned_long`, `long_long`,
`unsigned_long_long`, `float`, `double`, `char`, `boolean`, `any` and
`octet`. When checking `any` it will always be successful, regardless
of the content.

The metadata `check/type` can be used to override the `type` metadata.
This can be useful if the type to check differs to the type for code generation
or the highlevel API. In most cases though, `type` will be enough to specify.

## Deprecation

`empty` and `FSType` are deprecated. Please use regular expressions
or enums instead.

Sometimes the type should expresses that, for example, both an empty
or another type is valid. This type checker allowed a space-separated
list of types to expresses that. If any of those types match, the whole
type was valid. For example, the type `string empty` equals the type
`any`. This facility builds a union of the sets of instances existing
types specify. It is now deprecated due to a more general sum type
facility.

`check/type/min` and `check/type/max` are deprecated, please use the range
plugin instead.

## Example

```sh
# Mount the plugin
sudo kdb mount typetest.dump user/tests/cpptype dump cpptype

# Store a character value
kdb set user/tests/cpptype/key a

# Only allow character values
kdb setmeta user/tests/cpptype/key check/type char
kdb get user/tests/cpptype/key
#> a

# If we store another character everything works fine
kdb set user/tests/cpptype/key b
kdb get user/tests/cpptype/key
#> b

# If we try to store a string Elektra will not change the value
kdb set user/tests/cpptype/key 'Not a char'
# STDERR: .*The type char failed to match.*
# ERROR:  C04200
# RET:    5
kdb get user/tests/cpptype/key
#> b

# Undo modifications to the database
kdb rm user/tests/cpptype/key
sudo kdb umount user/tests/cpptype
```

## Limitations

`wchar` is missing.

Enum and records are part of other plugins.

The `CORBA` type system also has its limits. The types `string` and
`enum` can be unsatisfactory. While string is too general
and makes no limit on how the sequence of characters is structured,
the enumeration is too finite. For example, it is not possible to say
that a string is not allowed to have a specific symbol in it.
Combine this plugin with other type checker plugins to circumvent
such limitations.
