- infos = Information about type plugin is in keys below
- infos/author = Markus Raab <elektra@libelektra.org>
- infos/licence = BSD
- infos/provides = check
- infos/needs =
- infos/placements = presetstorage
- infos/status = maintained nodep memleak unfinished old
- infos/metadata = check/type check/type/min check/type/max
- infos/description = type checker using COBRA data types

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

## Example

```sh
sudo kdb mount typetest.dump user/typetest dump type
kdb set user/typetest/key a
kdb setmeta user/typetest/key check/type char
# RET:0

kdb get user/typetest/key
#> a

kdb rm user/typetest/key
kdb umount user/typetest
```


## Limitations

`wchar` is missing.

Enum and records are part of other plugins.

The `CORBA` type system also has its limits. The types `string` and
`enum`  can be unsatisfactory. While string is too general
and makes no limit on how the sequence of characters is structured,
the enumeration is too finite. For example, it is not possible to say
that a string is not allowed to have a specific symbol in it.
Combine this plugin with other type checker plugins to circumvent
such limitations.

