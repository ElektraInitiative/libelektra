- infos = Information about type plugin is in keys below
- infos/author = Markus Raab <elektra@libelektra.org>
- infos/licence = BSD
- infos/provides = check
- infos/needs =
- infos/placements = presetstorage
- infos/status = maintained nodep libc unfinished
- infos/metadata = check/type type check/enum check/enum/# check/enum/multi
- infos/description = type checker using COBRA data types

## Introduction

This plugin is a type checker plugin using the `CORBA` data types.

A common and successful type system happens to be CORBA. The system is well suited because of the many well-defined
mappings it provides to other programming languages.

The type checker plugin supports these types:
`short`, `unsigned_short`, `long`, `unsigned_long`, `long_long`, `unsigned_long_long`, `float`, `double`, `char`, `wchar`, `boolean`, 
`any`, `enum`, `string`, `wstring` and `octet`.

- Checking `any` will always be successful, regardless of the content.
- `string` matches any non-empty key value.
- `empty` only matches empty key values.
- `octet` and `char` are equivalent to each other.
- `enum` will do enum checking as described below.
- To use `wchar` and `wstring` the function `mbstowcs(3)` must be able convert the key value into a wide character string. `wstring`s can
be of any non-zero length, `wchar` must have exactly length 1.

## Example

```sh
#Mount the plugin
sudo kdb mount typetest.dump user/tests/type dump type

#Store a character value
kdb set user/tests/type/key a

#Only allow character values
kdb setmeta user/tests/type/key check/type char
kdb get user/tests/type/key
#> a

#If we store another character everything works fine
kdb set user/tests/type/key b
kdb get user/tests/type/key
#> b

#If we try to store a string Elektra will not change the value
kdb set user/tests/type/key 'Not a char'
#STDERR :.*Description : could not type check value of key.*
#ERROR : 52
#RET : 5
kdb get user/tests/type/key
#> b

#Undo modifications to the database
kdb rm user/tests/type/key
sudo kdb umount user/tests/type
```


## Limitations

Records are part of other plugins.

The `CORBA` type system also has its limits. The types `string` and
`enum`  can be unsatisfactory. While string is too general
and makes no limit on how the sequence of characters is structured,
the enumeration is too finite. For example, it is not possible to say
that a string is not allowed to have a specific symbol in it.
Combine this plugin with other type checker plugins to circumvent
such limitations.
