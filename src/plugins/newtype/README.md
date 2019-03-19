- infos = Information about type plugin is in keys below
- infos/author = Klemens Böswirth <k.boeswirth+git@gmail.com>
- infos/licence = BSD
- infos/provides = check
- infos/needs =
- infos/placements = postgetstorage presetstorage
- infos/status = maintained nodep libc tested unittest recommended
- infos/metadata = check/type type check/enum check/enum/# check/enum/delimiter
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

## Enums

If a key is set to the type `enum` the plugin will look for the metadata array `check/enum/#`.

For example:

    check/enum = #3
    check/enum/#0 = small
    check/enum/#1 = middle
    check/enum/#2 = large
    check/enum/#3 = huge

Only the values listed in this array will be accepted. The array indices don't have to be continuous, using e.g. only `#1`, `#2` and
`#4` is also allowed. Just make sure `check/enum` is set to the largest index in the array.

Furthermore `check/enum/delimiter` may contain a separator character, that separates multiple allowed occurrences.
If `check/enum/delimiter` contain more than a single character validation will fail.

For example:

    check/enum/delimiter = _

Then the value `middle_small` would validate. `middle_small_small` would be allowed as well, because multi-values are treated like bitfields.

### Enum Conversion

If a key is set to type `enum` and additionally the metadata `check/enum/convert` is set to `1`, the plugin will convert the string value
into its index on kdbGet and back again on kdbSet.

For example, using the array from above, a key with value `large` will be converted into the value `2`.

## Example

```sh
#Mount the plugin
sudo kdb mount typetest.dump user/tests/newtype dump newtype

#Store a character value
kdb set user/tests/newtype/key a

#Only allow character values
kdb setmeta user/tests/newtype/key type char
kdb get user/tests/newtype/key
#> a

#If we store another character everything works fine
kdb set user/tests/newtype/key b
kdb get user/tests/newtype/key
#> b

#If we try to store a string Elektra will not change the value
kdb set user/tests/newtype/key 'Not a char'
# RET:5
# ERROR:52
kdb get user/tests/newtype/key
#> b

#Undo modifications to the database
kdb rm user/tests/newtype/key
sudo kdb umount user/tests/newtype
```

For enums:

```sh
# Backup-and-Restore:/tests/enum

sudo kdb mount typeenum.ecf user/tests/newtype dump newtype

# valid initial value + setup valid enum list
kdb set user/tests/newtype/value middle
kdb setmeta user/tests/newtype/value check/enum '#2'
kdb setmeta user/tests/newtype/value 'check/enum/#0' 'low'
kdb setmeta user/tests/newtype/value 'check/enum/#1' 'middle'
kdb setmeta user/tests/newtype/value 'check/enum/#2' 'high'
kdb setmeta user/tests/newtype/value type enum

# should succeed
kdb set user/tests/newtype/value low

# should fail with error 52
kdb set user/tests/newtype/value no
# RET:5
# ERROR:52
```

Or with multi-enums:

```sh
# valid initial value + setup array with valid enums
kdb set user/tests/newtype/multivalue middle_small
kdb setmeta user/tests/newtype/multivalue check/enum/#0 small
kdb setmeta user/tests/newtype/multivalue check/enum/#1 middle
kdb setmeta user/tests/newtype/multivalue check/enum/#2 large
kdb setmeta user/tests/newtype/multivalue check/enum/#3 huge
kdb setmeta user/tests/newtype/multivalue check/enum/delimiter _
kdb setmeta user/tests/newtype/multivalue check/enum "#3"
kdb setmeta user/tests/newtype/multivalue type enum

# should succeed
kdb set user/tests/newtype/multivalue small_middle

# should fail with error 52
kdb set user/tests/newtype/multivalue all_small
# RET:5
# ERROR:52

# cleanup
kdb rm -r user/tests/newtype
sudo kdb umount user/tests/newtype
```

For booleans:

```sh
# Mount plugin
sudo kdb mount config.ecf user/tests/newtype dump newtype

# By default the plugin uses `1` (true) and `0` (false) to represent boolean values
kdb set user/tests/newtype/truthiness false
kdb setmeta user/tests/newtype/truthiness type boolean
kdb get user/tests/newtype/truthiness
#> 0

# The plugin does not change ordinary values
kdb set user/tests/newtype/key value
kdb get user/tests/newtype/key
#> value

# Undo changes
kdb rm -r user/tests/newtype
sudo kdb umount user/tests/newtype
```

```sh
sudo kdb mount config.ecf user/tests/newtype dump newtype
kdb set user/tests/newtype/truthiness 0
kdb setmeta user/tests/newtype/truthiness type boolean
 kdb set user/tests/newtype/truthiness yes
# RET: 0
 # Undo changes
kdb rm -r user/tests/newtype
sudo kdb umount user/tests/newtype
```

## Limitations

Records are part of other plugins.

The `CORBA` type system also has its limits. The types `string` and
`enum` can be unsatisfactory. While string is too general
and makes no limit on how the sequence of characters is structured,
the enumeration is too finite. For example, it is not possible to say
that a string is not allowed to have a specific symbol in it.
Combine this plugin with other type checker plugins to circumvent
such limitations.
