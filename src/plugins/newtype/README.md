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
- `boolean` only allows the values `1` and `0`. See also [Normalization](#normalization).
- To use `wchar` and `wstring` the function `mbstowcs(3)` must be able convert the key value into a wide character string. `wstring`s can
  be of any non-zero length, `wchar` must have exactly length 1.

## Enums

If a key is set to the type `enum` the plugin will look for the metadata array `check/enum/#`.

For example:

```
check/enum = #3
check/enum/#0 = small
check/enum/#1 = middle
check/enum/#2 = large
check/enum/#3 = huge
```

Only the values listed in this array will be accepted. The array indices don't have to be continuous, using e.g. only `#1`, `#2` and
`#4` is also allowed. Just make sure `check/enum` is set to the largest index in the array.

Furthermore `check/enum/delimiter` may contain a separator character, that separates multiple allowed occurrences.
If `check/enum/delimiter` contain more than a single character validation will fail.

For example:

```
check/enum/delimiter = _
```

Then the value `middle_small` would validate. `middle_small_small` would be allowed as well, because multi-values are treated like bitfields.

## Normalization

Some types support normalization in addition to the standard type checking. This means that an extended set of allowed values is normalized
into a different (in most cases standardized) set before type checking. The normalized values will be passed on from `kdbGet` to the rest of
Elektra and your application. During `kdbSet` changed values are also normalized before type checking and at the end of `kdbSet`, if
everything was successful, the values are restored to the ones originally provided by the user (no matter if they were changed before
`kdbSet` or already present in `kdbGet`).

Note: If normalization is used often times, you will get a normalization error instead of a type checking error.

### Booleans

The values that are accepted as `boolean`s are configured during mounting:

```
sudo kdb mount typetest.dump user/tests/newtype dump newtype booleans=#1 booleans/#0/true=a booleans/#0/false=b booleans/#0/true=t booleans/#1/false=f
```

The above line defines that the array of allowed boolean pairs. `booleans=#1` defines the last element of the array as `#1`. For each
element `#` the keys `boolean/#/true` and `boolean/#/false` define the true and false value respectively. True values are normalized to `1`,
false values to `0`.

Even though we didn't specify them the values `1` and `0` are still accepted. The normalized values are always okay to use. If no
configuration is given, the allowed values default to `1`, `yes`, `on`, `true`, `enabled` and `enable` as true values and `0`, `no`, `off`,
`false`, `disabled` and `disable` as false values.

The accepted values can also be overridden on a per-key-basis. Simply add the metakey `check/boolean/true` and `check/boolean/false` to set
the accepted true and false values. Only a single true/false value can be chosen. This is intended for use cases, where normally you prefer
to use only e.g. `1`, `0` and `true`, `false`, but what to override that for a key where e.g. `enabled` and `disabled` make more sense
contextually (e.g. for something like `/log/debug`). Because of this intention restoring also works differently, when `check/boolean/true`
and `check/boolean/false` are used. In this case we will always restore to the chosen override values.

Note: The values `1` and `0` are accepted, even if overrides are used. This means you can set a key with overrides to `0` (or `1`) and during
`kdbSet` it will be restored to the false (or true) override value. (This is useful for the high-level API.)

It is an error to specify only one of `boolean/#/true` and `boolean/#/false` or `check/boolean/true` and `check/boolean/false`.
_Boolean always come in pairs!_

### Enums

Enums also support normalization. Contrary to boolean normalization, enum normalization is always configured on a per-key-basis.

Simply set the metakey `check/enum/normalize` to `1` in order to normalize the string values to there indexes. Any other value is ignored.

Take for example a key with the following enum configuration:

```
check/enum = #3
check/enum/#0 = small
check/enum/#1 = medium
check/enum/#3 = huge
```

The value `small` will be normalized to `0`, `medium` to `1` and `huge` to `3`. During restore the values `0`, `1` and `3` will be restored
to `small`, `medium` and `huge`.

If you use normalization, you can pass string values or indices to `kdbGet` and `kdbSet`, but you will always get back indices from `kdbGet`
and string values from `kdbSet`. (Therefore you can seamlessly use `elektraGetUnsignedLongLong` from high-level API for normalized enums.)

The plugin also supports normalizing enums that use `check/enum/delimiter`, however be careful which indexes you use in this case. The indexes
of all values are simple bitwise or-ed (using `|`). In the above example `small_medium` would be normalized to `1` (`0 | 1 == 1`), the same
value as `medium`. This means during restore the value emitted will be `medium`.

A version that would work with delimiter and normalization is:

```
check/enum = #4
check/enum/#0 = none
check/enum/#1 = small
check/enum/#2 = medium
check/enum/#4 = huge
```

Here `small_medium` is normalized to `3`, which is a unique value.
During restore with delimiters the values might not be restored to there original form, but may be restored to an equivalent representation.
e.g. `small_none` may be restored to just `small` or `small_medium` may be restored to `medium_small`
This has technical reasons and we do not guarantee any restriction on what representation is produced during restore, other than the
normalized value being the same as for the user provided representation.

**_IMPORTANT:_** Do **not** use normalization together with enums, whose string values start with digits (e.g. `check/enum/#0 = 1abc`). This
breaks normalization! Indices are differentiated from string value by whether they start with a digit.

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
