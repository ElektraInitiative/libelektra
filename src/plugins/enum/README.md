- infos = Information about the enum plugin is in keys below
- infos/author = Thomas Waser <thomas.waser@libelektra.org>
- infos/licence = BSD
- infos/provides = check
- infos/needs =
- infos/recommends =
- infos/placements = presetstorage postgetstorage
- infos/status = unittest tested nodep libc obsolete
- infos/metadata = check/enum check/enum/# check/enum/multi
- infos/description = validates values against enum

**This plugin is obsolete:** Please use the `type` plugin instead.

## Introduction

The enum plugin checks string values of Keys by comparing it against a list of valid values.

## Usage

The plugin checks every Key in the Keyset for the Metakey `check/enum` containing a list
with the syntax `'string1', 'string2', 'string3', ..., 'stringN'` and compares each
value with the string value of the Key. If no match is found an error is returned.

Alternatively, if `check/enum` starts with `#`, a meta array `check/enum` is used.
For example:

```
check/enum = #3
check/enum/#0 = small
check/enum/#1 = middle
check/enum/#2 = large
check/enum/#3 = huge
```

Furthermore `check/enum/multi` may contain a separator character, that separates
multiple allowed occurrences.
For example:

```
check/enum/multi = _
```

Then the value `middle_small` would validate.
But `middle_small_small` would fail because every entry might only occur once.

## Example

```sh
# Backup-and-Restore:/tests/enum

sudo kdb mount enum.ecf /tests/enum enum dump

# valid initial value + setup valid enum list
kdb set /tests/enum/value middle
kdb setmeta /tests/enum/value check/enum "'low', 'middle', 'high'"

# should succeed
kdb set /tests/enum/value low

# should fail with error 04200
kdb set /tests/enum/value no
# RET:5
# ERROR:C03200
```

Or with multi-enums:

```sh
# valid initial value + setup array with valid enums
kdb set /tests/enum/multivalue middle_small
kdb setmeta /tests/enum/multivalue check/enum/#0 small
kdb setmeta /tests/enum/multivalue check/enum/#1 middle
kdb setmeta /tests/enum/multivalue check/enum/#2 large
kdb setmeta /tests/enum/multivalue check/enum/#3 huge
kdb setmeta /tests/enum/multivalue check/enum/multi _
kdb setmeta /tests/enum/multivalue check/enum "#3"

# should succeed
kdb set /tests/enum/multivalue ___small_middle__

# should fail with error 04200
kdb set /tests/enum/multivalue ___all_small__
# RET:5
# ERROR:C03200

# cleanup
kdb rm -r /tests/enum
sudo kdb umount /tests/enum
```

## Limitations

You cannot give enum values specific values.
If you only want to check for numerical values,
the plugin [range](../range) is better suited.
