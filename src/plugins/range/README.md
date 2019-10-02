- infos = Information about the range plugin is in keys below
- infos/author = Thomas Waser <thomas.waser@libelektra.org>
- infos/licence = BSD
- infos/needs =
- infos/provides =
- infos/recommends =
- infos/placements = presetstorage postgetstorage
- infos/status = maintained conformant compatible coverage specific unittest tested libc preview unfinished
- infos/metadata = check/range check/type
- infos/description = tests if a value is within a given range

## Introduction

The range plugin checks if a `Key`'s value is within a given range.

## Usage

The plugin checks every `Key` in the `KeySet` for the metakey `check/range` which contains either a single range with the syntax `[-]min-[-]max`, or a list of ranges or values separated by `,` and tests if the `Key`'s value is within the range(s).

`check/type` can be used to specify the data type. If not specified otherwise the default value is `long long`

Possible values:

- `short`, `long`, `long long`

  for signed integer values

- `unsigned short`, `unsigned long`, `unsigned long long`

  for unsigned integer values

- `float`, `double`, `long double`

  for floating point values

- `HEX`

  for hexadecimal values

- `char`

  for characters

## Dependencies

None.

## Examples

```sh
# Backup-and-Restore:/tests/range

sudo kdb mount range.ecf /tests/range range dump

# should succeed
kdb set /tests/range/value 5
kdb meta-set /tests/range/value check/range "1-10"
# RET: 0

# should fail
kdb set /tests/range/value 11
# RET:5

# should also fail
kdb set /tests/range/value "\-1"
# RET:5

# we can also allow only individual values:
kdb meta-set /tests/range/value check/range "1,2,4,8"

kdb set /tests/range/value 7
# RET:5

kdb set /tests/range/value 2
# RET:0

kdb rm -r /tests/range
sudo kdb umount /tests/range
```

## Limitations

None.
