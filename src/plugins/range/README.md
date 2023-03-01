- infos = Information about the range plugin is in keys below
- infos/author = Thomas Waser <thomas.waser@libelektra.org>
- infos/licence = BSD
- infos/needs =
- infos/provides =
- infos/recommends =
- infos/placements = presetstorage postgetstorage
- infos/status = maintained conformant compatible coverage specific unittest tested libc preview unfinished
- infos/metadata = check/range check/type type
- infos/description = tests if a value is within a given range

## Introduction

The range plugin checks if a `Key`'s value is within a given range.

## Installation

See [installation](/doc/INSTALL.md).
The package is called `libelektra5-experimental`.

## Usage

- The plugin checks every `Key` in the `KeySet` having a metakey `check/range`.
- For these keys, it checks whether the key value is within the specified range.
- `check/range` can contain either:
  1. a single range with the syntax `[-]min-[-]max`
  2. or a list of ranges or values separated by `,`
- Metakey `check/type` can be used to specify the data type. If not specified, metakey `type` will be used as fallback. If both are unspecified or unsupported, the type is assumed to be `long long`.

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
kdb set user:/tests/range/value 5
kdb meta-set spec:/tests/range/value check/range "1-10"
# RET: 0

# should fail
kdb set user:/tests/range/value 11
# RET:5

# should also fail
kdb set user:/tests/range/value "\-1"
# RET:5

# we can also allow only individual values: (using the --force flag, as the current value of 5 would not be allowed under the new policy)
kdb meta-set -f spec:/tests/range/value check/range "1,2,4,8"

kdb set user:/tests/range/value 7
# RET:5

kdb set user:/tests/range/value 2
# RET:0

kdb rm -r user:/tests/range
kdb rm -r spec:/tests/range
sudo kdb umount /tests/range
```

## Limitations

None.
