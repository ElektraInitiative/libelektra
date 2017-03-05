- infos = Information about the range plugin is in keys below
- infos/author = Thomas Waser <thomas.waser@libelektra.org>
- infos/licence = BSD
- infos/needs =
- infos/provides =
- infos/recommends =
- infos/placements = presetstorage postgetstorage
- infos/status = maintained conformant compatible coverage specific unittest tested libc preview experimental unfinished nodoc
- infos/metadata = check/range check/range/type
- infos/description = tests if a value is within a given range

## Introduction ##

The range plugin checks if a Key value is within a given range.

## Usage ##

The plugin checks every Key in the Keyset for the Metakey `check/range` which contains either a single range with the syntax `[-]min-[-]max`, or a list of ranges separated by `,` and tests if the Key value is within the range(s).

`check/type` or `type` can be used to specify the datatype. If not specified otherwise the default value is `INT`

Possible values:

* `short`, `long`, `long long`

   for signed integer values

* `unsigned short`, `unsigned long`, `unsigned long long`

   for unsigned integer values

* `float`, `double`, `long double`

   for floating point values

* `HEX`

   for hexadecimal values

* `char`

   for characters

 
## Dependencies ##

None.

## Examples ##

```sh
# Backup-and-Restore:/examples/range

sudo kdb mount range.ecf /examples/range range dump

# should succeed
kdb set /examples/range/value 5
kdb setmeta /examples/range/value check/range "1-10"
# RET: 0

# should fail
kdb set /examples/range/value 11
# RET:5

# should also fail
kdb set /examples/range/value "\-1"
# RET:5

kdb rm -r /examples/range
sudo kdb umount /examples/range
```

## Limitations ##

None.
