- infos = Information about the mathcheck plugin is in keys below
- infos/author = Thomas Waser <thomas.waser@libelektra.org>
- infos/licence = BSD
- infos/provides = check
- infos/needs =
- infos/placements = presetstorage
- infos/status = maintained tested/unit tested/shell nodep discouraged
- infos/metadata = check/math
- infos/description = validates a set of keys through a mathematical expression

## Introduction

Compares a key value to a mathematical expression using polish prefix notation defined in the `check/math` metakey.
Operations are `+ - / *` . Operants are keys with names relative to the parent key.
How the values are compared is specified at the beginning of the metakey using the conditions `<, <=, ==, !=, =>, >, :=`
`:=` is used to set key values.
All values are interpreted as `double` floating point values.

## Installation

See [installation](/doc/INSTALL.md).
The package is called `libelektra5-extra`.

### Keynames

Keynames are all either relative to to-be-tested key (starting with `./` or `../`), relative to the parentkey (starting with `@/`) or absolute (e.g. `system:/key`).

## Examples

`check/math = "== + ../testval1 + ../testval2 ../testval3"` compares the keyvalue to the sum of testval1-3 and yields an error if the values are not equal.
`check/math = "<= - @/testval1 * @/testval2 @/testval3"` tests if the keyvalue is less than or equal to `testval1 - (testval2 * testval3)` and yields an error if not.

Full example:

```sh
# Backup-and-Restore:user:/tests/mathcheck

sudo kdb mount mathcheck.dump user:/tests/mathcheck mathcheck

kdb set user:/tests/mathcheck/a 3.1
kdb set user:/tests/mathcheck/b 4.5
kdb set user:/tests/mathcheck/k 7.6
kdb meta-set user:/tests/mathcheck/k check/math "== + ../a ../b"

# should fail
kdb set user:/tests/mathcheck/k 7.7
# RET:5
# ERROR:C03200
# Set string to "7.7"
# Sorry, module mathcheck issued the error C03200:
# invalid value: 7.7 != 7.6
```

To calculate values on-demand you can use:

```sh
kdb meta-set user:/tests/mathcheck/k check/math ":= + @/a @/b"
kdb set user:/tests/mathcheck/a 8.0
kdb set user:/tests/mathcheck/b 4.5

kdb get user:/tests/mathcheck/k
#> 12.5

kdb set user:/tests/mathcheck/a 5.5

kdb get user:/tests/mathcheck/k
#> 10
```

It also works with constants:

```sh
kdb meta-set user:/tests/mathcheck/k check/math ":= + ../a '5'"
kdb set user:/tests/mathcheck/a 5.5

kdb get user:/tests/mathcheck/k
#> 10.5

kdb set user:/tests/mathcheck/a 8.0

kdb get user:/tests/mathcheck/k
#> 13

#cleanup
kdb rm -r user:/tests/mathcheck
sudo kdb umount user:/tests/mathcheck
```
