- infos = Information about the mathcheck plugin is in keys below
- infos/author = Thomas Waser <thomas.waser@libelektra.org>
- infos/licence = BSD
- infos/provides = check
- infos/needs =
- infos/placements = presetstorage
- infos/status = maintained unittest nodep discouraged
- infos/metadata = check/math
- infos/description = validates a set of keys through a mathematical expression

## Introduction

Compares a key value to a mathematical expression using polish prefix notation defined in the `check/math` metakey. 
Operations are `+ - / *` . Operants are keys with names relative to the parent key.
How the values are compared is specified at the beginning of the metakey using the conditions `<, <=, ==, !=, =>, >, :=`
`:=` is used to set key values.
All values are interpreted as `double` floating point values.

### Keynames

Keynames are all either relative to to-be-tested key (starting with `./` or `../`), relative to the parentkey (starting with `@/`) or absolute (e.g. `system/key`).

## Examples

`check/math = "== + ../testval1 + ../testval2 ../testval3"` compares the keyvalue to the sum of testval1-3 and yields an error if the values are not equal.
`check/math = "<= - @/testval1 * @/testval2 @/testval3"` tests if the keyvalue is less than or equal to testval1 - (testval2 * testval3) and yields an error if not.

Full example:
```sh
# Backup-and-Restore:/examples/mathcheck

sudo kdb mount mathcheck.dump /examples/mathcheck dump mathcheck

kdb set /examples/mathcheck/a 3.1
kdb set /examples/mathcheck/b 4.5
kdb set /examples/mathcheck/k 7.6
kdb setmeta user/examples/mathcheck/k check/math "== + ../a ../b"

# should fail
kdb set /examples/mathcheck/k 7.7
# RET:5
# ERRORS:123
# Set string to 7.7
# The command set failed while accessing the key database with the info:
# Error (#123) occurred!
# Description: invalid value
# Ingroup: plugin
# Module: mathcheck
# At: /home/thomas/Dev/Elektra/libelektra/src/plugins/mathcheck/mathcheck.c:399
# Reason: 7.7 != 7.6
# Mountpoint: /examples/mathcheck
# Configfile: /home/thomas/.config/mathcheck.dump.25680:1478749409.938013.tmp
```
To calculate values on-demand you can use:
```sh
kdb setmeta user/examples/mathcheck/k check/math ":= + @/a @/b"
kdb set /examples/mathcheck/a 8.0
kdb set /examples/mathcheck/b 4.5

kdb get /examples/mathcheck/k
#> 12.5

kdb set /examples/mathcheck/a 5.5

kdb get /examples/mathcheck/k
#> 10
```
It also works with constants:
```sh
kdb setmeta user/examples/mathcheck/k check/math ":= + ../a '5'"
kdb set /examples/mathcheck/a 5.5

kdb get /examples/mathcheck/k
#> 10.5

kdb set /examples/mathcheck/a 8.0

kdb get /examples/mathcheck/k
#> 13

#cleanup
kdb rm -r /examples/mathcheck
sudo kdb umount /examples/mathcheck
```
