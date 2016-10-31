- infos = Information about the mathcheck plugin is in keys below
- infos/author = Thomas Waser <thomas.waser@libelektra.org>
- infos/licence = BSD
- infos/provides = check
- infos/needs =
- infos/placements = presetstorage
- infos/status = maintained unittest discouraged nodep
- infos/metadata = check/math
- infos/description = validates a set of keys through a mathematical expression

## Introduction ##

Compares a key value to a mathematical expression using polish prefix notation defined in the `check/math` metakey. 
Operations are `+ - / *` . Operants are keys with names relative to the parent key.
How the values are compared is specified at the beginning of the metakey using the conditions `<, <=, ==, !=, =>, >, :=`
`:=` is used to set key values.
All values are interpreted as `double` floating point values.

### Keynames ###

Keynames are all either relative to to-be-tested key (starting with `./` or `../`), relative to the parentkey (starting with `@/`) or absolute (e.g. `system/key`).

## Examples ##

`check/math = "== + ../testval1 + ../testval2 ../testval3"` compares the keyvalue to the sum of testval1-3 and yields an error if the values are not equal.
`check/math = "<= - @/testval1 * @/testval2 @/testval3"` tests if the keyvalue is less than or equal to testval1 - (testval2 * testval3) and yields an error if not.

Full example:

    kdb mount mathcheck.dump /example/mathcheck dump mathcheck
    kdb set user/example/mathcheck/a 3.1
    kdb set user/example/mathcheck/b 4.5
    kdb set user/example/mathcheck/k 7.6
    kdb setmeta user/example/mathcheck/k check/math "== + ../a ../b"
    kdb set user/example/mathcheck/k 7.7   # fails

To calculate values on-demand you can use:

    kdb setmeta user/example/mathcheck/k check/math ":= + @/a @/b"
    kdb get user/example/mathcheck/k       # 12.5
    kdb set user/example/mathcheck/a 5.5
    kdb get user/example/mathcheck/k       # 10

It also works with constants:

    kdb setmeta user/example/mathcheck/k check/math ":= + ../a '5'"
    kdb get user/example/mathcheck/k       # 10.5
    kdb set user/example/mathcheck/a 8.0
    kdb get user/example/mathcheck/k       # 13

## Status ##

`mathcheck` does not work with icc 14.0.2 20140120, and some gcc versions when using debug.
Additionally, it fails to run on OpenBSD.
Until these problems are fixed it is discouraged to use.

