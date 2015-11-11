- infos = Information about the mathcheck plugin is in keys below
- infos/author = Thomas Waser <thomas.waser@libelektra.org>
- infos/licence = BSD
- infos/needs =
- infos/provides = check
- infos/placements = presetstorage
- infos/description =

## Introduction ##

Compares a key value to a mathematical expression using polish prefix notation defined in the `check/math` metakey. 
Operations are `+ - / *` . Operants are keys with names relative to the parent key.
How the values are compared is specified at the beginning of the metakey using the conditions `<, <=, ==, !=, =>, >, :=`
`:=` is used to set key values.
All values are interpreted as `double` floating point values.

## Examples ##

`check/math = "== + testval1 + testval2 testval3"` compares the keyvalue to the sum of testval1-3 and yields an error if the values are not equal.
`check/math = "<= - testval1 * testval2 testval3"` tests if the keyvalue is less than or equal to testval1 - (testval2 * testval3) and yields an error if not.

Full example:

	kdb mount mathcheck.dump /tmount/mathcheck dump mathcheck
	kdb set user/tmount/mathcheck/a 3.1
	kdb set user/tmount/mathcheck/b 4.5
	kdb set user/tmount/mathcheck/k 7.6
	kdb setmeta user/tmount/mathcheck/k check/math "== + a b"
	kdb set user/tmount/mathcheck/k 7.7 # fails

	kdb setmeta user/tmount/mathcheck/k check/math ":= + a b"
	kdb set user/tmount/mathcheck/a 5.5
	kdb get user/tmount/mathcheck/k
