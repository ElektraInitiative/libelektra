- infos = Information about the calculate plugin is in keys below
- infos/author = Thomas Waser <thomas.waser@libelektra.org>
- infos/licence = BSD
- infos/needs =
- infos/provides = check
- infos/placements = presetstorage
- infos/description =

## Introduction ##

Compares a key value to a mathematical expression using polish prefix notation defined in the `check/calc` metakey. 
Operations are `+ - / *` . Operants are keys with names relative to the parent key.
How the values are compared is specified at the begining of the Metakey using the conditions `<, <=, ==, !=, =>, >, :=`
`:=` is used to set key values.
To prevent loss of precision all values are interpreted as floating point values.

## Examples ##

`check/calc = "== + testval1 + testval2 testval3"` compares the keyvalue to the sum of testval1-3 and yields an error if the values are not equal.
`check/calc = "<= - testval1 * testval2 testval3"` tests if the keyvalue is less than or equal to testval1 - (testval2 * testval3) and yields an error if not.
