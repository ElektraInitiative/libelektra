- infos = Information about the units plugin is in keys below
- infos/author = Josef Wechselauer <e1326671@student.tuwien.ac.at>
- infos/licence = BSD
- infos/needs =
- infos/provides = check/transformation
- infos/recommends =
- infos/placements = presetstorage 
- infos/status = unfinished
- infos/metadata = check/units
- infos/description = validates and transforms values entered as units

## Introduction

This plugin provides the capability to enter values with additional SI units. The unit will be checked if it as a valid SI base-unit,  additional SI prefixes will be validated and the value re-calculated to it's base-unit in respect to the prefix. 

E.g. If the value **1 km** is entered, it will be checked if **m** is a valid SI unit and **k** is a valid SI prefix. If both are valid, the prefix will be used to calculate the correct value with the base unit, which would be 1000 m in this example. The value is stored without the SI unit (in this case without the **m**). 

This plugin is written in C.

## Usage

## Dependencies

None.

## Examples

```sh
sudo kdb mount /tests/units.ini /tests/units ini units
#> Mount a config file with the units plugin

sudo kdb setmeta /tests/units check/units any
#> Check the /tests/units key for validity

kdb set /tests/units "1m"
#> Suceeds, since the value is a valid decimal with SI-unit representation.

kdb set /tests/units "35453.327468 µH"
#> Suceeds, since the value is a valid decimal with SI-unit representation.

kdb set /tests/units "32"
#> Throws an error: value of key is not a representation for a value with a SI-unit

kdb set /tests/units "12 n"
#> Throws an error: value of key is not a representation for a value with a SI-unit
#> This could be changed in a further step (see issue #1398) to be valid, as for example the user wants to enter 1k to represent 1000 without an SI-Unit. 
#> For now, this is ignored as the entered units should be checked for correctness

kdb set /tests/units 432 km
#> Throws an error: quotes are expected.
```


## Limitations

None.