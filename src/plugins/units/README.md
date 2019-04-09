- infos = Information about the units plugin is in keys below
- infos/author = Josef Wechselauer <e1326671@student.tuwien.ac.at>
- infos/licence = BSD
- infos/needs =
- infos/provides = check, transformation
- infos/recommends =
- infos/placements = presetstorage 
- infos/status = unfinished
- infos/metadata =
- infos/description = validates and transforms values entered as units

## Introduction

This plugin provides the capability to enter values with additional SI units. The unit will be checked if it as a valid SI base-unit,  additional SI prefixes will be validated and the value re-calculated to it's base-unit in respect to the prefix. 

E.g. If the value **1 km** is entered, it will be checked if **m** is a valid SI unit and **k** is a valid SI prefix. If both are valid, the prefix will be used to calculate the correct value with the base unit, which would be 1000 m in this example. The value is stored without the SI unit (in this case without the **m**). 

This plugin is written in C.

## Usage

## Dependencies

## Examples

## Limitations

