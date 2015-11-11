- infos = Information about the conditionals plugin is in keys below
- infos/author = Thomas Waser <thomas.waser@libelektra.org>
- infos/licence = BSD
- infos/needs =
- infos/provides =
- infos/placements = presetstorage
- infos/description =

## Introduction ##

This plugin uses if-then-else like conditions stored in the metakey `check/condition` to validate data.

## Syntax ##

`(IF-condition) ? (THEN-condition) : (ELSE-condition)` where the ELSE-condition is optional

Condition:  `Key` *Operation* `('String' | '1234.56' | Key | '')`

Operations: `!=, ==, <, <=, =>, >, :=`, where:

- `:=` is used to set a key value
- others are for comparison as in C


## Example ##

`(this/key  != 'value') ? (then/key == some/other/key) : (or/key <= '125')` 

Meaning: IF `this/key` NOT EQUAL TO `'value'` THEN `then/key` MUST EQUAL `some/other/key` ELSE `or/key` MUST BE LESS THAN `125`


Another full example:

    kdb mount conditionals.dump /tmount/conditionals conditionals dump
    kdb set user/tmount/conditionals/fkey 3.0
    kdb set user/tmount/conditionals/hkey hello
    kdb setmeta user/tmount/conditionals/key check/condition "(hkey == 'hello') ? (fkey == '3.0')" # success
    kdb setmeta user/tmount/conditionals/key check/condition "(hkey == 'hello') ? (fkey == '5.0')" # fail
