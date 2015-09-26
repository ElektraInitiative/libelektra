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

`(IF Condition) ? (THEN Condition) : (ELSE Condition)` where the ELSE-condition is optional

Condition:  `Key` *Operation* `('String' | '1234.56' | Key | '')`

Operations: `!=, ==, <, <=, =>, >, :=`
	`:=` is used to set a keyvalue


## Example ##

`(this/key  != 'value') ? (then/key == some/other/key) : (or/key <= '125')` 

Meaning: IF `this/key` NOT EQUAL TO `'value'` THEN `then/key` MUST EQUAL `some/other/key` ELSE `or/key` MUST BE LESS THAN `125`


