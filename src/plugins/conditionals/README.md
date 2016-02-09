- infos = Information about the conditionals plugin is in keys below
- infos/author = Thomas Waser <thomas.waser@libelektra.org>
- infos/licence = BSD
- infos/provides =
- infos/needs =
- infos/recommends =
- infos/placements = presetstorage
- infos/status = maintained unittest nodep libc preview
- infos/metadata = check/condition assign/condition
- infos/description =

## Introduction ##

This plugin uses if-then-else like conditions.

## Check Syntax ##

Stored in the metakey `check/condition` to validate data is:

`(IF-condition) ? (THEN-condition) : (ELSE-condition)` where the ELSE-condition is optional

Condition:  `Key` *Operation* `('String' | '1234.56' | Key | '')`

Operations: `!=, ==, <, <=, =>, >, :=`, where:

- `:=` is used to set a key value
- others are for comparison as in C


### Assign Syntax ###

`(IF-condition) ? ('ThenValue') : ('ElseValue')`

Depending on if the condition is met, either 'ThenValue' or 'ElseValue' will be assigned as key value if the metakey `assign/condition` is used.



## Example ##

`(this/key  != 'value') ? (then/key == some/other/key) : (or/key <= '125')` 

Meaning: IF `this/key` NOT EQUAL TO `'value'` THEN `then/key` MUST EQUAL `some/other/key` ELSE `or/key` MUST BE LESS THAN `125`


Another full example:

	kdb mount conditionals.dump /tmount/conditionals conditionals dump
	kdb set user/tmount/conditionals/fkey 3.0
	kdb set user/tmount/conditionals/hkey hello
	kdb setmeta user/tmount/conditionals/key check/condition "(hkey == 'hello') ? (fkey == '3.0')" # success
	kdb setmeta user/tmount/conditionals/key check/condition "(hkey == 'hello') ? (fkey == '5.0')" # fail

Assignment example:

	kdb mount conditionals.dump /tmount/conditionals conditionals dump
	kdb set user/tmount/conditionals/hkey Hello
	kdb setmeta user/tmount/conditionals/hkey assign "(hkey == 'Hello') ? ('World')"
	
	kdb get user/tmount/conditionals/hkey 
	>World 	

