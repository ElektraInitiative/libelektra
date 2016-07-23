- infos = Information about the conditionals plugin is in keys below
- infos/author = Thomas Waser <thomas.waser@libelektra.org>
- infos/licence = BSD
- infos/provides =
- infos/needs =
- infos/recommends =
- infos/placements = postgetstorage presetstorage
- infos/status = maintained unittest nodep libc preview global
- infos/metadata = check/condition assign/condition condition/validsuffix
- infos/description =

## Introduction ##

This plugin uses if-then-else like conditions. It also works as global plugin.

## Check Syntax ##

Stored in the metakey `check/condition` to validate data is:

`(IF-condition) ? (THEN-condition) : (ELSE-condition)` where the ELSE-condition is optional

Condition:  `Key` *Operation* `('String' | '1234.56' | Key | '')`

Operations: `!=, ==, <, <=, =>, >, :=`, where:

- `:=` is used to set a key value
- others are for comparison as in C

### Testing if Key exists ###

`(! a/key)` evaluates to true if the key `a/key` doesn't exist, to false if it exists.

### Assign Syntax ###

`(IF-condition) ? ('ThenValue') : ('ElseValue')`

Depending on if the condition is met, either 'ThenValue' or 'ElseValue' will be assigned as key value if the metakey `assign/condition` is used.

### EXPERIMENTAL: Nested Conditions ###

Multiple conditions can be nested and combined using parentheses and `&&` (logical AND) or `||` (logical OR). Additional parentheses must be used to form valid conditions again. `(` `(condition 1) && (condition 2)` `)`

### valid suffix ###

The `condition/validsuffix` can be used to define a list of valid suffixes to numeric values. If two operants have the same valid suffix or one of them no suffix they will be treated by their numeric value ignoring their suffix.
`condition/validsuffix = 'm', 'cm', 'km'` would treat `2.3m` just as the numeric value `2.3` when comparing to another value having the same or no suffix.

### Keynames ###

Keynames are all either relative to to-be-tested key (starting with `./` or `../`), relative to the parentkey (starting with `@/`) or absolute (e.g. `system/key`).

## Example ##

`(this/key  != 'value') ? (then/key == some/other/key) : (or/key <= '125')` 

Meaning: IF `this/key` NOT EQUAL TO `'value'` THEN `then/key` MUST EQUAL `some/other/key` ELSE `or/key` MUST BE LESS THAN `125`


Another full example:

	kdb mount conditionals.dump /tmount/conditionals conditionals dump
	kdb set user/tmount/conditionals/fkey 3.0
	kdb set user/tmount/conditionals/hkey hello
	kdb setmeta user/tmount/conditionals/key check/condition "(../hkey == 'hello') ? (../fkey == '3.0')" # success
	kdb setmeta user/tmount/conditionals/key check/condition "(../hkey == 'hello') ? (../fkey == '5.0')" # fail

Assignment example:

	kdb mount conditionals.dump /tmount/conditionals conditionals dump
	kdb set user/tmount/conditionals/hkey Hello
	kdb setmeta user/tmount/conditionals/hkey assign/condition "(./hkey == 'Hello') ? ('World')"
	kdb get user/tmount/conditionals/hkey # output: World

Global plugin example:

	% cat /tmp/main.ini
	key1 = val1
	[key1]
	check/condition = (./ == 'val1') ? (../sub/key == 'true')

	% cat /tmp/sub.ini
	key = false
 
	% kdb mount /tmp/main.ini system/test ni
	
	% kdb mount /tmp/sub.ini system/test/sub ini

	% kdb export system/test
	key1 = val1
	sub/key = false
	Error (#135) occurred!
	Description: Validation failed
	Ingroup: plugin
	Module: conditionals
	At: /home/thomas/Dev/Elektra/libelektra/src/plugins/conditionals/conditionals.c:696
	Reason: Validation of Key key1: (./ == 'val1') ? (../sub/key == 'true') failed. ((../sub/key == 'true') failed)
	Mountpoint: system/test
	Configfile: /tmp/main.ini

	% kdb set system/test/sub/key true
	Set string to true

	% kdb export system/test
	key1 = val1
	sub/key = true

