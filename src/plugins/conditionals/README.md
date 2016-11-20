- infos = Information about the conditionals plugin is in keys below
- infos/author = Thomas Waser <thomas.waser@libelektra.org>
- infos/licence = BSD
- infos/provides =
- infos/needs =
- infos/recommends =
- infos/placements = postgetstorage presetstorage
- infos/status = maintained unittest nodep libc global preview
- infos/metadata = check/condition assign/condition condition/validsuffix
- infos/description = ensures key values through conditions

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

    (IF-condition) ? ('ThenValue') : ('ElseValue')

Depending on if the condition is met, either 'ThenValue' or 'ElseValue' will be assigned as key value if the metakey `assign/condition` is used.

### Experimental: Nested Conditions ###

Multiple conditions can be nested and combined using parentheses and `&&` (logical AND) or `||` (logical OR). Additional parentheses must be used to form valid conditions again. `(` `(condition 1) && (condition 2)` `)`

### Valid Suffix ###

The `condition/validsuffix` can be used to define a list of valid suffixes to numeric values. If two operants have the same valid suffix or one of them no suffix they will be treated by their numeric value ignoring their suffix.
`condition/validsuffix = 'm', 'cm', 'km'` would treat `2.3m` just as the numeric value `2.3` when comparing to another value having the same or no suffix.

### Keynames ###

Keynames are all either relative to to-be-tested key (starting with `./` or `../`), relative to the parentkey (starting with `@/`) or absolute (e.g. `system/key`).

## Example ##

    (this/key  != 'value') ? (then/key == some/other/key) : (or/key <= '125')

Meaning: IF `this/key` NOT EQUAL TO `'value'` THEN `then/key` MUST EQUAL `some/other/key` ELSE `or/key` MUST BE LESS THAN `125`


Another full example:
```sh
#Backup-and-Restore:/tmount/conditionals
sudo kdb mount conditionals.dump /tmount/conditionals conditionals dump
kdb set user/tmount/conditionals/fkey 3.0
kdb set user/tmount/conditionals/hkey hello
# will succeed
kdb setmeta user/tmount/conditionals/key check/condition "(../hkey == 'hello') ? (../fkey == '3.0')"
# will fail 
kdb setmeta user/tmount/conditionals/key check/condition "(../hkey == 'hello') ? (../fkey == '5.0')"
# RET:5
# ERRORS:135
#
# cleanup
#
kdb rm -r /tmount/conditionals
sudo kdb umount /tmount/conditionals
```
Assignment example:
```sh
#Backup-and-Restore:/tmount/conditionals
sudo kdb mount conditionals.dump /tmount/conditionals conditionals dump
kdb set user/tmount/conditionals/hkey Hello
kdb setmeta user/tmount/conditionals/hkey assign/condition "(./ == 'Hello') ? ('World')"
# alternative syntax: "(../hkey == 'Hello') ? ('World')
kdb get user/tmount/conditionals/hkey
World
#
# cleanup
#
kdb rm -r /tmount/conditionals
sudo kdb umount /tmount/conditionals
```
Global plugin example:
```sh
#Backup-and-Restore:/tmount/conditionals
sudo kdb mount main.ini /tmount/conditionals conditionals ni
sudo kdb mount sub.ini /tmount/conditionals/sub conditionals ini
#
# mount conditionals as global plugin
#
sudo kdb global-mount conditionals
#
# create testfiles
#
$ echo "key1 = val1" > `kdb file /tmount/conditionals`
$ echo "[key1]" >> `kdb file /tmount/conditionals`
$ echo "check/condition = (./ == 'val1') ? (../sub/key == 'true')" >> `kdb file /tmount/conditionals`
$ echo "key = false" > `kdb file /tmount/conditionals/sub`
#
# should fail and yield an error
#
kdb export /tmount/conditionals ini
key1 = val1
sub/key = false
# RET:5
# ERRORS:135
# Error (#135) occurred!
# Description: Validation failed
# Ingroup: plugin
# Module: conditionals
# At: /home/thomas/Dev/Elektra/libelektra/src/plugins/conditionals/conditionals.c:696
# Reason: Validation of Key key1: (./ == 'val1') ? (../sub/key == 'true') failed. ((../sub/key == 'true') failed)
# Mountpoint: system/test
# Configfile: /home/thomas/.config/main.ini
kdb set /tmount/conditionals/sub/key true
#
# should succeed 
#
kdb export /tmount/conditionals ini
key1 = val1
sub/key = true
#
# cleanup
#
kdb rm -r /tmount/conditionals
sudo kdb umount /tmount/conditionals/sub
sudo kdb umount /tmount/conditionals
```
