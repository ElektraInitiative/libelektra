- infos = Information about the conditionals plugin is in keys below
- infos/author = Thomas Waser <thomas.waser@libelektra.org>
- infos/licence = BSD
- infos/provides =
- infos/needs =
- infos/recommends =
- infos/placements = postgetstorage presetstorage
- infos/status = unittest nodep libc global preview
- infos/metadata = check/condition assign/condition condition/validsuffix check/condition/any/# check/condition/all/# check/condition/none/# assign/condition/#
- infos/description = ensures key values through conditions

## Introduction

This plugin uses if-then-else like conditions. It also works as global plugin.

## Installation

See [installation](/doc/INSTALL.md).
The package is called `libelektra5-extra`.

## Check Syntax

Stored in the metakey `check/condition` to validate data is:

`(IF-condition) ? (THEN-condition) : (ELSE-condition)` where the ELSE-condition is optional

Condition: `Key` _Operation_ `('String' | '1234.56' | Key | '')`

Operations: `!=, ==, <, <=, =>, >, :=`, where:

- `:=` is used to set a key value
- others are for comparison as in C

### Testing if Key Exists

`(! a/key)` evaluates to true if the key `a/key` doesn't exist, to false if it exists.

### Assign Syntax

```
(IF-condition) ? ('ThenValue') : ('ElseValue')
```

Depending on if the condition is met, either 'ThenValue' or 'ElseValue' will be assigned as key value if the metakey `assign/condition` is used.

### Experimental: Nested Conditions

Multiple conditions can be nested and combined using parentheses and `&&` (logical AND) or `||` (logical OR). Additional parentheses must be used to form valid conditions again. `(` `(condition 1) && (condition 2)` `)`

### Valid Suffix

The `condition/validsuffix` can be used to define a list of valid suffixes to numeric values. If two operants have the same valid suffix or one of them no suffix they will be treated by their numeric value ignoring their suffix.
`condition/validsuffix = 'm', 'cm', 'km'` would treat `2.3m` just as the numeric value `2.3` when comparing to another value having the same or no suffix.

### Keynames

Keynames are all either relative to to-be-tested key (starting with `./` or `../`), relative to the parentkey (starting with `@/`) or absolute (e.g. `system:/key`).

### Multiple Statements

It's also possible to test multiple conditions using `check/condition/{any,all,none}` as a meta array. Where `any` means that at least one statement has to evaluate to true, `all` that all statements have to evaluate to true, and `none` that no statement is allowed to evaluate to false (default).
For multiple assign statements use `assign/condition` as a meta array. The first `assign/condition/#` statement that evaluates to true will be assigned and the rest ignored.

## Example

```
(this/key  != 'value') ? (then/key == some/other/key) : (or/key <= '125')
```

Meaning: IF `this/key` NOT EQUAL TO `'value'` THEN `then/key` MUST EQUAL `some/other/key` ELSE `or/key` MUST BE LESS THAN `125`

Another full example:

<!-- FIXME [new_backend]: tests disabled -->

```
#Backup-and-Restore:user:/tests/conditionals

sudo kdb mount conditionals.dump user:/tests/conditionals conditionals dump

kdb set user:/tests/conditionals/fkey 3.0
kdb set user:/tests/conditionals/hkey hello

# will succeed
kdb meta set user:/tests/conditionals/key check/condition "(../hkey == 'hello') ? (../fkey == '3.0')"

# will fail
kdb meta set user:/tests/conditionals/key check/condition "(../hkey == 'hello') ? (../fkey == '5.0')"
# RET:5
# ERROR:C03200
```

Assignment example:

<!-- FIXME [new_backend]: tests disabled -->

```
kdb set user:/tests/conditionals/hkey Hello
kdb meta set user:/tests/conditionals/hkey assign/condition "(./ == 'Hello') ? ('World')"
# alternative syntax: "(../hkey == 'Hello') ? ('World')

kdb get user:/tests/conditionals/hkey
#> World

# cleanup
kdb rm -r user:/tests/conditionals
sudo kdb umount user:/tests/conditionals
```

Global plugin example:

<!-- FIXME [new_backend]: tests disabled -->
<!-- FIXME [new_backend]: outdated (global-mount) -->

```
# Backup old list of global plugins
kdb set user:/tests/msr $(mktemp)
kdb export system:/elektra/globalplugins > $(kdb get user:/tests/msr)

sudo kdb mount main.ini /tests/conditionals ni
sudo kdb mount sub.ini /tests/conditionals/sub ni

# mount conditionals as global plugin
sudo kdb global-mount conditionals || $(exit 0)

# create testfiles
echo 'key1=val1'                                               >  `kdb file system:/tests/conditionals`
echo '[key1]'                                                    >> `kdb file system:/tests/conditionals`
echo "check/condition=(./ == 'val1') ? (../sub/key == 'true')" >> `kdb file system:/tests/conditionals`

echo "key=false" > `kdb file system:/tests/conditionals/sub`

# should fail and yield an error
kdb export system:/tests/conditionals ni
# ERROR:C03200
# Sorry, module conditionals issued the error C03200:
# Validation failed: Validation of Key key1: (./ == 'val1') ? (../sub/key == 'true') failed. ((../sub/key == 'true') failed)

kdb set system:/tests/conditionals/sub/key true

# should succeed
kdb export system:/tests/conditionals ni

# cleanup
kdb rm -r /tests/conditionals
sudo kdb umount /tests/conditionals/sub
sudo kdb umount /tests/conditionals

sudo kdb global-umount conditionals

kdb rm -r system:/elektra/globalplugins
kdb import system:/elektra/globalplugins < $(kdb get user:/tests/msr)
rm $(kdb get user:/tests/msr)
kdb rm user:/tests/msr
```
