- infos = Information about the enum plugin is in keys below
- infos/author = Thomas Waser <thomas.waser@libelektra.org>
- infos/licence = BSD
- infos/provides = check
- infos/needs =
- infos/recommends = 
- infos/placements = presetstorage
- infos/status = productive maintained tested nodep libc nodoc
- infos/metadata = check/enum check/enum/# check/enum/multi
- infos/description = validates values against enum

## Introduction ##

The enum plugin checks string values of Keys by comparing it against a list of valid values.

## Usage ##

The plugin checks every Key in the Keyset for the Metakey `check/enum` containing a list
with the syntax `'string1', 'string2', 'string3', ..., 'stringN'` and compares each 
value with the string value of the Key. If no match is found an error is returned.

Alternatively, if `check/enum` starts with `#`, a meta array `check/enum` is used.
For example:

    check/enum = #3
    check/enum/#0 = small
    check/enum/#1 = middle
    check/enum/#2 = large
    check/enum/#3 = huge

Furthermore (only in the alternative syntax) `check/enum/multi` may contain a separator
character, that separates multiple allowed occurrences.
For example:

    check/enum/multi = _

Then the value `middle_small` would validate.
But `middle_small_small` would fail because every entry might only occur once.

## Example ##
```sh
# Backup-and-Restore:/example/enum
kdb mount enum.ecf /example/enum dump enum
kdb set user/example/enum/value middle # init to something valid
kdb setmeta user/example/enum/value check/enum "'low', 'middle', 'high'"
kdb set user/example/enum/value low
# Expected:
# RET:0
kdb set user/example/enum/value no
# Expected:
# RET:5
# ERRORS:121
# The command set failed while accessing the key database with the info:
# Error (#121) occurred!
# Description: Validation failed
# Ingroup: plugin
# Module: enum
# At: /home/thomas/Dev/Elektra/libelektra/src/plugins/enum/enum.c:142
# Reason: Validation of key "user/example/enum/value" with string "no" failed.
# Mountpoint: user/example/enum
# Configfile: /home/thomas/.config/enum.ecf.3155:1478751858.766117.tmp
kdb rm user/example/enum/value
kdb umount /example/enum
```
Or with multi-enums:
```sh
# Backup-and-Restore:/example/enum
kdb mount enum.ecf /example/enum dump enum
kdb set user/example/enum/value middle_small
kdb setmeta user/example/enum/value check/enum/#0 small
kdb setmeta user/example/enum/value check/enum/#1 middle
kdb setmeta user/example/enum/value check/enum/#2 large
kdb setmeta user/example/enum/value check/enum/#3 huge
kdb setmeta user/example/enum/value check/enum/multi _
kdb setmeta user/example/enum/value check/enum "#3"
kdb set user/example/enum/value ___small_middle__
# Expected: 
# RET:0
kdb set user/example/enum/value ___all_small__
# Expected:
# RET:5
# ERRORS:121
# The command set failed while accessing the key database with the info:
# Error (#121) occurred!
# Description: Validation failed
# Ingroup: plugin
# Module: enum
# At: /home/thomas/Dev/Elektra/libelektra/src/plugins/enum/enum.c:142
# Reason: Validation of key "user/example/enum/value" with string "___all_small__" failed.
# Mountpoint: user/example/enum
# Configfile: /home/thomas/.config/enum.ecf.2762:1478751812.258338.tmp
kdb umount /example/enum
```
