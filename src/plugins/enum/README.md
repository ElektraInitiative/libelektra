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

Furthermore `check/enum/multi` may contain a separator character, that separates 
multiple allowed occurrences.
For example:

    check/enum/multi = _

Then the value `middle_small` would validate.
But `middle_small_small` would fail because every entry might only occur once.

## Example ##

    kdb mount enum.ecf /example/enum enum
    kdb set user/example/enum/value middle # init to something valid
    kdb setmeta user/example/enum/value check/enum "'low', 'middle', 'high'"
    kdb set user/example/enum/value low # success
    kdb set user/example/enum/value no  # fail
    kdb rm user/example/enum/value

Or with multi-enums:

    kdb set user/example/enum/value middle_small  # valid init
    kdb setmeta user/example/enum/value check/enum/#0 small
    kdb setmeta user/example/enum/value check/enum/#1 middle
    kdb setmeta user/example/enum/value check/enum/#2 large
    kdb setmeta user/example/enum/value check/enum/#3 huge
    kdb setmeta user/example/enum/value check/enum/multi _
    kdb setmeta user/example/enum/value check/enum "#3"
    kdb set user/example/enum/value ___small_middle__ # success
    kdb set user/example/enum/value ___all_small__   # fail: "all" invalid

