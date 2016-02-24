- infos = Information about the enum plugin is in keys below
- infos/author = Thomas Waser <thomas.waser@libelektra.org>
- infos/licence = BSD
- infos/provides = check
- infos/needs =
- infos/recommends = 
- infos/placements = presetstorage
- infos/status = productive maintained tested nodep libc nodoc
- infos/metadata = check/enum
- infos/description =

## Introduction ##

The Enum plugin checks string values of Keys by comparing it against a list of valid values.

## Usage ##

The plugin checks every Key in the Keyset for the Metakey `check/enum` containing a list
 with the syntax `'string1', 'string2', 'string3', ..., 'stringN'` and compares each 
value with the string value of the Key. If no match is found an error is returned.

## Example ##

	kdb mount enum.ecf /example/enum enum
	kdb set user/example/enum/value middle # init to something valid
	kdb setmeta user/example/enum/value check/enum "'low', 'middle', 'high'"
	kdb set user/example/enum/value low # success
	kdb set user/example/enum/value no  # fail
