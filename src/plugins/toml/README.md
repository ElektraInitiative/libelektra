- infos = Information about the toml plugin is in keys below
- infos/author = Jakob Fischer <jakobfischer93@gmail.com>
- infos/licence = BSD
- infos/provides = storage/toml
- infos/needs = null base64 directoryvalue
- infos/recommends = type
- infos/placements = getstorage setstorage
- infos/status = experimental unfinished nodoc
- infos/metadata = order comment/# comment/#/start comment/#/space type tomltype origvalue
- infos/description = This storage plugin reads and writes TOML files, using Flex and Bison.

# TODO: Documentation

## NULL/empty keys
The plugin supports null and empty keys with the help of the [null](../null/README.md) plugin.

## Requirements:

    - The plugin needs Flex (TODO: min version) and Bison (minimal version 3).

## Strings
Although the plugin can read any kind of TOML string (bare, basic, literal, basic multiline, literal multiline) it will write back all non-bare strings as basic strings (and it's multiline version of it).
Therefore, any string set with `kdb set` must be treated as a basic string and possible escape sequences and special meanings of quotation characters must be taken care of.

Example
```
# Mount TOML file
sudo kdb mount test_strings.toml user/tests/storage toml type

# setting a string containing a newline escape sequence
kdb set 'user/tests/storage/string' 'I am a basic string\not a literal one.'

kdb get 'user/tests/storage/string'
# > I am a basic string
# > ot a literal one


# setting the string again, but escape the backslash with another backslash so that the \not gets written out
kdb set 'user/tests/storage/string' 'I am a basic string\\not a literal one.'

kdb get 'user/tests/storage/string'
# > I am a basic string\not a literal one

# Cleanup
kdb rm -r user/tests/storage
sudo kdb umount user/tests/storage
```

## Simple Tables

## Table Arrays

## Inline Tables

## Arrays

## Limitations

	- Comments and newlines between the last array element and closing brackets are discarded.
	- Trailing commas in arrays and inline tables are discarded
	- Only spaces in front of comments are preserved.
	- Currently, Elektra's sparse arrays are not preserved on writing (they get a continuous array without index holes).

## TODOs

	- Write documentation
	- Correct interaction with other plugins, especially directory value
	- Check, how used plugins are chained (I heard something about wrapper plugin?). Order of filter plugins may be relevant in writeScalar()
	- Error checks in write.c
	- Handle writing of sparse arrays somehow (maybe there is a plugin), otherwise create special meaning string to write in TOML file (eg '!ELEKTRA_NO_ELEMENT!')?
	- Don't discard trailing array comments
	- Maybe use date plugin
	- Make distinction on writing basic and literal strings
	- Write used metakeys in METADATA.ini
