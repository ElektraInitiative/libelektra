- infos = Information about the toml plugin is in keys below
- infos/author = Jakob Fischer <jakobfischer93@gmail.com>
- infos/licence = BSD
- infos/provides = storage/toml
- infos/needs =
- infos/recommends = type
- infos/placements = getstorage setstorage
- infos/status = experimental unfinished nodoc
- infos/metadata = order comment/# comment/#/start comment/#/space type tomltype
- infos/description = This storage plugin reads and writes TOML files, using Flex and Bison.

# TODO: Documentation

## NULL keys
The plugin supports null keys. They are represented as special string of value '!ELEKTRA_NULL!' in a toml file, since toml doesn't support empty assignments. Be aware that a string of that value (full string must match exactly) will always be translated into a null key.

## Requirements:

    - The plugin needs Flex (TODO: min version) and Bison (minimal version 3).

## Strings
Although the plugin can read any kind of TOML string (bare, basic, literal, basic multiline, literal multiline) it will write back literal strings as basic strings. Multiline literal strings are mapped to multiline basic strings.
Therefore, you must treat any string you set with `kdb set` as a basic string and mind possible escape sequences and special meaning of quotation characters. Also be carful to not create invalid escape sequences, which lead to invalid TOML files.

Example
```
# Mount TOML file
sudo kdb mount test_strings.toml user/tests/storage toml type

# setting a string containing a newline escape sequence
kdb set 'user/tests/storage/string' 'I am a basic string\not a literal one.'

kdb get 'user/tests/storage/string'
# > I am a basic string
# > ot a literal one


# setting the string again, but escape the backslash with another backslash
kdb set 'user/tests/storage/string' 'I am a basic string\\not a literal one.'

kdb get 'user/tests/storage/string'
# > I am a basic string\not a literal one

# Cleanup
kdb rm -r user/tests/storage
sudo kdb umount user/tests/storage
```



## Limitations:

	- Comments and newlines between the last array element and closing brackets are discarded.
	- Trailing commas in arrays and inline tables are discarded
	- Only spaces in front of comments are preserved.

## TODOs:

	- Write documentation
	- Don't depend on order metakey when comparing array elements.
	- Error checks in write.c
	- Handle sparse arrays somehow (maybe there is a plugin), otherwise create special meaning string to write in TOML file (eg '!ELEKTRA_NO_ELEMENT!')?
	- Include directoryvalue, base64, null (+ maybe date) plugins
	- Make distinction on writing basic and literal strings
	- Write used metakeys in METADATA.ini
