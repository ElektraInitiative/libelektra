- infos = Information about the toml plugin is in keys below
- infos/author = Jakob Fischer <jakobfischer93@gmail.com>
- infos/licence = BSD
- infos/provides = storage/toml
- infos/needs =
- infos/recommends = type
- infos/placements = getstorage setstorage
- infos/status = experimental unfinished nodoc
- infos/metadata = order comment/# comment/#/start comment/#/space type
- infos/description = This storage plugin reads and writes TOML files, using Flex and Bison.

# TODO: Documentation

## NULL keys
The plugin supports null keys. They are represented as special string of value '!ELEKTRA_NULL!' in a toml file, since toml doesn't support empty assignments. Be aware that a string of that value (full string must match exactly) will always be translated into a null key.

## Requirements:

    - The plugin needs Flex (TODO: min version) and Bison (TODO: min version).

## Limitations:

	- Comments and newlines between the last array element and closing brackets are discarded.
	- Trailing commas in arrays and inline tables are discarded
	- Only spaces in front of comments are preserved.

## Questions

    - Don't know where/how exactly to store trailing array comments/newline info
        * Can't associate those info to array top key in the way like file ending comments are preserved
        * Maybe add own metakey (eg. epilogue/comment/#1)?
		
## TODOs
	- Document functions
	- proper sorting -> arrays
	- base64 for binary k/v
	- check why space restoration in front of comments not working
	- Error checks for writing
	- Correct string handling (which type of string, do in type.c)
	- Maybe preserve spaces
		- Before array values
		- Before assignment/simple table header/table array header
    - Maybe use date plugin
