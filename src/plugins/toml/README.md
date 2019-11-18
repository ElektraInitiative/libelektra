- infos = Information about the toml plugin is in keys below
- infos/author = Jakob Fischer <jakobfischer93@gmail.com>
- infos/licence = BSD
- infos/provides = storage/toml
- infos/needs =
- infos/recommends = type
- infos/placements = getstorage setstorage
- infos/status = experimental unfinished nodoc
- infos/metadata = order comment/# comment/#/start comment/#/space
- infos/description = This storage plugin reads and writes TOML files, using Flex and Bison.

# TODO: Documentation

## Requirements:

    - The plugin needs Flex (TODO: min version) and Bison (TODO: min version).

## Limitations:

	- Comments and newlines between the last array element and closing brackets are discarded.
	- Trailing commas in arrays and inline tables are discarded
	- Comments/newlines between the last element of an array and the closing bracket are discarded.
	- Currently, only spaces in front of comments are preserved.

## Questions

    - Don't know where/how exactly to store trailing array comments/newline info
        * Can't associate those info to array top key in the way like file ending comments are preserved
        * Maybe add own metakey (eg. epilogue/comment/#1)?
	- How to handle null keys? (See storage plugin tutorial)
		* TOML doesn't allow empty assignments (eg 'key = ')
		* So how should empty values be represented in a toml file?
	- type plugin:
		* Don't know how boolean conversion with type plugin really works
			+ if type plugin active, should i expect to always receive "0" / "1" on setting a key?
				-> i receive 0/1 and then write true/false to the file (because tomls format)?
			+ on reading: i read tomls true/false and convert it to elektras 0/1 (all explicitly done by toml plugin)
				-> or shouldn't i convert the value, just pass true/false and the type plugin converts it to 0/1?
		* Is it false to set origvalue manually?
			+ it seems to for booleans
			+ but how does it look for eg integer, or strings which must have escapes applied?
		* Is it OK to create type metakeys other than that supplied by the type plugin?

	- Don't know why keys are deemed binary on all kdb get calls
		* what are the possible problem sources?
		
## TODOs
	- Document functions
	- ORDER on writing file
	- Error checks for writing
	- Replace existing keyRel calls
	- Maybe preserve spaces
		- Before array values
		- Before assignment/simple table header/table array header
	- Don't store origvalue, if transformed value is equal to untransformed value
    - Maybe use date plugin
