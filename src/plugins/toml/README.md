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
	- Leap seconds for RFC3339 date times are not checked. For seconds, a value of 60 is always valid.
	As a result of this decision, no valid RFC3339 date is determined invalid, but invalid RFC3339 dates may be determined valid.
	Leap seconds can occur on the ends of June/Dec, where a time of 23:59:60 (positive leap second) or 23:59:58 (negative leap second) is the highest valid value for a second.

## Questions

    - Don't know where/how exactly to store trailing array comments/newline info
        * Can't associate those info to array top key in the way like file ending comments are preserved
        * Maybe add own metakey (eg. epilogue/comment/#1)?

## TODOs
    - Use date plugin
    - directory value plugin?
    - empty array with ###empty_array
