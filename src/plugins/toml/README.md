- infos = Information about the toml plugin is in keys below
- infos/author = Jakob Fischer <jakobfischer93@gmail.com>
- infos/licence = BSD
- infos/provides = storage/toml
- infos/needs =
- infos/placements = getstorage setstorage
- infos/status = experimental unfinished nodoc
- infos/metadata =
- infos/description = This storage plugin reads and writes TOML files, using Flex and Bison.

# TODO: Documentation

## Requirements:

    - The plugin needs Flex (TODO: min version) and Bison (TODO: min version).

## Limitations:

    - Leap seconds for RFC3339 date times are not checked. For seconds, a value of 60 is always valid.
      As a result of this decision, no valid RFC3339 date is determined invalid, but invalid RFC3339 dates may be determined valid.
      Leap seconds can occur on the ends of June/Dec, where a time of 23:59:60 (positive leap second) or 23:59:58 (negative leap second) is the highest valid value for a second.
    - Currently, a newline at the beginning of a multiline string is not ignored (which it should be).

## Questions

    - How to handle escaped chars in basic strings, especially in context of file recreation?
        * Apply the escape chars, save string result as the key value, also save original string as meta key?
        * But then, how handle modification of value ? Dump original key on change? (Also how to detect change?)
    	* If this is fixed, fix also leading newlines in multiline strings (see Limitations)
    - Don't know where/how exactly to store trailing array comments/newline info
        * Can't associate those info to array top key in the way like file ending comments are preserved
        * Maybe add own metakey (eg. epilogue/comment/#1)?
    - How handle type of values?
        * Different types of of integers (decimal, hex, octal, binary)
            - Translate to long, type metakey as long, save original base as metakeys or whole original representation as metakey?
            - On value write, dump original representation info?
        * Can I introduce arbitrary type metakey values (for file recreation)?
            - eg type of a Datetime?
            - already done that for simple table / table array
    - To what extend whitespace should be preserved?
        * Makes grammar very messy

## TODOs

    - Order metakeys for file recreation
    - Type metakeys
