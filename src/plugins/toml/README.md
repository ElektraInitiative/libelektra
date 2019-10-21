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

## Limitations:
    - Leap seconds for RFC3339 date times are not checked. For seconds, a value of 60 is always valid.
      As a result of this decision, no valid RFC3339 date is determined invalid, but invalid RFC3339 dates may be determined valid.
      Leap seconds can occur on the ends of June/Dec, where a time of 23:59:60 (positive leap second) or 23:59:58 (negative leap second) is the highest valid value for a second.

## TODOs
    - Order metakeys for file recreation
    - Type metakeys
    - Handle leading newlines in multiline strings (Remove them, save as metakey?)
    - How to handle escaped chars in basic strings, especially in context of file recreation?
        * Apply the escape chars, save string result as the key value, also save original string as meta key?
        * But then, how handle modification of value ? Dump original key on change? (Also how to detect change?)
        * On write to file, 
