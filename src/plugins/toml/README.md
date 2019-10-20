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
    - (TODO: fix) For now, Hours/Minutes/Seconds in date times can have arbitrary values consisting of 2 digits (eg. 99:99:99 is a perfectly fine time) 
    - Leap seconds for RFC3339 date times are not checked. As a result a value for second of 60 is always valid.
