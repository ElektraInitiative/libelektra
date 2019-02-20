- infos = Information about the quickdump plugin is in keys below
- infos/author = Klemens Böswirth <k.boeswirth+git@gmail.com>
- infos/licence = BSD
- infos/needs =
- infos/provides = storage/quickdump
- infos/recommends =
- infos/placements = getstorage setstorage
- infos/status = tested nodep libc compatible preview
- infos/metadata =
- infos/description = much quicker version of dump

## Introduction

`quickdump` is a storage plugin based on the `dump` format. It is a lot quicker than the old `dump` plugin, because it does not use commands
and stores string lengths as binary data. Through these changes all string comparisons and integer-string conversions can be eliminated,
which made up for a lot of the time spent by the `dump` plugin.

The format is also useful for IPC and streaming, because of this it is used by the `specload` plugin.

## Format

A `quickdump` file starts with the magic number `0x454b444200000001`. The first 4 bytes are the ASCII codes for `EKDB` (for Elektra KDB),
followed by a version number. This 64-bit is always stored as big-endian (i.e. the way it is written above).

After the magic number the file is just a list of Keys. Each Key consists of a name, a value and any number of meta key names and values.
Each name and value is written as a 64-bit length `n` followed by exactly `n` bytes of data. For strings we do not store a null terminator.
Therefore the length also does not account for that. When reading a string, you will need to allocate `n+1` bytes and set the last on to `0`.
Note that ALL lengths are stored in little-endian format, because most modern machines are little-endian.

The end of a key is marked by a null byte. This cannot be confused with null bytes embedded in binary key values, because of the length
prefixes before each key and meta value.

## Usage

TODO

## Dependencies

None.

## Examples

TODO

## Limitations

Currently does not support shared meta data (keyCopyMeta).
