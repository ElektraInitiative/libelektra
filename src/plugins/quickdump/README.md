- infos = Information about the quickdump plugin is in keys below
- infos/author = Klemens Böswirth <k.boeswirth+git@gmail.com>
- infos/licence = BSD
- infos/needs =
- infos/provides = storage/quickdump
- infos/recommends =
- infos/placements = getstorage setstorage
- infos/status = tested nodep libc compatible preview
- infos/metadata =
- infos/description = much quicker version of dump (2x or more in most cases)

## Introduction

`quickdump` is a storage plugin based on the `dump` format. It is a lot quicker (see [benchmarks.md](benchmarks.md)) than the old `dump`
plugin, because it does not use commands and stores string lengths as binary data. Through these changes all string comparisons and
integer-string conversions can be eliminated, which made up for a lot of the time spent by the `dump` plugin.

The format is also useful for IPC and streaming, because of this it is used by the `specload` plugin.

## Format

A `quickdump` file starts with the magic number `0x454b444200000002`. The first 4 bytes are the ASCII codes for `EKDB` (for Elektra KDB),
followed by a version number. This 64-bit is always stored as big-endian (i.e. the way it is written above).

After the magic number the file is just a list of Keys. Each Key consists of a name, a value and any number of metakey names and values.
Each name and value is written as a 64-bit length `n` followed by exactly `n` bytes of data. For strings we do not store a null terminator.
Therefore the length also does not account for that. When reading a string, the plugin allocates `n+1` bytes and sets the last one to `0`.
Note that ALL lengths are stored in little-endian format, because most modern machines are little-endian.

We don't store the full name of the key. Instead we only store the name relative to the parent key.

The end of a key is marked by a null byte. This cannot be confused with null bytes embedded in binary key values, because of the length
prefixes before each key and metavalue.

To distinguish between binary and string keys the (length of the) key value is prefixed with either a `b` or an `s`. Each metakey is
prefixed with an `m`, unless we detect that the same metakey was already present on a previous key (e.g. through `keyCopyMeta`). In this
case the prefix `c` is used and instead of the metakey name and value, we write the name of the previous key and the metakey name.

### Version 1

The old format used the magic number `0x454b444200000001` and stored the full keynames, instead of one relative to the parent key. It can
still be read by this plugin, but we will always write the new format.

## Usage

Like any other storage plugin, you simply use `quickdump` during mounting, import or export.

```
sudo kdb mount quickdump.eqd user/tests/quickdump quickdump
```

## Dependencies

None.

## Examples

```sh
# Mount a backend using quickdump
sudo kdb mount quickdump.eqd user/tests/quickdump quickdump
# RET: 0

# Set some keys and metakeys
kdb set user/tests/quickdump/key value
#> Create a new key user/tests/quickdump/key with string "value"

kdb setmeta user/tests/quickdump/key meta "metavalue"

kdb set user/tests/quickdump/otherkey "other value"
#> Create a new key user/tests/quickdump/otherkey with string "other value"

# Show resulting file (not part of test, because xxd is not available everywhere)
# xxd $(kdb file user/tests/quickdump/key)
# 00000000: 454b 4442 0000 0002 0300 0000 0000 0000  EKDB............
# 00000010: 6b65 7973 0500 0000 0000 0000 7661 6c75  keys........valu
# 00000020: 656d 0400 0000 0000 0000 6d65 7461 0900  em........meta..
# 00000030: 0000 0000 0000 6d65 7461 7661 6c75 6500  ......metavalue.
# 00000040: 0800 0000 0000 0000 6f74 6865 726b 6579  ........otherkey
# 00000050: 730b 0000 0000 0000 006f 7468 6572 2076  s........other v
# 00000060: 616c 7565 00                             alue.


# Change mounted file:
cp $(kdb file user/tests/quickdump/key) /tmp/a.tmp

# 1. change key from 'value' to 'other value'
(head -c 20 /tmp/a.tmp; printf "\x0b\x00\x00\x00\x00\x00\x00\x00other value"; tail -c +34 /tmp/a.tmp) > /tmp/b.tmp

rm /tmp/a.tmp

# 2. add copy metadata instruction to otherkey
(head -c -1 /tmp/b.tmp; printf "c\x03\x00\x00\x00\x00\x00\x00\x00key\x04\x00\x00\x00\x00\x00\x00\x00meta\x00") > $(kdb file user/tests/quickdump/key)

kdb get user/tests/quickdump/key
#> other value

kdb getmeta user/tests/quickdump/key meta
#> metavalue

kdb get user/tests/quickdump/otherkey
#> other value

kdb getmeta user/tests/quickdump/otherkey meta
#> metavalue

# Cleanup
kdb rm -r user/tests/quickdump
sudo kdb umount user/tests/quickdump
```

## Limitations

None.
