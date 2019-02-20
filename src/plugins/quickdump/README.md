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

After the magic number the file is just a list of Keys. Each Key consists of a name, a value and any number of metakey names and values.
Each name and value is written as a 64-bit length `n` followed by exactly `n` bytes of data. For strings we do not store a null terminator.
Therefore the length also does not account for that. When reading a string, you will need to allocate `n+1` bytes and set the last on to `0`.
Note that ALL lengths are stored in little-endian format, because most modern machines are little-endian.

The end of a key is marked by a null byte. This cannot be confused with null bytes embedded in binary key values, because of the length
prefixes before each key and metavalue.

To distinguish between binary and string keys the (length of the) key value is prefixed with either a `b` or an `s`. Each metakey is
prefixed with an `m`, unless we detect that the same metakey was already present on a previous key (e.g. through `keyCopyMeta`). In this
case the prefix `c` is used and instead of the metakey name and value, we write the name of the previous key and the metakey name.

## Usage

Like any other storage plugin, you simply use `quickdump` during mounting, import or export.

```
sudo kdb mount quickdump.eqd user/tests/quickdump quickdump
```

## Dependencies

None.

## Examples

```
# Mount a backend using quickdump
kdb mount quickdump.eqd user/tests/quickdump quickdump
#> RET: 0

# Set some keys and metakeys
kdb set user/tests/quickdump/key value
#> Create a new key user/tests/quickdump/key with string "value"

kdb setmeta user/tests/quickdump/key meta "metavalue"

kdb set user/tests/quickdump/otherkey "other value"
#> Create a new key user/tests/quickdump/otherkey with string "other value"

# Show resulting file
xxd $(kdb file user/tests/quickdump/key)
#> 00000000: 454b 4442 0000 0001 1800 0000 0000 0000  EKDB............
#> 00000010: 7573 6572 2f74 6573 7473 2f71 7569 636b  user/tests/quick
#> 00000020: 6475 6d70 2f6b 6579 7305 0000 0000 0000  dump/keys.......
#> 00000030: 0076 616c 7565 6d04 0000 0000 0000 006d  .valuem........m
#> 00000040: 6574 6109 0000 0000 0000 006d 6574 6176  eta........metav
#> 00000050: 616c 7565 001d 0000 0000 0000 0075 7365  alue.........use
#> 00000060: 722f 7465 7374 732f 7175 6963 6b64 756d  r/tests/quickdum
#> 00000070: 702f 6f74 6865 726b 6579 730b 0000 0000  p/otherkeys.....
#> 00000080: 0000 006f 7468 6572 2076 616c 7565 00    ...other value.

# Change mounted file:
#  - change user/tests/quickdump/key from 'value' to 'new value'
#  - add copy metadata instruction to user/tests/quickdump/otherkey
xxd -r << EOF > $(kdb file user/tests/quickdump/key)
00000000: 454b 4442 0000 0001 1800 0000 0000 0000  EKDB............
00000010: 7573 6572 2f74 6573 7473 2f71 7569 636b  user/tests/quick
00000020: 6475 6d70 2f6b 6579 7309 0000 0000 0000  dump/keys.......
00000030: 006e 6577 2076 616c 7565 6d04 0000 0000  .new valuem.....
00000040: 0000 006d 6574 6109 0000 0000 0000 006d  ...meta........m
00000050: 6574 6176 616c 7565 001d 0000 0000 0000  etavalue........
00000060: 0075 7365 722f 7465 7374 732f 7175 6963  .user/tests/quic
00000070: 6b64 756d 702f 6f74 6865 726b 6579 730b  kdump/otherkeys.
00000080: 0000 0000 0000 006f 7468 6572 2076 616c  .......other val
00000090: 7565 6318 0000 0000 0000 0075 7365 722f  uec........user/
000000a0: 7465 7374 732f 7175 6963 6b64 756d 702f  tests/quickdump/
000000b0: 6b65 7904 0000 0000 0000 006d 6574 6100  key........meta.
EOF

kdb get user/tests/quickdump/key
#> new value

kdb getmeta user/tests/quickdump/key meta
#> metavalue

kdb get user/tests/quickdump/otherkey
#> other value

kdb getmeta user/tests/quickdump/otherkey meta
#> metavalue

# Cleanup
kdb rm -r user/tests/quickdump
kdb umount user/tests/quickdump
```

## Limitations

None.
