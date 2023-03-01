- infos = Information about the quickdump plugin is in keys below
- infos/author = Klemens Böswirth <k.boeswirth+git@gmail.com>
- infos/licence = BSD
- infos/needs =
- infos/provides = storage/quickdump
- infos/recommends =
- infos/placements = getstorage setstorage
- infos/status = maintained compatible tested nodep libc preview
- infos/metadata =
- infos/description = much quicker version of dump (2x or more in most cases)

## Introduction

`quickdump` is a storage plugin based on the `dump` format. It is a lot quicker (see [benchmarks.md](benchmarks.md)) than the old `dump`
plugin, because it does not use commands and stores string lengths as binary data. Through these changes all string comparisons and
integer-string conversions can be eliminated, which made up for a lot of the time spent by the `dump` plugin.

The format is also useful for IPC and streaming, because of this it is used by the `specload` plugin.

## Format

A `quickdump` file starts with the magic number `0x454b444200000003`. The first 4 bytes are the ASCII codes for `EKDB` (for Elektra KDB),
followed by a version number. This 64-bit is always stored as big-endian (i.e. the way it is written above).

After the magic number the file is just a list of Keys. Each Key consists of a name, a value and any number of metakey names and values.
Each name and value is written as a 64-bit length `n` followed by exactly `n` bytes of data. For strings we do not store a null terminator.
Therefore the length also does not account for that. When reading a string, the plugin allocates `n+1` bytes and sets the last one to `0`.
Note that ALL lengths are stored in little-endian format, because most modern machines are little-endian. To save disk space, we use a variable
length encoding for integers. The exact format is described below.

We don't store the full name of the key. Instead we only store the name relative to the parent key.

The end of a key is marked by a null byte. This cannot be confused with null bytes embedded in binary key values, because of the length
prefixes before each key and metavalue.

To distinguish between binary and string keys the (length of the) key value is prefixed with either a `b` or an `s`. Each metakey is
prefixed with an `m`, unless we detect that the same metakey was already present on a previous key (e.g. through `keyCopyMeta`). In this
case the prefix `c` is used and instead of the metakey name and value, we write the name of the previous key and the metakey name.

### Variable Length Integer encoding

The basic idea of the format is to store integers in base 128. This means we only use 7 bits per byte and the 8th bit (marker bit) indicates
whether or not there are more bytes to read. However, to make things more efficient we move all those marker bits to the first byte. Then we
can read one byte and immediately know, how much bytes follow. This is similar to what UTF-8 does.

The table below shows how the encoding works. The first byte is shown in full (`x` is either `0` or `1`), then `[n]` indicates that `n` bytes
of data follow.

```
xxxxxxx1     up to  7 bits in 1 byte
xxxxxx10 [1] up to 14 bits in 2 bytes
xxxxx100 [2] up to 21 bits in 3 bytes
xxxx1000 [3] up to 28 bits in 4 bytes
xxx10000 [4] up to 35 bits in 5 bytes
xx100000 [5] up to 42 bits in 6 bytes
x1000000 [6] up to 49 bits in 7 bytes
10000000 [7] up to 56 bits in 8 bytes
00000000 [8] up to 64 bits in 9 bytes
```

Thanks to https://github.com/stoklund/varint for listing various integer encodings.

## Usage

Like any other storage plugin, you simply use `quickdump` during mounting, import or export.

```
sudo kdb mount quickdump.eqd user:/tests/quickdump quickdump
```

## Dependencies

None.

## Examples

```sh
# Mount a backend using quickdump
sudo kdb mount quickdump.eqd user:/tests/quickdump quickdump
# RET: 0

# Set some keys and metakeys
kdb set user:/tests/quickdump/key value
#> Create a new key user:/tests/quickdump/key with string "value"

kdb meta-set user:/tests/quickdump/key meta "metavalue"

kdb set user:/tests/quickdump/otherkey "other value"
#> Create a new key user:/tests/quickdump/otherkey with string "other value"

# Show resulting file (not part of test, because xxd is not available everywhere)
# xxd $(kdb file user:/tests/quickdump/key)
# 00000000: 454b 4442 0000 0003 076b 6579 730b 7661  EKDB.....keys.va
# 00000010: 6c75 656d 096d 6574 6113 6d65 7461 7661  luem.meta.metava
# 00000020: 6c75 6500 116f 7468 6572 6b65 7973 176f  lue..otherkeys.o
# 00000030: 7468 6572 2076 616c 7565 00              ther value.


# Change mounted file (in a very stupid way to enable shell-recorder testing):
cp $(kdb file user:/tests/quickdump/key) a.tmp

# 1. change key from 'value' to 'other value'
(head -c 13 a.tmp; printf "%bother value" '\0027'; tail -c 40 a.tmp) > b.tmp

rm a.tmp

# 2. add copy metadata instruction to otherkey
(head -c 64 b.tmp; printf "c%bkey%bmeta\0" '\0007' '\0011') > c.tmp

rm b.tmp

# test file name, so KDB isn't destroyed if mounting failed
[ "x$(kdb file user:/tests/quickdump/key)" != "x$(kdb file user:/)" ] && mv c.tmp $(kdb file user:/tests/quickdump/key)

kdb get user:/tests/quickdump/key
#> other value

kdb meta get user:/tests/quickdump/key meta
#> metavalue

kdb get user:/tests/quickdump/otherkey
#> other value

kdb meta get user:/tests/quickdump/otherkey meta
#> metavalue

# Cleanup
kdb rm -r user:/tests/quickdump
sudo kdb umount user:/tests/quickdump
```

## Limitations

None.
