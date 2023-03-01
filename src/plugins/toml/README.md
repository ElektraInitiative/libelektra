- infos = Information about the toml plugin is in keys below
- infos/author = Jakob Fischer <jakobfischer93@gmail.com>
- infos/licence = BSD
- infos/provides = storage/toml
- infos/needs = base64
- infos/recommends = type
- infos/placements = getstorage setstorage
- infos/status = experimental unfinished
- infos/metadata = order comment/# comment/#/start comment/#/space type tomltype origvalue
- infos/description = This storage plugin reads and writes TOML files using Flex and Bison.

# Introduction

This plugin is a storage plugin for reading and writing TOML files. The plugin retains most of the file structure of a read TOML file such as comments, empty lines and TOML tables.
It supports all kinds of TOML specific types and tables, including nested inline tables and multiline strings.

# Requirements

The plugin needs Flex (>=2.6.2) and Bison (>=3) for parsing TOML files.

# Types

## Reading

On reading, the plugin will set the `type` metakey for strings, integers, floats and boolean values, if applicable.
For decimal integers, the metakey is set to `long_long`.
For binary/octal/hexadecimal integers, the metakey is set to `unsigned_long_long`.
For floats, the metakey will be set to `double`.
These types are chosen to conform with the TOML format as stated on the offical [TOML](https://toml.io/en/v1.0.0-rc.1) page.

On reading, non-decimal integers will get converted to decimal.
The non-decimal representation will be stored in the `origvalue` metakey of the key.
When writing a key, the value of that metakey will be written instead, in order to retain the original format.
Note that the `origvalue` metakey gets removed if the value of the key changes.

```sh
sudo kdb mount test.toml user:/tests/storage/types toml type

# Create a TOML file with 4 keys
mkdir -p $(dirname $(kdb file user:/tests/storage/types))
echo 'plain_decimal = 1000' >> `kdb file user:/tests/storage/types`
echo 'file_permissions = 0o777' >> `kdb file user:/tests/storage/types`
echo 'pi = 3.1415' >> `kdb file user:/tests/storage/types`
echo 'division_gone_wrong = -inf' >> `kdb file user:/tests/storage/types`

# Print the content of the toml file
cat `kdb file user:/tests/storage/types`
# > plain_decimal = 1000
# > file_permissions = 0o777
# > pi = 3.1415
# > division_gone_wrong = -inf

# Print types and values of the keys with `kdb`

kdb meta get 'user:/tests/storage/types/plain_decimal' 'type'
# > long_long
kdb get 'user:/tests/storage/types/plain_decimal'
# > 1000

kdb meta get 'user:/tests/storage/types/file_permissions' 'type'
# > unsigned_long_long
# The octal value will be converted to decimal
kdb get 'user:/tests/storage/types/file_permissions'
# > 511

kdb meta get 'user:/tests/storage/types/pi' 'type'
# > double
kdb get 'user:/tests/storage/types/pi'
# > 3.1415

kdb meta get 'user:/tests/storage/types/division_gone_wrong' 'type'
# > double
kdb get 'user:/tests/storage/types/division_gone_wrong'
# > -inf

# Cleanup
kdb rm -r user:/tests/storage/types
sudo kdb umount user:/tests/storage/types
```

## Writing

On writing, for most values, the plugin will infer the appropriate type and will write them accordingly.
This means, values, that match a TOML float, integer or date will be written without any quotes around them.
If a value does not match any of these types, it will be written as a string.

The plugin uses an existing `type` metakey only to check if it should write a value as a `string` or a `boolean`.

With this functionality, numbers can be written as a string to the file, if wanted.

To write a boolean value, the `type` metakey must be set to `boolean` for the key.
Otherwise, no conversion to the TOML boolean values will take place.
Per default, Elektra uses 0/1 to represent boolean values.

```sh
sudo kdb mount test.toml user:/tests/storage/types toml type

# Create a key, which may be a integer, boolean or string
kdb set 'user:/tests/storage/types/value' '1'

# The plugin infers `long_long` for this value
kdb meta get 'user:/tests/storage/types/value' 'type'
# > long_long

# The value is written as an integer
cat `kdb file user:/tests/storage/types`
# > value = 1

# Manually set the `type` metakey to boolean
kdb meta-set 'user:/tests/storage/types/value' 'type' 'boolean'

# The value is written as a boolean
cat `kdb file user:/tests/storage/types`
# > value = true

# Manually set the `type` metakey to string
kdb meta-set 'user:/tests/storage/types/value' 'type' 'string'

# The value is written as a string
cat `kdb file user:/tests/storage/types`
# > value = "1"

# Cleanup
kdb rm -r user:/tests/storage/types
sudo kdb umount user:/tests/storage/types
```

# Numbers

The plugin supports reading and writing of any kind of number supported by the TOML format, such as floating point numbers and binary/octal/decimal/hexadecimal integers.
To write a non-decimal integer, add the corresponding prefix to the number (`0b` for binary, `0o` for octal, `0x` for hexadecimal).
The value will be written in the given base to the file, but converted to decimal within Elektra (see [Reading](##reading)).
Note that the plugin doesn't warn about invalid prefix/digit combinations. If the combination is not valid, it will be written as a string instead.

If the `type` plugin is enabled and you want to change the value of an existing number key which needs conversion (all keys which have a `origvalue`),
you have to change the value of `origvalue` instead of the key value. Otherwise the `type` plugin will give an error.

```sh
# Mount a new TOML file
sudo kdb mount test.toml user:/tests/storage/numbers toml type

# Write an octal value
kdb set 'user:/tests/storage/numbers/a' '0o777'

# Get the converted decimal value
kdb get 'user:/tests/storage/numbers/a'
# > 511

# Get the original octal value
kdb meta get 'user:/tests/storage/numbers/a' 'origvalue'
# > 0o777

# Get the type of the number; since it's originally octal, we get `unsigned_long_long`
kdb meta get 'user:/tests/storage/numbers/a' 'type'
# > unsigned_long_long

# Change the value by changing the `origvalue` metakey
kdb meta-set 'user:/tests/storage/numbers/a' 'origvalue' '0o666'

# Get the new value as decimal
kdb get 'user:/tests/storage/numbers/a'
# > 438

# Change the value to an invalid octal value.
kdb meta-set 'user:/tests/storage/numbers/a' 'origvalue' '0o888'

# The key value is no longer considered a number
kdb meta get 'user:/tests/storage/numbers/a' 'type'
# > string

# Cleanup
kdb rm -r user:/tests/storage/numbers
sudo kdb umount user:/tests/storage/numbers
```

# Strings

The plugin can read any kind of TOML string: basic, literal, basic multiline and literal multiline.

Similar to the TOML-specific values (table, array) discussed below, we use the `tomltype` metakey to store the kind of string.
The values used are: `string_basic`, `string_literal`, `string_ml_basic` and `string_ml_literal`.

When writing a file, the value of `tomltype` will be respected. The plugin will therefore preserve the kind of string throughout a kdbGet/kdbSet cycle.
You can also set `tomltype` manually to transform the string into a different type. If `tomltype` is set to a kind of string that is
incompatible with the current value of the key, the plugin will report an error.

If `tomltype` is not set, the plugin defaults to a basic string, or if there are at least two `\n` to a basic multiline string. This way a `kdb set` will
always work correctly, as long as the given string is valid UTF-8.

```sh
# Mount TOML file
sudo kdb mount test_strings.toml user:/tests/storage toml type

# setting a string containing a newline escape sequence
kdb set 'user:/tests/storage/string' 'I am a basic string\not a literal one.'

kdb get 'user:/tests/storage/string'
# > I am a basic string
# > ot a literal one

# setting the string again, but escape the backslash with another backslash
kdb set 'user:/tests/storage/string' 'I am a basic string\\not a literal one.'

kdb get 'user:/tests/storage/string'
# > I am a basic string\not a literal one

# Cleanup
kdb rm -r user:/tests/storage
sudo kdb umount user:/tests/storage
```

The plugin supports all kinds of escape sequences used by TOML in basic and basic multiline strings, like `\n`, `\r`, `\t` and
even `\u`/`\U` for Unicode escape sequences. When reading a file, all these escape sequences will be translated into valid UTF-8
byte sequences that will be used as the key values.

When writing a file, the plugin uses the escape sequences, whenever a part of the key value cannot be placed literally into the file.
If a key contains a non-UTF-8 string value, the plugin replaces any invalid sections with the Replacement Character U+FFFD and reports
a warning.

Finally, the plugin also uses `origvalue` to remember the original formatting of the string. This is particularly useful for escape sequences.
Without this mechanism, a `\u` escape sequence would always be written literally (not as an escape sequence), because when reading the file
it is translated into UTF-8 bytes. For multiline strings `origvalue` also includes any leading or escaped newlines.

# Binary/NULL values

The plugin handles binary values by using the [base64](../base64/README.md) plugin.
As a result, binary values get written as base64 encoded strings, which start with the special prefix `@BASE64`.
`NULL` key values are written as special strings of value `@NULL`.

```
# Mount TOML file
sudo kdb mount test_binary.toml user:/tests/storage toml type

# Creating a key with a NULL value
kdb set 'user:/tests/storage/nullkey'
# > Create a new key user:/test/nullkey with null value

# Print file content
cat `kdb file user:/tests/storage`
# > nullkey = '@NULL'

# Write base64 encoded data to the file
echo "base64 = '@BASE64SSBhbSBiYXNlIDY0IGVuY29kZWQgZm9yIG5vIHJlYXNvbi4='" > `kdb file user:/test`

# Print the value of the key, which is a binary value
kdb get 'user:/test/base64'
#> \x49\x20\x61\x6d\x20\x62\x61\x73\x65\x20\x36\x34\x20\x65\x6e\x63\x6f\x64\x65\x64\x20\x66\x6f\x72\x20\x6e\x6f\x20\x72\x65\x61\x73\x6f\x6e\x2e

# Print the value again, but apply the escape codes
echo -e `kdb get 'user:/test/base64'`
#> I am base 64 encoded for no reason.

# Cleanup
kdb rm -r user:/tests/storage
sudo kdb umount user:/tests/storage
```

# TOML specific structures

TOML specific structures are represented by the metakey `tomltype` on a certain key.
It will be set when the TOML plugin reads a TOML structure from a file. Additionally, this metakey can be set by the user, if they want a certain TOML structure to be written.
No automatic inference of this metakey is done on writing.

## Simple Tables

TOML's simple tables are represented by setting the `tomltype` metakey to `simpletable`.

```sh
# Mount TOML file
sudo kdb mount test_table.toml user:/tests/storage toml type

# Create three keys, which are all a subkey of 'common',
# but we have no 'common' simple table key yet
kdb set 'user:/tests/storage/common/a' '0'
kdb set 'user:/tests/storage/common/b' '1'
kdb set 'user:/tests/storage/common/c' '2'

# Print the content of the resulting TOML file
cat `kdb file user:/tests/storage`
#> common.a = 0
#> common.b = 1
#> common.c = 2

# Create a simple table key
kdb meta-set 'user:/tests/storage/common' 'tomltype' 'simpletable'

# Print the content of the resulting TOML file
cat `kdb file user:/tests/storage`
#> [common]
#> a = 0
#> b = 1
#> c = 2

# Cleanup
kdb rm -r user:/tests/storage
sudo kdb umount user:/tests/storage
```

## Table Arrays

Table arrays are represented by setting the `tomltype` metakey to `tablearray`. It is not required to also set the array metakey, since the plugin will set the metakey, if it is missing.

```sh
# Mount TOML file
sudo kdb mount test_table_array.toml user:/tests/storage toml type

# Create a table array containing two entries, each with a key 'a' and 'b'
kdb meta-set 'user:/tests/storage/tablearray' 'tomltype' 'tablearray'
kdb set 'user:/tests/storage/tablearray/#0/a' '1'
kdb set 'user:/tests/storage/tablearray/#0/b' '2'

kdb set 'user:/tests/storage/tablearray/#1/a' '3'
kdb set 'user:/tests/storage/tablearray/#1/b' '4'

# Print the highest index of the table array
kdb meta get 'user:/tests/storage/tablearray' 'array'
#> #1

# Print the content of the resulting TOML file
cat `kdb file user:/tests/storage`
#> [[tablearray]]
#> a = 1
#> b = 2
#> [[tablearray]]
#> a = 3
#> b = 4

# Cleanup
kdb rm -r user:/tests/storage
sudo kdb umount user:/tests/storage
```

## Inline Tables

Inline tables are represented by setting the `tomltype` metakey to `inlinetable`. The plugin also supports reading/writing nested inline tables.

```sh
# Mount TOML file
sudo kdb mount test_inline_table.toml user:/tests/storage toml type

# Create a table array containing two entries, each with a key 'a' and 'b'
kdb meta-set 'user:/tests/storage/inlinetable' 'tomltype' 'inlinetable'
kdb set 'user:/tests/storage/inlinetable/a' '1'
kdb set 'user:/tests/storage/inlinetable/b' '2'
kdb meta-set 'user:/tests/storage/inlinetable/nested' 'tomltype' 'inlinetable'
kdb set 'user:/tests/storage/inlinetable/nested/x' '3'
kdb set 'user:/tests/storage/inlinetable/nested/y' '4'

# Print the content of the resulting TOML file
cat `kdb file user:/tests/storage`
#> inlinetable = { a = 1, b = 2, nested = { x = 3, y = 4 } }

# Cleanup
kdb rm -r user:/tests/storage
sudo kdb umount user:/tests/storage
```

## Arrays

Arrays are recognized by the `array` metakey. On writing, the plugin will detect arrays automatically and set the appropriate metakey if it is missing.

```sh
# Mount TOML file
sudo kdb mount test_array.toml user:/tests/storage toml type

# Create array elements
kdb set 'user:/tests/storage/array/#0' '1'
kdb set 'user:/tests/storage/array/#1' '2'
kdb set 'user:/tests/storage/array/#2' '3'

# Print the highest index of the array
kdb meta get 'user:/tests/storage/array' 'array'
#> #2

# Print the content of the resulting TOML file
cat `kdb file user:/tests/storage`
#> array = [1, 2, 3]


# Cleanup
kdb rm -r user:/tests/storage
sudo kdb umount user:/tests/storage
```

# Comments and Empty Lines

The plugin preserves all comments with only one limitation for arrays. The whitespace in front of a comment is also saved.

Comments can also be created by assigning meta keys to a key.
The meta keys must be of the form `comment/#n`, where `n` is a positive number, indicating the position of the comment relative to the key.
An index of 0 is always the inline comment of the key.
Indices greater than zero are for comments preceding the given key, where 1 is the top-most comment and the highest index comment is right above the key.

Preceding whitespace can be added to a comment by creating a `comment/#n/space` metakey.
This metakey can contain any number of space or tab characters, which will be placed before the `#` starting the comment.
Any whitespace _after_ the `#` is considered part of the comment and will therefore be part of `comment/#n`.

File ending comments must be assigned to the file root key.

Empty lines in front of a key can be created by adding an empty `comment/#n/start` entry to it. In this case, no `comment/#n` key is needed.

```sh
# Mount TOML file
sudo kdb mount test_comments.toml user:/tests/storage toml type

# create a key-value pair, ready for comment decoration

kdb set 'user:/tests/storage/key' '1'

# add an inline comment with 4 leading spaces
kdb meta-set 'user:/tests/storage/key' 'comment/#0' ' This value is very interesting'
kdb meta-set 'user:/tests/storage/key' 'comment/#0/space' '    '

# add some comments preceding the key
kdb meta-set 'user:/tests/storage/key' 'comment/#1' ' I am the top-most comment relative to my key.'
kdb meta-set 'user:/tests/storage/key' 'comment/#2' ' I am in the middle. Just boring.'
kdb meta-set 'user:/tests/storage/key' 'comment/#3' ' I am in the line right above my key.'

# add file ending comments and empty lines
kdb meta-set 'user:/tests/storage' 'comment/#1' ' First file-ending comment'
kdb meta-set 'user:/tests/storage' 'comment/#2/start' ''
kdb meta-set 'user:/tests/storage' 'comment/#3' ' Second file-ending comment. I am the last line of the file.'

# Print the content of the resulting TOML file
cat `kdb file user:/tests/storage`
#> # I am the top-most comment relative to my key.
#> # I am in the middle. Just boring.
#> # I am in the line right above my key.
#> key = 1    # This value is very interesting
#> # First file-ending comment
#>
#> # Second file-ending comment. I am the last line of the file.

# Cleanup
kdb rm -r user:/tests/storage
sudo kdb umount user:/tests/storage
```

## Comments in Arrays

Any amount of comments can be placed between array elements or between the first element and the opening brackets.

However, only one comment - an inline comment - can be placed after the last element and the closing brackets.
On reading, the plugin discards any non-inline comments between the last element and the closing brackets.

```sh
# Mount TOML file
sudo kdb mount test_array_comments.toml user:/tests/storage toml type

# Create array elements
kdb set 'user:/tests/storage/array/#0' '1'
kdb set 'user:/tests/storage/array/#1' '2'
kdb set 'user:/tests/storage/array/#2' '3'

# Add inline comment after the array
kdb meta-set 'user:/tests/storage/array' 'comment/#0' ' Inline comment after the array'
kdb meta-set 'user:/tests/storage/array' 'comment/#0/start' '#'
kdb meta-set 'user:/tests/storage/array' 'comment/#0/space' '     '

# Add comments for array elements
kdb meta-set 'user:/tests/storage/array/#0' 'comment/#0' ' Inline comment of first element'
kdb meta-set 'user:/tests/storage/array/#0' 'comment/#0/start' '#'
kdb meta-set 'user:/tests/storage/array/#0' 'comment/#0/space' '    '

kdb meta-set 'user:/tests/storage/array/#0' 'comment/#1' ' Comment preceding the first element'
kdb meta-set 'user:/tests/storage/array/#0' 'comment/#1/space' '    '

kdb meta-set 'user:/tests/storage/array/#0' 'comment/#2' ' Another comment preceding the first element'
kdb meta-set 'user:/tests/storage/array/#0' 'comment/#2/space' '      '

kdb meta-set 'user:/tests/storage/array/#1' 'comment/#0' ' Inline comment of second element'
kdb meta-set 'user:/tests/storage/array/#1' 'comment/#0/space' '    '

kdb meta-set 'user:/tests/storage/array/#1' 'comment/#1' ' Comment preceding the second element'
kdb meta-set 'user:/tests/storage/array/#1' 'comment/#1/space' '      '

kdb meta-set 'user:/tests/storage/array/#2' 'comment/#0' ' Inline comment of the last element'
kdb meta-set 'user:/tests/storage/array/#2' 'comment/#0/space' '     '

# Print the content of the resulting TOML file
cat `kdb file user:/tests/storage`
#> array = [    # Comment preceding the first element
#>       # Another comment preceding the first element
#> 1,    # Inline comment of first element
#>       # Comment preceding the second element
#> 2,    # Inline comment of second element
#> 3     # Inline comment of the last element
#> ]     # Inline comment after the array

# Cleanup
kdb rm -r user:/tests/storage
sudo kdb umount user:/tests/storage
```

# Order

The plugin preserves the file order by the usage of the metakey `order`. When reading a file, the order metakey will be set according to the order as read in the file.
If new keys are added, eg. via `kdb set`, the order of the set key will be set to the next-to-highest order value present in the existing key set.

However, the order is only relevant between elements with the same TOML-parent. For example keys of a simple table are only sorted with respect to each other, not with any keys outside that table. If that table has it's order changed and moves to another position in the file, so will it's subkeys.

When sorting elements under the same TOML-parent, tables (simple and array) will always be sorted after non-table elements, regardless of their order.
With this limitation, we prevent that a newly set key, that is not part of a certain table array/simple table, would be placed after the table declaration, making it a member of that table on a subsequent read.

```sh
# Mount TOML file
sudo kdb mount test_order.toml user:/tests/storage toml type

# Create three keys in reverse alphabetical order under the subkey common
# Additionally, create one key not in the common subkey space

kdb set 'user:/tests/storage/common/c' '0'
kdb set 'user:/tests/storage/common/b' '1'
kdb set 'user:/tests/storage/common/a' '2'
kdb set 'user:/tests/storage/d' '3'

# Print the content of the resulting TOML file
# The keys are ordered as they were set

cat `kdb file user:/tests/storage`
#> common.c = 0
#> common.b = 1
#> common.a = 2
#> d = 3

# Create a simple table for the three keys under `common`
kdb meta-set 'user:/tests/storage/common' 'tomltype' 'simpletable'

# Print the content of the resulting TOML file
cat `kdb file user:/tests/storage`
#> d = 3
#> [common]
#> c = 0
#> b = 1
#> a = 2

# Cleanup
kdb rm -r user:/tests/storage
sudo kdb umount user:/tests/storage
```

In this example, `d` and `common` have the same parent, the file root. This means, they need to be sorted with each other. `d` would be placed before `common` by it's order, since it was set before, and thus, has lesser order.
However, their order never gets compared, since `common` is a simple table and `d` is not, so `d` will get sorted before the table regardless of order.

# Limitations

While the plugin has good capabilities in handling the TOML file format, it currently lacks some features possible with Elektra:

- Sparse arrays are not preserved on writing, they get a continuous array without index holes.
- Values on non-leaf keys are currently not supported, they get discarded. This applies especially to the parent key of the mountpoint.
- Custom metakeys cannot be written by the plugin, so they get discarded.

Additionally, there are some minor limitations related to the TOML file format, mostly related to the preservation of the original file structure:

- On writing, each string is written as a TOML basic string (single or multiline). See [Strings](#Strings)
- Comments and newlines between the last array element and closing brackets are discarded.
- Trailing commas in arrays and inline tables are discarded
- Only spaces in front of comments are preserved.
