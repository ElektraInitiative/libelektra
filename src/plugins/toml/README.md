- infos = Information about the toml plugin is in keys below
- infos/author = Jakob Fischer <jakobfischer93@gmail.com>
- infos/licence = BSD
- infos/provides = storage/toml
- infos/needs = null base64 directoryvalue
- infos/recommends = type
- infos/placements = getstorage setstorage
- infos/status = experimental unfinished nodoc
- infos/metadata = order comment/# comment/#/start comment/#/space type tomltype origvalue
- infos/description = This storage plugin reads and writes TOML files, using Flex and Bison.

# TODO: Documentation

# NULL/empty keys
The plugin supports null and empty keys with the help of the [null](../null/README.md) plugin.

# Requirements:

    - The plugin needs Flex (TODO: min version) and Bison (minimal version 3).

# Strings
Although the plugin can read any kind of TOML string (bare, basic, literal, basic multiline, literal multiline) it will write back all non-bare strings as basic strings (and it's multiline version of it).
Therefore, any string set with `kdb set` must be treated as a basic string and possible escape sequences and special meanings of quotation characters must be taken care of.

Example
```
# Mount TOML file
sudo kdb mount test_strings.toml user/tests/storage toml type

# setting a string containing a newline escape sequence
kdb set 'user/tests/storage/string' 'I am a basic string\not a literal one.'

kdb get 'user/tests/storage/string'
# > I am a basic string
# > ot a literal one


# setting the string again, but escape the backslash with another backslash so that the \not gets written out
kdb set 'user/tests/storage/string' 'I am a basic string\\not a literal one.'

kdb get 'user/tests/storage/string'
# > I am a basic string\not a literal one

# Cleanup
kdb rm -r user/tests/storage
sudo kdb umount user/tests/storage
```

# TOML specific structures
TOML specific structures are represented by the metakey `tomltype` on a certain key.
It will be set when the TOML plugin reads a TOML structure from a file. Additionally, this metakey can be set by the user, if they want a certain TOML structure to be written.
No automatic inference of this metakey is done on writing.

## Simple Tables
TOML's simple tables are represented by setting the `tomltype` metakey to `simpletable`.
Example
```
# Mount TOML file
sudo kdb mount test_strings.toml user/tests/storage toml type

# Create three keys, which are all a subkey of `common`,
# but we have no `common` simple table key yet
kdb set 'user/tests/storage/common/a' '0'
kdb set 'user/tests/storage/common/b' '1'
kdb set 'user/tests/storage/common/c' '2'

# Print the content of the resulting TOML file
cat `kdb file user/tests/storage`
#> common.a = 0
#> common.b = 1
#> common.c = 2

# Create a simple table key
kdb meta-set 'user/tests/storage/common' 'tomltype' 'simpletable'

# Print the content of the resulting TOML file
cat `kdb file user/tests/storage`
#> [common]
#> a = 0
#> b = 1
#> c = 2

# Cleanup
kdb rm -r user/tests/storage
sudo kdb umount user/tests/storage
```

## Table Arrays
Table arrays are represented by setting the `tomltype` metakey to `tablearray`. It is not required to also set the array metakey, since the plugin will set the metakey, if it is missing.

```
# Mount TOML file
sudo kdb mount test_strings.toml user/tests/storage toml type

# Create a table array containing two entries, each with a key `a` and `b`
kdb meta-set 'user/tests/storage/tablearray' 'tomltype` `tablearray`
kdb set 'user/tests/storage/tablearray/#0/a' '1'
kdb set 'user/tests/storage/tablearray/#0/b' '2'

kdb set 'user/tests/storage/tablearray/#1/a' '3'
kdb set 'user/tests/storage/tablearray/#1/b' '4'

# Print the highest index of the table array
kdb meta-get 'user/tests/storage/tablearray' 'array'
#> #1

# Print the content of the resulting TOML file
cat `kdb file user/tests/storage`
#> [[tablearray]]
#> a = 1
#> b = 2
#> [[tablearray]]
#> a = 3
#> b = 4

# Print the content of the resulting TOML file
cat `kdb file user/tests/storage`
#> [common]
#> a = 0
#> b = 1
#> c = 2

# Cleanup
kdb rm -r user/tests/storage
sudo kdb umount user/tests/storage
```

## Inline Tables

## Arrays
Arrays are recognized by the `array` metakey. On writing, the plugin will detect arrays automatically and set the appropriate metakey if it is missing.
TODO: Example

# Order
The plugin preserves the file order by the usage of the metakey `order`. When reading a file, the order metakey will be set according to the order as read in the file.
If you add new keys by eg. `kdb set` the order of the set key will be set to the next-to-highest order value present in the existing key set.
However, the order is only relevant between elements with the same TOML-parent. For example keys of a simple table are only sorted with each other, not with any keys outside that table. If that table has it's order changed and moves to another position in the file, so will it's subkeys.

There is an additional limitation on ordering for prevention of messing up resulting TOML-file semantics on writing.
When sorting elements under the same TOML-parent, tables (simple and array) will always be sorted after non-table elements, regardless of their order.
With this limitation, we prevent that a newly set key, that is not part of a certain table array/simple table, would be placed after the table declaration, making it a member of that table on a subsequent read.

Example
```
# Mount TOML file
sudo kdb mount test_strings.toml user/tests/storage toml type

# Create three keys in reverse alphabetical order under the subkey common
# Additionally, create one key not in the common subkey space
kdb set 'user/tests/storage/common/c' '0'
kdb set 'user/tests/storage/common/b' '1'
kdb set 'user/tests/storage/common/a' '2'
kdb set 'user/tests/storage/d' '3'

# Print the content of the resulting TOML file
# The keys are ordered as they were set
cat `kdb file user/tests/storage`
#> common.c = 2
#> common.b = 1
#> common.a = 0
#> d = 3

# Create a simple table for the three keys under `common`
kdb meta-set 'user/tests/storage/common' 'tomltype' 'simpletable'

# Print the content of the resulting TOML file
cat `kdb file user/tests/storage`
#> d = 3
#> [common]
#> c = 0
#> b = 1
#> a = 2

# Cleanup
kdb rm -r user/tests/storage
sudo kdb umount user/tests/storage
```
In this example, `d` and `common` have the same parent (the file root). This means, they need to be sorted with each other. `d` would be placed before `common` by it's order (since it was set before, and thus, has lesser order).
Their order however never gets compared, since `common` is a simple table and `d` is not, so `d` will get sorted before the table regardless of order.

TODO: Maybe explain better/more clear/shorter?

# Limitations

	- Comments and newlines between the last array element and closing brackets are discarded.
	- Trailing commas in arrays and inline tables are discarded
	- Only spaces in front of comments are preserved.
	- Currently, Elektra's sparse arrays are not preserved on writing (they get a continuous array without index holes).

# TODOs

	- Write documentation
	- Change storage of toml specific structures (change tomltype metakey)
	- Correct interaction with other plugins, especially directory value
		- directoryvalue seems to be in conflict with the toml plugin
		- eg on writing, when having a value on a simpletable, the dirval plugin steals all metakeys of the simpletable
		- and the simpletable key loses all  it's previous metakeys, like order and tomltype, resulting in incorrectly written TOML files
		- (this happens on the KeySet the TOML plugin receives on kdbSet call, without any changes made by the TOML plugin)
	- Check, how used plugins are chained (I heard something about wrapper plugin?). Order of filter plugins may be relevant in writeScalar()
	- Error checks in write.c
	- Handle writing of sparse arrays somehow (maybe there is a plugin), otherwise create special meaning string to write in TOML file (eg '!ELEKTRA_NO_ELEMENT!')?
	- Don't discard trailing array comments
	- Maybe use date plugin
	- Make distinction on writing basic and literal strings
