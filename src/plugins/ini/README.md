- infos = Information about ini plugin is in keys below
- infos/author = Thomas Waser <thomas.waser@libelektra.org>
- infos/licence = BSD
- infos/provides = storage/ini
- infos/needs =
- infos/placements = getstorage setstorage
- infos/status = maintained unittest shelltest nodep libc configurable 1000
- infos/metadata = order
- infos/description = storage plugin for ini files

## Introduction

This plugin allows read/write of INI files. INI files consist of simple
key value pairs of the form `key = value`. Additionally keys can be
categorized into different sections. Sections must be enclosed in "[]",
for example "[section]". Each section is converted into a directory key
(without value) and keys below the section are located below the section
key. If the same section appears multiple times, the keys of all sections
with the same name are merged together under the section key.

The plugin is feature rich and customizable (+1000 in status)

## Usage

If you want to add an ini file to the global key database, simply use mount:

```sh
sudo kdb mount file.ini user/example ini
```

Then you can modify the contents of the ini file using set:

```sh
kdb set user/example/key value
#> Create a new key user/example/key with string "value"
kdb set user/example/section
#> Create a new key user/example/section with null value
kdb set user/example/section/key value
#> Create a new key user/example/section/key with string "value"
```

Find out which file you modified:

```sh
kdb file user/example
# STDOUT-REGEX: .+/file.ini

# Undo modifications
kdb rm -r user/example
sudo kdb umount user/example
```

## Comments

The ini plugin supports the use of comments. Comment lines start with
a ';' or a '#'. Comments are put into the comment metadata of the key
following the comment. This can be either a section key or a normal key.
When creating new comments (e.g. via `kdb setmeta`) you can prefix
your comment with the comment indicator of your choice (';' or '#')
which will be used when writing the comment to the file. If the comment
is not prefixed with a comment indicator, the ini plugin will use the
character defined by the `comment` option, or default to '#'.

## Multi-Line Support

The ini plugin supports multiline ini values. Continuations of previous values
have to start with whitespace characters.

For example consider the following ini file:

```ini
key1 = value1
key2 = value2
	with continuation
	lines
```

This would result in a KeySet containing two keys. One key named `key1` with the value `value1` and
another key named `key2` with the value `value2\nwith continuation\nlines`.

By default this feature is enabled. The default continuation character is tab (`\t`).
If you want to use another character, then please specify the configuration option `linecont`.

## Arrays

The ini plugin handles repeating keys by turning them into an elektra array when the `array` config is set.

For example a ini file looking like:

```ini
[sec]
a = 1
a = 2
a = 3
a = 4
```

will be interpreted as

```
/sec
/sec/a
/sec/a/#0
/sec/a/#1
/sec/a/#2
/sec/a/#3
```

The following example shows how you can store and retrieve array values using the `ini` plugin.

```sh
# Mount the INI plugin with array support
sudo kdb mount config.ini user/examples/ini ini array=true

# Add an array storing song titles
kdb set user/examples/ini/songs/#0 "Non-Zero Possibility"
kdb set user/examples/ini/songs/#1 "Black Art Number One"
kdb set user/examples/ini/songs/#2 "A Story Of Two Convicts"

# Check if INI saved all array entries
kdb ls user/examples/ini/songs
#> user/examples/ini/songs
#> user/examples/ini/songs/#0
#> user/examples/ini/songs/#1
#> user/examples/ini/songs/#2

# Retrieve an array item
kdb get user/examples/ini/songs/#2
#> A Story Of Two Convicts

# Check the file written by the INI plugin
kdb file user/examples/ini | xargs cat
#> songs = Non-Zero Possibility
#> songs = Black Art Number One
#> songs = A Story Of Two Convicts

# Undo modifications
kdb rm -r user/examples/ini
sudo kdb umount user/examples/ini
```

## Sections

The ini plugin supports 3 different sectioning modes (via `section=`):

- `NONE` sections wont be printed as `[Section]` but as part of the key name `section/key`
- `NULL` only binary keys will be printed as `[Section]`
- `ALWAYS` sections will be created automatically. This is the default setting:

```sh
sudo kdb mount /empty.ini dir/empty ini
kdb set dir/empty/a/b ab
kdb get dir/empty/a       # <-- key is suddenly here
cat empty.ini
#> [a]
#> b = ab

# Undo modifications
kdb rm -r dir/empty
sudo kdb umount dir/empty
```

By changing the option `section` you can suppress the automatic creation of keys.
E.g., if you use `NULL` instead you only get a section if you explicitly create it:

```sh
sudo kdb mount /empty.ini dir/empty ini section=NULL
kdb set dir/empty/a/b ab
kdb get dir/empty/a       # no key here
# RET: 1
cat empty.ini
#> a/b = ab
kdb rm dir/empty/a/b
kdb set dir/empty/a    # create section first
kdb set dir/empty/a/b ab
cat empty.ini
#> [a]
#> b = ab

# Undo modifications
kdb rm -r dir/empty
sudo kdb umount dir/empty
```

### Merge Sections

The ini plugin supports merging duplicated section entries when the `mergesections` config is set.
The keys will be appended to the first occurrence of the section key.

## Ordering

The ini plugin preserves the order.
Inserted subsections get appended to the corresponding parent section and new sections by name.

Example:

```sh
sudo kdb mount test.ini /examples/ini ini

cat > `kdb file /examples/ini` <<EOF \
[Section1]\
key1 = val1\
[Section3]\
key3 = val3\
EOF

kdb file /examples/ini | xargs cat
#> [Section1]
#> key1 = val1
#> [Section3]
#> key3 = val3

kdb set /examples/ini/Section1/Subsection1/subkey1 subval1
kdb set /examples/ini/Section2/key2 val2
kdb file /examples/ini | xargs cat
#> [Section1]
#> key1 = val1
#> [Section1/Subsection1]
#> subkey1 = subval1
#> [Section2]
#> key2 = val2
#> [Section3]
#> key3 = val3

# Undo modifications
kdb rm -r /examples/ini
sudo kdb umount /examples/ini
```

