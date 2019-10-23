- infos = Information about the ini plugin is in keys below
- infos/author = Thomas Waser <thomas.waser@libelektra.org>
- infos/licence = BSD
- infos/needs =
- infos/provides = storage/ini
- infos/recommends = binary
- infos/placements = getstorage setstorage
- infos/status = maintained unittest shelltest nodep libc configurable 1000
- infos/metadata = order
- infos/description = storage plugin for ini files

## Introduction

[lock/a/lock]
This plugin allows Elektra's users to read and write INI files.
INI files consist of simple key value pairs of the form `key=value`.
Additionally keys can be categorized into different sections.
Sections must be enclosed in "[]", for example "[section]".
Each section is converted into a directory key
(without value) and keys below the section are located below the section
key. If the same section appears multiple times, the keys of all sections
with the same name are merged together under the section key.

The plugin is feature rich and customizable (+1000 in status)

## Usage

If you want to add an ini file to the global key database, simply use mount:

```sh
sudo kdb mount file.ini user/tests/ini ini
```

Then you can modify the contents of the ini file using set:

```sh
kdb set user/tests/ini/key value
#> Create a new key user/tests/ini/key with string "value"
kdb set user/tests/ini/section
#> Create a new key user/tests/ini/section with null value
kdb set user/tests/ini/section/key value
#> Create a new key user/tests/ini/section/key with string "value"
```

Find out which file you modified:

```sh
kdb file user/tests/ini
# STDOUT-REGEX: .+/file.ini

# Undo modifications
kdb rm -r user/tests/ini
sudo kdb umount user/tests/ini
```

## Comments

The ini plugin supports the use of comments. Comment lines start with
a ';' or a '#'. Comments are put into the comment metadata of the key
following the comment. This can be either a section key or a normal key.
When creating new comments (e.g. via `kdb meta-set`) you can prefix
your comment with the comment indicator of your choice (';' or '#')
which will be used when writing the comment to the file. If the comment
is not prefixed with a comment indicator, the ini plugin will use the
character defined by the `comment` option, or default to '#'.

## Multi-Line Support

The ini plugin supports multiline ini values. Continuations of previous values
have to start with whitespace characters.

For example consider the following ini file:

```ini
key1=value1
key2=value2
	with continuation
	lines
```

This would result in a KeySet containing two keys. One key named `key1` with the value `value1` and
another key named `key2` with the value `value2\nwith continuation\nlines`.

By default this feature is enabled. The default continuation character is tab (`\t`).
If you want to use another character, then please specify the configuration option `linecont`.

## Arrays

The ini plugin handles repeating keys by turning them into an elektra array when the `array` config is set.

For example an ini file looking like:

```ini
[sec]
a=1
a=2
a=3
a=4
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
sudo kdb mount config.ini user/tests/ini ini array=true

# Add an array storing song titles
kdb set user/tests/ini/songs/#0 "Non-Zero Possibility"
kdb set user/tests/ini/songs/#1 "Black Art Number One"
kdb set user/tests/ini/songs/#2 "A Story Of Two Convicts"

# Check if INI saved all array entries
kdb ls user/tests/ini/songs
#> user/tests/ini/songs
#> user/tests/ini/songs/#0
#> user/tests/ini/songs/#1
#> user/tests/ini/songs/#2

# Retrieve an array item
kdb get user/tests/ini/songs/#2
#> A Story Of Two Convicts

# Check the file written by the INI plugin
kdb file user/tests/ini | xargs cat
#> songs=Non-Zero Possibility
#> songs=Black Art Number One
#> songs=A Story Of Two Convicts

# Undo modifications
kdb rm -r user/tests/ini
sudo kdb umount user/tests/ini
```

## Binary Data

By default the INI plugin does not support binary data. You can use the [Base64 plugin](../base64/) to remove this limitation.

```sh
# Mount INI and recommended plugin Base64
# We add the required plugin `base64` (which provides `binary`) at the end too,
# since otherwise this command leaks memory.
sudo kdb mount --with-recommends config.ini user/tests/ini ini base64

# Add empty binary value
printf 'nothing="@BASE64"\n' > `kdb file user/tests/ini`
# Copy binary data
kdb cp system/elektra/modules/ini/exports/get user/tests/ini/binary
# Add textual data
kdb set user/tests/ini/text 'Na na na na na na na na na na na na na na na na Batman!'

# Print empty binary value
kdb get user/tests/ini/nothing
#>

# Print binary data
kdb get user/tests/ini/binary
# STDOUT-REGEX: ^(\\x[0-9a-f]{1,2})+$

# Print textual data
kdb get user/tests/ini/text
#> Na na na na na na na na na na na na na na na na Batman!

kdb rm -r user/tests/ini
sudo kdb umount user/tests/ini
```

## Metadata

The INI plugin also supports to store arbitrary metadata in comments after the `@META` declarative.

```sh
sudo kdb mount config.ini user/tests/ini ini

# Add a new key and some metadata
kdb set user/tests/ini/brand new
kdb meta-set user/tests/ini/brand description "The Devil And God Are Raging Inside Me"
kdb meta-set user/tests/ini/brand rationale "Because I Love It"

# The plugin stores metadata as comments inside the INI file
kdb file user/tests/ini | xargs cat
#> #@META description = The Devil And God Are Raging Inside Me
#> #@META rationale = Because I Love It
#> brand=new

# Retrieve metadata
kdb meta-ls user/tests/ini/brand | grep -v 'internal'
# rationale
# description

kdb meta-get user/tests/ini/brand description
#> The Devil And God Are Raging Inside Me
kdb meta-get user/tests/ini/brand rationale
#> Because I Love It

# The plugin ignores some metadata such as `comment`!
kdb meta-set user/tests/ini/brand comment "Where Art Thou?"
kdb meta-get user/tests/ini/brand comment
# STDERR: Metakey not found
# RET: 2

kdb rm -r user/tests/ini
sudo kdb umount user/tests/ini
```

## Sections

The ini plugin supports 3 different sectioning modes (via `section=`):

- `NONE` sections wont be printed as `[Section]` but as part of the key name `section/key`
- `NULL` only empty keys will be printed as `[Section]`
- `ALWAYS` sections will be created automatically. This is the default setting:

```sh
sudo kdb mount /empty.ini dir/tests ini
kdb set dir/tests/a/b ab
kdb get dir/tests/a       # <-- key is suddenly here
cat empty.ini
#> [a]
#> b=ab

# Undo modifications
kdb rm -r dir/tests
sudo kdb umount dir/tests
```

By changing the option `section` you can suppress the automatic creation of keys.
E.g., if you use `NULL` instead you only get a section if you explicitly create it:

```sh
sudo kdb mount /empty.ini dir/tests ini section=NULL
kdb set dir/tests/a/b ab
kdb get dir/tests/a       # no key here
# RET: 11
cat empty.ini
#> a/b=ab
kdb rm dir/tests/a/b
kdb set dir/tests/a    # create section first
kdb set dir/tests/a/b ab
cat empty.ini
#> [a]
#> b=ab

# Undo modifications
kdb rm -r dir/tests
sudo kdb umount dir/tests
```

### Merge Sections

The ini plugin supports merging duplicated section entries when the `mergesections` config is set.
The keys will be appended to the first occurrence of the section key.

## Ordering

The ini plugin preserves the order.
Inserted subsections get appended to the corresponding parent section and new sections by name.

Example:

```sh
sudo kdb mount test.ini /tests/ini ini

cat > `kdb file /tests/ini` <<EOF \
[Section1]\
key1=val1\
[Section3]\
key3=val3\
EOF

kdb file /tests/ini | xargs cat
#> [Section1]
#> key1=val1
#> [Section3]
#> key3=val3

kdb set /tests/ini/Section1/Subsection1/subkey1 subval1
kdb set /tests/ini/Section2/key2 val2
kdb file /tests/ini | xargs cat
#> [Section1]
#> key1=val1
#> [Section1/Subsection1]
#> subkey1=subval1
#> [Section2]
#> key2=val2
#> [Section3]
#> key3=val3

# Undo modifications
kdb rm -r /tests/ini
sudo kdb umount /tests/ini
```

The current implementation of the ordering sometimes breaks compatibility
with the cache plugin [(see ini bug)](https://github.com/ElektraInitiative/libelektra/issues/2592).

## Special Characters

The INI plugin also supports values and keys containing delimiter characters (`=`) properly.

```sh
sudo kdb mount test.ini user/tests/ini ini

printf '[section1]\n'    >  `kdb file user/tests/ini`
printf 'hello=world\n' >> `kdb file user/tests/ini`

kdb get user/tests/ini/section1/hello
#> world

kdb set user/tests/ini/section1/x=x 'a + b=b + a'
kdb get user/tests/ini/section1/x=x
#> a + b=b + a

# Undo modifications
kdb rm -r user/tests/ini
sudo kdb umount user/tests/ini
```
