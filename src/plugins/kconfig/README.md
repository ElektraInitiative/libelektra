- infos = Information about the kconfig plugin is in keys below
- infos/author = Dardan Haxhimustafa <mail@dardan.im>
- infos/licence = BSD
- infos/needs =
- infos/provides = storage/kconfig
- infos/recommends =
- infos/placements = getstorage setstorage
- infos/status = recommended maintained compatible specific experimental unfinished nodoc concept
- infos/metadata =
- infos/description = Reads and writes the KConfig INI format

## Introduction

This plugin can be used to parse and serialize a [KConfig](https://cgit.kde.org/kconfig.git) INI file.

Information about the syntax:

- Files are expected to be encoded in `UTF-8`.
- Empty lines are ignored.
- Lines that start with a `#` character are considered comments. Comments are ignored too.
- Configurations consist of groups and keys. Only keys can have values.
- Key names can't start with a `[` character
- Keys can contain spaces and any special characters except `=`.
- If a key has a value, then it will be followed with an `=` symbol and then the value will be read until the end of the line.
  The white space characters around the `=` symbol are ignored.
- In values, the following escape sequences can be used:
  - `\n` and `\r` are mapped to newline
  - `\t` is mapped to tab
  - `\\` is mapped to `\`
- Values can contain any character from the `UTF-8` set except for newline and `\` followed by an invalid escape sequence.
- Keys can be localized. The locale is surrounded with `[` and `]` and cannot start with `$`.
- Same key names can be used multiple times if it has different locales. The following example is valid:
  ```
  greeting[en] = Hello
  greeting[de] = Hallo
  ```
- Keys can have metadata. Those are one byte long, start with `$` and are surrounded with `[` and `]`.
- The same key name can't be used multiple times with different metadata (different to locales). The following example is invalid:
  ```
  key.name[$a] = Something
  key.name[$i] = Something else
  ```
- Group names begin have a `[` symbol at the beginning of a line and every key that follows them is part of this group (until the next
  group is declared)

An example of how a valid config file might look like:

```
[group][subgroup]
key.name[en][$i][$e]=Key Value
key.name[de]=Key Wert
```

And how it will be represented in kdb:

```
keyNew (PREFIX "group/subgroup/key.name[en]", KEY_VALUE, "Key Value", KEY_META, "kconfig", "ie", KEY_END)
keyNew (PREFIX "group/subgroup/key.name[de]", KEY_VALUE, "Key Wert", KEY_END)
```

## Usage

The following example shows you how you can read data using this plugin.

```sh
# Mount the plugin to the cascading namespace `/tests/kconfig`
sudo kdb mount configrc /tests/kconfig kconfig

# Manually add a key-value pair to the database
mkdir -p "$(dirname "$(kdb file user:/tests/kconfig)")"
echo 'key=Value' > "$(kdb file user:/tests/kconfig)"

# Retrieve the new value
kdb get /tests/kconfig/key
#> Value

# Set the value to Example
kdb set user:/tests/kconfig/key Example

# Verify that the value has changed in the file too
cat `kdb file user:/tests/kconfig`
#> key=Example

# Manually add a gorup to the database
echo '[group][subgroup]' >> "$(kdb file user:/tests/kconfig)"

# Manually add a key that contains metas to that group
echo 'key.name[$a][$i]=New Value' >> "$(kdb file user:/tests/kconfig)"

# Retrieve the new value
kdb get /tests/kconfig/group/subgroup/key.name
#> New Value

# Retrieve the meta values
kdb meta get /tests/kconfig/group/subgroup/key.name kconfig
#> ai

# Manually add a group and a localized key
echo '[localized keys]' >> `kdb file user:/tests/kconfig`
echo 'greeting[en]=Hello' >> `kdb file user:/tests/kconfig`
echo 'greeting[de]=Hallo' >> `kdb file user:/tests/kconfig`

# Retrieve the english greeting
kdb get '/tests/kconfig/localized keys/greeting[en]'
#> Hello

# Retrieve the german greeting
kdb get '/tests/kconfig/localized keys/greeting[de]'
#> Hallo

# Undo modifications to the database
sudo kdb umount /tests/kconfig
```

## Limitations

- Comments from the file are discarded on save (same as the default KConfig functionality)
- No validation for meta values or locale codes
