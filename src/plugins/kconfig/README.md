- infos = Information about the kconfig plugin is in keys below
- infos/author = Dardan Haxhimustafa <mail@dardan.im>
- infos/licence = BSD
- infos/needs =
- infos/provides = storage/kconfig
- infos/recommends =
- infos/placements = getstorage setstorage
- infos/status = recommended maintained compatible specific experimental unfinished nodoc concept
- infos/metadata =
- infos/description = Reads and writes the KConfig ini format

## Introduction

This plugin can be used to parse a KConfig ini file into a KeySet and save a given KeySet to such a file.

The main differences between KConfig parser and default ini parser are:

- Comments can only start with character `#`, `;`
- Keys can contain spaces and special characters (including `;`) except from the main special characters (`[`, `]`, `=`, `#`and `$`)
- Can contain multiple keys with different locales (`keyName[en]` and `keyName[de]`)
- Cannot contain multiple keys with different medatada (either `keyname[$a]` or `keyname[$b]`)

## Usage

The following example shows you how you can read and write data using this plugin.

```sh
# Mount the plugin to the cascading namespace `/tests/kconfig`
sudo kdb mount configrc /tests/kconfig kconfig

# Manually add a key-value pair to the database
printf 'key=Value' > `kdb file /tests/Kconfig`

# Retrieve the new value
kdb get /tests/kconfig/key
#> Value

# Undo modifications to the database
sudo kdb umount /tests/kconfig
```

## Limitations

- Comments from file are discarded on save (save as the default KConfig functionality)
- No validation for meta values or locale codes
