- infos = Information about the dini plugin is in keys below
- infos/author = Markus Raab <markus@libelektra.org>
- infos/licence = BSD
- infos/needs =
- infos/provides = storage/ini
- infos/recommends =
- infos/placements = getstorage setstorage
- infos/status = compatible shelltest tested nodep libc
- infos/metadata =
- infos/description = default INI plugin

## Introduction

The dini plugin is proposed to become the new default plugin.
It pulls in all deps as needed for INI.

To be compatible with legacy installations, it will
use the dump plugin to parse dump configuration files.

## Usage

```bash
kdb change-storage-symlink dini
```

## Dependencies

- Loads the `dump` plugin at run-time (optional, only for parsing)
- Loads the `ini` plugin and its dependencies

## Examples

```sh
# Create a need for legacy support
sudo kdb mount config.file user/tests/dini dump

kdb set user/tests/dini/key "legacy value"
#> Create a new key user/tests/dini/key with string "legacy value"

kdb umount user/tests/dini

# Mount dini plugin to `user/tests/dini`
sudo kdb mount config.file user/tests/dini dini

kdb get user/tests/dini/key
#> legacy value

kdb set user/tests/dini/key2 value
#> Create a new key user/tests/dini/key2 with string "value"

kdb get user/tests/dini/key
#> legacy value

kdb get user/tests/dini/key2
#> value

cat `kdb file user/tests/dini`

# Undo modifications to the key database
kdb rm -r user/tests/dini
sudo kdb umount user/tests/dini
```

## Limitations

None.
