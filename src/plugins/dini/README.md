- infos = Information about the dini plugin is in keys below
- infos/author = Markus Raab <markus@libelektra.org>
- infos/licence = BSD
- infos/needs =
- infos/provides = storage/ini
- infos/recommends =
- infos/placements =
- infos/status = recommended productive maintained conformant compatible shelltest tested nodep libc experimental concept
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
sudo kdb mount config.file /examples/dini dump

kdb set /examples/dini/key "legacy value"
#> Using name user/examples/dini/key
#> Create a new key user/examples/dini/key with string "legacy value"

kdb umount /examples/dini


# Mount dini plugin to cascading namespace `/examples/dini`
sudo kdb mount config.file /examples/dini dini

kdb get /examples/dini/key
#> legacy value

kdb set /examples/dini/key2 value
#> Using name user/examples/dini/key2
#> Create a new key user/examples/dini/key2 with string "value"

kdb get /examples/dini/key
#> legacy value

kdb get /examples/dini/key2
#> value

cat `kdb file /examples/dini`

# Undo modifications to the key database
kdb rm -r /examples/dini
sudo kdb umount /examples/dini
```

## Limitations

None.
