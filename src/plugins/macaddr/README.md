- infos = Information about the macaddr plugin is in keys below
- infos/author = Thomas Bretterbauer <e01306821@student.tuwien.ac.at>
- infos/licence = BSD
- infos/needs =
- infos/provides = check
- infos/recommends =
- infos/placements = presetstorage
- infos/status = maintained unittest tested
- infos/metadata = check/macaddr
- infos/description = one-line description of macaddr

## Introduction

Copy this macaddr if you want to start a new
plugin written in C.

## Usage

You can use `scripts/copy-macaddr`
to automatically rename everything to your
plugin name:

```bash
cd src/plugins
../../scripts/copy-macaddr yourplugin
```

Then update the README.md of your newly created plugin:

- enter your full name+email in `infos/author`
- make sure `status`, `placements`, and other clauses conform to
  descriptions in `doc/CONTRACT.ini`
- update the one-line description above
- add your plugin in `src/plugins/README.md`
- and rewrite the rest of this `README.md` to give a great
  explanation of what your plugin does

## Dependencies

None.

## Examples

```sh
# Backup-and-Restore: user/tests/mac

# Mount `macaddr` plugin
kdb mount config.dump /tests/mac dump macaddr

# Check the validity of the MAC address stored in `/tests/mac`
kdb setmeta /tests/mac check/macaddr

# Setting a MAC address
kdb set /tests/mac 00:A0:C9:14:C8:29
# RET: 0

kdb umount /tests/mac
```

## Limitations

None.
