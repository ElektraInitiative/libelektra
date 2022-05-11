- infos = Storage Plugin for the LDIF format
- infos/author = Burkhard Hampl <e11776165@student.tuwien.ac.at>, Richard Stöckl <e11908080@student.tuwien.ac.at>
- infos/licence = BSD
- infos/needs =
- infos/provides = storage/ldif
- infos/recommends =
- infos/placements = getstorage
- infos/status = nodep libc configurable limited
- infos/metadata =
- infos/description = deserialize LDIF files

## Introduction

Copy this ldif if you want to start a new
plugin written in C.

## Installation

See [installation](/doc/INSTALL.md).
The package is called `libelektra5-experimental`.

## Usage

You can use `scripts/copy-ldif`
to automatically rename everything to your
plugin name:

```bash
cd src/plugins
../../scripts/copy-ldif yourplugin
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

OpenLDAP

## Examples

```sh
# Backup-and-Restore: user:/tests/ldif

kdb set user:/tests/ldif/key value
#> Create a new key user:/tests/ldif/key with string "value"

kdb get /tests/ldif/key
#> value
```

## Limitations

None.
