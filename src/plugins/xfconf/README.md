- infos = Information about the xfconf plugin is in keys below
- infos/author = Author Name <elektra@libelektra.org> Richard Stöckl <e11908080@student.tuwien.ac.at>
- infos/licence = BSD
- infos/needs =
- infos/provides = storage/xfconf
- infos/recommends =
- infos/placements = prerollback rollback postrollback getresolver pregetstorage getstorage procgetstorage postgetstorage setresolver presetstorage setstorage precommit commit postcommit
- infos/status = recommended productive maintained reviewed conformant compatible coverage specific unittest shelltest tested libc configurable final preview memleak experimental difficult unfinished old nodoc concept orphan obsolete discouraged -1000000
- infos/metadata =
- infos/description = one-line description of xfconf

## Introduction

Copy this xfconf if you want to start a new
plugin written in C.

## Installation

See [installation](/doc/INSTALL.md).
The package is called `libelektra5-experimental`.

## Usage

You can use `scripts/copy-xfconf`
to automatically rename everything to your
plugin name:

```bash
cd src/plugins
../../scripts/copy-xfconf yourplugin
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
# Backup-and-Restore: user:/tests/xfconf

kdb set user:/tests/xfconf/key value
#> Create a new key user:/tests/xfconf/key with string "value"

kdb get /tests/xfconf/key
#> value
```

## Limitations

None.
