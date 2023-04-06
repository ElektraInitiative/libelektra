- infos = Information about the ansible plugin is in keys below
- infos/author = Author Name <elektra@libelektra.org>
- infos/licence = BSD
- infos/needs =
- infos/provides =
- infos/recommends =
- infos/placements = prerollback rollback postrollback getresolver pregetstorage getstorage procgetstorage postgetstorage setresolver presetstorage setstorage precommit commit postcommit
- infos/status = recommended productive maintained reviewed conformant compatible coverage specific unittest shelltest tested nodep libc configurable final preview memleak experimental difficult unfinished old nodoc concept orphan obsolete discouraged -1000000
- infos/metadata =
- infos/description = one-line description of ansible

## Introduction

Copy this ansible if you want to start a new
plugin written in C++.

## Installation

See [installation](/doc/INSTALL.md).
The package is called `libelektra5-experimental`.

## Usage

You can use `scripts/copy-ansible`
to automatically rename everything to your
plugin name:

```bash
cd src/plugins
../../scripts/copy-ansible yourplugin
```

Then update the `README.md` of your newly created plugin:

- enter your full name+email in `infos/author`
- make sure `status`, `placements`, and other clauses conform to
  descriptions in `doc/CONTRACT.ini`
- update the one-line description above
- write an introduction what your plugin does
- add your plugin in `src/plugins/README.md`
- and rewrite the rest of this `README.md` to give a great
  explanation of what your plugin does

## Plugin Configuration

None.

## Dependencies

None.

## Examples

```sh
# Backup-and-Restore: user:/tests/ansible

kdb set user:/tests/ansible/key value
#> Create a new key user:/tests/ansible/key with string "value"

kdb get /tests/ansible/key
#> value
```

## Limitations

None.
