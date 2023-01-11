- infos = Information about the template plugin is in keys below
- infos/author = Author Name <elektra@libelektra.org>
- infos/licence = BSD
- infos/needs =
- infos/provides =
- infos/recommends =
- infos/placements = prerollback rollback postrollback getresolver pregetstorage getstorage procgetstorage postgetstorage setresolver presetstorage setstorage precommit commit postcommit
- infos/status = recommended productive maintained compatible tested/unit tested/shell nodep configurable memleak experimental concept obsolete discouraged -1000000
- infos/features/storage = limited
- infos/metadata =
- infos/description = one-line description of template

## Introduction

Copy this template if you want to start a new
plugin written in C.

## Installation

See [installation](/doc/INSTALL.md).
The package is called `libelektra5-experimental`.

## Usage

You can use `scripts/copy-template`
to automatically rename everything to your
plugin name:

```bash
cd src/plugins
../../scripts/copy-template yourplugin
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
# Backup-and-Restore: user:/tests/template

kdb set user:/tests/template/key value
#> Create a new key user:/tests/template/key with string "value"

kdb get /tests/template/key
#> value
```

## Limitations

None.
