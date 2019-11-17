- infos = Information about the kconfig plugin is in keys below
- infos/author = Author Name <elektra@libelektra.org>
- infos/licence = BSD
- infos/needs =
- infos/provides = storage/kconfig storage kconfig
- infos/recommends =
- infos/placements = getstorage setstorage
- infos/status = recommended maintained compatible specific experimental unfinished nodoc concept
- infos/metadata =
- infos/description = one-line description of kconfig

## Introduction

Copy this kconfig if you want to start a new
plugin written in C++.

## Usage

You can use `scripts/copy-kconfig`
to automatically rename everything to your
plugin name:

```bash
cd src/plugins
../../scripts/copy-kconfig yourplugin
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
# Backup-and-Restore: user/tests/kconfig

kdb set user/tests/kconfig/key value
#> Create a new key user/tests/kconfig/key with string "value"

kdb get /tests/kconfig/key
#> value
```

## Limitations

None.
