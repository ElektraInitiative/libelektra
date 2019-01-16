- infos = Information about the yaypeg plugin is in keys below
- infos/author = René Schwaiger <sanssecours@me.com>
- infos/licence = BSD
- infos/needs = directoryvalue yamlsmith
- infos/provides = storage/yaml
- infos/recommends =
- infos/placements = getstorage
- infos/status = maintained preview experimental unfinished nodoc concept discouraged
- infos/metadata =
- infos/description = This storage plugin uses a PEG based parser to read YAML files

## Introduction

Copy this yaypeg if you want to start a new
plugin written in C++.

## Usage

You can use `scripts/copy-yaypeg`
to automatically rename everything to your
plugin name:

	cd src/plugins
	../../scripts/copy-yaypeg yourplugin

Then update the README.md of your newly created plugin:

- enter your full name+email in `infos/author`
- make sure `status`, `placements`, and other clauses conform to
  descriptions in `doc/CONTRACT.ini`
- update the one-line description above
- and rewrite the rest of this `README.md` to give a great
  explanation of what your plugin does

## Dependencies

None.

## Examples

```sh
# Backup-and-Restore: user/tests/yaypeg

kdb set user/tests/yaypeg/key value
#> Create a new key user/tests/yaypeg/key with string "value"

kdb get /tests/yaypeg/key
#> value
```

## Limitations

None.
