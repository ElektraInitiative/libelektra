- infos = Information about the hexcolor plugin is in keys below
- infos/author = Philipp Gackstatter <philipp.gackstatter@student.tuwien.ac.at>
- infos/licence = BSD
- infos/needs =
- infos/provides =
- infos/recommends =
- infos/placements = presetstorage
- infos/status = maintained unittest nodep
- infos/metadata = check/hexcolor
- infos/description = Validation of Hexcolors

## Introduction

Copy this hexcolor if you want to start a new
plugin written in C.

## Usage

You can use `scripts/copy-hexcolor`
to automatically rename everything to your
plugin name:

```bash
cd src/plugins
../../scripts/copy-hexcolor yourplugin
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
# Backup-and-Restore: user/tests/hexcolor

kdb set user/tests/hexcolor/key value
#> Create a new key user/tests/hexcolor/key with string "value"

kdb get /tests/hexcolor/key
#> value
```

## Limitations

None.
