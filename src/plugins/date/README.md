- infos = Information about the date plugin is in keys below
- infos/author = Name <name@libelektra.org>
- infos/licence = BSD
- infos/needs =
- infos/provides =
- infos/recommends =
- infos/placements = presetstorage postgetstorage
- infos/status = recommended productive maintained reviewed conformant compatible coverage specific unittest tested libc final
- infos/metadata =
- infos/description = one-line description of date

## Introduction ##

Copy this date if you want to start a new
plugin written in C.

## Usage ##

You can use `scripts/copy-date`
to automatically rename everything to your
plugin name:

	cd src/plugins
	../../scripts/copy-date yourplugin

Then update the README.md of your newly created plugin:

- enter your name+email in `infos/author`
- make sure `status` and other clauses conform to
  descriptions in `doc/CONTRACT.ini`
- update the one-line description above
- add your plugin in `src/plugins/README.md`
- and rewrite the rest of this `README.md` to give a great
  explanation of what your plugin does

## Dependencies ##

None.

## Examples ##

TBD

## Limitations ##

None.
