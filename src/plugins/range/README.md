- infos = Information about the range plugin is in keys below
- infos/author = Author Name <elektra@libelektra.org>
- infos/licence = BSD
- infos/needs =
- infos/provides =
- infos/recommends =
- infos/placements = presetstorage postgetstorage
- infos/status = maintained conformant compatible coverage specific unittest tested libc preview experimental unfinished nodoc
- infos/metadata =
- infos/description = one-line description of range

## Introduction ##

Copy this range if you want to start a new
plugin written in C.

## Usage ##

You can use `scripts/copy-range`
to automatically rename everything to your
plugin name:

	cd src/plugins
	../../scripts/copy-range yourplugin

Then update the README.md of your newly created plugin:

- enter your full name+email in `infos/author`
- make sure `status` and other clauses conform to
  descriptions in `doc/CONTRACT.ini`
- update the one-line description above
- add your plugin in `src/plugins/README.md`
- and rewrite the rest of this `README.md` to give a great
  explanation of what your plugin does

## Dependencies ##

None.

## Examples ##

None.

## Limitations ##

None.
