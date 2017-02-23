- infos = Information about the file plugin is in keys below
- infos/author = Author Name <elektra@libelektra.org>
- infos/licence = BSD
- infos/needs =
- infos/provides = storage
- infos/recommends =
- infos/placements = getstorage setstorage
- infos/status = maintained conformant compatible coverage specific unittest tested nodep libc configurable preview memleak experimental difficult unfinished old nodoc concept
- infos/metadata =
- infos/description = one-line description of file

## Introduction

Copy this file if you want to start a new
plugin written in C.

## Usage

You can use `scripts/copy-file`
to automatically rename everything to your
plugin name:

	cd src/plugins
	../../scripts/copy-file yourplugin

Then update the README.md of your newly created plugin:

- enter your full name+email in `infos/author`
- make sure `status` and other clauses conform to
  descriptions in `doc/CONTRACT.ini`
- update the one-line description above
- add your plugin in `src/plugins/README.md`
- and rewrite the rest of this `README.md` to give a great
  explanation of what your plugin does

## Dependencies

None.

## Examples

None.

## Limitations

None.
