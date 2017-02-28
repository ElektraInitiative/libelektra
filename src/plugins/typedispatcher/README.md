- infos = Information about the typedispatcher plugin is in keys below
- infos/author = Thomas Waser <thomas.waser@libelektra.org>
- infos/licence = BSD
- infos/needs =
- infos/provides = check
- infos/recommends =
- infos/placements =
- infos/status = maintained compatible coverage specific unittest libc configurable preview memleak experimental difficult unfinished nodoc concept 
- infos/metadata =
- infos/description = one-line description of typedispatcher

## Introduction

Copy this typedispatcher if you want to start a new
plugin written in C.

## Usage

You can use `scripts/copy-typedispatcher`
to automatically rename everything to your
plugin name:

	cd src/plugins
	../../scripts/copy-typedispatcher yourplugin

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
