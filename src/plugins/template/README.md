- infos = Information about the template plugin is in keys below
- infos/author = Author Name <elektra@libelektra.org>
- infos/licence = BSD
- infos/needs =
- infos/provides =
- infos/recommends =
- infos/placements = prerollback rollback postrollback getresolver pregetstorage getstorage postgetstorage setresolver presetstorage setstorage precommit commit postcommit
- infos/status = recommended productive maintained reviewed conformant compatible coverage specific unittest shelltest tested nodep libc configurable final preview memleak experimental difficult unfinished old nodoc concept orphan obsolete discouraged -1000000
- infos/metadata =
- infos/description = one-line description of template

## Introduction

Copy this template if you want to start a new
plugin written in C.

## Usage

You can use `scripts/copy-template`
to automatically rename everything to your
plugin name:

	cd src/plugins
	../../scripts/copy-template yourplugin

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
# Mount template plugin to cascading namespace `/examples/template`
sudo kdb mount config.file /examples/template template

kdb set /examples/template/key value
#> Using name user/examples/template/key
#> Create a new key user/examples/template/key with string "value"

kdb get /examples/template/key
#> value

# Undo modifications to the key database
kdb rm -r /examples/template
sudo kdb umount /examples/template
```

## Limitations

None.
