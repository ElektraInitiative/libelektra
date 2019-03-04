- infos = Information about the cache plugin is in keys below
- infos/author = Mihael Pranjic <mpranj@limun.org>
- infos/licence = BSD
- infos/needs =
- infos/provides =
- infos/recommends =
- infos/placements = pregetcache postgetcache
- infos/status = maintained unittest global preview concept
- infos/metadata =
- infos/description = caches keysets from previous `kdbGet()` calls

## Introduction

This caching plugin stores keysets from previous `kdbGet()` calls
to improve performance when reading configuration files.

## Usage

You can use `scripts/copy-cache`
to automatically rename everything to your
plugin name:

    cd src/plugins
    ../../scripts/copy-cache yourplugin

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
# Backup-and-Restore: user/tests/cache

kdb set user/tests/cache/key value
#> Create a new key user/tests/cache/key with string "value"

kdb get /tests/cache/key
#> value
```

## Limitations

None.
