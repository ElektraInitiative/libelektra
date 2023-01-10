- infos = Information about the c plugin is in keys below
- infos/author = Markus Raab <elektra@libelektra.org>
- infos/licence = BSD
- infos/needs = ccode
- infos/provides = storage/c
- infos/recommends =
- infos/placements = getstorage setstorage
- infos/status = maintained nodep libc preview nodoc
- infos/features/storage = write
- infos/metadata =
- infos/description = C-struct exports for Elektra

## Usage

Export Elektra’s C-structs (e.g. `ksNew(.. keyNew(`). This is
useful for generating test data, e.g.:

```sh
kdb export user:/testdata c
```
