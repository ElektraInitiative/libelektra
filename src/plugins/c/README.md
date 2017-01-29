- infos = Information about the c plugin is in keys below
- infos/author = Markus Raab <elektra@libelektra.org>
- infos/licence = BSD
- infos/needs = ccode
- infos/provides =
- infos/recommends =
- infos/placements =
- infos/status = maintained nodep libc writeonly preview nodoc
- infos/metadata =
- infos/description = C-struct exports for Elektra

## Usage ##

Export Elektra's C-structs (e.g. `ksNew(.. keyNew(`). This is
useful for generating test data, e.g.:

	kdb export user/testdata c

