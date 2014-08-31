- infos = Information about the noresolver plugin is in keys below
- infos/author = Markus Raab <elektra@libelektra.org>
- infos/licence = BSD
- infos/needs =
- infos/provides = resolver
- infos/placements = rollback getresolver setresolver
- infos/description = Returns success on every call and can be used as resolver.

It also exports a function checks if a filename is valid. It returns 1
for a relative path and 0 for an absolute path (always successfully).
