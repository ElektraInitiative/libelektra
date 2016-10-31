- infos = Information about the noresolver plugin is in keys below
- infos/author = Markus Raab <elektra@libelektra.org>
- infos/licence = BSD
- infos/provides = resolver
- infos/needs =
- infos/placements = rollback getresolver setresolver
- infos/status = maintained configurable nodep libc unfinished nodoc concept
- infos/description = resolver dummy that always succeeds

## Explanation ##

Returns success on every call and can be used as resolver.

It also exports a function checks if a filename is valid. It returns 1
for a relative path and 0 for an absolute path (always successfully).
