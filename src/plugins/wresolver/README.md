- infos = Information about the wresolver plugin is in keys below
- infos/author = Markus Raab <elektra@libelektra.org>
- infos/licence = BSD
- infos/needs =
- infos/provides = resolver
- infos/placements = rollback getresolver setresolver
- infos/description = Returns success on every call and can be used as resolver.

## Introduction ##

Resolver for non-POSIX, e.g. w32/w64 systems.

Uses SHGetFolderPath for w32/w64 to get the "home directory".
