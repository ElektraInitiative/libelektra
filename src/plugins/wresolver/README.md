- infos = Information about the wresolver plugin is in keys below
- infos/author = Markus Raab <elektra@libelektra.org>
- infos/licence = BSD
- infos/provides = resolver
- infos/needs =
- infos/placements = rollback getresolver setresolver
#ifdef _WIN32
- infos/status = recommended maintained nodep configurable unfinished nodoc
#else
- infos/status = recommended maintained nodep configurable unfinished nodoc
#endif
- infos/description = Returns success on every call and can be used as resolver.

## Introduction

Resolver for non-POSIX, e.g. w32/w64 systems.

Uses SHGetFolderPath for w32/w64 to get the "home directory".
