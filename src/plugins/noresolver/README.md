- infos = Information about the noresolver plugin is in keys below
- infos/author = Markus Raab <elektra@libelektra.org>
- infos/licence = BSD
- infos/provides = resolver
- infos/needs =
- infos/placements = rollback getresolver setresolver commit
- infos/status = maintained nodep libc configurable discouraged
- infos/description = resolver dummy that always succeeds

## Introduction

`noresolver` is designed for non-file-based storage plugins like [uname](../uname).
It is a trivial resolver that:

- does not resolve file names but forwards them as given to the storage plugin
- does not provide any consistency guarantees but storage plugins directly write to the config files

## Explanation

Returns success on every call and can be used as resolver.

It also exports a function checks if a filename is valid. It returns 1
for a relative path and 0 for an absolute path (always successfully).

The path passed to the storage plugin via the parent key is exactly the value
that was set during mounting. This plugin _does not_ resolve any paths. If a
relative path was set while mounting, storage plugins may treat it as relative
to the current working directory.
