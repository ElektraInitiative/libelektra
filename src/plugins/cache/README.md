- infos = Information about the cache plugin is in keys below
- infos/author = Mihael Pranjić <mpranj@limun.org>
- infos/licence = BSD
- infos/needs =
- infos/provides =
- infos/recommends =
- infos/placements = pregetcache postgetcache
- infos/status = unittest nodep experimental
- infos/metadata =
- infos/description = caches keysets from previous `kdbGet()` calls

## Introduction

This caching plugin stores keysets from previous `kdbGet()` calls
to improve performance when reading configuration files.

## Usage

The cache plugin is compiled and enabled on compatible systems by default.
No actions are needed to enable it.

## Dependencies

POSIX compliant system (including XSI extensions).
The plugin is only compiled if the plugins `resolver` and `mmapstorage`
are also available.

## Location of Cache

The cache files are located in the user's home directory below `~/.cache/elektra/` and
shall not be altered, otherwise the behavior is undefined. If `XDG_CACHE_HOME` is set, the
cache files are located below `$XDG_CACHE_HOME/elektra`.

## Configuration of Cache

Use the tool `kdb cache` to enable, disable or clear the cache.

## Limitations

Incompatible with storage plugins, which do not always produce the same keyset on any invocation
concerning the same configuration file. A notable example here is the `ini` plugin (see issue #2592).
