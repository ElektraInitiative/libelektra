- infos = Information about the cachefilter plugin is in keys below
- infos/author = Marvin Mall <namoshek@libelektra.org>
- infos/licence = BSD
- infos/needs =
- infos/provides = filter
- infos/recommends =
- infos/placements = postgetstorage presetstorage
- infos/status = maintained unittest libc nodep global experimental nodoc
- infos/metadata =
- infos/description = ensures result keyset only contains requested keys

## Introduction

A global plugin that steps in during `kdbGet()` process to filter the results in a way, so that no other keys than the requested one or descendants of it are returned. During `kdbSet()` the filtered keys are added back to the output, so that they don't get lost during the storage process. In other words, the plugin caches filtered keys to easen the use of the API.

## Usage

There is not much to do to use the plugin. Just mount is as global plugin and you are done:
    
    kdb global-mount cachefilter
