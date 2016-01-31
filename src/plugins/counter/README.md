- infos = Information about the counter plugin is in keys below
- infos/author = Name <name@libelektra.org>
- infos/licence = BSD
- infos/needs =
- infos/provides =
- infos/placements = pregetstorage
- infos/description =

## Introduction ##

Counts and prints usage statistics.
Only useful for debugging the plugin framework.

## Module Loading ##

Will not log when loaded as module (config `/module` present), unless `/logmodule` is set:

    kdb check -c "logmodule=" counter
