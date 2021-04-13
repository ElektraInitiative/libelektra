- infos = Information about the counter plugin is in keys below
- infos/author = Markus Raab <elektra@libelektra.org>
- infos/licence = BSD
- infos/provides = tracing
- infos/needs =
- infos/recommends =
- infos/placements = pregetstorage
- infos/status = maintained nodep configurable final global nodoc
- infos/metadata =
- infos/description = counts and prints usage statistics

## Introduction

Counts and prints usage statistics.
Only useful for debugging the plugin framework.

## Installation

See [installation](/doc/INSTALL.md).
The package is called `libelektra5-extra`.

## Module Loading

Will not log when loaded as module (config `/module` present), unless `/logmodule` is set:

```sh
kdb plugin-check -c "logmodule=" counter
```
