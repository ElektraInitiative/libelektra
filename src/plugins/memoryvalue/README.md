- infos = Information about the memoryvalue plugin is in keys below
- infos/author = Marcel Hauri <firstname> [at] <lastname> .at
- infos/licence = BSD
- infos/needs =
- infos/provides =
- infos/recommends =
- infos/placements = postgetstorage presetstorage
- infos/ordering = type
- infos/status = maintained
- infos/metadata = check/memoryvalue
- infos/description = checks for correct memory value settings and normalizes them to bytes

## Introduction

Memory Value Plugin checks the correct format of memory specifications.

E.g. setting the max RAM consumption of JVM
*maxMem=2048MB valid*

*maxMem=2048MXB invalid*

*maxMem=2048P invalid*

*maxMem=2048 invalid*

## Usage 
set meta to use the plugin
kdb setmeta user/tests/memoryvalue/m1 check/memoryvalue ""

set key
kdb set user/tests/memoryvalue/m1 12MB

retrieve the key
kdb get user/tests/memoryvalue/m1

