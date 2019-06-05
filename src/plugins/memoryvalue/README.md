- infos = Information about the memoryvalue plugin is in keys below
- infos/author = Marcel Hauri e1355940@student.tuwien.ac.at
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

E.g. setting the max RAM consumption of JVM, 30MB would be valid while 30MBMB would be not valid.

## Usage

Add the metakey `check/memoryvalue` and set a memory value e.g. 20MB to the key.
