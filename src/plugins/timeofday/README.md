- infos = Information about the timeofday plugin is in keys below
- infos/author = Markus Raab <elektra@libelektra.org>
- infos/licence = BSD
- infos/provides = tracing
- infos/needs =
- infos/placements = pregetstorage procgetstorage postgetstorage presetstorage precommit postcommit prerollback postrollback
- infos/status = maintained nodep configurable hook
- infos/description = Prints timestamps during execution of backend

## Introduction

This plugin is a logging plugin which prints a timestamp during
all placements of backend.

## Installation

See [installation](/doc/INSTALL.md).
The package is called `libelektra5-extra`.

## Usage

If you want to measure how long your storage plugin needs to do the read
and write you mount the plugin using:

```sh
kdb mount file.ysp user:/trace_point your_storage_plugin timeofday
```

Then you can benchmark your storage plugin in the get path:

```sh
kdb get user:/benchmark
#> get     0000000356      di      0000000356      pos     pregetstorage
#> get     0000000530      di      0000000174      pos     postgetstorage
#> hello
```

and in the set path:

```sh
kdb set user:/benchmark
#> get     0000000342      di      0000000342      pos     pregetstorage
#> get     0000000532      di      0000000190      pos     postgetstorage
#> Set null value
#> set     0000000766      di      0000000234      pos     presetstorage
#> set     0000001002      di      0000000236      pos     precommit
#> set     0000008944      di      0000007942      pos     postcommit
```

The first digit column shows the complete time passed, the second column
shows the time from invocation to invocation.

## Module Loading

Will not log when loaded as module (config `/module` present), unless `/logmodule` is set:

```sh
kdb plugin-check -c "logmodule=" timeofday
```
