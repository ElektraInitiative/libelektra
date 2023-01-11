- infos = Information about the tracer plugin is in keys below
- infos/author = Markus Raab <elektra@libelektra.org>
- infos/licence = BSD
- infos/provides = tracing
- infos/needs =
- infos/placements = pregetstorage procgetstorage postgetstorage presetstorage precommit postcommit prerollback postrollback
- infos/status = maintained nodep configurable hook
- infos/description = Traces the execution path of a backend

## Introduction

This plugin is added on every possible position within a backend.
It allows you to trace when the backend is executed.

## Installation

See [installation](/doc/INSTALL.md).
The package is called `libelektra5-extra`.

## Usage

If you want to trace how and if the backend is called:

```sh
kdb mount file.ysp user:/trace_point your_storage_plugin tracer
```

So now we can trace whats below your trace point.

```sh
kdb ls user:/trace_point
```

Ok, no tracer is called because resolver immediately told that there is
no file.

```sh
kdb get user:/trace_point
#> Did not find key
```

Ok, same conclusion.

```sh
kdb set user:/trace_point hello
#> create a new key user:/trace_point with string "hello"
#> tracer: set(0xd34cc0, user:/trace_point): user:/trace_point 1
#> tracer: set(0xd34cc0, user:/trace_point): user:/trace_point 1
#> tracer: set(0xd34cc0, user:/trace_point): user:/trace_point 1
```

Now the 3 placements in set are called.

```sh
kdb get user:/trace_point
#> tracer: get(0x22e1cc0, user:/trace_point): 0
#> tracer: get(0x22e1cc0, user:/trace_point): 0
#> hello
```

Now the 2 placements in get are called.

## Module Loading

Will not log when loaded as module (config `/module` present), unless `/logmodule` is set:

```sh
kdb plugin-check -c "logmodule=" tracer
```
