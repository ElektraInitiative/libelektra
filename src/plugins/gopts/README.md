- infos = Information about the gopts plugin is in keys below
- infos/author = Klemens Böswirth <k.boeswirth+git@gmail.com>
- infos/licence = BSD
- infos/needs =
- infos/provides =
- infos/recommends =
- infos/placements = hook procgetstorage
- infos/status = recommended productive maintained unittest nodep libc configurable
- infos/metadata = args args/index command opt opt/long opt/arg opt/flagvalue opt/help opt/hidden opt/# opt/#/long opt/#/arg opt/#/flagvalue opt/#/hidden env env/#
- infos/description = Parses command-line options using elektra-opts

## Introduction

This plugin allows applications to access command-line options and environment variables via the KDB.

It is implemented as a simple frontend for the parser implemented in the internal `elektraGetOpts`.

For more information on how to write the necessary specification and on using command-line options in general, take a look at [the dedicated tutorial](../../../doc/tutorials/command-line-options.md)

Depending on the calling context and configuration, this plugin might use operating system specific functions to determine command-line arguments and environment variables.
Which operating system's functions the plugin uses is determined at compile-time.

> **Note:** One of the system-specific implementations of this plugin relies on procfs.
> Therefore, if you compile the plugin on a system with procfs, the plugin may not work on other machine with the same OS or even on the same machine, if procfs is not mounted.

## Installation

See [installation](/doc/INSTALL.md).
The package is called `libelektra5-experimental`.

## Basic Usage

The preferred way of using this plugin is via a `kdbOpen` contract:

```c
KeySet * contract = ksNew (0, KS_END);
elektraGOptsContract (contract, argc, argv, environ, parentKey, NULL);

KDB * kdb = kdbOpen (contract, parentKey);

// gopts automatically mounted
KeySet * ks = ksNew (0, KS_END);
kdbGet (kdb, ks, parentKey);
```

## Configuration Keys

The plugin accepts a number of configuration keys.

- `/offset`: You can set this key to an integer `n`.
  The plugin will then ignore the first `n` command-line arguments and only pass the rest on to the parser.
- `/help/usage`: The value of this key is used to replace the standard usage line in the auto-generated help message.
- `/help/prefix`: The value of this key is inserted between the usage line and the options list in the auto-generated help message.

## Global KeySet

The plugin also takes part of its configuration from the global KeySet.
All keys the plugin uses are below `system:/elektra/gopts`.
This prefix is abbreviated to `//` below.

This plugin may use the following keys from the global KeySet:

- `//parent`: If present, the plugin will use this key instead of the one provided by `kdbGet` as the parent key pass on to the parser.
  Specifically, the plugin uses this keys value as the key name for a new key that is passed to the parser.
- `//argc`: If present and `//parent` is present as well, the plugin expects `//argv` to be present as well.
  This key must be binary and its value must be an `int`.
- `//argv`: If present and `//parent` is present as well, the plugin expects `//argc` to be present as well.
  The values of `//argc` and `//argv` will be used instead of using the OS specific implementation.
  This key must be binary and its value must be a `const char * const *`.
- `//envp`: If present and `//parent` is present as well, the value of this key will be used as the list of environment variables.
  This key must be binary and its value must be a `const char * const *`.
- `//args`: If present and `//parent` is present as well, but `//argc` and `//argv` are absent, this will be used as the list of command-line arguments.
  This key must be binary and its value must be a zero-byte separated (and terminated) list of strings.
- `//env`: If present and `//parent` is present as well, but `//envp` is absent, this will be used as the list of environment variable.
  This key must be binary and its value must be a zero-byte separated (and terminated) list of strings.
