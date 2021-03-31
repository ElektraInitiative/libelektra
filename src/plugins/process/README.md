- infos = Information about the process plugin is in keys below
- infos/author = Armin Wurzinger <e1528532@libelektra.org>
- infos/licence = BSD
- infos/needs =
- infos/provides =
- infos/recommends =
- infos/placements = pregetstorage getstorage postgetstorage presetstorage setstorage
- infos/status = maintained unittest shelltest configurable experimental unfinished
- infos/metadata =
- infos/description = executes other plugins inside an own process

## Introduction

The process plugin uses the `pluginprocess` library in order to execute other plugins
inside an own process.
This is useful for runtimes or libraries that cannot be reinitialized in the same process
after they have been used.
It is also useful for plugins which cause memory leaks to be isolated in an own process.

## Installation

See [installation](/doc/INSTALL.md).
The package is called `libelektra5-experimental`.

## Usage

Mount this plugin and specify the plugin configuration parameter `plugin` to a plugin that exists on
the system. The proxied plugin will then be executed in a separate process and this plugin handles
the communication between the processes.

## Dependencies

The `pluginprocess` library, which is only available on POSIX environments currently.

## Examples

```sh
# Mount the dump plugin a separate process via the process plugin to `user:/examples/process`
sudo kdb mount config.file user:/tests/process process plugin=dump

kdb set user:/tests/process/key value
#> Create a new key user:/tests/process/key with string "value"

kdb get user:/tests/process/key
#> value

# Undo modifications
kdb rm -r user:/tests/process
sudo kdb umount user:/tests/process
```

## Limitations

- Currently only plugins that act on one of the above-mentioned `placements` can be proxied
  successfully. Other metainformation like `needs` or `provides` or `recommends` will not be
  available when proxying using this plugin.
- This plugin cannot act as a proxy for itself to prevent loops.
