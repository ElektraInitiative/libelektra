- infos = Information about the process plugin is in keys below
- infos/author = Armin Wurzinger <e1528532@libelektra.org>
- infos/licence = BSD
- infos/needs = dump
- infos/provides =
- infos/recommends =
- infos/placements = prerollback rollback postrollback getresolver pregetstorage getstorage postgetstorage setresolver presetstorage setstorage precommit commit postcommit
- infos/status = maintained unittest shelltest tested configurable experimental
- infos/metadata = 
- infos/description = executes other plugins inside an own process

## Introduction

This plugin utilizes the pluginprocess library in order to execute arbitrary other
plugins in an own process, acting as a proxy itself. Therefore it is not required 
to explicitly change a plugin's implementation if it shall be executed in an own
process.

## Usage

Mount this plugin and specify the plugin configuration parameter `plugin` to a plugin that exists on the system. The proxied plugin will then be executed in a separate process and this plugin handles the communication between the processes.

## Dependencies

The `pluginprocess` library, which is only available on POSIX environments currently.

## Examples

```sh
# Mount the dump plugin a separate process via the process plugin to the cascading namespace `/examples/process`
sudo kdb mount config.file /tests/process process plugin=dump

kdb set /tests/process/key value
#> Using name user/tests/process/key
#> Create a new key user/tests/process/key with string "value"

kdb get /tests/process/key
#> value

# Undo modifications
kdb rm -r /tests/process
sudo kdb umount /tests/process
```

## Limitations

This plugin cannot act as a proxy for itself to prevent loops.
