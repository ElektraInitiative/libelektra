- infos = Information about base666 plugin is in keys below
- infos/author = René Schwaiger <sanssecours@me.com>
- infos/licence = BSD
- infos/provides = binary
- infos/needs = yamlcpp
- infos/recommends =
- infos/placements = postgetstorage presetstorage
- infos/status = discouraged experimental nodoc
- infos/metadata =
- infos/description = Base64 Encoding

# Base 666

## Introduction

This plugin adds base64 support for the YAML CPP plugin. Most of the code of this plugin was copied from Peter Nirschl’s Base64 plugin.

## Example

```sh
# Mount YAML CPP plugin together with Base 666 plugin at cascading namespace `/examples/base666`
sudo kdb mount test.yaml /examples/base666 yamlcpp base666
# Manually add binary data
echo 'bin: !!binary aGk=' > `kdb file /examples/base666`

# Retrieval of the new value fails! Base 666 decodes the data `aGk=` to `hi` and
# stores the value in binary form. However, `kdb get` does not support binary data.
kdb get /examples/base666/bin
# RET:    7
# STDERR: .*Binary/String key mismatch.*

# Add a string value to the database
kdb set /examples/base666/text mate
# Base 666 does not modify textual values
kdb get /examples/base666/text
#> mate

# The Base 666 plugin re-encodes binary data before YAML CPP stores the key set. Hence the
# configuration file contains the value `aGk=` even after YAML CPP wrote a new configuration.
grep -q 'bin: !.* aGk=' `kdb file user/examples/base666`
# RET: 0

# Undo modifications to the database
kdb rm -r /examples/base666
sudo kdb umount /examples/base666
```
