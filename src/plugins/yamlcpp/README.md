- infos = Information about the yamlcpp plugin is in keys below
- infos/author = René Schwaiger <sanssecours@me.com>
- infos/licence = BSD
- infos/needs =
- infos/provides = storage/yaml
- infos/recommends =
- infos/placements = getstorage setstorage
- infos/status = maintained preview experimental unfinished concept discouraged
- infos/metadata =
- infos/description = This storage plugin reads and writes data in the YAML format

## Introduction

The YAML CPP plugin reads and writes configuration data via the [yaml-cpp][] library.

## Usage

You can mount this plugin via `kdb mount`:

```sh
kdb mount config.yaml /examples/yamlcpp yamlcpp
```

. To unmount the plugin use  `kdb umount`:

```sh
kdb umount /examples/yamlcpp
```

.

### Examples

The following examples show how you can store and retrieve data via `yamlcpp`.

```sh
# Mount yamlcpp plugin to cascading namespace `/examples/yamlcpp`
kdb mount config.yaml /examples/yamlcpp yamlcpp

# Manually add a mapping to the database
echo "🔑 : 🐳"               > `kdb file /examples/yamlcpp`
# Retrieve the value of the manually added key
kdb get /examples/yamlcpp/🔑
#> 🐳

# Save the location of the config file so we can use it later
echo `kdb file /examples/yamlcpp` > /tmp/data_file
# Manually add syntactically incorrect data
echo "some key: @some  value" >> `kdb file /examples/yamlcpp`
kdb get "/examples/yamlcpp/some key"
# STDERR-REGEX: .*Sorry, the error .#185. occurred ;(⏎
#               Description: Parsing failed⏎
#               .*yaml-cpp: error at line 1, column 11: unknown token.*
# RET: 5

# Overwrite incorrect data
echo "🔑: value" >  `cat /tmp/data_file`

# Add some values via `kdb set`
kdb set /examples/yamlcpp/fleetwood mac
kdb set /examples/yamlcpp/the chain

# Retrieve the new values
kdb get /examples/yamlcpp/the
#> chain
kdb get /examples/yamlcpp/fleetwood
#> mac

# Undo modifications to the key database
kdb rm -r /examples/yamlcpp
kdb umount /examples/yamlcpp
```

## Dependencies

This plugin requires [yaml-cpp][]. On a Debian based OS the package for the library is called `libyaml-cpp-dev` . On macOS you can install the package `yaml-cpp` via [HomeBrew](https://brew.sh).

## Limitations

- The plugin only supports [leaf values](https://github.com/ElektraInitiative/libelektra/issues/106)
- Adding and removing keys does remove **comments** inside the configuration file
- No support for Elektra’s **array data type**
- The plugin currently lacks proper **type support** for scalars

[yaml-cpp]: https://github.com/jbeder/yaml-cpp
