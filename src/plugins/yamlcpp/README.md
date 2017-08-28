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

## Examples

```sh
# Mount yamlcpp plugin to cascading namespace `/examples/yamlcpp`
kdb mount config.yaml /examples/yamlcpp yamlcpp

# Manually add some mappings to the database
echo "🔑 : 🐳"               > `kdb file /examples/yamlcpp`
echo "some key : 'some value'" >> `kdb file /examples/yamlcpp`

# Retrieve the value of the manually added keys
kdb get /examples/yamlcpp/🔑
#> 🐳
kdb get "/examples/yamlcpp/some key"
#> some value

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

- **Error checking** is quite limited and not properly integrated into Elektra
- The plugin only reads **simple mappings** (no nesting)
- Adding and removing keys does remove **comments** inside the configuration file
- No support for Elektra’s **array data type**
- The plugin currently lacks proper **type support** for scalars
- The current **test suite** is quite limited
- Building the plugin with **GCC on macOS** does not work

[yaml-cpp]: https://github.com/jbeder/yaml-cpp
