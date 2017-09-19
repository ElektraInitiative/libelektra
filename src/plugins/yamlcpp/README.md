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

### Leaf Values

One of the limitations of this plugin is, that it only supports values inside [leaf nodes](https://github.com/ElektraInitiative/libelektra/issues/106). Let us look at an example to show what that means. The YAML file below:

```yaml
root:
  subtree:    🍂
  below root: leaf
level 1:
  level 2:
    level 3:  🍁
```

stores all of the values (`🍂`, `leaf` and `🍁`) in the leaves of the mapping. The drawing below makes this situation a little bit clearer.

![Tree](./yamlcpp/Tree.pdf)

The key set that this plugin creates using the data above looks like this (assuming we mount the plugin to `user/examples/yamlcpp`):

|     Name                                      | Value |
|-----------------------------------------------|-------|
| user/examples/yamlcpp/level                   |       |
| user/examples/yamlcpp/level 1/level 2         |       |
| user/examples/yamlcpp/level 1/level 2/level 3 | 🍁    |
| user/examples/yamlcpp/root                    |       |
| user/examples/yamlcpp/root/below root         | leaf  |
| user/examples/yamlcpp/root/subtree            | 🍂    |

. Now why is this plugin unable to store values outside leaf nodes? For example, why can we not store a value inside `user/examples/yamlcpp/level 1/level 2`? To answer this question we need to look at the YAML representation:

```yaml
level 1:
  level 2:
    level 3:  🍁
```

. In a naive approach we might just try to add a value e.g.  `🙈` right next to level 2:

```yaml
level 1:
  level 2: 🙈
    level 3:  🍁
```

. This however would be not correct, since then the YAML node `level 2` would contain both a scalar value (`🙈`) and a mapping (`{ level 3:  🍁 }`). We could solve this dilemma using a list:

```yaml
level 1:
  level 2:
    - 🙈
    - level 3:  🍁
```

. However, if we use this approach we are not able to support Elektra’s array type properly.

### Other Limitations

- Adding and removing keys does remove **comments** inside the configuration file
- No support for Elektra’s **array data type**
- The plugin currently lacks proper **type support** for scalars

[yaml-cpp]: https://github.com/jbeder/yaml-cpp
