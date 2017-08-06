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

## Dependencies

This plugin requires [yaml-cpp][]. On a Debian based OS the package for the library is called `libyaml-cpp-dev` . On macOS you can install the package `yaml-cpp` via [HomeBrew](https://brew.sh).

## Limitations

Currently this plugin offers **no functionality** at all.

[yaml-cpp]: https://github.com/jbeder/yaml-cpp
