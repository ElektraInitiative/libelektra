- infos = Information about the simplespeclang plugin is in keys below
- infos/author = Markus Raab <markus@libelektra.org>
- infos/licence = BSD
- infos/needs =
- infos/provides = storage
- infos/recommends =
- infos/placements = getstorage setstorage
- infos/status = maintained specific nodep preview experimental difficult unfinished old concept
- infos/metadata =
- infos/description = provides conceptual specification language

## Introduction ##

See [the validation tutorial](/doc/tutorials/validation.md) for introduction.
This plugin provides a conceptual simplistic specification language.

It currently supports to specify:

- structure (which keys are allowed)
- enums (which values are allowed)
- multi-enums (by name convention: if name ends with `*`)

## Purpose ##

The plugin demonstrates how simple a configuration specification can be within the Elektra framework.
It is conceptual and mainly for educational usage.
You can base your plugins

## Configuration ##

- `/keyword/enum`, default `enum`: used as keywords for enum definitions.
- `/keyword/assign`, default `=`: used as keywords for assignment.

Configuration within the specification language:

- `mountpoint <filename>`: defines a file-name for `kdb spec-mount`
- `plugins <pluginspec>`: defines list of plugins for `kdb spec-mount`

## Usage ##

First you need to mount the plugin to spec, e.g.:

```sh
kdb mount myspec.ssl spec/test simplespeclang
```

Then you can write your specification (with default keywords):

```sh
cat > `kdb file spec/test` << HERE
mountpoint filename.txt
enum key = value1 value2 value3
enum key2 = value1 value2
HERE
```

And finally you need a specification mount, so that all necessary
plugins are present:

```sh
kdb spec-mount /test
```

Also make sure that `spec` is mounted as global plugin:

```sh
kdb global-mount
```

Then you are only able to write valid keys:

```sh
kdb set /test/key value1  # accepted
kdb set /test/key2 value3 # rejected, no value3 for key2
kdb set /test/something else # rejected, no key something
```
