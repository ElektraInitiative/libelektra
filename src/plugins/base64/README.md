- infos = Information about base64 plugin is in keys below
- infos/author = Peter Nirschl <peter.nirschl@gmail.com>
- infos/licence = BSD
- infos/provides = binary
- infos/needs =
- infos/recommends =
- infos/placements = postgetstorage presetstorage
- infos/status = maintained unittest nodep libc final configurable
- infos/metadata =
- infos/description = Base64 Encoding

# Base64

## Introduction

The Base64 Encoding (specified in [RFC4648](https://www.ietf.org/rfc/rfc4648.txt)) is used to encode arbitrary binary data to ASCII strings.

This is useful for configuration files that must contain ASCII strings only.

The `base64` plugin encodes binary values before `kdb set` writes the configuration to the file.
The values are decoded back to their original value after `kdb get` has read from the configuration file.

## Usage

To mount a simple backend that uses the Base64 encoding, you can use:

```sh
sudo kdb mount test.ecf /tests/base64/test base64
```

. To unmount the plugin use the following command:

```sh
sudo kdb umount /tests/base64/test
```

. All encoded binary values will look something like this:

```
@BASE64SGVsbG8gV29ybGQhCg==
```

. And for a null-key it will be:

```
@BASE64
```

## Modes

The plugin supports two different modes:

1. Escaping Mode
2. Meta Mode

. By default the plugin uses escaping mode which has the advantage that a storage plugin does not have to change its behavior at all to work in conjunction with Base64.

### Escaping Mode

In order to identify the base64 encoded content, the values are marked with the prefix `@BASE64`. To distinguish between the `@` as character and `@` as Base64 marker, all strings starting with `@` will be modified so that they begin with `@@`.

#### Example

The following example shows how you can use this plugin together with the TOML plugin to store binary data.

```sh
# Mount the TOML and Base64 plugin
sudo kdb mount test_config.toml user:/tests/base64 toml base64

# Copy binary data
kdb cp system:/elektra/modules/base64/exports/get user:/tests/base64/binary

# Print binary data
kdb get user:/tests/base64/binary
# STDOUT-REGEX: ^(\\x[0-9a-f]{1,2})+$

# The value inside the configuration file is encoded by the Base64 plugin
kdb file user:/tests/base64 | xargs cat
# STDOUT-REGEX: binary.*=.*"@BASE64[a-zA-Z0-9+/]+={0,2}"

# Undo modifications
kdb rm -r user:/tests/base64
sudo kdb umount user:/tests/base64
```

### Meta Mode

Some file formats such as [YAML](http://yaml.org) already support Base64 encoded data. In YAML [binary data](http://yaml.org/type/binary.html) starts with the tag `!!binary` followed by a Base64 encoded scalar:

```yaml
!!binary SGVsbG8sIFlBTUwh # “Hello, YAML!”
```

. For YAML it would not make sense to use the format of the escaping mode:

```yaml
# Another YAML implementation will not be able to decode this data
# because of the prefix `@BASE64`!
!!binary "@BASE64SGVsbG8sIFlBTUwh"
```

. Base64 supports another mode called “meta mode”. In this mode the Base64 plugin encodes the value, but does not add a prefix. To use the escaping mode a plugin must add the configuration key `/binary/meta`. Afterwards the Base64 plugin encodes and decodes all data that contains the metakey `type` with the value `binary`.

The diagram below shows how the Base64 conversion process works in conjunction with the YAML CPP plugin.

![Tree](./Base64.pdf)

#### Example

The following example shows you how you can use the TOML plugin together with Base64’s meta mode.

```sh
# Mount ni and Base64 plugin (provides `binary`) with the configuration key `binary/meta`
sudo kdb mount test_config.ni user:/tests/base64 ni base64 binary/meta=

# Save base64 encoded data `"value"` (`0x76616c7565`)
kdb set user:/tests/base64/encoded dmFsdWUA
kdb file user:/tests/base64/encoded | xargs cat | grep encoded
#> encoded = dmFsdWUA

# Tell Base64 plugin to decode and encode key value
kdb meta-set user:/tests/base64/encoded type binary

# Receive key data (the `\x0` at the end marks the end of the string)
kdb get user:/tests/base64/encoded
#> \x76\x61\x6c\x75\x65\x00

# Undo modifications
kdb rm -r user:/tests/base64
sudo kdb umount user:/tests/base64
```

For another usage example, please take a look at the ReadMe of the [YAML CPP plugin](../yamlcpp).
