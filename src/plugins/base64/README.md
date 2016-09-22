- infos = Information about base64 plugin is in keys below
- infos/author = Peter Nirschl <peter.nirschl@gmail.com>
- infos/licence = BSD
- infos/provides = filefilter
- infos/needs =
- infos/recommends =
- infos/placements = postgetstorage presetstorage
- infos/status = experimental
- infos/metadata =
- infos/description = Base64 Encoding

# base64 Plugin #

## Introduction ##

The Base64 Encoding (specified in [RFC4648](https://www.ietf.org/rfc/rfc4648.txt)) is used to encode arbitrary binary data to ASCII strings.

This is useful for configuration files that must contain ASCII strings only.

The `base64` plugin encodes all binary values before `kdb set` writes the configuration to the file.
The values are decoded back to its original value after `kdb get` has read from the configuration file.

In order to identify the base64 encoded content, the values are marked with a prefix (see section Configuration).

## Examples ##

    kdb mount test.ecf /test base64 /base64/prefix="encodedContent-"

## Configuration ##

A custom prefix can be provided in the plugin configuration at `/base64/prefix`.
If no such Key is provided, then `"bin:"` is used as default.

Please provide a prefix that is not used otherwise within your configuration to avoid decoding mistakes.

If you provide `/base64/prefix="binaryData:"` as plugin configuration all encoded values will look like

    binaryData:SGVsbG8gV29ybGQhCg==
