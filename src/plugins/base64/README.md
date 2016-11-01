- infos = Information about base64 plugin is in keys below
- infos/author = Peter Nirschl <peter.nirschl@gmail.com>
- infos/licence = BSD
- infos/provides = filefilter
- infos/needs =
- infos/recommends =
- infos/placements = postgetstorage presetstorage
- infos/status = maintained unittest nodep libc final experimental
- infos/metadata =
- infos/description = Base64 Encoding

## Introduction ##

The Base64 Encoding (specified in [RFC4648](https://www.ietf.org/rfc/rfc4648.txt)) is used to encode arbitrary binary data to ASCII strings.

This is useful for configuration files that must contain ASCII strings only.

The `base64` plugin encodes all binary values before `kdb set` writes the configuration to the file.
The values are decoded back to its original value after `kdb get` has read from the configuration file.

In order to identify the base64 encoded content, the values are marked with the prefix `@BASE64`.
To distinguish between the `@` as character and `@` as Base64 marker, all strings starting with `@` will be modified so that they begin with `@@`.

See the documentation of the [null plugin](../null/), as it uses the same pattern for masking `@`.

## Examples ##

To mount a simple backend that uses the Base64 encoding, you can use:

    kdb mount test.ecf /test base64

All encoded binary values will look something like this:

    @BASE64SGVsbG8gV29ybGQhCg==
