- infos = Information about the unit plugin is in keys below
- infos/author = Marcel Hauri <e1355940@student.tuwien.ac.at>
- infos/licence = BSD
- infos/needs =
- infos/provides =
- infos/recommends =
- infos/placements = presetstorage postgetstorage
- infos/status = maintained reviewed conformant compatible coverage unittest tested libc final
- infos/features/storage = limited
- infos/metadata = check/unit
- infos/description = validates units of memory and normalizes to bytes

## Installation

See [installation](/doc/INSTALL.md).
The package is called `libelektra5-extra`.

## Validation options

The following representation standards of units are currently supported and can be used by setting `check/unit`:

- `<numeric value><one or more spaces><memory unit>`

  e.g. 20 MB, 256 KB

## Normalization

The following representation standards of units are currently supported and can be use by setting `check/unit` to:

- `<numeric value><one or more spaces><memory unit>`

  e.g. 20 MB, 256 KB, the value then will be normalized to bytes

## Examples

e.g. 20 KB will become 20000 Bytes

## Limitations

Only basic units are supported (everything from Bytes to Petabytes, but no KiB, MiB or Gib)
The max value is limited by the capacity of unsigned Longs. Using greater values will result
in an error.
