- infos = Information about the size plugin is in keys below
- infos/author = Thomas Waser <thomas.waser@libelektra.org>
- infos/licence = BSD
- infos/needs =
- infos/provides = conv
- infos/recommends =
- infos/placements = postgetstorage presetstorage
- infos/status = maintained conformant compatible coverage specific unittest shelltest tested nodep libc configurable experimental
- infos/metadata = transform/size
- infos/description = transforms WIDTHxHEIGHT values into separate keys

## Introduction

The `size` plugin transforms values with the format `WIDTH<separator>HEIGHT` into a separate `Width` and `Height` key. The metakey `transform/size` triggers the conversion.

## Configuration

By default `x` is assumed as separator. If the plugin configuration `/separator` is set, its value is used instead. If set, `transform/size`'s value takes precedence.

## Examples

```sh
# Backup-and-Restore:/examples/size

sudo kdb mount size.ecf /examples/size size dump

kdb set /examples/size/SizeKey 20x4
kdb setmeta user/examples/size/SizeKey transform/size ""

kdb get /examples/size/SizeKey/Width
#> 20
kdb get /examples/size/SizeKey/Height
#> 4

kdb rm -r /examples/size
sudo kdb umount /examples/size
```

## Limitations

None.
