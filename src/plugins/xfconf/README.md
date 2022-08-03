- infos = Information about the xfconf plugin is in keys below
- infos/author = Richard Stöckl <e11908080@student.tuwien.ac.at>
- infos/licence = BSD
- infos/needs =
- infos/provides = storage/xfconf
- infos/recommends =
- infos/placements = getstorage setstorage
- infos/status = maintained libc configurable experimental unfinished
- infos/metadata =
- infos/description = storage plugin for xfconf

## Introduction

This is a storage plugin to mount the xfconf configuration settings.

## Dependencies

xfconf from the XFCE project

## Usage

The usage is identical to most storage plugins except that the channel option during mount must be defined in order to tell xfconf which channel to mount.

## Examples

```sh
# Backup-and-Restore: user:/tests/xfconf

# mount the xfwm channel
kdb mount /dev/null /test/xfwm xfconf "channel=xfwm4"

# store old button layout
set "OLD_LAYOUT=$(kdb get /test/xfwm/general/button_layout)"

# set only a close button
kdb set system:/test/xfwm/general/button_layout "C|"

# read the new layout
kdb get /test/xfwm/general/button_layout
#> C|

# restore old layout
kdb set /test/xfwm/general/button_layout "$OLD_LAYOUT"
```

## Limitations

- usage of a dummy file such as `/dev/null`
- xfconf locks can only be read but not set as this is not possible in xfconf
- comments and sorting are not implemented due the lack of both in xfconf
