- infos = Information about the xfconf plugin is in keys below
- infos/author = Richard Stöckl <e11908080@student.tuwien.ac.at>
- infos/licence = BSD
- infos/needs =
- infos/provides = storage/xfconf
- infos/recommends =
- infos/placements = postgetstorage presetstorage
- infos/status = maintained libc configurable experimental unfinished concept limited memleak
- infos/metadata =
- infos/description = storage plugin for xfconf

## Introduction

This is a storage plugin to mount the xfconf configuration settings.

## Dependencies

xfconf from the XFCE project

## Usage

The usage is identical to most storage plugins except that the channel option during mount must be defined in order to tell xfconf which channel to mount.

## Examples

```zsh
# Backup-and-Restore: user:/tests/xfconf

# mount the xfwm channel
kdb mount /dev/null /test/xfwm xfconf "channel=xfwm4" 2&>/dev/null

# store old button layout
set "OLD_LAYOUT=$(kdb get /test/xfwm/general/button_layout 2&>/dev/null)"

# set only a close button
kdb set system:/test/xfwm/general/button_layout "C|" 2&>/dev/null

# read the new layout
kdb get /test/xfwm/general/button_layout 2&>/dev/null
#> C|

# restore old layout
kdb set /test/xfwm/general/button_layout "$OLD_LAYOUT" 2&>/dev/null
```

## Limitations

- usage of a dummy file such as `/dev/null`
- xfconf locks can only be read but not set as this is not possible in xfconf
- comments and sorting are not implemented due the lack of both in xfconf
- due to memory leaks in xfconf upstream, valgrind reports errors when running the tests
