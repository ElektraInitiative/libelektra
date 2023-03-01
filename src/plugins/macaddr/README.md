- infos = Information about the macaddr plugin is in keys below
- infos/author = Thomas Bretterbauer <e01306821@student.tuwien.ac.at>
- infos/licence = BSD
- infos/needs =
- infos/provides = check
- infos/recommends =
- infos/placements = postgetstorage presetstorage
- infos/status = maintained nodep
- infos/metadata = check/macaddr
- infos/description = Validates MAC-addresses and returns them as integers

## Introduction

This plugin validates MAC-addresses. The following MAC-address-formats are supported:

    XX-XX-XX-XX-XX-XX
    XX:XX:XX:XX:XX:XX
    XXXXXX-XXXXXX
    Integer values (0 - 281474976710655)

`kdbGet` returns an integer-representation of these values.

## Installation

See [installation](/doc/INSTALL.md).
The package is called `libelektra5-extra`.

## Usage

```sh
# Backup-and-Restore: user:/tests/mac

# Mount `macaddr` plugin
sudo kdb mount macconf.ecf user:/tests/mac macaddr

# Setting a MAC address using colons
kdb set user:/tests/mac/mac1 00:A0:C9:14:C8:29
# RET: 0

# Setting a MAC address using hyphens
kdb set user:/tests/mac/mac2 00-A0-C9-14-C8-29
# RET: 0

# Setting a MAC address using one hyphen
kdb set user:/tests/mac/mac3 00A0C9-14C829
# RET: 0

# Setting a MAC address using an integer value
kdb set user:/tests/mac/mac4 17661175009296
# RET: 0

# Marking written keys as MAC addresses
kdb meta set user:/tests/mac/mac1 check/macaddr ""
kdb meta set user:/tests/mac/mac2 check/macaddr ""
kdb meta set user:/tests/mac/mac3 check/macaddr ""
kdb meta set user:/tests/mac/mac4 check/macaddr ""

# Setting a MAC address using an invalid address
kdb set user:/tests/mac/mac1 00:G1:C9:14:C8:29
# RET: 5

# Setting a MAC address using an invalid address
kdb set user:/tests/mac/mac1 00:E1:C914:C8:29
# RET: 5

# Setting a MAC address using an invalid address
kdb set user:/tests/mac/mac4 281474976710656
# RET: 5

# Retrieving a MAC address with colons as integer
kdb get user:/tests/mac/mac1
#> 690568349737

# Retrieving a MAC address with hyphens as integer
kdb get user:/tests/mac/mac2
#> 690568349737

# Retrieving a MAC address with one hyphen as integer
kdb get user:/tests/mac/mac3
#> 690568349737

# Retrieving an integer MAC address
kdb get user:/tests/mac/mac4
#> 17661175009296

kdb rm -r user:/tests/mac
sudo kdb umount user:/tests/mac
```

## Dependencies

None.

## Limitations

None.
