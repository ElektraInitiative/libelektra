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

This plugin validates MAC-addresses. The following MAC-address-formats are allowed:

    XX-XX-XX-XX-XX-XX
    XX:XX:XX:XX:XX:XX
    XXXXXX-XXXXXX

If `set` is used, the value will be transformed to the format of the middle one.

`kdbGet` returns an integer-representation of these values.

TODO:

- Update readme
- Better error-messages
- Fix bugs
- Remove unused code
- Maybe also support formats `XXXXXXXXXXXX` and `XXXX.XXXX.XXXX`


## Usage


```sh
# Backup-and-Restore: user/tests/mac

# Mount `macaddr` plugin
kdb mount config.dump /tests/mac dump macaddr

# Check the validity of the following MAC addresses
kdb setmeta /tests/mac/mac1 check/macaddr True
kdb setmeta /tests/mac/mac2 check/macaddr True
kdb setmeta /tests/mac/mac3 check/macaddr True
kdb setmeta /tests/mac/mac4 check/macaddr True

# Setting a MAC address using colons
kdb set /tests/mac/mac1 00:A0:C9:14:C8:29
# RET: 0

# Setting a MAC address using hyphens
kdb set /tests/mac/mac2 00-A0-C9-14-C8-29
# RET: 0

# Setting a MAC address using one hyphen
kdb set /tests/mac/mac3 00A0C9-14C829
# RET: 0

# Setting a MAC address using an integer value
kdb set /tests/mac/mac4 17661175009296
# RET: 0

# Setting a MAC address using an invalid address
kdb set /tests/mac/mac1 00:G1:C9:14:C8:29
# RET: 5

# Setting a MAC address using an invalid address
kdb set /tests/mac/mac1 00:E1:C914:C8:29
# RET: 5

# Setting a MAC address using an invalid address
kdb set /tests/mac/mac4 281474976710656
# RET: 5

# Retrieving a MAC address with colons as integer
kdb get /tests/mac/mac1
#> 690568349737

# Retrieving a MAC address with hyphens as integer
kdb get /tests/mac/mac2
#> 690568349737

# Retrieving a MAC address with one hyphen as integer
kdb get /tests/mac/mac3
#> 690568349737

# Retrieving an integer MAC address
kdb get /tests/mac/mac4
#> 17661175009296

kdb umount /tests/mac
```

## Dependencies

None.

## Limitations

None.
