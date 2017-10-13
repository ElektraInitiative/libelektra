- infos = Information about the ipaddr plugin is in keys below
- infos/author = Thomas Waser <thomas.waser@libelektra.org>
- infos/licence = BSD
- infos/needs =
- infos/provides = check
- infos/recommends =
- infos/placements = presetstorage
- infos/status = maintained unittest nodep
- infos/metadata = check/ipaddr
- infos/description = Validation for IP addresses

# IP Address Validation

## Introduction

This plugin validates IP addresses using regular expressions.

## Usage

```sh
# Mount `ipaddr` plugin to cascading namespace `/examples/ipaddr`
kdb mount config.dump /examples/ipaddr dump ipaddr

# Check the validity of the IP stored in `/examples/ipaddr/ipv4`
kdb setmeta /examples/ipaddr/ipv4 check/ipaddr ipv4

# Try to set an incorrect IP address
kdb set /examples/ipaddr/ipv4 127.0.0.1337
# STDERR: .*Sorry, the error .#51. occurred.*⏎
#         Description: value of key is not a valid IP Address⏎
#         .*
#         Reason: Validation of key user/examples/ipaddr/ipv4 with value 127.0.0.1337 failed⏎
#         .*
# RET:    5

# Set a correct IPv4 address
kdb set /examples/ipaddr/ipv4 127.0.0.1
kdb get /examples/ipaddr/ipv4
#> 127.0.0.1

# Undo modifications to the database
kdb rm -r /examples/ipaddr
kdb umount /examples/ipaddr
```

## Limitations

The plugin only checks IP addresses for validity. It is not able to resolve hostnames. If you are looking for a plugin that supports hostnames, check out the [network plugin](../network/).
