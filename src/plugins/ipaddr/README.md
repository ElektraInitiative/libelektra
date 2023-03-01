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

## Installation

See [installation](/doc/INSTALL.md).
The package is called `libelektra5-experimental`.

## Usage

```sh
# Mount `ipaddr` plugin to cascading namespace `/tests/ipaddr`
kdb mount config.dump /tests/ipaddr dump ipaddr

# Check the validity of the IP stored in `system:/tests/ipaddr/ipv4`
kdb meta set spec:/tests/ipaddr/ipv4 check/ipaddr ipv4

# Try to set an incorrect IP address
kdb set system:/tests/ipaddr/ipv4 127.0.0.1337
# STDERR: .*Validation Semantic.*
# ERROR:  C03200
# RET:    5

# Set a correct IPv4 address
kdb set system:/tests/ipaddr/ipv4 127.0.0.1
kdb get system:/tests/ipaddr/ipv4
#> 127.0.0.1

# By default the plugin allows both IPv4 and IPv6 addresses
kdb meta set spec:/tests/ipaddr/address check/ipaddr ""

# Set correct IP addresses
kdb set system:/tests/ipaddr/address 1.2.3.4
kdb set system:/tests/ipaddr/address ::1

# Try to set incorrect addresses
kdb set system:/tests/ipaddr/address bad::ip
# RET: 5
kdb set system:/tests/ipaddr/address 1.2.-3.4
# RET: 5

# Undo modifications to the database
kdb rm -r /tests/ipaddr
kdb umount /tests/ipaddr
```

## Limitations

The plugin only checks IP addresses for validity. It is not able to resolve hostnames. If you are looking for a plugin that supports hostnames, check out the [network plugin](../network/).
