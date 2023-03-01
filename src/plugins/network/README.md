- infos = Information about network plugin is in keys below
- infos/author = Markus Raab <elektra@libelektra.org>
- infos/licence = BSD
- infos/provides = check
- infos/needs =
- infos/placements = presetstorage
- infos/status = maintained unittest nodep libc nodoc
- infos/metadata = check/ipaddr check/port check/port/listen
- infos/description = Checks keys if they contain a valid ip address

## Introduction

This plugin is a check plugin that checks if a key contains a valid ip
address. It uses the `POSIX.1-2001` interface `getaddrinfo()` in order
to check if an ip address is valid.

Furthermore `getaddrinfo()` is used in `check/port` to resolve a port by its service name
which is defined under `/etc/services`. The portname is translated to the respective portnumber.
The plugin can be used to check for valid port numbers and if the set port is free to use.

## Purpose

While, in theory, a regular expression can express if a string is a
network address, in practice, such an attempt does not work well. The
reason is that an unmanageable number of valid shortenings for IPv6
addresses makes the regular expression hard to write and understand.

So the idea of building such a complicated regular expression was
discarded, but instead a dedicated checker was introduced. The idea is to
use the operating system facilities to resolve the network address. If
this succeeds, it is guaranteed that this network address will be valid
when it is resolved by the same interface afterwards.

Many network address translators coexist. In `POSIX.1-2001` a powerful
address translator is provided with the interface `getaddrinfo()`. It
is a common network address translation for both IPv4 and IPv6. We used
it to implement this plugin.

## Usage

Every key tagged with the metakey `check/ipaddr` will be checked
using `getaddrinfo()`. If additionally the values `ipv4` or `ipv6`
are supplied, the address family will be specified.

### Example

```sh
# Mount Network plugin to `user:/tests/network`
sudo kdb mount config.file user:/tests/network network

# Set valid IPv4 address
kdb set user:/tests/network/host 127.0.0.1
# Check for valid IPv4 address
kdb meta set user:/tests/network/host check/ipaddr ipv4

# Try to set invalid IPv4 address
kdb set user:/tests/network/host 133.133.133.1337
# RET: 5
# STDERR:.*Validation Semantic: name:.*133.133.133.1337.*
kdb get user:/tests/network/host
#> 127.0.0.1

# Set valid IPv4 address
kdb set user:/tests/network/host 1.2.3.4
#> Set string to "1.2.3.4"
kdb get user:/tests/network/host
#> 1.2.3.4

# Check for any valid network address
kdb meta set user:/tests/network/host check/ipaddr ''
# If identifier `localhost` is not a valid network address it is not part of /etc/hosts
kdb set user:/tests/network/host localhost || ! grep -q localhost /etc/hosts

kdb get user:/tests/network/host
# STDOUT-REGEX: localhost|1.2.3.4

# Undo modifications to the key database
kdb rm -r user:/tests/network
sudo kdb umount user:/tests/network
```

If `check/port` is specified on a given key, the plugin will validate if the port is a
correct number between 1 and 65535.

If `check/port/listen` is specified, the plugin will check if the application can be started
and listen on the given port.

If `gethostbyname()` returns a "try again" error, two retries will be attempted.
If all retries were consumed, the plugin will proceed in the same manner as with other errors.

## Future Work

`check/port/connect` to check if the port can be pinged/reached (usually for clients).
If not reachable, users receive a warning. A correct timeout setting will be problematic though.
