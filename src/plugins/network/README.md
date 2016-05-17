- infos = Information about network plugin is in keys below
- infos/author = Markus Raab <elektra@libelektra.org>
- infos/licence = BSD
- infos/provides = check
- infos/needs =
- infos/placements = presetstorage
- infos/status = maintained libc
- infos/metadata = check/ipaddr
- infos/description = Checks keys if they contain a valid ip address

## Introduction ##

This plugin is a check plugin that checks if a key contains a valid ip
address. It uses the `POSIX.1-2001` interface `getaddrinfo()` in order
to check if an ip address is valid.

## Purpose ##

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

## Usage ##

Every key tagged with the metakey `check/ipaddr` will be checked
using `getaddrinfo()`.  If additionally the values `ipv4` or `ipv6`
are supplied, the address family will be specified.
