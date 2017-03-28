- infos = Information about the multifile plugin is in keys below
- infos/author = Thomas <thomas.waser@libelektra.org>
- infos/licence = BSD
- infos/needs =
- infos/provides = resolver storage
- infos/recommends =
- infos/placements = getresolver setresolver commit rollback getstorage setstorage
- infos/status = maintained conformant compatible specific shelltest tested libc configurable preview experimental difficult nodoc
- infos/metadata =
- infos/description = mounts multiple files within a directory 

## Introduction

For some applications it is beneficially to have multiple configuration files.
One way to achieve this, is to mount different files for the application.

In some situations we are not able to specify every configuration file with separate mounts
because new configuration files might be created any time.
Instead we want to include every configuration file matching a given pattern.

The multifile-resolver does so by calling resolver and storage plugins for each file matching a given pattern.


## Plugin Configuration

- `pattern`:
  The pattern to be used to match configuration files.
  Internally glob (3) will be used.
- `storage`:
  The storage plugin to use.
- `resolver`:
  The resolver plugin to use.


## Usage

`kdb mount -R multifile -c storage="ini",pattern="*/*.ini",resolver="resolver" /path /mountpoint`

## Examples

```sh
rm -rf /tmp/multitest || $(exit 0)
mkdir -p /tmp/multitest || $(exit 0)

cat > /tmp/multitest/lo.ini << EOF \
[lo]\
addr = 127.0.0.1\
Link encap = Loopback\
EOF

cat > /tmp/multitest/lan.ini << EOF \
[eth0]\
addr = 192.168.1.216\
Link encap = Ethernet\
EOF

cat > /tmp/multitest/wlan.ini << EOF \
[wlan0]\
addr = 192.168.1.125\
Link encap = Ethernet\
EOF

kdb mount -R multifile -c storage="ini",pattern="*.ini",resolver="resolver" /tmp/multitest system/multi

kdb ls system/multi
#> system/multi/lan.ini/eth0
#> system/multi/lan.ini/eth0/Link encap
#> system/multi/lan.ini/eth0/addr
#> system/multi/lo.ini/lo
#> system/multi/lo.ini/lo/Link encap
#> system/multi/lo.ini/lo/addr
#> system/multi/wlan.ini/wlan0
#> system/multi/wlan.ini/wlan0/Link encap
#> system/multi/wlan.ini/wlan0/addr

kdb set system/multi/lan.ini/eth0/addr 10.0.0.2

kdb get system/multi/lan.ini/eth0/addr
#> 10.0.0.2

cat > /tmp/multitest/test.ini << EOF \
[testsection]\
key = val\
EOF

kdb ls system/multi
#> system/multi/lan.ini/eth0
#> system/multi/lan.ini/eth0/Link encap
#> system/multi/lan.ini/eth0/addr
#> system/multi/lo.ini/lo
#> system/multi/lo.ini/lo/Link encap
#> system/multi/lo.ini/lo/addr
#> system/multi/test.ini/testsection
#> system/multi/test.ini/testsection/key
#> system/multi/wlan.ini/wlan0
#> system/multi/wlan.ini/wlan0/Link encap
#> system/multi/wlan.ini/wlan0/addr

kdb rm -r system/multi/test.ini

stat /tmp/multifile/test.ini
# RET:1

kdb umount system/multi
```

## Limitations

- You cannot get rid of the configuration file name.
