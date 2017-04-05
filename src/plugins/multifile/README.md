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
rm -rf ~/.config/multitest || $(exit 0)
mkdir -p ~/.config/multitest || $(exit 0)

cat > ~/.config/multitest/lo.ini << EOF \
[lo]\
addr = 127.0.0.1\
Link encap = Loopback\
EOF

cat > ~/.config/multitest/lan.ini << EOF \
[eth0]\
addr = 192.168.1.216\
Link encap = Ethernet\
EOF

cat > ~/.config/multitest/wlan.ini << EOF \
[wlan0]\
addr = 192.168.1.125\
Link encap = Ethernet\
EOF

kdb mount -R multifile -c storage="ini",pattern="*.ini",resolver="resolver" multitest user/multi

kdb ls user/multi
#> user/multi/lan.ini/eth0
#> user/multi/lan.ini/eth0/Link encap
#> user/multi/lan.ini/eth0/addr
#> user/multi/lo.ini/lo
#> user/multi/lo.ini/lo/Link encap
#> user/multi/lo.ini/lo/addr
#> user/multi/wlan.ini/wlan0
#> user/multi/wlan.ini/wlan0/Link encap
#> user/multi/wlan.ini/wlan0/addr

kdb set user/multi/lan.ini/eth0/addr 10.0.0.2

kdb get user/multi/lan.ini/eth0/addr
#> 10.0.0.2

cat > ~/.config/multitest/test.ini << EOF \
[testsection]\
key = val\
EOF

kdb ls user/multi
#> user/multi/lan.ini/eth0
#> user/multi/lan.ini/eth0/Link encap
#> user/multi/lan.ini/eth0/addr
#> user/multi/lo.ini/lo
#> user/multi/lo.ini/lo/Link encap
#> user/multi/lo.ini/lo/addr
#> user/multi/test.ini/testsection
#> user/multi/test.ini/testsection/key
#> user/multi/wlan.ini/wlan0
#> user/multi/wlan.ini/wlan0/Link encap
#> user/multi/wlan.ini/wlan0/addr

kdb rm -r user/multi/test.ini

#stat ~/.config/multifile/test.ini
# RET:1

#kdb umount user/multi
```

## Limitations

- You cannot get rid of the configuration file name.
