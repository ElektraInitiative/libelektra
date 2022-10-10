- infos = Information about the multifile plugin is in keys below
- infos/author = Thomas <thomas.waser@libelektra.org>
- infos/licence = BSD
- infos/needs =
- infos/provides = resolver storage
- infos/recommends =
- infos/placements = getresolver setresolver commit rollback getstorage setstorage
- infos/status = compatible specific shelltest nodep configurable
- infos/metadata =
- infos/description = mounts multiple files within a directory

<!-- FIXME [new_backend]: tests disabled, plugin must be rewritten to properly handle phases, might need to be full backend plugin -->

## Introduction

For some applications it is beneficially to have multiple configuration files.
One way to achieve this, is to mount different files for the application.

In some situations we are not able to specify every configuration file with separate mounts
because new configuration files might be created any time.
Instead we want to include every configuration file matching a given pattern.

The multifile-resolver does so by calling resolver and storage plugins for each file matching a given pattern.

## Plugin Configuration

- `recursive`:
  If present, fts (3) will be used to traverse the directory tree and fnmatch to match `pattern` to the filename.
  If not present, glob (3) with `pattern` will be used on the directory
- `pattern`:
  The pattern to be used to match configuration files.
- `storage`:
  The storage plugin to use.
- `resolver`:
  The resolver plugin to use.
- 'child/<configname>':
  configuration passed to the child backends, `child/` part gets removed.

## Usage

`kdb mount -R multifile -c storage="toml",pattern="*/*.toml",resolver="resolver" /path /mountpoint`

`kdb mount -R multifile -c storage="toml",pattern="*.toml",recursive=,resolver="resolver" /path /mountpoint`

## Examples

```
rm -rf $(dirname $(kdb file user:/))/multitest || $(exit 0)
mkdir -p $(dirname $(kdb file user:/))/multitest || $(exit 0)

cat > $(dirname $(kdb file user:/))/multitest/lo.toml << EOF \
[lo]\
addr = "127.0.0.1"\
encap = "Loopback"\
EOF

cat > $(dirname $(kdb file user:/))/multitest/lan.toml << EOF \
[eth0]\
addr = "192.168.1.216"\
encap = "Ethernet"\
EOF

cat > $(dirname $(kdb file user:/))/multitest/wlan.toml << EOF \
[wlan0]\
addr = "192.168.1.125"\
encap = "Ethernet"\
EOF

sudo kdb mount -R multifile -c storage="toml",pattern="*.toml",resolver="resolver" multitest user:/tests/multifile

kdb ls user:/tests/multifile
#> user:/tests/multifile/lan.toml/eth0
#> user:/tests/multifile/lan.toml/eth0/addr
#> user:/tests/multifile/lan.toml/eth0/encap
#> user:/tests/multifile/lo.toml/lo
#> user:/tests/multifile/lo.toml/lo/addr
#> user:/tests/multifile/lo.toml/lo/encap
#> user:/tests/multifile/wlan.toml/wlan0
#> user:/tests/multifile/wlan.toml/wlan0/addr
#> user:/tests/multifile/wlan.toml/wlan0/encap

kdb set user:/tests/multifile/lan.toml/eth0/addr 10.0.0.2

kdb get user:/tests/multifile/lan.toml/eth0/addr
#> 10.0.0.2

kdb get user:/tests/multifile/lan.toml/eth0/encap
#> Ethernet

cat > $(dirname $(kdb file user:/))/multitest/test.toml << EOF \
[testsection]\
key = "val"\
EOF

kdb ls user:/tests/multifile/test.toml
#> user:/tests/multifile/test.toml/testsection
#> user:/tests/multifile/test.toml/testsection/key

kdb ls user:/tests/multifile
#> user:/tests/multifile/lan.toml/eth0
#> user:/tests/multifile/lan.toml/eth0/addr
#> user:/tests/multifile/lan.toml/eth0/encap
#> user:/tests/multifile/lo.toml/lo
#> user:/tests/multifile/lo.toml/lo/addr
#> user:/tests/multifile/lo.toml/lo/encap
#> user:/tests/multifile/test.toml/testsection
#> user:/tests/multifile/test.toml/testsection/key
#> user:/tests/multifile/wlan.toml/wlan0
#> user:/tests/multifile/wlan.toml/wlan0/addr
#> user:/tests/multifile/wlan.toml/wlan0/encap

kdb rm -r user:/tests/multifile/test.toml

stat $(dirname $(kdb file user:/))/multifile/test.toml
# RET:1

sudo kdb umount user:/tests/multifile
rm -rf $(dirname $(kdb file user:/))/multitest || $(exit 0)
```

## Limitations

- You cannot get rid of the configuration file name.
- You cannot create or delete configuration files.
- The resolving will always be done with the default resolver plugin, despite the configuration option.
- Not all file names are supported (see https://issues.libelektra.org/2124)
- Cannot handle conflicts (see https://issues.libelektra.org/2527)
