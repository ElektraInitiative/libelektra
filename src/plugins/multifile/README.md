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

`kdb mount -R multifile -c storage="ini",pattern="*/*.ini",resolver="resolver" /path /mountpoint`

`kdb mount -R multifile -c storage="ini",pattern="*.ini",recursive=,resolver="resolver" /path /mountpoint`

## Examples

```sh
rm -rf $(dirname $(kdb file user:/))/multitest || $(exit 0)
mkdir -p $(dirname $(kdb file user:/))/multitest || $(exit 0)

cat > $(dirname $(kdb file user:/))/multitest/lo.ini << EOF \
[lo]\
addr = 127.0.0.1\
Link encap = Loopback\
EOF

cat > $(dirname $(kdb file user:/))/multitest/lan.ini << EOF \
[eth0]\
addr = 192.168.1.216\
Link encap = Ethernet\
EOF

cat > $(dirname $(kdb file user:/))/multitest/wlan.ini << EOF \
[wlan0]\
addr = 192.168.1.125\
Link encap = Ethernet\
EOF

sudo kdb mount -R multifile -c storage="ini",pattern="*.ini",resolver="resolver" multitest user:/tests/multifile

kdb ls user:/tests/multifile
#> user:/tests/multifile/lan.ini/eth0
#> user:/tests/multifile/lan.ini/eth0/Link encap
#> user:/tests/multifile/lan.ini/eth0/addr
#> user:/tests/multifile/lo.ini/lo
#> user:/tests/multifile/lo.ini/lo/Link encap
#> user:/tests/multifile/lo.ini/lo/addr
#> user:/tests/multifile/wlan.ini/wlan0
#> user:/tests/multifile/wlan.ini/wlan0/Link encap
#> user:/tests/multifile/wlan.ini/wlan0/addr

kdb set user:/tests/multifile/lan.ini/eth0/addr 10.0.0.2

kdb get user:/tests/multifile/lan.ini/eth0/addr
#> 10.0.0.2

cat > $(dirname $(kdb file user:/))/multitest/test.ini << EOF \
[testsection]\
key = val\
EOF

kdb ls user:/tests/multifile
#> user:/tests/multifile/lan.ini/eth0
#> user:/tests/multifile/lan.ini/eth0/Link encap
#> user:/tests/multifile/lan.ini/eth0/addr
#> user:/tests/multifile/lo.ini/lo
#> user:/tests/multifile/lo.ini/lo/Link encap
#> user:/tests/multifile/lo.ini/lo/addr
#> user:/tests/multifile/test.ini/testsection
#> user:/tests/multifile/test.ini/testsection/key
#> user:/tests/multifile/wlan.ini/wlan0
#> user:/tests/multifile/wlan.ini/wlan0/Link encap
#> user:/tests/multifile/wlan.ini/wlan0/addr

kdb rm -r user:/tests/multifile/test.ini

stat $(dirname $(kdb file user:/))/multifile/test.ini
# RET:1

sudo kdb umount user:/tests/multifile
```

Recursive:

```sh
rm -rf $(dirname $(kdb file user:/))/multitest || $(exit 0)
mkdir -p $(dirname $(kdb file user:/))/multitest $(dirname $(kdb file user:/))/multitest/a/a1/a12 $(dirname $(kdb file user:/))/multitest/a/a2/a22 $(dirname $(kdb file user:/))/multitest/b/b1|| $(exit 0)

echo "a1key = a1val" > $(dirname $(kdb file user:/))/multitest/a/a1/a12/testa1.file
echo "a2key = a2val" > $(dirname $(kdb file user:/))/multitest/a/a2/a22/testa2.file
echo "b1key = b1val" > $(dirname $(kdb file user:/))/multitest/b/b1/testb1.file

sudo kdb mount -R multifile -c storage="ini",pattern="*.file",recursive=,resolver="resolver" multitest user:/tests/multifile

kdb ls user:/tests/multifile
#> user:/tests/multifile/a/a1/a12/testa1.file/a1key
#> user:/tests/multifile/a/a2/a22/testa2.file/a2key
#> user:/tests/multifile/b/b1/testb1.file/b1key

rm -rf $(dirname $(kdb file user:/))/multitest

sudo kdb umount user:/tests/multifile
```

## Limitations

- You cannot get rid of the configuration file name.
- You cannot create new configuration files.
- Not all file names are supported (see https://issues.libelektra.org/2124)
- Cannot handle conflicts (see https://issues.libelektra.org/2527)
