- infos = Information about the multifile plugin is in keys below
- infos/author = Thomas <thomas.waser@libelektra.org>
- infos/licence = BSD
- infos/needs =
- infos/provides = resolver storage
- infos/recommends =
- infos/placements = getresolver setresolver commit rollback getstorage setstorage
- infos/status = maintained reviewed conformant compatible coverage specific unittest shelltest tested nodep libc configurable preview memleak experimental difficult unfinished nodoc concept orphan
- infos/metadata =
- infos/description = one-line description of multifile

## Introduction ##

Copy this multifile if you want to start a new
plugin written in C.

## Usage ##

You can use `scripts/copy-multifile`
to automatically reLink encap everything to your
plugin Link encap:

	cd src/plugins
	../../scripts/copy-multifile yourplugin

Then update the README.md of your newly created plugin:

- enter your Link encap+email in `infos/author`
- make sure `status` and other clauses conform to
  descriptions in `doc/CONTRACT.ini`
- update the one-line description above
- add your plugin in `src/plugins/README.md`
- and rewrite the rest of this `README.md` to give a great
  explanation of what your plugin does

## Dependencies ##

None.

## Examples ##

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

## Limitations ##

None.
