- infos = Information about the blockresolver plugin is in keys below
- infos/author = Name <name@libelektra.org>
- infos/licence = BSD
- infos/needs =
- infos/provides = resolver
- infos/recommends =
- infos/placements = rollback getresolver setresolver commit
- infos/status = maintained conformant compatible coverage specific unittest tested nodep libc configurable preview experimental difficult unfinished concept
- infos/metadata =
- infos/description = resolver for parts in a file configuration file

## Introduction ##

The `blockresolver` can be used to only resolve a tagged block inside a configuration file.

### Implementation details ###

`blockresolver` extracts the requested block from the configurations file and writes it into a temporary file. Afterwards Elektra will only work on the temporary file until kdbSet is called. On kdbSet the contents of the temporary file will be merged with parts outside of the requested block from the original file.

## Usage ##

    `kdb mount -R blockresolver /path/to/my/file /mountpoint -c identifier="identifier-tag"`

where `identifier` specifies the tag `blockresolver` will search for in the configuration file. 

A block consists of 2 parts:
- beginning: the identifier suffixed with `start` 
- end: the identifier suffixed with `stop`

## Limitations ##

Currently the identifier must be unique.
 
## Example ##
```sh
# Backup-and-Restore:system/examples/blockresolver
sudo kdb mount -R blockresolver /tmp/test.block system/examples/blockresolver -c identifier=">>> block config" ini
#
# create testfile
#
$ echo "text" > /tmp/test.block
$ echo "more text" >> /tmp/test.block
$ echo "some more text" >> /tmp/test.block
$ echo ">>> block config start" >> /tmp/test.block
$ echo "[section1]" >> /tmp/test.block
$ echo "key1 = val1" >> /tmp/test.block
$ echo "[section2]" >> /tmp/test.block
$ echo "key2 = val2" >> /tmp/test.block
$ echo ">>> block config stop" >> /tmp/test.block
$ echo "text again" >> /tmp/test.block
$ echo "and more text" >> /tmp/test.block
$ echo "text" >> /tmp/test.block
#
# check testfile
#
$ cat /tmp/test.block
text
more text
some more text
>>> block config start
[section1]
key1 = val1
[section2]
key2 = val2
>>> block config stop
text again
and more text
text
#
# only the block between the tags is read!
#
kdb export system/examples/blockresolver ini
[section1]
key1 = val1
[section2]
key2 = val2
#
# add a new key to the resolved block 
#
kdb set system/examples/blockresolver/section1/key12 val12
#
$ cat /tmp/test.block
text
more text
some more text
>>> block config start
[section1]
key1 = val1
key12 = val12
[section2]
key2 = val2
>>> block config stop
text again
and more text
text
#
# cleanup
#
kdb rm -r system/examples/blockresolver
sudo kdb umount system/examples/blockresolver
```
