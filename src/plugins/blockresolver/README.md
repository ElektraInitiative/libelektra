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
# Backup-and-Restore:/tmount/blockresolver
sudo kdb mount test.block /tmount/blockresolver -c identifier="### block config" ini
#
# create testfile
#
$ echo "text" > `kdb file /tmount/blockresolver`
$ echo "more text" >> `kdb file /tmount/blockresolver`
$ echo "some more text" >> `kdb file /tmount/blockresolver`
$ echo "### block config start" >> `kdb file /tmount/blockresolver`
$ echo "[section1]" >> `kdb file /tmount/blockresolver`
$ echo "key1 = val1" >> `kdb file /tmount/blockresolver`
$ echo "[section2]" >> `kdb file /tmount/blockresolver`
$ echo "key2 = val2" >> `kdb file /tmount/blockresolver`
$ echo "### block config stop" >> `kdb file /tmount/blockresolver`
$ echo "text again" >> `kdb file /tmount/blockresolver`
$ echo "and more text" >> `kdb file /tmount/blockresolver`
$ echo "text" >> `kdb file /tmount/blockresolver`
#
# check testfile
#
$ cat `kdb file /tmount/blockresolver`
text
more text
some more text
### block config start
[section1]
key1 = val1
[section2]
key2 = val2
### block config stop
text again
and more text
text
#
# only the block between the tags is read!
#
kdb export /tmount/blockresolver
[section1]
key1 = val1
[section2]
key2 = val2
#
# add a new key to the resolved block 
#
kdb set system/blocktest/section1/key12 val12
#
$ cat `kdb file /tmount/blockresolver`
text
more text
some more text
### block config start
[section1]
key1 = val1
key12 = val12
[section2]
key2 = val2
### block config stop
text again
and more text
text
#
# cleanup
#
kdb rm -r /tmount/blockresolver
kdb umount /tmount/blockresolver
```
