- infos = Information about the file plugin is in keys below
- infos/author = Thomas Waser <thomas.waser@libelektra.org>
- infos/licence = BSD
- infos/needs =
- infos/provides = storage/file
- infos/recommends =
- infos/placements = getstorage setstorage
- infos/status = specific unittest tested nodep libc configurable preview experimental
- infos/metadata =
- infos/description = reads complete file into a key

## Introduction

The file plugin reads the content of a file and stores it into the parent key.

## Installation

See [installation](/doc/INSTALL.md).
The package is called `libelektra5-experimental`.

## Configuration

- `binary`

  treats the file as a binary file instead of a text file

- `info`

  adds additional information about the file as metadata to the parent key.

  - `info/size` filesize
  - `info/ctime` time of last status change
  - `info/atime` time of last access
  - `info/mtime` time of last modification
  - `info/uid` user ID of owner
  - `info/gid` group ID of owner
  - `info/mode` protection
  - `info/inode` inode number

## Usage

`kdb mount file /testfile file`

## Dependencies

None.

## Examples

```sh
# Mount the file `file/multiline` at `system:/tests/file`
sudo kdb mount "$PWD/src/plugins/file/file/singleline" system:/tests/file file info=

# Check the content of the file
kdb get system:/tests/file
#> this is a single line testfile

#  List available attributes of the mounted file
kdb meta-ls system:/tests/file
#> info/atime
#> info/ctime
#> info/gid
#> info/inode
#> info/mode
#> info/mtime
#> info/size
#> info/uid

# Check out the file’s permissions
kdb meta get system:/tests/file info/mode
# STDOUT-REGEX: 1006[46]4

# Unmount the file
sudo kdb umount system:/tests/file
```

## Limitations

None.
