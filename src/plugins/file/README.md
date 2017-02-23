- infos = Information about the file plugin is in keys below
- infos/author = Thomas Waser <thomas.waser@libelektra.org>
- infos/licence = BSD
- infos/needs =
- infos/provides = storage
- infos/recommends =
- infos/placements = getstorage setstorage
- infos/status = maintained conformant compatible coverage specific unittest tested nodep libc configurable preview memleak experimental difficult old nodoc concept
- infos/metadata =
- infos/description = reads complete file into a key

## Introduction

The file plugin reads the content of a file and stores it into the parent key.

## Configuration

- `binary` 

	treats the file as a binary file instead of a text file

- `info`

	adds additional informations about the file as metadata to the parent key.
	
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

None.

## Limitations

None.
