- infos = Information about FSTAB plugin is in keys below
- infos/author = Markus Raab <elektra@libelektra.org>
- infos/licence = BSD
- infos/provides = storage/fstab
- infos/needs =
- infos/recommends = struct type path
- infos/placements = getstorage setstorage
- infos/status = tested/unit nodep experimental obsolete
- infos/features/storage = limited
- infos/description = Parses files in a syntax like /etc/fstab file

## Introduction

This plugin is an implementation of a parser and generator of the /etc/fstab file.

## Installation

See [installation](/doc/INSTALL.md).
The package is called `libelektra5-extra`.

## Old fstab Entries

(Deprecated, remove this section after it is reimplemented in the new way)

For each device in fstab elektra will store the following keys:

```
pseudoname/device
pseudoname/mpoint
pseudoname/type
pseudoname/options
pseudoname/dumpfreq
pseudoname/passno
```

Each represents a column in fstab.

The pseudoname can be any name for setting keys,
the will be generated when getting keys, so don't
expect the same name.

the directory `/` will be called `rootfs`

all swap devices will be called `swapXX` with a number from 00 on for XX

otherwise the mount point without the '/' character will be used.

At the other point there is the issue with the pseudonames,
you can't rely on the pseudoname you have set.

The biggest issue is that you can't change or delete existing
entries. All entries you set will be appended to the other filesystems.

So if you get the filesystems and change the type of the file system
of the rootfs and set it again the resulting fstab will be like:

```
/dev/sda6       /               ext3>----   >----defaults,errors=remount-ro 0 1
/dev/sda6       /               jfs>----   >----defaults,errors=remount-ro 0 1
```

which will be not like you desired!

setmntent is used, so it is only conforming to BSD 4.3 and linux and you
can't use any comments.

## New fstab Entries

Specification:

```ini
[/_]
type = array
explanation = the name of the key is the mount point (so the former
  mpoint is not needed); the value is the number of entries in the
  array
[/_/#]
explanation = an entry in the array
[/_/#/device]
[/_/#/type]
[/_/#/options]
[/_/#/dumpfreq]
[/_/#/passno]
```

Example: A fstab that looks like:

```
/dev/sr0        /media/cdrom   udf,iso9660 user,noauto     0       0
```

would have a key name that is an array (so the value is the number of
children, in this case 1):

```
system:/filesystems/\/media\/cdrom
```

with the array entry:

```
system:/filesystems/\/media\/cdrom0/#0/
```

So when following line is added

```
/dev/sr0        /media/cdrom   ramfs user,noauto     0       0
```

Implementation hint: use `keyAddBaseName()` to get escaping of `/`, then
add array items below it

If a mount point exists more than once (that could be proc, swap or
overlay mount points) the array below gets incremented (otherwise #0 is
used for every unique entry).

The order of the array must, of course, be preserved. Other lines may
be reordered for now, a proper "order" could be done later.

Spaces in the names are replaced by \040 in the fstab.

## Example

Mount the plugin:

```sh
sudo kdb mount /etc/fstab system:/filesystems fstab struct type path
```
