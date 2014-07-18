- infos = Information about FSTAB plugin is in keys below
- infos/author = Markus Raab <elektra@libelektra.org>
- infos/licence = BSD
- infos/needs =
- infos/provides = storage
- infos/placements = getstorage setstorage
- infos/recommends = struct type path
- infos/description = Parses files in a syntax like /etc/fstab file

## Introduction ##

This plugin is an implementation of a parser and generator of the /etc/fstab file.


## Special Values ##

####fstab Devices####
For each device in fstab elektra will store the following keys:
 pseudoname/device
 pseudoname/mpoint
 pseudoname/type
 pseudoname/options
 pseudoname/dumpfreq
 pseudoname/passno

Each represents a column in fstab.

The pseudoname can be any name for setting keys,
the will be generated when getting keys, so don't
expect the same name.

the directory / will be called
 rootfs

all swap devices will be called
 swapXX
with a number from 00 on for XX

otherwise the mountpoint without  the '/' character will be used.


## Restrictions ##

setmntent is used and restrict the capabilities in many ways.
At one point you can't use any comments, they will be generated
automatically.

At the other point there is the issue with the pseudonames,
you can't rely on the pseudoname you have set.

The biggest issue is that you can't change or delete existing
entries. All entries you set will be appended to the other filesystems.

So if you get the filesystems and change the type of the filesystem
of the rootfs and set it again the resulting fstab will be like:
 /dev/sda6       /               ext3>----   >----defaults,errors=remount-ro 0 1
 /dev/sda6       /               jfs>----   >----defaults,errors=remount-ro 0 1

which will be not like you desired!

#### Portability ####

setmntent is used, so it is only conforming to BSD 4.3 and linux.

## Examples ##

##Examples##

Mount the plugin:
>$ kdb mount /etc/fstab system/fstab fstab