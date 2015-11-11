- infos = Information about path plugin is in keys below
- infos/author = Markus Raab <elektra@libelektra.org>
- infos/licence = BSD
- infos/needs =
- infos/provides = check
- infos/placements = presetstorage
- infos/description = Checks if keys enriched with appropriate metadata contain valid paths as values

## Introduction ##

This plugin checks whether the value of a key is a valid filesystem path. 

## Purpose ##

The motivation to write this plugin is given by the two paths that exist
in /etc/fstab: the device file and the mountpoint. A missing file is
not necessarily an error, because the device file may appear later when
a device is plugged in and the mountpoint may be there when another
subsequent mount was executed. So only warnings are yielded in that
case. One situation, however, presents an error: Only an absolute path
is allowed to occur for both device and mount point. When checking for
relative files, it is not enough to look at the first character if it is
a `/`, because remote file systems and some special names are valid, too.

## Usage ##

If the meta key `check/path` is present, it is checked if the value is a
valid absolute filesystem path. If a metavalue is present, an additional
check will be done if it is a directory or device file.

