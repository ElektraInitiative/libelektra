- infos = Information about path plugin is in keys below
- infos/author = Markus Raab <elektra@libelektra.org>
- infos/licence = BSD
- infos/needs =
- infos/provides = check
- infos/placements = presetstorage
- infos/description = Checks if keys enriched with appropriate metadata contain valid paths as values

## Introduction ##

This plugin checks whether a Key is a valid path or not. 

## Purpose ##

The motivation to write this plugin is given by the two paths that exist in/etc/fstab: the device ﬁle and the mountpoint. A missing ﬁle is not necessarily an error,because the device ﬁle may appear later when a device is plugged in and the mountpoint may be there when another subsequent mount was executed. So only warnings are yielded in that case. One situation, however, presents an error: Only an absolute path is allowed to occur for both device and mount point. When checking for relative ﬁles, it is not enough to look at the ﬁrst character if it is a `/`, because remote ﬁle systems and some special names are valid, too.

## Usage ##

If the meta key `check/path` is present, it is checked if the value is a valid absolutepath. If a metavalue is present, an additional check will be done if it is a directory or device ﬁle.

