- infos = Information about path plugin is in keys below
- infos/author = Markus Raab <elektra@libelektra.org>
- infos/licence = BSD
- infos/placements = presetstorage
- infos/needs =
- infos/provides = check
- infos/status = maintained nodep libc nodoc
- infos/metadata = check/path check/path/mode check/path/user
- infos/description = Checks if keys enriched with appropriate metadata contain valid paths as values as well
  as correct permissions

## Introduction

This plugin checks whether the value of a key is a valid file system path and optionally if
correct permissions are set for a certain user.

## Installation

See [installation](/doc/INSTALL.md).
The package is called `libelektra5-extra`.

## Purpose

The motivation to write this plugin is given by the two paths that exist
in /etc/fstab: the device file and the mountpoint. A missing file is
not necessarily an error, because the device file may appear later when
a device is plugged in and the mountpoint may be there when another
subsequent mount was executed. So only warnings are yielded in that
case. One situation, however, presents an error: Only an absolute path
is allowed to occur for both device and mountpoint. When checking for
relative files, it is not enough to look at the first character if it is
a `/`, because remote file systems and some special names are valid, too.

If `check/path/mode = <permission>` is also present it will check for the correct permissions
of the file/directory. Optionally, you can also add `check/path/user = <user>"` which then checks the permissions
for the given user. When calling `kdb set` on the actual key, you have to run as `root` user
or the file permissions cannot be checked (you will receive an error message). It is also possible to leave the
`check/path/user` empty (just provide an empty string) which then takes the executing user as target to check.
So for example `sudo kdb set ...` will check if `root` can access the target file/directory whereas `kdb set ...`
will take the current executing process/user. If `check/path/user` is not given at all, the plugin
will check accessibility for the `root` user only (which again requires `sudo`)

`check/path/mode = rw` and `check/path/user = tomcat` for example will check if the user
`tomcat` has read and write access to the path which was set for the key. Please note that the file has to exist already
and it is not checked if the user has the right to create a file in the directory.

Permissions available:

- `r`: **R**ead
- `w`: **W**rite
- `x`: e**X**ecute

## Usage

If the metakey `check/path` is present, it is checked if the value is a
valid absolute file system path. If a metavalue is present, an additional
check will be done if it is a directory or device file.

## Examples

An example on which the user should have no permission at all for the root directory.

```sh
sudo kdb mount test.dump user:/tests path dump
sudo kdb set user:/tests/path "$HOME"
sudo kdb meta-set user:/tests/path check/path ""
sudo kdb meta-set user:/tests/path check/path/user ""
sudo kdb meta-set user:/tests/path check/path/mode "rw"

# Standard users should not be able to read/write the root folder
[ $(id -u) = 0 ] && printf >&2 'User is root\n' || kdb set user:/tests/path "/root"
# STDERR: User is root|.*C03200.*

# Set something which the current user can access for sure
kdb set user:/tests/path "$HOME"
# STDOUT-REGEX: .*Set string to "/.*".*

#cleanup
sudo kdb rm -r user:/tests/
sudo kdb umount user:/tests
```

An example where part of the permissions are missing for a tmp file

```sh
sudo kdb mount test.dump user:/tests path dump
sudo kdb set user:/tests/path "$HOME"
sudo kdb meta-set user:/tests/path check/path ""
sudo kdb meta-set user:/tests/path check/path/user ""
sudo kdb meta-set user:/tests/path check/path/mode "rwx"

# Standard users should not be able to read/write the root folder
kdb set user:/tests/path/tempfile $(mktemp)
chmod +rw `kdb get user:/tests/path/tempfile`
kdb set user:/tests/path `kdb get user:/tests/path/tempfile`
# ERROR:C03200

# Set something which the current user can access for sure
chmod +x `kdb get user:/tests/path/tempfile`
kdb set user:/tests/path `kdb get user:/tests/path/tempfile`
# STDOUT-REGEX: Set string to "/.*".*

#cleanup
sudo rm -rf `kdb get user:/tests/path/tempfile`
sudo kdb rm -r user:/tests/
sudo kdb umount user:/tests
```

## Future work

Add a check which ensures that the given path is a file/directory/symbolic link/hard link/etc.
