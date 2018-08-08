- infos = Information about path plugin is in keys below
- infos/author = Markus Raab <elektra@libelektra.org>
- infos/licence = BSD
- infos/placements = presetstorage
- infos/needs =
- infos/provides = check
- infos/status = maintained nodep libc nodoc
- infos/metadata = check/path check/permission
- infos/description = Checks if keys enriched with appropriate metadata contain valid paths as values as well
as correct permissions

## Introduction

This plugin checks whether the value of a key is a valid file system path and optionally if 
correct permissions are set for a certain user. 

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

If `check/permission = [permission], [user]` is also present it will check for the correct permissions
of the file/directory. `check/permission = rw` and `check/permission/user = tomcat` for example will check if the user `tomcat` has read and 
write access to the path which was set in `check/path`.
 
 Permissions available:
 - `r`: **R**ead
 - `w`: **W**rite
 - `x`: e**X**ecute

## Usage

If the metakey `check/path` is present, it is checked if the value is a
valid absolute file system path. If a metavalue is present, an additional
check will be done if it is a directory or device file.

## Example
```sh
sudo kdb mount some_file.ecf /tests/path some_file dump

# Assume user tomcat has read and write permissions on /var/log/application-file.log
kdb set /tests/path/value /var/log/application-file.log

#This checks if the file actually exists
sudo kdb setmeta user/tests/path/value check/path

#This checks if the user has read and write permissions for the application-file.log file
sudo kdb setmeta user/tests/path/value check/permission "rw, tomcat"

#Generate a file which is only accessable for root
touch /var/log/application-file-restricted.log
sudo chmod 700 /var/log/application-file-restricted.log
sudo chown root:root /var/log/application-file-restricted.log

#This should trigger the error
kdb set /tests/path/value /var/log/application-file-restricted.log
# ERROR:194
# Reason: User tomcat has no permission to read the given file.

# cleanup
kdb rm -r /tests/path
sudo kdb umount /tests/path
```

## Future work
Add a check which ensures that the given path is a file/directory/symbolic link/hard link/ etc.