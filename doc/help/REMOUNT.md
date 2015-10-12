kdb-remount(1) - Use an existing backend to mount a new file
============================================================

## DESCRIPTION

This command allows a user to use an existing backend (such as one from a previous mount) to mount a new configuration file to a new mount point in the key database.  

## USAGE

`kdb remount <new path> <new mountpoint> <existing mountpoint>`

Where `new path` is the path to the file the user wants to mount, (Absolute for system files, relative for user files)  
`new mountpoint` is where in the key database the new configuration file should be mounted at, (For a cascading mount pount, `mountpoint` should start with `/`)  
and `existing mountpoint` is the mountpoint where the existing backend is mounted.  

## EXAMPLES

To mount a new file using an existing backend that is mounted to `system/example`:  
	`kdb remount /etc/configuration2.ini system/example2 system/example`  

