kdb-mount(1) - Mount a file to the key database
===============================================

## DESCRIPTION

This command allows a user to mount a new "backend".  
A backend allows a file to be interpreted as keys in the key database such that any edits to the keys are reflected in the file and vice versa.  
Additionally, the user can use this command to list the currently mounted backends by running the command with no arguments.  

## USAGE

`kdb mount [<path> <mountpoint>] [<plugin> [<config>] [..]]`  

Where `path` is the path to the file the user wants to mount. (Absolute for system files, relative for user files)  
`mountpoint` is where in the key database the new backend should be mounted. (For a cascading mount pount, `mountpoint` should start with `/`)  
`plugin` should be an Elektra plugin, or a list of plugins, as well as any necessary configuration arguments for those plugins.  
Plugins may be followed by a `,` seperated list of keys and their corresponding values which will be written below the backend configuration.  

## DEFAULT PLUGINS

Some plugins, such as `sync` are added automatically when mounting a backend.  
The user can change which plugins are added by editing the space-separated list stored in the follwing key:  
`/sw/kdb/current/plugins`  

## EXAMPLES

To list the currently mounted backends:  
	`kdb mount`  

To mount a system configuration file using the ini format:  
	`kdb mount /etc/configuration.ini system/example ini`  

Print a null-terminated output of paths and backend names:  
	`kdb mount -02 | xargs -0n 2 echo`  

To mount the /etc/file system file with two plugins with a respective configuration option each:  
	`kdb mount /etc/file system/file plugin1 plugin1config=config1 plugin2 plugin2config=config2`  

To recode and rename a configuration file using Elektra:  
	`kdb mount s.ini recode.txt ni rename cut=path iconv recode=utf8..latin1`  

