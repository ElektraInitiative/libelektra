kdb-mount(1) - Mount a file to the key database
===============================================

## SYNOPSIS

`kdb mount [<path> <mountpoint>] [<plugin> [<config>] [..]]`  

- Where `path` is the path to the file the user wants to mount.
  See `kdb info resolver` for details what an absolute and relative path means.
- `mountpoint` is where in the key database the new backend should be mounted. (For a cascading mount pount, `mountpoint` should start with `/`)  
- `plugin` should be an Elektra plugin.
  A list of such plugins with configuration can be given.
- Plugins may be followed by a `,` separated list of keys and their corresponding values which will be written below the backend configuration.  


## DESCRIPTION

This command allows a user to mount a new *backend*.

The idea of mounting is explained [in elektra-mounting(7)](elektra-mounting.md).

Mounting in Elektra allows the user to mount a file into the current key database like a user may mount a parition into the current filesystem by creating a *backend*.  
This functionality is key to Elektra as it allows users to build a global key database comprised of many different conifguration files.  
A backend acts as a worker to allow Elektra to interpret configuration files as keys in the central key database such that any edits to the keys are reflected in the file and vice versa.  
Additionally, the user can use this command to list the currently mounted backends by running the command with no arguments.  



## IMPORTANT

This command writes into the `/etc` directory and as such it requires root permissions.
Use `kdb file system/elektra/mountpoints` to find out where exactly it will write to.


## OPTIONS

- `-H`, `--help`:
  Show the man page.
- `-V`, `--version`:
  Print version info.
- `-d`, `--debug`:
  Give debug information or ask debug questions (in interactive mode).
- `-i`, `--interactive`:
  Instead of passing all mounting information by parameters ask the user interactively.
- `-R`, `--resolver <name>`:
  Specify the resolver plugin to use if no resolver is given, the default resolver is used.
  See also `/sw/kdb/current/resolver` [below](#KDB).
- `-0`, `--null`:
  Use binary 0 termination.
- `-1`, `--first`:
  Suppress the first column.
- `-2`, `--second`:
  Suppress the second column.
- `-3`, `--third`:
  Suppress the third column.
- `-c`, `--plugins-config`:
  Add a plugin configuration for all plugins.



## KDB

- `/sw/kdb/current/resolver`:
  The default resolver that will be added automatically.

- `/sw/kdb/current/plugins`:
  It contains a space-separated list of plugins
  which are added automatically (by default sync).


## EXAMPLES

To list the currently mounted backends:  
	`kdb mount`  

To mount a system configuration file using the ini format:  
	`kdb mount /etc/configuration.ini system/example ini`  

Print a null-terminated output of paths and backend names:  
	`kdb mount -02 | xargs -0n 2 echo`  

To mount the /etc/file system file with two plugins with a respective configuration option each:  
	`kdb mount /etc/file system/file plugin1 plugin1config=config1 plugin2 plugin2config=config2`  

To mount the /etc/file system file with two plugins and setting both to be verbose:  
	`kdb mount -c verbose=1 /etc/file system/file plugin1 plugin2`

To recode and rename a configuration file using Elektra:  
	`kdb mount s.ini recode.txt ni rename cut=path iconv recode=utf8..latin1`  

## SEE ALSO

- [elektra-mounting(7)](elektra-mounting.md).
- [elektra-plugins(7)](elektra-plugins.md).
