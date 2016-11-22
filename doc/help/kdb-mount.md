kdb-mount(1) - Mount a file to the key database
===============================================

## SYNOPSIS

`kdb mount [<path> <mountpoint>] [<plugin> [<config>] [..]]`  

- Where `path` is the path to the file the user wants to mount.
  See `kdb info resolver` for details what an absolute and relative path means.
  See also IMPORTANT below.
- `mountpoint` is where in the key database the new backend should be mounted.
  For a cascading mountpoint, `mountpoint` should start with `/`.
  See also IMPORTANT below.
- A list of such plugins with a configuration for each of them can be given:
 - `plugin` should be an Elektra plugin.
 - Plugins may be followed by a `,` separated list of `keys=values` pairs which will be used as plugin configuration.


## DESCRIPTION

This command allows a user to mount a new *backend*.
The idea of mounting is explained [in elektra-mounting(7)](elektra-mounting.md).

Mounting in Elektra allows the user to mount a file into the current key database like a user may mount a partition into the current filesystem.
This functionality is key to Elektra as it allows users to build a global key database comprised of many different configuration files.
A backend acts as a worker to allow Elektra to interpret configuration files as keys in the central key database such that any edits to the keys are reflected in the file and vice versa.
Additionally, the user can use this command to list the currently mounted backends by running the command with no arguments.


## IMPORTANT

This command writes into the `/etc` directory and as such it requires root permissions.
Use `kdb file system/elektra/mountpoints` to find out where exactly it will write to.

Absolute paths are still relative to their namespace (see `kdb info resolver`).
Only system+spec mountpoints are actually absolute.
Read [elektra-namespaces(7)](elektra-namespaces.md) for further information.

For cascading mountpoints (starting with `/`) a mountpoint for the namespace
`dir`, `user` and `system` is created. Each of this mountpoint uses a different
configuration file, either below current directory, below home directory
or anywhere in the system.
Use `kdb file <path>` to determine where the file(s) are.


## OPTIONS

- `-H`, `--help`:
  Show the man page.
- `-V`, `--version`:
  Print version info.
- `-p`, `--profile`=<profile>:
  Use a different kdb profile.
- `-d`, `--debug`:
  Give debug information or ask debug questions (in interactive mode).
- `-q`, `--quiet`:
  Suppress non-error messages.
- `-i`, `--interactive`:
  Instead of passing all mounting information by parameters ask the user interactively.
- `-R`, `--resolver`=<name>:
  Specify the resolver plugin to use if no resolver is given, the default resolver is used.
  See also [below in KDB](#KDB).
- `-0`, `--null`:
  Use binary 0 termination.
- `-1`, `--first`:
  Suppress the first column.
- `-2`, `--second`:
  Suppress the second column.
- `-3`, `--third`:
  Suppress the third column.
- `-c`, `--plugins-config`=<config>:
  Add a plugin configuration for all plugins.
- `-C`, `--color`=[when]:
  Print never/auto(default)/always colored output.
- `-W`, `--with-recommends`:
  Also add recommended plugins and warn if they are not available.



## KDB

- `/sw/elektra/kdb/#0/current/quiet`:
  Same as `-q`: Suppress default messages.

- `/sw/elektra/kdb/#0/current/resolver`:
  The resolver that will be added automatically, if `-R` is not given.

- `/sw/elektra/kdb/#0/current/plugins`:
  It contains a space-separated list of plugins and their configs
  which are added automatically (by default sync).
  The plugin-configuration syntax is as described above in the
  [synopsis](#SYNOPSIS).



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
`kdb mount recode.txt dir/recode ni rename cut=path iconv to=utf8,from=latin1`

## SEE ALSO

- [elektra-glossary(7)](elektra-glossary.md).
- [kdb-spec-mount(7)](kdb-spec-mount.md).
- [kdb-umount(7)](kdb-umount.md).
- [elektra-mounting(7)](elektra-mounting.md).
- [elektra-plugins-framework(7)](elektra-plugins-framework.md).
