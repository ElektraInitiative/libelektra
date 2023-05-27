# kdb-mount(1) - Mount a file to the key database

## SYNOPSIS

`kdb mount [<path> <mount point>] [<plugin> [<config>] [..]]`<br>

- Where `path` is the path to the file the user wants to mount.
  See `kdb plugin-info resolver` for details what an absolute and relative path means.
  See also IMPORTANT below.

- `mountpoint` is where in the key database the new backend should be mounted.
  For a cascading mount point, `mountpoint` should start with `/`.
  See also IMPORTANT below.

- A list of such plugins with a configuration for each of them can be given:
  - `plugin` should be an Elektra plugin.
  - Plugins may be followed by a `,` separated list of `keys=values` pairs which will be used as plugin configuration.

## DESCRIPTION

This command allows a user to persistently mount a new _backend_.
Mounting in Elektra allows the user to mount a file into the current key database like a user may mount a partition into the current file system.
This functionality is key to Elektra as it allows users to build a global key database comprised of many different configuration files.

A backend acts as a worker to allow Elektra to interpret configuration files as keys in the central key database such that any edits to the keys are reflected in the file and vice versa.
Additionally, the user can use this command to list the currently mounted backends by running the command with no arguments.
More about mounting is explained in [elektra-mounting(7)](elektra-mounting.md).

## IMPORTANT

This command writes into the `/etc` directory to make the mounting persistent.
As such it requires root permissions.
Use `kdb file system:/elektra/mountpoints` to find out where exactly it will write to.

Absolute paths are still relative to their namespace (see `kdb plugin-info resolver`).
Only system+spec mount points are actually absolute.
Read [elektra-namespaces(7)](elektra-namespaces.md) for further information.

For cascading mount points (starting with `/`) a mount point for the namespace
`dir`, `user` and `system` is created. Each of this mount point uses a different
configuration file, either below current directory, below home directory
or anywhere in the system.
Use `kdb file <path>` to determine where the file(s) are.

## OPTIONS

- `-H`, `--help`:
  Show usage of command.
- `-V`, `--version`:
  Print version info.
- `-C`, `--color <when>`:
  Print never/auto(default)/always colored output.
- `-v`, `--verbose`:
  Explain what is happening. Prints additional information in case of errors/warnings.
- `-d`, `--debug`:
  Give debug information or ask debug questions (in interactive mode). Prints additional information in case of errors/warnings.
- `-q`, `--quiet`:
  Suppress non-error messages.
- `-i`, `--interactive`:
  Instead of passing all mounting information by parameters ask the user interactively.
- `-s`, `--strategy`:
  (experimental, use with care)
  By default mounting is rejected if the mountpoint already exists. With the strategy
  _unchanged_ you can change the behavior to be successful if _exactly_ the same config
  would be written (see #1306 why this does not always work correctly).
- `-R`, `--resolver <resolver>`
  Specify the resolver plugin to use if no resolver is given, the default resolver is used.
  See also [below in KDB](#KDB).
- `-0`, `--null`:
  Use binary 0 termination.
- `-1`, `--first`:
  Suppress the first column.
- `-2`, `--second`:
  Suppress the second column.
- `-c`, `--plugins-config <plugins-config>`:
  Add a plugin configuration for all plugins.
- `-W`, `--with-recommends`:
  Also add recommended plugins and warn if they are not available.
- `-f`, `--force`:
  Unmount before mounting: Does not fail on already existing mount points.

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

To list the currently mounted backends:<br>
`kdb mount`

To mount a system configuration file using the ini format:<br>
`kdb mount /etc/configuration.ini system:/example ini`

Print a null-terminated output of paths and backend names:<br>
`kdb mount -02 | xargs -0n 2 echo`

To mount the /etc/file system file with two plugins with a respective configuration option each:<br>
`kdb mount /etc/file system:/file plugin1 plugin1config=config1 plugin2 plugin2config=config2`

To mount the /etc/file system file with two plugins and setting both to be verbose:<br>
`kdb mount -c verbose=1 /etc/file system:/file plugin1 plugin2`

To recode and rename a configuration file using Elektra:<br>
`kdb mount recode.txt dir:/recode ni rename cut=path iconv to=utf8,from=latin1`

## SEE ALSO

- [elektra-glossary(7)](elektra-glossary.md).
- [kdb-spec-mount(1)](kdb-spec-mount.md).
- [kdb-umount(1)](kdb-umount.md).
- [kdb-mountOdbc(1)](kdb-mountOdbc.md).
- [elektra-mounting(7)](elektra-mounting.md).
