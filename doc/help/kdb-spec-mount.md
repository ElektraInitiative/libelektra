kdb-spec-mount(1) - Mount a spec file to the key database
=========================================================

## SYNOPSIS

`kdb spec-mount [/<mountpoint>] [<plugin> [<config>] [..]]`  

- `mountpoint` is where in the key database the new backend should be mounted to.
  It must be a cascading mount pount, i.e., `mountpoint` must start with `/`.
- `plugin` are be extra Elektra plugins to be used (next to the one specified in `spec/`).
- Plugins may be followed by a `,` separated list of `keys=values` pairs which will be used as plugin configuration.


## DESCRIPTION

This command allows a user to mount a new *backend* described by an already mounted specification.
To mount a specification use [kdb-mount(7)](kdb-mount.md).

The idea of mounting is explained [in elektra-mounting(7)](elektra-mounting.md) and.

The `spec` [namespace](elektra-namespaces.md) contains meta-configuration that describes the configuration in all other namespaces.
The meta data used for the specification is described in [METADATA.ini](/doc/METADATA.ini).

During `spec-mount` the `spec` keys are searched for relevant meta data:

- For every meta data `mountpoint` an additional cascading mountpoint will be mounted.
- The `infos/*` and `config/needs` from [CONTRACT.ini](/doc/CONTRACT.ini), that are tagged by `usedby = spec`, will work as described there.
- For other meta data suitable plugins are searched and mounted additionally.


## IMPORTANT

This command writes into the `/etc` directory and as such it requires root permissions.
Use `kdb file system/elektra/mountpoints` to find out where exactly it will write to.


## OPTIONS

- `-H`, `--help`:
  Show the man page.
- `-V`, `--version`:
  Print version info.
- `-R`, `--resolver <name>`:
  Specify the resolver plugin to use if no resolver is given, the default resolver is used.
  See also `/sw/kdb/current/resolver` [below](#KDB).
  Note that the resolver will only added as dependency, but not directly added.
- `-c`, `--plugins-config`:
  Add a plugin configuration for all plugins.



## KDB

- `/sw/kdb/current/resolver`:
  The resolver that will be added automatically, if `-R` is not given.

- `/sw/kdb/current/plugins`:
  It contains a space-separated list of plugins
  which are added automatically (by default sync).
  The plugin-configuration syntax is as described above.


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

- [elektra-glossary(7)](elektra-glossary.md).
- [kdb-umount(7)](kdb-umount.md).
- [elektra-mounting(7)](elektra-mounting.md).
- [elektra-plugins(7)](elektra-plugins.md).
