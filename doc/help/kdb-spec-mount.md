kdb-spec-mount(1) - Mount a spec file to the key database
=========================================================

## SYNOPSIS

`kdb spec-mount [/<mountpoint>] [<plugin> [<config>] [..]]`  

- `mountpoint` is where in the key database the new backend should be mounted to.
  It must be a cascading mount pount, i.e., `mountpoint` must start with `/`.
- `plugin` are be extra Elektra plugins to be used (next to the one specified in `spec/`).
- Plugins may be followed by a `,` separated list of `keys=values` pairs which will be used as plugin configuration.

`kdb smount` is an alias and can be used in the same way as `kdb spec-mount`.


## DESCRIPTION

This command allows a user to mount a new *backend* described by a previously mounted specification.
To mount a specification file first use [kdb-mount(7)](kdb-mount.md).

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
- `-p`, `--profile`=<profile>:
  Use a different kdb profile.
- `-R`, `--resolver`=<name>:
  Specify the resolver plugin to use if no resolver is given, the default resolver is used.
  See also [below in KDB](#KDB).
  Note that the resolver will only added as dependency, but not directly added.
- `-c`, `--plugins-config`=<config>:
  Add a plugin configuration for all plugins.
- `-C`, `--color`=[when]:
  Print never/auto(default)/always colored output.
- `-W`, `--with-recommends`:
  Also add recommended plugins and warn if they are not available.



## KDB

- `/sw/elektra/kdb/#0/current/resolver`:
  The resolver that will be added automatically, if `-R` is not given.

- `/sw/elektra/kdb/#0/current/plugins`:
  It contains a space-separated list of plugins and their configs
  which are added automatically (by default sync).
  The plugin-configuration syntax is as described above in the
  [synopsis](#SYNOPSIS).


## EXAMPLES

To mount /example as described in `spec/example`:  
`kdb spec-mount /example`

Additionally, add `ini` plugin (instead of some default resolver) with `some` as config:  
`kdb spec-mount /example ini some=value`

## SEE ALSO

- [elektra-glossary(7)](elektra-glossary.md).
- [kdb-umount(7)](kdb-umount.md).
- [elektra-mounting(7)](elektra-mounting.md).
- [elektra-plugins-framework(7)](elektra-plugins-framework.md).
