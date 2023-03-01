# kdb-spec-mount(1) - Mount a spec file to the key database

## SYNOPSIS

`kdb spec-mount [/<mountpoint>] [<plugin> [<config>] [..]]`

- `mountpoint` is where in the key database the new backend should be mounted to.
  It must be a cascading mount point, i.e., `mountpoint` must start with `/`.
- `plugin` are extra Elektra plugins to be used (next to the one specified in `spec:/<mountpoint>`).
- Plugins may be followed by a `,` separated list of `keys=values` pairs which will be used as plugin configuration.

## DESCRIPTION

This command allows a user to mount a new _backend_ described by a previously mounted specification.
To mount a specification file to `spec`-[namespace](elektra-namespaces.md) first use [kdb-mount(7)](kdb-mount.md):

```sh
sudo kdb mount /path/to/some-spec-file.ini spec:/example/mountpoint ni
```

The idea of mounting is explained [in elektra-mounting(7)](elektra-mounting.md).
The `spec` [namespace](elektra-namespaces.md) contains metaconfiguration that describes the configuration in all other namespaces.
The metadata used for the specification is described in [METADATA.ini](/doc/METADATA.ini).

During `spec-mount` the `spec` keys are searched for relevant metadata:

- For every metadata `mountpoint` an additional cascading mount point will be mounted.
  The metadata `mountpoint` is usually at the parent key (the top-level key of the specification).
- The `infos/*` and `config/needs` from [CONTRACT.ini](/doc/CONTRACT.ini), that are tagged by `usedby = spec`, will work as described there.
- For other metadata suitable plugins are searched and mounted additionally.

For example:

```sh
kdb meta get spec:/example/mountpoint mountpoint  # verify that we have a mountpoint here
sudo kdb spec-mount /example/mountpoint  # mounts /example/mountpoint according to
                                         # the specification as found at
                                         # spec:/example/mountpoint
```

## IMPORTANT

This command writes into the `/etc` directory and as such it requires root permissions.
Use `kdb file system:/elektra/mountpoints` to find out where exactly it will write to.

## OPTIONS

- `-H`, `--help`:
  Show the man page.
- `-V`, `--version`:
  Print version info.
- `-p`, `--profile <profile>`:
  Use a different kdb profile.
- `-C`, `--color <when>`:
  Print never/auto(default)/always colored output.
- `-v`, `--verbose`:
  Explain what is happening.
- `-q`, `--quiet`:
  Suppress non-error messages.
- `-R`, `--resolver <resolver>`:
  Specify the resolver plugin to use if no resolver is given, the default resolver is used.
  See also [below in KDB](#KDB).
  Note that the resolver will only added as dependency, but not directly added.
- `-c`, `--plugins-config <plugins-config>`:
  Add a plugin configuration for all plugins.
- `-W`, `--with-recommends`:
  Also add recommended plugins and warn if they are not available.

## KDB

- `/sw/elektra/kdb/#0/current/verbose`:
  Same as `-v`: Explain what is happening.

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

To mount /example as described in `spec:/example`:<br>
`sudo kdb spec-mount /example`

Additionally, add `ini` plugin (instead of some default resolver) with a config for INI:<br>
`sudo kdb spec-mount /example ini section=NULL`

## SEE ALSO

- [elektra-glossary(7)](elektra-glossary.md).
- [kdb-umount(7)](kdb-umount.md).
- [elektra-mounting(7)](elektra-mounting.md).
- [see application integration tutorial](/doc/tutorials/application-integration.md)
- [see validation tutorial](/doc/tutorials/validation.md)
