# kdb-global-mount(1) - Globally mount plugins

## SYNOPSIS

`kdb global-mount [<plugin> [<config>] [..]]`

- `plugin` are the Elektra plugins to be mounted globally.
- Plugins may be followed by a `,` separated list of `keys=values` pairs which will be used as plugin configuration.

`kdb gmount` is an alias and can be used in the same way as `kdb global-mount`.

## DESCRIPTION

This command allows a user to globally mount some plugins that will be part of every interaction with the global keydatabase.

## IMPORTANT

This command writes into the `/etc` directory and as such it requires root permissions.
Use `kdb file system/elektra/globalplugins` to find out where exactly it will write to.

## OPTIONS

- `-H`, `--help`:
  Show the man page.
- `-V`, `--version`:
  Print version info.
- `-p`, `--profile <profile>`:
  Use a different kdb profile.
- `-C`, `--color <when>`:
  Print never/auto(default)/always colored output.
- `-W`, `--with-recommends`:
  Also add recommended plugins and warn if they are not available.
- `-v`, `--verbose`:
  Explain what is happening. Prints additional information in case of errors/warnings.
- `-d`, `--debug`:
  Give debug information. Prints additional debug information in case of errors/warnings.

## KDB

- `/sw/elektra/kdb/#0/current/plugins/global`:
  It contains a space-separated list of plugins
  which are added automatically (by default `spec`).
  The plugin-configuration syntax is as described above in the [synopsis](#SYNOPSIS).

## EXAMPLES

Trace every interaction with the key database (very noisy!):<br>
`kdb global-mount tracer`

For every change of KDB, write to syslog and notify by dbus:<br>
`kdb global-mount syslog dbus`

## SEE ALSO

- [elektra-glossary(7)](elektra-glossary.md).
- [kdb-umount(1)](kdb-umount.md).
- [elektra-mounting(7)](elektra-mounting.md).
