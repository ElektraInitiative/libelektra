# How-To: kdb export

## Introduction

The kdb tool allows users to interact with Elektra’s Key Database via the command line.
This tutorial explains the export function of kdb. This command lets you export Keys from
the Elektra Key Database.

The command to use kdb export is:

```sh
kdb export [options] source [format]
```

In this command, source is the root key of which Keys should be exported. For
instance, `kdb export system:/export` would export all the keys below
`system:/export`. Additionally, this command exports keys under the `system:/elektra`
directory by default. It does this so that information about the keys stored under
this directory will be included if the Keys are later imported into an Elektra Key
Database. This command exports keys to `stdout` to store them into the Elektra
Key Database. Typically, the export command is used with redirection to write the
Keys to a file.

### Format

The format argument can be a very powerful option to use with kdb export.
The format argument allows a user to specify which plugin is used to export the
keys from the key database. The user can specify any storage plugin to serve as the
format for the exported Keys. For instance, if a user mounted their hosts file to `system:/hosts`
using `kdb mount /etc/hosts system:/hosts hosts`, they would be able to export these keys using
the hosts format by using the command `kdb export system:/hosts hosts > hosts.ecf`.
This command would essentially create a backup of their current `/etc/hosts` file in a valid format
for `/etc/hosts`.

If no format is specified, the format `dump` will be used instead. The dump format is the standard way
of expressing keys and all their relevant information. This format is intended to be used only within Elektra.
The dump format is a good means of backing up Keys from the key database for use with Elektra later
such as reimporting them. At the time of writing, `dump` is the only way to fully preserve all parts of the
`KeySet`.

## Options

The kdb export command takes one special option: `-E` or alternatively `--without-elektra`, which tells `kdb` to omit the `system:/elektra` directory of keys.

## Example

```sh
kdb export system:/backup > backup.ecf
```

This command would export all keys stored under `system:/backup`, along with relevant keys in `system:/elektra`, into a file called `backup.ecf`.
