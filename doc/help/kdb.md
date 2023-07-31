# kdb(1) -- key database access tools

## INTRODUCTION

**kdb** provides access to the global key database of Elektra via
command-line.

Concepts are in man page section 7 and are prefixed with `elektra-`.
If you do not yet know about Elektra,
you should start reading [elektra-introduction(7)](elektra-introduction.md).
CLI Tools of Elektra are in man page section 1 and are prefixed with `kdb-`.

The man pages can also be viewed online at:
https://doc.libelektra.org/api/latest/html/pages.html

And the page you are currently reading at:
https://doc.libelektra.org/api/latest/html/doc_help_kdb_md.html

## OVERVIEW

In this manual we give an overview of the tool suite
`kdb`. It is part of Elektra’s tools. The tool suite `kdb` consists
of individual commands. Most commands are independent and some commands
are sharing an executable. Some commands are written as external scripts.

The included commands can be listed via:<br>
`kdb`

External commands can be listed via:<br>
`kdb list-tools`

Only a few commands are enough for daily use.
We can retrieve a key by:<br>
`kdb get user:/key`

We store a key permanently with a value given by:<br>
`kdb set user:/key value`

We list all available keys arranged below a key by:<br>
`kdb ls user:/key`

Documentation of plugins is available using the
[kdb-plugin-info(1)](kdb-plugin-info.md) tool:<br>
`kdb plugin-info`

Run `kdb plugin-list` for a list of plugins:<br>
`kdb plugin-list`

Each of these commands has its own man page for
further details.

## BASIC OPTIONS

Every core-tool has the following options:

- `-H`, `--help`:
  Show usage of command.
- `-V`, `--version`:
  Print version info.
- `-C`, `--color <when>`:
  Print never/auto(default)/always colored output.
- `--`:
  Do not process any following arguments starting with `-` as options.

## COMMON OPTIONS

Most tools have the following options:

- `-v`, `--verbose`:
  Explain what is happening. Also shows Configfile and Mountpoint in case of an error/warning
- `-q`, `--quiet`:
  Suppress non-error messages.
- `-d`, `--debug`:
  Shows the line at which an error happened in case an error or warning is issued

## KDB

The `kdb` utility reads its own configuration from three sources
within the KDB (key database):

1. /sw/kdb/**profile**/ (for legacy configuration)
2. /sw/elektra/kdb/#0/%/ (for empty profile, `%` in Elektra
   is used to represent emptiness)
3. /sw/elektra/kdb/#0/**profile**/ (for current profile,
   if no `-p` or `--profile` is given, `current` will be
   used)

Here the last source where a configuration value is found, wins.

For example, to permanently change verbosity one can use:

- `kdb set /sw/elektra/kdb/#0/current/verbose 1`:
  Be verbose for every tool.

- `kdb set /sw/elektra/kdb/#0/current/quiet 1`:
  Be quiet for every tool.

If `%` is passed to
`-p` or `--profile`, the KDB will not be consulted for configuration and
only the command-line arguments are used.

## PROFILES

Profiles allow users to change many/all configuration settings of a tool
at once. It influences from where the KDB entries are read.
Without a `-p` or `--profile` argument following profiles are used
(in the order of preference):

- `current`:
  Is the profile to be used only if no `-p` argument was used.
- `%`:
  Is the fallback profile. It will be used if keys cannot be found in the main profile.

For example if you use:<br>
`kdb export -p admin system`

It will read its format configuration from `/sw/elektra/kdb/#0/admin/format`.

If you want different configuration per user or directories, you should prefer
to use the `user` and `dir` namespaces. Then the correct configuration will
be chosen automatically according to the current user or current working directory.

Sometimes it is useful to start with default options, for example it is not
possible to invert the `-q` option.
In such situations one can simply select a non-existing profile, then `-q`
works as usual:<br>
`kdb mount -p nonexist -q /abc dir:/abc`

If `%` is used as profile name for `-p`, the `kdb` tools disables reading from `KDB`
for their own configuration settings. Then, they only use command-line arguments.

To explicitly state the default behavior, we use:<br>
`-p current`

To state that we do not want to read any configuration settings for `kdb`
from KDB, we use:<br>
`-p %`

> Note that KDB will still be used to perform the actions you want to perform
> with the `kdb` tool.

## BOOKMARKS

Elektra recommends [to use rather long paths](/doc/tutorials/application-integration.md)
because it ensures flexibility in the future (e.g. to use profiles and have a compatible
path for new major versions of configuration).

Long paths are, however, cumbersome to enter in the CLI.
Thus one can define bookmarks. Bookmarks are keys whose key name starts with `+`.
They are only recognized by the `kdb` tool or tools that explicitly have
support for it. Your applications should not depend on the presence of a
bookmark.

Bookmarks are stored below:<br>
`/sw/elektra/kdb/#0/current/bookmarks`

For every key found there, a new bookmark will be introduced.

Bookmarks can be used to start key names by using `+` (plus) as first character.
The string until the first `/` will be considered as bookmark.

For example, if you set the bookmark kdb:

```sh
kdb set user:/sw/elektra/kdb/#0/current/bookmarks
kdb set user:/sw/elektra/kdb/#0/current/bookmarks/kdb user:/sw/elektra/kdb/#0/current
```

You are able to use:

```sh
kdb ls +kdb/bookmarks
kdb get +kdb/format
```

## RETURN VALUES

- 0:
  successful.
- 1:
  Invalid options passed.
- 2:
  Invalid arguments passed.
- 3:
  Command terminated unsuccessfully without specifying error code.
- 4:
  Unknown command.
- 5:
  KDB Error, could not read/write from/to KDB.
- 6:
  Reserved error code.
- 7:
  Unknown errors, wrong exceptions thrown.
- 8-10:
  Reserved error codes.
- 11-20:
  Command-specific error codes. See man page of specific command.

## SEE ALSO

- [elektra-introduction(7)](elektra-introduction.md)
- [kdb(1)](kdb.md)
- Get a [big picture about Elektra](/doc/BIGPICTURE.md)
