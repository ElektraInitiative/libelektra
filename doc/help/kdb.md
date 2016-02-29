kdb(1) -- key database access tools
===================================

Elektra provides a universal and secure framework to store configuration
parameters in a global, hierarchical key database.

The core is a small library implemented in C. The plugin-based framework fulfills many
configuration-related tasks to avoid any unnecessary code duplication
across applications while it still allows the core to stay without any
external dependency. Elektra abstracts from cross-platform-related issues
with an consistent API, and allows applications to be aware of other
applications' configurations, leveraging easy application integration.

## OVERVIEW

The man pages can also be viewed online at:
http://doc.libelektra.org/api/current/html/pages.html

And the page you are currently reading at:
http://doc.libelektra.org/api/current/html/md_doc_help_kdb.html

Concepts are in man page section 7 and are prefixed with `elektra-`.
You should start reading [elektra-introduction(7)](elektra-introduction.md).

Tools are in man page section 1 and are prefixed with `kdb-`.
You should start reading [kdb-introduction(1)](kdb-introduction.md).

Documentation of plugins is available using the
[kdb-info(1)](kdb-info.md) tool.
Run `kdb list` for a list of plugins.

## COMMON OPTIONS

Every core-tool has the following options:

- `-H`, `--help`:
  Show the man page.
- `-V`, `--version`:
  Print version info.
- `-p`, `--profile`=<profile>:
  Use a different kdb profile, see below.

## KDB

The `kdb` utility reads its own configuration from three sources
within the KDB (key database):

1. /sw/kdb/**profile**/ (for legacy configuration)
2. /sw/elektra/kdb/#0/%/ (for empty profile)
3. /sw/elektra/kdb/#0/**profile**/ (for current profile)

The last source where a configuration value is found, wins.

## PROFILES

Profiles allow users to change many/all configuration options of a tool
at once. It influences from where the KDB entries are read.
For example if you use:
	`kdb export -p admin system`

It will read its format configuration from `/sw/elektra/kdb/#0/admin/format`.

If you want different configuration per user or directories, you should prefer
to use the `user` and `dir` namespaces. Then the correct configuration will
be chosen automatically and you do not have to specify the correct `-p`.

## BOOKMARKS

Elektra recommends [to use rather long paths](/doc/tutorials/application-integration.md)
because it ensures flexibility in the future (e.g. to use profiles and have a compatible
path for new major versions of configuration).

Long paths are, however, cumbersome to enter in the CLI.
Thus one can define bookmarks. Bookmarks are key-names that start with `+`.
They are only recognized by the `kdb` tool or tools that explicit have
support for it. Your applications should not depend on the presence of a
bookmark.

Bookmarks are stored below:
	`/sw/elektra/kdb/#0/current/bookmarks`

For every key found there, a new bookmark will be introduced.

Bookmarks can be used to start key-names by using `+` (plus) as first character.
The string until the first `/` will be considered as bookmark.

For example, if you set the bookmark kdb:
	`kdb set user/sw/elektra/kdb/#0/current/bookmarks`
	`kdb set user/sw/elektra/kdb/#0/current/bookmarks/kdb user/sw/elektra/kdb/#0/current`

You are able to use:
	`kdb ls +kdb/bookmarks`
	`kdb get +kdb/format`

## RETURN VALUES


- 0:
  successful.
- 1:
  Invalid options passed.
- 2:
  Invalid arguments passed.
- 3:
  Command terminated unsuccessfully.
- 4:
  Unknown command.
- 5:
  KDB Error, could not read/write from/to KDB.
- 7-8:
  Unkown errors, wrong exceptions thrown.
- 9-10:
  Reserved error codes.

## OPTIONS

Commonly used options for all programs:

- `-H`, `--help`:
   Show the man page.
- `-V`, `--version`:
   Print version info.
- `-p <profile>`, `--profile <profile>`:
   Use a different profile instead of current.

## SEE ALSO

- [elektra-introduction(7)](elektra-introduction.md)
- [kdb-introduction(1)](kdb-introduction.md)
