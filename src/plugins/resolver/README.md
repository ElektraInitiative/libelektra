- infos = All information you want to know is in keys below
- infos/author = Markus Raab <elektra@markus-raab.org>
- infos/licence = BSD
- infos/provides = resolver
- infos/needs =
- infos/placements = rollback getresolver setresolver commit
- infos/status = productive maintained specific unittest tested libc
- infos/description = system independent resolver

## Introduction

The `@PLUGIN_SHORT_NAME@` handles operating system dependent tasks.
One task is the resolving of the filenames for user and system (hence its name).

Use following command to see to which file is resolved to:

```sh
kdb file <Elektra path you are interested in>
```

See the constants of this plugin for further information, they are:

```
system:/elektra/modules/@PLUGIN_SHORT_NAME@/constants
system:/elektra/modules/@PLUGIN_SHORT_NAME@/constants/ELEKTRA_VARIANT_SYSTEM
system:/elektra/modules/@PLUGIN_SHORT_NAME@/constants/ELEKTRA_VARIANT_USER
system:/elektra/modules/@PLUGIN_SHORT_NAME@/constants/KDB_DB_HOME
system:/elektra/modules/@PLUGIN_SHORT_NAME@/constants/KDB_DB_SYSTEM
system:/elektra/modules/@PLUGIN_SHORT_NAME@/constants/KDB_DB_USER
system:/elektra/modules/@PLUGIN_SHORT_NAME@/constants/KDB_DB_SPEC
system:/elektra/modules/@PLUGIN_SHORT_NAME@/constants/KDB_DB_DIR
```

The built-in resolving considers following cases:

- for spec with absolute path: path
- for spec with relative path: `KDB_DB_SPEC` + path
- for dir with absolute path: `pwd` + path (or above when path is found)
- for dir with relative path: `pwd` + `KDB_DB_DIR` + path (or above when path is found)
- for user with absolute path: `~` + path
- for user with relative path: `~` + `KDB_DB_USER` + path
- for system with absolute path: path
- for system with relative path: `KDB_DB_SYSTEM` + path

(~ and `pwd` are resolved from system)

## Example

For an absolute path `/example.ini`, you might get following values:

- for spec: `/example.ini`
- for dir: `$PWD/example.ini`
- for user: `~/example.ini`
- for system: `/example.ini`

For an relative path example.ini, you might get following values:

- for spec: `/usr/share/elektra/specification/example.ini`
- for dir: `$PWD/.dir/example.ini`
- for user: `~/.config/example.ini`
- for system: `/etc/kdb/example.ini`

See [the mount tutorial](/doc/tutorials/mount.md) for more examples.

## Variants

Many variants exist that additionally influence the resolving
process, typically by using environment variables.

Environment variables are very simple for one-time usage but their
maintenance in start-up scripts is problematic. Additionally, they
are controlled by the user, so they cannot be trusted. So it is not
recommended to use environment variables.

Note that the file permissions apply, so it might be possible for
non-root to modify keys in `system`.

See [COMPILE.md](/doc/COMPILE.md) for a documentation of possible
variants.

## Installation

See [installation](/doc/INSTALL.md).
The default variant of this plugin `resolver_fm_hpu_b` is part of the `libelektra5` package. All other variants are part of the `libelektra5-extra` package.

### XDG Compatibility

The resolver is fully [XDG compatible](http://standards.freedesktop.org/basedir-spec/basedir-spec-latest.html)
if configured with the variant:

- `xp`, `xh` or `xu` for user (either using `passwd`, `HOME` or `USER` as fallback
  or any combination of these fallbacks)
- `x` for system, no fallback necessary

Additionally `KDB_DB_USER` needs to be left unchanged as `.config`.

`XDG_CONFIG_DIRS` will be used to resolve system paths the following
way:

- if unset or empty `/etc/xdg` will be used instead
- all elements are searched in order of importance
- if a file was found, the search process is stopped
- if no file was found, the least important element will be used for
  potential write attempts.

## Reading Configuration

1. If no update needed (unchanged modification time): quit successfully
2. Otherwise call (storage) plugin(s) to read configuration
3. remember the last stat time (last update)

## Writing Configuration

1. On unchanged configuration: quit successfully
2. On empty configuration: remove the configuration file and quit successfully
3. Otherwise, open the configuration file
   If not available recursively create directories and retry.

#ifdef ELEKTRA_LOCK_MUTEX

4. Try to lock a global mutex, if not possible -> conflict

#endif

#ifdef ELEKTRA_LOCK_FILE

5. Try to lock the configuration file, if not possible -> conflict

#endif

6. Check the update time -> might lead to conflict
7. Update the update time (in order to not self-conflict)

We have an optimistic approach. Locking is only used to detect concurrent
cooperative processes in the short moment between prepare and commit.
A conflict will be raised in that situation. When processes do not lock
the file it might be overwritten. This is, however, very unlikely on
file systems with nanosecond precision.

## Exported Functions and Data

The resolver provides 2 functions for other plugins to use.

### filename

resolves `path` in with namespace `namespace`, creates a temporary file to work on and returns a struct containing the resolved data.

Signature:
`ElektraResolved * filename (elektraNamespace namespace, const char * path, ElektraResolveTempfile tempFile, Key * warningsKey)`

`ElektraResolved` and `ElektraResolveTempfile` are both defined in [shared.h](shared.h).

`ElektraResolved` is a struct with the following members:

- `relPath`: contains the path relative to the namespace.
- `dirname`: contains the parent directory of the resolved file.
- `fullPath`: contains the full path of the resolved file.
- `tmpFile`: contains the full path of the created temporary file.

`ElektraResolveTempfile` dictates if and where a temporary file should be created. Possible values:

- `ELEKTRA_RESOLVER_TEMPFILE_NONE`: don't create a temporary file.
- `ELEKTRA_RESOLVER_TEMPFILE_SAMEDIR`: create a temporary file in the same directory as the resolved file.
- `ELEKTRA_RESOLVER_TEMPFILE_TMPDIR`: create a temporary file in `/tmp`.

### freeHandle

frees the handle returned by `filename`.

Signature:
`void * freeHandle (ElektraResolved * handle)`

where `handle` is the handle returned by `filename`.

## Limitations

- If none of the resolving techniques work, the resolver will fail during `kdbOpen`.
  This happens, for example, with the default resolver (ELEKTRA_VARIANT_USER `hpu`)
  if neither: `$HOME`, `$USER`, nor any home directory in `/etc/passwd` is set.
- Conflicts with removed files are not handled.
- Links are not handled.
- uid/gid from files are not restored.
