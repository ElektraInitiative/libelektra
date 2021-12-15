<!-- TODO: not an appropriate place for this type of docs -->

# `kdb mount` Command

## Basics: Direct Mode

The `kdb mount` command is used to mount a new backend at some mountpoint.
The basic usage of the command is:

```
kdb mount [OPTIONS...] <MOUNTPOINT> <BACKEND_PLUGIN> [...]
```

The arguments are as follows:

- `<MOUNTPOINT>`: the mountpoint (i.e. a key name) at which the new backend shall be mounted
- `<BACKEND_PLUGIN>`: the backend plugin used for the backend.
  This is the main plugin responsible for the mountpoint.
- `[...]`: any arguments after `<BACKEND_PLUGIN>` are passed as is to the backend plugin for further processing.
  How these options are interpreted is entirely up to the backend plugin and may differ drastically between backend plugins.
- `[OPTIONS...]`: general options processed by `kdb mount`

This mode of operation is called Direct Mode, because the command-line arguments are processed directly by the backend plugin.
The `kdb mount` command also supports different modes called Mountpoint Info and Import Mode.
These are described further below.

### Supported Options

The options supported by `kdb mount` are:

- `-I --import=<IMPORT_PLUGIN>:<IMPORT_FILE>`:
  Use Import Mode for mounting (see below).
- `-v --verbose`:
  Print more information about what is happening.
- `-d --debug`:
  Print even more information than `--verbose`.
  This option automatically implies `--verbose`.
- `-W --with-recommends`:
  Instruct the backend plugin to use recommended plugins in addition to required ones.
  This option will simply be ignored, if the backend plugin doesn't support it.
  This option is not supported in Import Mode.
- `-q --quiet`:
  Print nothing, if the operation was successful.
  Error information will still be printed.
- `-C --confirm`:
  Print what will be changed in the KDB and ask the user for confirmation, before applying the change.
- `-m --merge`:
  This option is only supported in Import Mode.
  Instead of failing when `<MOUNTPOINT>` is already in use, a merge will be attempted.
- `-s --strategy=<STRATEGY>`:
  This option is only supported in Import Mode when using `--merge`.
  Specifies the merge strategy to use to resolve conflicts.
- `-f --force`:
  **This option is dangerous.**
  If `<MOUNTPOINT>` is already in use, the existing backend will be removed and replaced by the newly created one.
  The operation will still fail, if the backend plugin reports an error, or if the validation in Import Mode fails.
  It is not possible to create an invalid backend configuration with this option, but replacing existing backends may break applications that relied on it.
- `-c --config=<KEY>=<VALUE>`:
  A repeatable option to define config keys that are stored in the `/config` part of the backend configuration.
  `<KEY>` must be a cascading key and cannot contain the `=` character.
  This option is not supported in Import Mode.
- `--config-file=<CONFIG_PLUGIN>:<CONFIG_FILE>`:
  Defines file `<CONFIG_FILE>` that will be read with the (storage) plugin `<CONFIG_PLUGIN>`.
  The resulting `KeySet` will be stored in the `/config` part of the backend configuration.
  This is an alternative to `--config`, which can be used if binary values or keys with `=` characters are needed.
  This option is not supported in Import Mode.

## Mountpoint Info

The `kdb mount` command can also be used to display information about existing backends.
For this mode invoke the command without at most one argument:

```
kdb mount [MOUNTPOINT]
```

Here `[MOUNTPOINT]` is an optional argument.
If it is present information about the backend mounted at `[MOUNTPOINT]` will be printed.
Without the any arguments `kdb mount` prints basic information about all existing backends.

## Import Mode

Some backend plugins may require complex configuration.
It is not always (easily) possible to express the full scope of such configurations in a series of command-line arguments.
To solve this problem, `kdb mount` also has an import mode:

```
kdb mount -I <IMPORT_PLUGIN>:<IMPORT_FILE> [IMPORT_OPTIONS...] <MOUNTPOINT> <BACKEND_PLUGIN>
```

or

```
kdb mount --import=<IMPORT_PLUGIN>:<IMPORT_FILE> [IMPORT_OPTIONS...] <MOUNTPOINT> <BACKEND_PLUGIN>
```

> **Note:** Any arguments after `<BACKEND_PLUGIN>` will be ignored in this mode.
> Additionally, not all options for `kdb mount` may be compatible with import mode.

In this mode `kdb mount` will use the (storage) plugin `<IMPORT_PLUGIN>` to load the file `<IMPORT_FILE>`.
The resulting `KeySet` will be passed to `<BACKEND_PLUGIN>` for validation and then used directly as the configuration for the new backend mounted at `<MOUNTPOINT>`.
Essentially, this is like a `kdb import` directly into `system:/elektra/mountpoints` with some additional validation.
The corresponding `kdb import` command would be (`<EM>` is the escaped form of `<MOUNTPOINT>`):

```
cat <IMPORT_FILE> | kdb import system:/elektra/mountpoints/<EM> <IMPORT_PLUGIN>`
```

> **Note:** The command above is only theoretical, since `kdb import` doesn't allow writing to keys below `system:/elektra/mountpoints` to avoid creating invalid backend configurations.

## Example Usage: `backend`

To create a standard file-based backend the default backend plugin `backend` can be used.

As a reminder, the plugin `backend` will receive all the remaining arguments `[...]` from the command:

```
kdb mount [OPTIONS...] <MOUNTPOINT> <BACKEND_PLUGIN> [...]
```

These arguments are interpreted one by one as follows:

- The first argument will be used as the relative path of the backing configuration file.
- If an argument starts with a `/`, it is a config key for the preceding plugin.
- Otherwise, it specifies a plugin.

Arguments `<plugin>` that specify a plugin result in a new section `plugins/<plugin>` with at least the key `plugins/<plugin>/name = <plugin>` in the backend configuration.

The arguments that start with a `/` must be of the form `<key>=<value>`, where `<key>` must be a cascading key name (`<value>` can be empty).
The config key `<key>` with the value `<value>` will be assigned to the preceding plugin.
In other words, this results in the key `plugins/<plugin>/config/<key> = <value>` being added to the backend configuration, where `<plugin>` is the last `<plugin>` that was specified.

An example `kdb mount` invocation could look like this:

```
kdb mount \
  -c general/config/a=1 \
  --config=general/config/b=2 \
  --config=general/config/c=3 \
  /hosts \
  backend \
  myhosts \
  glob \
  /set/#0= \
  /set/#1= \
  /set/#2= \
  /set/#3= \
  /set/#4/flags= \
  hosts \
  error
```

This will result in the following configuration:

```
system:/elektra/mountpoints/\/hosts/plugins/resolver/name (="resolver")
system:/elektra/mountpoints/\/hosts/plugins/glob/name (="glob")
system:/elektra/mountpoints/\/hosts/plugins/glob/config/set/#0
system:/elektra/mountpoints/\/hosts/plugins/glob/config/set/#1
system:/elektra/mountpoints/\/hosts/plugins/glob/config/set/#2
system:/elektra/mountpoints/\/hosts/plugins/glob/config/set/#3
system:/elektra/mountpoints/\/hosts/plugins/glob/config/set/#4/flags
system:/elektra/mountpoints/\/hosts/plugins/hosts/name (="hosts")
system:/elektra/mountpoints/\/hosts/plugins/sync/name (="sync")
system:/elektra/mountpoints/\/hosts/plugins/error/name (="error")
system:/elektra/mountpoints/\/hosts/plugins/network/name (="network")

system:/elektra/mountpoints/\/hosts/plugins/backend/name (="backend")

system:/elektra/mountpoints/\/hosts/config/general/config/a (="1")
system:/elektra/mountpoints/\/hosts/config/general/config/b (="2")
system:/elektra/mountpoints/\/hosts/config/general/config/c (="3")

system:/elektra/mountpoints/\/hosts/definition/path (="myhosts")

system:/elektra/mountpoints/\/hosts/definition/positions/get/resolver (="resolver")
system:/elektra/mountpoints/\/hosts/definition/positions/get/storage (="hosts")
system:/elektra/mountpoints/\/hosts/definition/positions/get/poststorage/#0 (="glob")

system:/elektra/mountpoints/\/hosts/definition/positions/set/resolver (="resolver")
system:/elektra/mountpoints/\/hosts/definition/positions/set/prestorage/#0 (="glob")
system:/elektra/mountpoints/\/hosts/definition/positions/set/prestorage/#1 (="error")
system:/elektra/mountpoints/\/hosts/definition/positions/set/prestorage/#2 (="network")
system:/elektra/mountpoints/\/hosts/definition/positions/set/storage (="hosts")
system:/elektra/mountpoints/\/hosts/definition/positions/set/precommit/#0 (="sync")
system:/elektra/mountpoints/\/hosts/definition/positions/set/commit (="resolver")
system:/elektra/mountpoints/\/hosts/definition/positions/set/rollback (="resolver")
```

The positions for the plugins are determined automatically via their plugin contract.
If you need a more advanced configuration, you must use the Import Mode for `kdb mount`.

Specifying the same plugin multiple times is valid, but will generate a warning.
It will not result in multiple instances of a single plugin.
For that you must use Import Mode.

It is still possible that two names refer to the same plugin because of e.g. symlinks.
This will not be detected here.
It will, however, result in a warning when the backend is initialized at runtime.
This is because symlinks could change at any time.

> **Note:** This example also shows, that if none is specified, the default resolver plugin `resolver` will be added automatically.
> This is not the case for the storage plugin.
> You must define exactly one storage plugin, or an error will be reported.
