- infos = Information about the process plugin is in keys below
- infos/author = Klemens Böswirth <k.boeswirth+git@gmail.com>
- infos/licence = BSD
- infos/needs =
- infos/provides =
- infos/recommends =
- infos/placements = postgetstorage presetstorage
- infos/status = maintained unittest shelltest nodep configurable
- infos/metadata =
- infos/description = one-line description of process

## Introduction

This plugin spawns a new process with a user-defined executable and delegates all operations to the new process.

## Usage

Set the config key `executable` and the arrays `args/#`, `env/#` to the path of an executable, the arguments that shall be passed and the environment variables to be set.

```
kdb mount test.dump /tests/process process 'executable=/usr/bin/pluginproc' 'args=#1' 'args/#0=--load-plugin' 'args/#1=myplugin'
```

During `elektraStdprocioOpen` the plugin will collect the `args/#` and `env/#` values into two arrays `argv` and `envp`.
Additionally, the array `copyenv/#` is read as a list of environment variables.
Each variable will be looked up via `getenv` and then added to `envp`.

The plugin then `fork`s a new process and the child calls `execve` with the path from `app` as well as `argv` and` envp`.
The child process is expected to listen to `stdin` and write to `stdout` according to the protocol described below.

If communication can be established, the child process will be kept running until `elektraStdprocioClose`.

## Protocol

The entire protocol is text-based (apart from binary key values) and request-response-based, and happens over `stdin`/`stdout`.
The parent process sends request to the child process.
The child then processes the request and sends a response back.

To encode keysets and keys we use the format of the `dump` plugin.
It is important to note that the `dump` plugin is instructed to write the full keynames (normally it removes the parent prefix).
There is, however, an exception during initialization, which is described in the appropriate section.
The child process may also delegate directly to the `dump` plugin, or reimplement the encoding.

The communications protocol used by the plugin has 3 phases:

- Initialization: Initial handshake after first starting the child-process.
- Operation: The main phase, which implements all the operations of an Elektra plugin (open, get, set, close, ...).
- Termination: The plugin is being closed and the parent-process tells the child to shut down.

> **Note:** In the sections below we use the following format to describe messages:
>
> ```
> Parent > Child
>
> HELLO WORLD
> [users]
> (user)
> {ok|error}
> ```
>
> This denotes a message sent from parent to child, containing
>
> - the literal text `HELLO WORLD`
> - followed by a newline
> - followed by a keyset called `users`
> - followed by a newline
> - followed by dynamic text called `user`
> - followed by either the literal text `ok` or the literal text `error`
>
> The names `users` and `user` don't actually appear in the message.
> They are only used as a reference for the descriptions of the message.
>
> The actual message sent could look like this:
>
> ```
> HELLO WORLD
> kdbOpen 2
> $key string 3 5
> joe
> 12345
> $end
> appleseed
> ```

### Initialization

The parent writes the protocol header to `stdin` of the child.

```
Parent > Child

ELEKTRA_PROCESS INIT v1
```

The child must respond with an acknowledgement, followed by a `contract` keyset.

```
Child > Parent

ELEKTRA_PROCESS ACK v1
(name)
[contract]
```

Here `(name)` is the module name the child uses.
This will be used to replace `process` in the contract of the plugin.
The `[contract]` keyset must contain the `infos` keys, which will be used replace the ones at the top of this README and the ones in `process.c`.
It must also contain `exports/_` keys for every operation that is implemented by the child.
All keys in `[contract]` should be below `system:/elektra/modules/process`.
The parent will rename these keys appropriately.

A child that implements all operations should include these keys (in addition to the relevant `infos` keys):

```
system:/elektra/modules/process/exports/open = 1
system:/elektra/modules/process/exports/close = 1
system:/elektra/modules/process/exports/get = 1
system:/elektra/modules/process/exports/set = 1
```

After this initial handshake, the child should simply wait for further requests from the parent.

> **Note**: Under normal circumstances this handshake will always be followed by an `open` request immediately.

### Operation

When the parent needs the child to process an operation (open, get, ...), it will send a request like this:

```
Parent > Child

{open|get|set|close}
[parent]
[data]
```

First we send the `opname` (one of `open`, `get`, `set` or `close`) of the operation that shall be performed by the child.
The keyset `[parent]` always consists of a single key, namely the `parentKey` (or `errorKey`) that was passed to the plugin.
Finally, `[data]` is the keyset that was passed to the plugin.
The `[data]` keyset is not present in `open` and `close` operations, since those don't receive a `KeySet` in the C API.
However, in the `open` operation `[data]` is replaced `[config]` which is the `KeySet` returned by `elektraPluginGetConfig` in the C API.
This is needed, because the child process cannot request the config keyset otherwise.

The child should then perform the requested operation and respond with

```
Child > Parent

{success|noupdate|error}
[parent]
[returned]
```

Here `(result)` is one of `success`, `noupdate` and `error`, which correspond to `ELEKTRA_PLUGIN_STATUS_SUCCESS`, `ELEKTRA_PLUGIN_STATUS_NO_UPDATE` and `ELEKTRA_PLUGIN_STATUS_ERROR` respectively.
The keysets `[parent]` and `[returned]` are the modified versions of the one sent by the parent.

### Termination

When the parent no longer needs the child process, it will send a final termination request.

```
Parent > Child

ELEKTRA_PROCESS TERMINATE
```

The child process should now exit (and thereby close its ends of the `stdin`/`stdout` pipes).

> **Note:** Under normal circumstances this only happens, when plugin is being closed, i.e. during a `elektraPluginClose` call for a `process` instance.

### Errors

If an unexpected error occurs on either side of the protocol, the connection should be terminated and the child process shall exit.

## Examples

```sh
# mount the Whitelist Java Plugin via process
# NOTE: the copyenv and copyenv/#0 are normal not needed, but we need them to make this script work as an automated test
sudo kdb mount config.file user:/tests/process dump process 'executable=/usr/bin/java' 'args=#3' 'args/#0=-cp' "args/#1=$BUILD_DIR/src/bindings/jna/plugins/whitelist/build/libs/whitelist-$(kdb --version | sed -nE 's/KDB_VERSION: (.+)/\1/gp')-all.jar" 'args/#2=org.libelektra.process.PluginProcess' 'args/#3=org.libelektra.plugin.WhitelistPlugin' 'copyenv=#0' "copyenv/#0=LD_LIBRARY_PATH"

# Define whitelist
kdb meta-set user:/tests/process/key "check/whitelist/#0" ""
kdb meta-set user:/tests/process/key "check/whitelist/#1" allowed0
kdb meta-set user:/tests/process/key "check/whitelist/#2" allowed1

# Should be allowed
kdb set user:/tests/process/key allowed0
#> Set string to "allowed0"

kdb set user:/tests/process/key allowed1
#> Set string to "allowed1"

# Should cause error
kdb set user:/tests/process/key not_allowed
# RET: 5
# STDERR:.*Validation Semantic: .*'not_allowed' does not adhere to whitelist.*

# cleanup
kdb rm -r user:/tests/process
sudo kdb umount user:/tests/process
```

> **Note:** The mount line of the snippet above can be simplified by using the `mount-java` helper:
>
> ```
> sudo kdb mount-java config.file user:/tests/process dump java:org.libelektra.plugin.WhitelistPlugin
> ```

## Limitations

- The `error` and `commit` functions are currently not supported. Therefore, implementing a resolver is not supported.
- Exporting additional functions (e.g. `checkconf`) is currently not supported.
- With the current backend system, `process` can only be used for plugins in the `postgetstorage` or `presetstorage` positions.
- The executable must be defined as an absolute path during mounting.
