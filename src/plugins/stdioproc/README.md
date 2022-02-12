- infos = Information about the stdioproc plugin is in keys below
- infos/author = Klemens Böswirth <k.boeswirth+git@gmail.com>
- infos/licence = BSD
- infos/needs =
- infos/provides =
- infos/recommends =
- infos/placements = prerollback rollback postrollback getresolver pregetstorage getstorage procgetstorage postgetstorage setresolver presetstorage setstorage precommit commit postcommit
- infos/status = maintained unittest shelltest experimental
- infos/metadata =
- infos/description = one-line description of stdioproc

## Introduction

This plugin spawns a new process with a user-defined executable and delegates all operations to the new process.

## Usage

Set the config key `app` and the arrays `args/#`, `env/#` to the path of an executable, the arguments that shall be passed and the environment variables to be set.

```
kdb mount test.dump /tests/stdioproc specload 'app=/usr/bin/pluginproc' 'args/#0=--load-plugin' 'args/#1=myplugin'
```

During `elektraStdprocioOpen` the plugin will collect the `args/#` and `env/#` values into two arrays `argv` and `envp`.
It then `fork`s a new process and the child calls `execve` with the path from `app` as well as `argv` and` envp`.
The child process is expected to listen to `stdin` and write to `stdout` according to the protocol described below.

If communication can be established, the child-process will be kept running until `elektraStdprocioClose`.

## Protocol

The entire protocol is text-based and request-response-based, and happens over `stdin`/`stdout`.
The parent process sends request to the child process.
The child then processes the request and sends a response back.

To encode keysets and keys we use the format of the `dump` plugin.
The child-process may also delegate directly to the `dump` plugin, or re-implement the encoding.

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
> ```
>
> This denotes a message sent from parent to child, containing
>
> - the literal text `HELLO WORLD`
> - followed by a newline
> - followed by a keyset called `users`
> - followed by a newline
> - followed by dynamic text called `user`
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

ELEKTRA_STDIOPROC INIT v1
```

The child must respond with an acknowledgement, followed by a `contract` keyset.

```
Child > Parent

ELEKTRA_STDIOPROC ACK v1
(name)
[contract]
```

Here `(name)` is the module name the child uses.
This will be used to replace `stdioproc` in the contract of the plugin.
The `contract` keyset must contain the `infos` keys that will be used replace the ones at the top of this README and the ones in `stdioproc.c`.
The `contract` keyset must also contain keys for every operation that is implemented by the child.
A child that implements all operations should include the keys:

```
exports/open = 1
exports/close = 1
exports/get = 1
exports/set = 1
exports/commit = 1
exports/error = 1
exports/checkconf = 1
```

After this initial handshake, the child should simply wait for further requests from the parent.

> **Note**: Under normal circumstances this handshake will always be followed by an `open` request immediately.

### Operation

When the parent needs the child to process an operation (open, get, ...), it will send a request like this:

```
Parent > Child

(opname)
[parent]
[data]
```

Here `(opname)` is one of `open`, `get`, `set`, `close`, `error`, `commit`, `checkconf` and describes the operation that shall be performed by the child.
The keyset `[parent]` always consists of a single key, namely the `parentKey` (or `errorKey`) that was passed to the plugin.
Finally, `[data]` is the keyset that was passed to the plugin.

The child should then perform the requested operation and respond with

```
Parent > Child

(result)
[parent]
[data]
```

Here `(result)` is one of `success`, `noupdate` and `error`, which correspond to `ELEKTRA_PLUGIN_STATUS_SUCCESS`, `ELEKTRA_PLUGIN_STATUS_NO_UPDATE` and `ELEKTRA_PLUGIN_STATUS_ERROR` respectively.
The keysets `[parent]` and `[data]` are the modified versions of the one sent by the parent.

### Termination

When the parent no longer needs the child process, it will send a final termination request.

```
Parent > Child

ELEKTRA_STDIOPROC TERMINATE
```

The child process should now exit (and thereby close its ends of the `stdin`/`stdout` pipes).

> **Note:** Under normal circumstances this only happens, when plugin is being closed.

### Errors

If an unexpected error occurs on either side of the protocol, the connection should be terminated and the child process shall exit.

## Examples

TODO

## Limitations

TODO
