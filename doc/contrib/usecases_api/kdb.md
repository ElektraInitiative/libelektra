# API based on use cases for KDB

## Get configuration

In `libelektra-kdb` this use case is directly implemented as the `kdbGet()` function.
The API has the additional precondition, that `kdbOpen()` was used to create a `KDB` instance and that a `KeySet` exists into which the configuration will be loaded.

The use case is also implemented by `libelektra-highlevel`.
However, it is not a direct mapping.
Instead, the `elektraOpen()` function takes care of loading configuration, which can then be accessed key by key via the `elektraGet*()` function family.

## Set configuration

In `libelektra-kdb` this use case is directly implemented as the `kdbSet()` function.
The API has the additional precondition, that the current configuration has been loaded (at least once) via `kdbGet()` using the same `KDB` instance.
Even though `kdbGet()` must be called, the entire configuration can be overwritten via `kdbSet()` as described in the use case.

## Modify configuration

In `libelektra-kdb` this use case is directly implemented as the `kdbSet()` function.

The use case is also implemented by `libelektra-highlevel`.
After loading the configuration via `elektraOpen()`, the `elektraSet*()` function family can be used to change individual configuration values (key by key).

## Keeping configuration up-to-date

This use case is implemented by a combination of `libelektra-kdb`, `libelektra-notification` and at least one notification hook plugin.
The notification hook plugin(s) must be enabled via the contract in `kdbOpen()`.
Then `libelektra-kdb` will send update notifications via the notification hook plugin(s).

With the API of `libelektra-notification` applications can register callbacks to be called, when a notification is received.

## Validating configuration with specification

This use case is implemented by a combination of `libelektra-kdb` and the spec hook plugin (normally the `spec` plugin).
The `spec` plugin is enabled as a spec hook plugin by default, so this use case works out of the box.

The main validation logic is implemented in the `spec` plugin.
