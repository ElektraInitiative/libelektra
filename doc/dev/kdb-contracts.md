# KDB Contracts

The `kdbOpen()` function accepts a `KeySet * contract` parameter.
This parameter allows you to extend and configure the KDB within your application without permanently changing files on disk.

## Contract Structure

The contract consists of Keys below `system:/elektra/contract/<type>`, where `<type>` is one of a set of predefined contract types.
Currently, the types `globalkeyset` and `mountglobal` are supported.

### Global KeySet Contracts

To insert data into the global KeySet during `kdbOpen`, you can add keys below `system:/elektra/contract/globalkeyset`.
All these keys will be renamed and copied into the global KeySet that is passed to all plugins.

Specifically, the subset below `system:/elektra/contract/globalkeyset` is moved to `system:/elektra` and then inserted into the global KeySet.

### Mounting Global Plugins

You can also mount a global plugin with a contract.
To do this, add a key `system:/elektra/contract/mountglobal/<plugin>` where `<plugin>` is the name of the plugin you want to mount.
The keys below `system:/elektra/contract/mountglobal/<plugin>` will be moved to `user:/` and used as the config for `<plugin>`.

## Pre-defined Contracts

There are a few pre-defined contracts that can be accessed via helper functions.

### GOpts

To mount and configure the `gopts` plugin you can use `elektraGOptsContract`.
There is also `elektraGOptsContractFromStrings`, but its use is discouraged unless you really need it.
It mainly exists for use from various bindings.

### I/O binding

To set the I/O binding to be used in a KDB instance, use `elektraIoContract`.

### Notification

To set up notifications use the `elektraNotificationContract` function.
It automatically sets up the `internalnotification` plugin.

If you also need to set up notification transport plugins, you should manually add the relevant `system:/elektra/contract/mountglobal/<plugin>` keys and the required config below.
