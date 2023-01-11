- infos = Information about augeas plugin is in keys below
- infos/author = Felix Berlakovich <elektra@berlakovich.net>
- infos/licence = BSD
- infos/provides = storage
- infos/needs =
- infos/recommends = glob keytometa
- infos/placements = getstorage setstorage
- infos/status = maintained tested/unit configurable
- infos/metadata = order
- infos/description = reading and writing configurations via libaugeas

## Introduction

This is a plugin for reading and writing configuration files with help from Augeas.
The plugin should be able to read all configuration files for which an Augeas lens exists.
However, not all stock lenses of Augeas have been tested yet.
A detailed description of the lens language and a tutorial on how to write new lenses
can be found at http://augeas.net/

## Dependencies

- `libaugeas-dev`: You need version 0.16 or higher

## Installation

See [installation](/doc/INSTALL.md).
The package is called `libelektra5-augeas`.

If you have installed Augeas manually, it may be necessary to update the ld configuration. This is especially
true if an older version of Augeas is installed also. Such a situation may lead to an error similar to this:

```
/usr/lib/libaugeas.so.0: version `AUGEAS_0.16.0` not found (required by kdb)
```

This is because `ld` tries to link `/usr/lib/libaugeas.so.0` which is an older version of Augeas. Simply add
the path to the newer library to your ld search paths (consult your system documentation on how to do this).

## Mounting and Configuration

In order to mount the hosts file with the augeas plugin, issue the
following command:

```sh
sudo kdb mount /etc/hosts system:/tests/hosts augeas lens=Hosts.lns
```

And to unmount the file run:

```sh
sudo kdb umount system:/tests/hosts
```

The value of the plugin configuration option "lens" should be the
module name of the lens (Hosts in the example) with a '.lns' suffix.
Depending on your distribution and kind of installation, lenses can
be found at `/usr/share/augeas/lenses/dist`,
`/usr/local/share/augeas/lenses/dist`, or something similar.
The lens module name is equal to the filename without extension in pascal notation.
For example, the lens `/usr/share/augeas/lenses/dist/hosts.aug` contains the module Hosts.

Note that, without configuring the plugin to use a lens, the plugin
will print an error message on the first usage:

```sh
sudo kdb mount /etc/hosts system:/tests/hosts augeas #The lens is missing here!
kdb ls system:/tests/hosts
# RET: 5
# ERROR: *C01200*
```

This happens because the plugin does not know which lens it should use to read and write the configuration.
For that reason, the lens configuration option was supplied together with the mount command.

And again to unmount the `hosts` file simply run:

```sh
sudo kdb umount system:/tests/hosts
```

## Restrictions

### Inner Node Values

Currently no Augeas lens supports values for inner nodes.
Unfortunately no validation plugin exists yet that would prevent such modifications early:

```sh
sudo kdb mount /etc/hosts system:/tests/hosts augeas lens=Hosts.lns
kdb set system:/tests/hosts/1 somevalue
# RET: 5
# ERROR: *C01100*
```

The operation simply fails with an undescriptive error.

Finally, to unmount the `hosts` file simply run:

```sh
sudo kdb umount system:/tests/hosts
```

### Leaky Abstraction of Order

Most Augeas lenses require subtrees to be in a specific order. For example the hosts lens requires the ipaddr node
of an entry to precede the canonical node. Unfortunately the Augeas storage plugin has no knowledge about this required
order. Therefore the correct order must be ensured via order metakeys. Otherwise saving the KeySet may fail. As an example
consider the following kdb shell script:

```
kdbGet system:/hosts
keySetName system:/hosts/6
ksAppendKey
keySetName system:/hosts/6/ipaddr
keySetString 14.14.14.14
ksAppendKey
keySetName system:/hosts/6/canonical
keySetString newhost
ksAppendKey
kdbSet system:/hosts
```

This fails with an error similar to this

```
Sorry, module storage issued the error C03100:
an Augeas error occurred: Failed to match
some augeas match expression
with tree
{ \"canonical\" = \"newhost\" } { \"ipaddr\" = \"14.14.14.14\" }
```

Whereas the following script succeeds due to the correct order

```
kdbGet system:/hosts
keySetName system:/hosts/6
ksAppendKey
keySetName system:/hosts/6/ipaddr
keySetString 14.14.14.14
keySetMeta order 100
ksAppendKey
keySetName system:/hosts/6/canonical
keySetString newhost
keySetMeta order 110
ksAppendKey
kdbSet system:/hosts
```

## Planned Improvements

- a validation plugin preventing inner node values
