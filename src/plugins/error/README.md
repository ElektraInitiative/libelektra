- infos = Information about error plugin is in keys below
- infos/author = Markus Raab <elektra@libelektra.org>
- infos/licence = BSD
- infos/provides = error
- infos/needs =
- infos/recommends =
- infos/placements = presetstorage
- infos/status = productive maintained conformant shelltest tested nodep libc configurable discouraged
- infos/metadata = trigger/warnings trigger/error trigger/error/nofail
- infos/description = Provokes errors for testing the plugin framework

## Introduction

Plugins (should) rarely return an error or warnings, e.g. writing
the configuration basically only fails on file system problems. Such
behavior is difficult to produce for tests.

This plugin tackles this issue by yielding error/warnings on request.

## Installation

See [installation](/doc/INSTALL.md).
The package is called `libelektra5-extra`.

## Usage

### By metadata

Mount this plugin additionally with a working resolver and a storage
e.g.:

```sh
sudo kdb mount error.dump /error error dump
```

When following metakey is present during storing (`kdbSet()`) the keyset:

```
trigger/warnings
```

a warning will be added. The plugin will still return success, but when
the following metakey is present:

```
trigger/error
```

the plugin will return with an error.

The value of the metadata needs to contain the number of the requested
error or warning.

So an error and warnings can be injected directly with the kdb tool.
E.g. the warning number C01330:

```sh
kdb meta set system:/error/key trigger/warnings C01330
```

or the error number C01200 (will not modify the KDB because `kdbSet()` will
fail for the error plugin then):

```sh
kdb meta set user:/error/key trigger/error C01200
# RET:5
```

When you are finished you can unmount it with:

```sh
sudo kdb umount /error
```

### By config

To yield an error in kdbOpen() the metadata approach does not work. So
the plugin also can yield warning/errors using configuration.

To do that, configure the plugin using:

```
on_open/warnings
on_open/error
```

E.g. you can use:

```sh
sudo kdb mount error.dump /error error on_open/error=C03100 dump
```

Then you get an error on any access, e.g.:

```sh
kdb ls system:/error
```

Will yield error C01200:

```
Description: Tried to get a key from a missing backend
Mountpoint: system:/error
```

because the opening of the plugin failed (resulting to a missing
backend).

When you are finished you can unmount it with:

```sh
sudo kdb umount /error
```
