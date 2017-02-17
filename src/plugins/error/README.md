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
the configuration basically only fails on filesystem problems. Such
behaviour is difficult to produce for tests.

This plugin tackles this issue by yielding error/warnings on request.

## Usage

### By metadata

Mount this plugin additionally with a working resolver and a storage
e.g.:

    kdb mount error.dump /error error dump

When following metakey is present during storing (`kdbSet()`) the keyset:

    trigger/warnings

a warning will be added. The plugin will still return success, but when
the following metakey is present:

    trigger/error

the plugin will return with an error.

The value of the metadata needs to contain the number of the requested
error or warning.


So an error and warnings can be injected directly with the kdb tool.
E.g. the warning number 3:

    kdb setmeta system/error/key trigger/warnings 3

or the error number 10 (will not modify the KDB because `kdbSet()` will
fail for the error plugin then):

    kdb setmeta user/error/key trigger/error 10

### By config

To yield an error in kdbOpen() the metadata approach does not work. So
the plugin also can yield warning/errors using configuration.

To do that, configure the plugin using:

    on_open/warnings
    on_open/error

E.g. you can use:

    kdb mount error.dump /error error on_open/error=10 dump

Then you get an error on any access, e.g.:

    kdb ls system/error

Will yield error #63:

    Description: Tried to get a key from a missing backend
    Mountpoint: system/error

because the opening of the plugin failed (resulting to a missing
backend).

