- infos = Information about error plugin is in keys below
- infos/author = Markus Raab <elektra@libelektra.org>
- infos/licence = BSD
- infos/needs =
- infos/provides = error
- infos/placements = presetstorage
- infos/description = Provokes errors for testing the plugin framework

## Introduction ##

Some plugins rarely return error codes in some situations, e.g. writing
the configuration basically only fails on filesystem problems. Such
behaviour is difficult to produce for test cases.

This plugin tackles this issue by yielding errors on command.

## Usage ##

### by meta data ###

Mount this plugin additionally to resolver or storage.

When following meta key is present in the keyset:

	trigger/warnings

a warning will be added. The plugin will still return success, but when
following meta key is present:

	trigger/error

the plugin will return with an error.

The value of the meta data contains the error/warning number.

So errors can be injected directly with the kdb tool:

	kdb setmeta $ROOT/error_trigger trigger/error 10


### by config ###

To yield an error in kdbOpen() the meta data approach does not work. So
the plugin also can yield warning/errors using configuration.
