- infos = Plugin for internal notification
- infos/author = Thomas Wahringer <waht@libelektra.org>
- infos/licence = BSD
- infos/needs =
- infos/provides =
- infos/recommends =
- infos/placements = postgetstorage postcommit
- infos/status = global libc unittest concept unfinished experimental nodoc
- infos/metadata =
- infos/description = Plugin for internal notification

## Usage ##

Allows programs to register integer variables that get automatically updated
when the value of the key changes.

Use as global plugin:
$ sudo kdb global-mount internalnotification

## Exported Methods ##

This plugin exports the function `int elektraInternalnotificationRegisterInt (Plugin * handle, int * variable, Key * key)`.
If the given key is contained in a KeySet on a kdbGet or kdbSet operation, its value is
converted from string to integer and the registered variable is updated with the current value.
This also works if the registered key is a cascading key.
The function address is exported as `system/elektra/modules/internalnotification/exports/elektraInternalnotificationRegisterInt`.
The plugin handle required for this plugin is exported as `system/elektra/modules/internalnotification/exports/handle`.
Please note that the plugin API may change as this plugin is experimental.

## Todo/Issues ##

- Correct data storage for registered keys? elektraPlugin(Set|Get)Data?
- Can we get rid of exporting the plugin handle?
- Different data types for values:
  - integer: Before conversion: check if key is string, if key is binary, just use the value
  - string: Update string value
  - callback: Accept any callback (+ refactor plugin to use callbacks internally for integer and strings)
