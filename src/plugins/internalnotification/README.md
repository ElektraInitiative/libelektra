- infos = Plugin for internal notification
- infos/author = Thomas Wahringer <waht@libelektra.org>
- infos/licence = BSD
- infos/needs =
- infos/provides =
- infos/recommends =
- infos/placements = postgetstorage precommit
- infos/status = global nodep libc experimental unfinished nodoc concept unittest
- infos/metadata =
- infos/description = Plugin for internal notification

## Usage ##

Allows users of the plugin to register integer variables that get automatically
updated when the value of the key changes.

Use as global plugin:
$ sudo kdb global-mount internalnotification

## Exported Methods ##

This plugin exports the function `int elektraInternalnotificationRegisterInt (Plugin * handle, int * variable, Key * key)`.
If the given key is contained in a KeySet on a kdbGet or kdbSet operation, its value is
converted from string to integer and the registered variable is updated with the current value.
This also works if the registered key is a cascading key.
The function address is exported as `system/elektra/modules/internalnotification/exports/elektraInternalnotificationRegisterInt`.
The plugin handle required for this plugin is exported as `system/elektra/modules/internalnotification/exports/handle`.

## Todo/Issues ##

- Correct storage for registered keys? elektraPlugin(Set|Get)Data?
- Why did "postcommit" placement not work?
  - are global hooks working correctly? (i.e. is plugin installed on postcommit hook position?)
  - eventually bypass global hooks by placing plugin into globalPlugins-array on elektraInternalnotificationOpen
- integer: Before conversion check if key is string, if key is binary, use value
- string: Update string value (semantics: before changing the string, make copy!)
- callback: Accept any callback (refactor plugin to use callbacks internally for integer and strings)
