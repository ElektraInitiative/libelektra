- infos = Plugin for internal notification
- infos/author = Thomas Wahringer <waht@libelektra.org>
- infos/maintainer = Maximilian Irlinger <max@maxirlinger.at>
- infos/licence = BSD
- infos/needs =
- infos/provides =
- infos/recommends =
- infos/placements = postgetstorage postcommit
- infos/status = unittest libc nodep configurable global experimental unfinished nodoc concept
- infos/metadata =
- infos/description = Plugin for internal notification

## Usage

Allows applications to automatically update registered variables when the value
of a specified key has changed.

Application developers should use the
[notification API](https://doc.libelektra.org/api/latest/html/group__kdbnotification.html)
instead of the functions exported by this plugin.
The API is easier to use and decouples applications from this plugin.

## Exported Functions

This plugin exports various functions starting with `register*` below
`system:/elektra/modules/internalnotification/exports/`.
These functions should not be used directly.
Instead the [notification API](https://doc.libelektra.org/api/latest/html/group__kdbnotification.html)
should be used.
