- infos = Information about the mozprefs plugin is in keys below
- infos/author = Thomas Waser <thomas.waser@libelektra.org>
- infos/licence = BSD
- infos/needs =
- infos/provides = storage
- infos/recommends =
- infos/placements = getstorage setstorage
- infos/status = maintained reviewed conformant compatible coverage unittest tested nodep libc preview experimental difficult unfinished concept
- infos/features/storage = limited limited
- infos/metadata =
- infos/description = storage plugin for mozilla preferences

## Basics

This plugin works on Mozilla preference files and is used in
Elektra’s [Firefox autoconfig script](autoconfig/README.md).

## Installation

See [installation](/doc/INSTALL.md).
The package is called `libelektra5-extra`.

### Preference Types

- Default preferences: `pref(....`, keys below `mountpoint/pref/`.
- User preferences: `user_pref(....`, keys below `mountpoint/user/`.
- Lock preferences: `lockPref(....`, keys below `mountpoint/lock/`.
- Sticky preferences: `sticky_pref(....`, keys below `mountpoint/sticky/`.

Only Keys below one of these points are valid, everything else will be dropped

### Data Types

- `integer`
- `string`
- `boolean`

### Hierarchy

In Mozilla preference files `.` is used to separate sections, while elektra uses `/`. For simplification, and because `/` isn't allowed in preference keys, the plugin treats `.` and `/` equally.

```bash
kdb set system:/prefs/lock/a/lock/key lock
kdb set system:/prefs/lock/a/lock.key lock
kdb set system:/prefs/lock/a.lock.key lock
```

will all result in `lockPref("a.lock.key", "lock");`

## Example

```sh
# Backup-and-Restore:user:/tests/mozprefs

sudo kdb mount prefs.js user:/tests/mozprefs mozprefs

kdb meta-set user:/tests/mozprefs/lock/a/lock/key type boolean
kdb set user:/tests/mozprefs/lock/a/lock/key true
kdb meta-set user:/tests/mozprefs/pref/a/default/key type string
kdb set user:/tests/mozprefs/pref/a/default/key "i'm a default key"
kdb meta-set user:/tests/mozprefs/user/a/user/key type integer
kdb set user:/tests/mozprefs/user/a/user/key 123

cat `kdb file user:/tests/mozprefs`
#> lockPref("a.lock.key", true);
#> pref("a.default.key", "i'm a default key");
#> user_pref("a.user.key", 123);

# cleanup
kdb rm -r user:/tests/mozprefs
sudo kdb umount user:/tests/mozprefs
```
