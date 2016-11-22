- infos = Information about the mozprefs plugin is in keys below
- infos/author = Thomas Waser <thomas.waser@libelektra.org>
- infos/licence = BSD
- infos/needs =
- infos/provides = storage
- infos/recommends =
- infos/placements = getstorage setstorage
- infos/status = maintained reviewed conformant compatible coverage specific unittest tested nodep libc preview experimental difficult limited unfinished concept
- infos/metadata =
- infos/description = storage plugin for mozilla preferences

## Basics ##

This plugin works on [Mozilla preference files](https://developer.mozilla.org/en-US/docs/Mozilla/Preferences/A_brief_guide_to_Mozilla_preferences)
and is used in Elektras [Firefox autoconfig script](autoconfig/README.md).

### Preference Types ###

- Default preferences: `pref(....`, keys below `mountpoint/pref/`.
- User preferences: `user_pref(....`, keys below `mountpoint/user/`.
- Lock preferences: `lockPref(....`, keys below `mountpoint/lock/`.
- Sticky preferences: `sticky_pref(....`, keys below `mountpoint/sticky/`.

Only Keys below one of these points are valid, everything else will be dropped

### Data Types ###

- `integer`
- `string`
- `boolean`

### Hierarchy ###

In Mozilla preference files `.` is used to separate sections, while elektra uses `/`. For simplification, and because `/` isn't allowed in preference keys, the plugin treats `.` and `/` equally. 

    kdb set system/prefs/lock/a/lock/key lock
    kdb set system/prefs/lock/a/lock.key lock
    kdb set system/prefs/lock/a.lock.key lock

will all result in `lockPref("a.lock.key", "lock");`

## Example ##
```sh
# Backup-and-Restore:/examples/prefs
sudo kdb mount prefs.js /examples/prefs mozprefs
kdb setmeta user/examples/prefs/lock/a/lock/key type boolean
kdb set /examples/prefs/lock/a/lock/key true
kdb setmeta user/examples/prefs/pref/a/default/key type string
kdb set /examples/prefs/pref/a/default/key "i'm a default key"
kdb setmeta user/examples/prefs/user/a/user/key type integer
kdb set /examples/prefs/user/a/user/key 123
#
kdb export user/examples/prefs ini
[lock/a/lock]
key = true
[pref/a/default]
key = i'm a default key
[user/a/user]
key = 123
#
#
$ cat `kdb file user/examples/prefs`
lockPref("a.lock.key", true);
pref("a.default.key", "i'm a default key");
user_pref("a.user.key", 123);
#
# cleanup
#
kdb rm -r /examples/prefs
sudo kdb umount /examples/prefs
```
