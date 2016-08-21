- infos = Information about the prefs plugin is in keys below
- infos/author = Name <name@libelektra.org>
- infos/licence = BSD
- infos/needs =
- infos/provides = storage
- infos/recommends =
- infos/placements = getstorage setstorage
- infos/status = maintained reviewed conformant compatible coverage specific tested nodep libc preview experimental difficult unfinished nodoc concept 
- infos/metadata =
- infos/description =

## Basics ##

### Preference Types ###

- Default preferences: below `mountpoint/pref/`.
- User preferences: below `mountpoint/user/`.
- Lock preferences: below `mountpoint/lock/`.

User preferences will override default preferences. Lock preferences can't be overwritten by user or default preferences. 
Only user preferences will be saved to `prefs.js`.

### Data Types ###

- `integer`
- `string`
- `boolean`


## Example ##

Append
```
user_pref("elektra.config.file", "/tmp/myPrefs.js");
user_pref("elektra.config.reload_trigger_port", 12345);
```
to `prefs.js` in `$HOME/.mozilla/firefox/<profile_dir>/`

Move `autoconfig/firefox.cfg` to the directory containing the firefox binary (e.g. `/usr/lib/firefox-esr/`)

Move `autoconfig/autoconfig.js` to the preferences directory inside the folder containing the firefox binary (e.g. `/usr/lib/firefox-esr/defaults/pref/`)


```
% kdb mount autoconfig/preload.ini /preload ini
% kdb export /preload

[open]
\/tmp\/myPrefs.js =
\/tmp\/myPrefs.js/generate = system/prefs/gen
\/tmp\/myPrefs.js/generate/plugin = prefs


% kdb mount autoconfig/prefExample.js system/prefs/gen prefs shell execute/set='echo -n "reload"|nc 127.0.0.1 12345'
% kdb export system/prefs/gen

[lock/a/lock]
1 = lock1
2 = lock2
[pref/a/default]
1 = 1
2 = 2
[user/a/user]
f = false
t = true

% kdb export system/prefs/gen prefs

lockPref("a.lock.1", "lock1");
lockPref("a.lock.2", "lock2");
pref("a.default.1", 1);
pref("a.default.2", 2);
user_pref("a.user.f", false);
user_pref("a.user.t", true);
```

```
% LD_PRELOAD=/usr/local/lib/libelektraintercept.so firefox-esr "about:config"
```

![about:config before](./autoconfig/config_1.jpg)
```
% kdb setmeta system/prefs/gen/lock/a/lock/3 type boolean
% kdb set system/prefs/gen/lock/a/lock/3 true
% kdb export system/prefs/gen

[lock/a/lock]
1 = lock1
2 = lock2
3 = true
[pref/a/default]
1 = 1
2 = 2
[user/a/user]
f = false
t = true
```
![about:config after](./autoconfig/config_2.jpg)

