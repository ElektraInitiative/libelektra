# elektrasettings #

 **__Warning__ !!! This lib is unstable and could destroy your Elektra Configuration. Only use in testing evironments.**

 Implementation of Elektra as GSettingsBackend

## What is working: ##
 * passing gsettings unit tests
 * all interface functionality but one
  * reading user and (system) default values
  * writing user values and trees of user values
  * reset (delete) a key
  * synchronitation (no conflict handling yet)
  * subscribing and unsubscribing for changes (needs elektras [dbus plugin](https://github.com/ElektraInitiative/libelektra/tree/master/src/plugins/dbus) mounted on subscribed path)
  * get writability of key (As far as definable as writable from Elektra)

## What is not ##
 * synchronitation conflict handling
 * code cleanup
 * proper error handling
 * optimizations
 * get permission (Elektra does not support this)
 * Setting write path in Elektra

## Pending Issues ##
 * See #762, #302, #775, #768

## Going to Change ##
 * default write path in elektra (currently this is /sw), this will probably going to be
 either a setting in kdb or an environment variable.

# Building #

## Dependencies ##
 * elektra (for standalone build)
 * gelektra (for standalone build)
 * glib
 * gio
 * dbus

## Build ##
### As part of Elektra Build ###
```sh
mkdir build && cd build
cmake -GSETTINGS_MODULE_PRIORITY=200 -DBINDINGS=gsettings ..
make
```
### Standalone Build ###
```sh
cd GSETTINGS_SOURCE
mkdir build && cd build
cmake -GSETTINGS_MODULE_PRIORITY=200 ..
make
```

 * GSETTINGS_MODULE_PRIORITY set this value above the value of othe GSettingsBackend modules so it gets selected as default
  * the dconf GSettingsBackend is known to have a value of `100`

# Installation #
```sh
(sudo) make install
```

If you do a system installation consider doing a migration, so you can immediately keep using your settings.

# Migration #
## Import dconf user settings ##
For now you can export your existing dconf database trough:
```sh
dconf dump > dconf.ini
```
and import it in elektra
```sh
cat dconf.ini | kdb import /sw ini
```

## Mount dbus plugin ##
export current settings and delete them so they will not be hidden
```sh
kdb export /sw dump > tmp
kdb rm -r /sw
```
mount sw at you prefered location
```sh
(sudo) kdb mount sw /sw dbus $restofplugins
```
import settings again
```sh
(sudo) cat tmp | kdb import /sw dump
rm tmp
```

If you want notifications to work you have to mount /sw with the dbus plugin.

# Debugging #

`G_MESSAGES_DEBUG=ElektraSettings` will enable debug output of the backend. If you have
set the priority of the module below `100` and dconf installed you also have to
set `GSETTINGS_BACKEND=elektra` to be able to test elektra as default backend.

##Examples##

### Get/Set and Reset a Gsettings Value ###
```sh
export G_MESSAGES_DEBUG=ElektraSettings
export GSETTINGS_BACKEND=elektra
gsettings get org.gnome.gedit.preferences.editor auto-indent
gsettings set org.gnome.gedit.preferences.editor auto-indent false
gsettings get org.gnome.gedit.preferences.editor auto-indent
gsettings reset org.gnome.gedit.preferences.editor auto-indent
gsettings get org.gnome.gedit.preferences.editor auto-indent
gsettings list-recursively org.gnome.gedit.preferences.editor auto-indent
```
### Force elektra as default backend when starting an application: ###
```sh
G_MESSAGES_DEBUG=ElektraSettings GSETTINGS_BACKEND=elektra gedit
```

an application can still request a different backend then elektra with `g_settings_new_with_backend ()`
