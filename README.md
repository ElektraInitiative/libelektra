# elektrasettings

 **__Warning__ !!! This lib is unstable and could destroy your Elektra Configuration. Only use in testing evironments.**

 Implementation of Elektra as GSettingsBackend

# What is working:
 * passing gsettings unit tests
 * all interface functionality but one
  * reading user and (system) default values
  * writing user values and trees of user values
  * reset (delete) a key
  * synchronitation (no conflict handling yet)
  * subscribing and unsubscribing for changes (needs elektras [dbus plugin](https://github.com/ElektraInitiative/libelektra/tree/master/src/plugins/dbus) mounted on subscribed path)
  * get writability of key (need info on how elektra defines writability)

# What is not
 * synchronitation conflict handling
 * code cleanup
 * proper error handling
 * optimizations
 * get permission = am i allowed to write bellow path? (needs info if doable in elektra)

# Outstanding decissions
 * default write path in elektra (currently this is /sw)

#Building
## Dependencies
 * elektra
 * glib
 * gio
 * dbus

## Build
```shell
cd src
mkdir build && cd build
cmake -DMODULE_PRIORITY=200 ..
make
```

 * MODULE_PRIORITY set this value above the value of othe GSettingsBackend modules so it gets selected as default
  * the dconf GSettingsBackend is known to have a value of `100`

# Installation
```shell
(sudo) make install
```

If you do a system installation consider doing a migration, so you can imediatly keep using your settings.

# Migration
## Import dconf user settings
For now you can export your exising dconf database trough:
```shell
dconf dump > dconf.ini
```
and import it in elektra
```shell
cat dconf.ini | kdb import /sw ini
```

## Mount dbus plugin
export current settings
```shell
kdb export /sw dump > tmp
```
mount sw at you prefered location
```shell
(sudo) kdb mount sw /sw dbus $restofplugins
```
import settings again
```shell
(sudo) cat tmp | kdb import /sw dump
rm tmp
```

If you want notfifications to work you have to mount /sw with the dbus plugin.

# Debugging
`G_MESSAGES_DEBUG=ElektraSettings` will enable debug output of the backend. If you have
set the priority of the module below `100` you also have to set `GSETTINGS_BACKEND=elektra`
to be able to test elektra as default backend.

##examples:
### Get/Set and Reset a Gsettings Value
```shell
export G_MESSAGES_DEBUG=ElektraSettings
export GSETTINGS_BACKEND=elektra
gsettings get org.gnome.gedit.preferences.editor auto-indent
gsettings set org.gnome.gedit.preferences.editor auto-indent false
gsettings get org.gnome.gedit.preferences.editor auto-indent
gsettings reset org.gnome.gedit.preferences.editor auto-indent
gsettings get org.gnome.gedit.preferences.editor auto-indent
gsettings list-recursively org.gnome.gedit.preferences.editor auto-indent
```
### Force elektra as default backend when starting an application:
```shell
G_MESSAGES_DEBUG=ElektraSettings GSETTINGS_BACKEND=elektra gedit
```

an application can still request a different backend then elektra with `g_settings_new_with_backend ()`

# Note
This repository exists only for development reasen, it will be later integrated 
in [libelektra](https://github.com/ElektraInitiative/libelektra) once it is mature enough.
