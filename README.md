# elektrasettings

 ** __ Warning __ !!! This lib is unstable and could destroy your Elektra Configuration. Only use in testing evironments.**

# Future:
 * Implementation of Elektra as GSettingsBackend

# What is working:
 * Basic operations are functional, but neither well tested nor error prove.

# What is not
 * `subscribe`, `unsubscribe` implemenations are still missing
 * proper error handling
 * optimizations

#Building
## Dependencies
 * libelektra-glib (gelektra)
 * glib
 * gio

## Build
```shell
cd src
mkdir build && cd build
cmake ..
make
```
# Installation
```shell
(sudo) make install
```
 If you install this library trough `make install` most application using gsettings
 will use it as default because the priority of the module ist set to `200`. If
 you do not want this module to be chosen as default gsetttings backend, lower the
 value bellow `100` as that is the value dconf currently gives his backend.

# Migration

For now you can export your exising dconf database trough:
'''shell
dconf dump > dconf.ini
'''
and import it in elektra
'''shell
(sudo) echo dconf.init | kdb import /sw ini
'''

# Debugging

`G_MESSAGES_DEBUG=ElektraSettings` will enable debug output of the backend. If you
set the priority of the module below `100` you also have to set `GSETTINGS_BACKEND=elektra`
to be able to test the backend.

examples:
### Get/Set and Reset a Gsettings Value
```shell
G_MESSAGES_DEBUG=ElektraSettings GSETTINGS_BACKEND=elektra gsettings get org.gnome.gedit.preferences.editor auto-indent
G_MESSAGES_DEBUG=ElektraSettings GSETTINGS_BACKEND=elektra gsettings set org.gnome.gedit.preferences.editor auto-indent false
G_MESSAGES_DEBUG=ElektraSettings GSETTINGS_BACKEND=elektra gsettings get org.gnome.gedit.preferences.editor auto-indent
G_MESSAGES_DEBUG=ElektraSettings GSETTINGS_BACKEND=elektra gsettings reset org.gnome.gedit.preferences.editor auto-indent
G_MESSAGES_DEBUG=ElektraSettings GSETTINGS_BACKEND=elektra gsettings get org.gnome.gedit.preferences.editor auto-indent
G_MESSAGES_DEBUG=ElektraSettings GSETTINGS_BACKEND=elektra gsettings list-recursively org.gnome.gedit.preferences.editor auto-indent
```
### Start an application with elektra as backend
`G_MESSAGES_DEBUG=ElektraSettings GSETTINGS_BACKEND=elektra gedit`
# Note
This repository exists only for development reasen, it will be later integrated 
in [libelektra](https://github.com/ElektraInitiative/libelektra) once it is mature enough.
