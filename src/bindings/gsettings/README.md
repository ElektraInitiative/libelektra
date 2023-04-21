- infos =
- infos/author = Gabriel Rauter <rauter.gabriel@gmail.com>
- infos/status = experimental
- infos/provides =
- infos/description = Elektra GSettings Backend

# ElektraSettings

**Warning !!!** This binding is unstable and could destroy your Elektra Configuration. Only use in testing environments.

A GSettings backend implementation based on Elektra.

## Installation

See [installation](/doc/INSTALL.md).
This binding is currently **not** part of a released package.

To quickly test the **experimental** ElektraSettings on a test system _[see Quick Start below](#hl-1)_.

## What is Working:

- passing gsettings unit tests
- all interface functionality but one
  - reading user and (system) default values
  - writing user values and trees of user values
  - reset (delete) a key
  - synchronization (no conflict handling yet)
  - subscribing and unsubscribing for changes (needs Elektra’s [dbus plugin](https://github.com/ElektraInitiative/libelektra/tree/master/src/plugins/dbus) mounted on subscribed path)
  - get writability of key (As far as definable as writable from Elektra)

## What is Not

- synchronization conflict handling
- code cleanup
- proper error handling
- optimizations
- get permission (Elektra does not support this)
- Setting write path in Elektra

## Pending Issues

- See #762, #302, #775, #768

## Going to Change

- default write path in elektra (currently this is /sw), this will probably going to be
  either a setting in kdb or an environment variable.

# Building

## Dependencies

- elektra (for standalone build)
- gelektra (for standalone build)
- glib
- gio
- dbus

## Build

### As Part of Elektra Build

```sh
mkdir build && cd build
cmake -GSETTINGS_MODULE_PRIORITY=200 -DBINDINGS=gsettings ..
make
```

### Standalone Build

```sh
cd GSETTINGS_SOURCE
mkdir build && cd build
cmake -GSETTINGS_MODULE_PRIORITY=200 ..
make
```

- `GSETTINGS_MODULE_PRIORITY` set this value above the value of other GSettingsBackend modules so it gets selected as default
- the dconf GSettingsBackend is known to have a value of `100`

# Installation

```sh
(sudo) make install
```

If you do a system installation consider doing a migration, so you can immediately keep using your settings.

# Migration

## Import dconf User Settings

For now you can export your existing dconf database trough:

```sh
dconf dump > dconf.ini
```

and import it in elektra

```sh
cat dconf.ini | kdb import /sw ini
```

## Mount dbus Plugin

export current settings and delete them so they will not be hidden

```sh
kdb export /sw dump > tmp
kdb rm -r user:/sw
```

mount sw at you preferred location

```sh
(sudo) kdb mount sw /sw dbus $restofplugins
```

import settings again

```sh
(sudo) cat tmp | kdb import /sw dump
rm tmp
```

If you want change notifications to work you have to mount /sw with the dbus plugin. Not mounting the dbus plugin is discouraged, because configuration changes will not propagate correctly.

<a id="hl-1"></a>

## Quick Start

If want to test Elektra's GSettings backend as default on your system, you can use the commands below to do so. These commands will compile Elektra with the GSettings backend. Note that this will replace the default backend (usually `dconf`). The ElektraSettings backend is currently **experimental**, so use it with caution and back up your date before doing this.

```sh
cmake -GNinja -DBINDINGS="ALL;glib;gsettings;-jna" -DBUILD_SHARED=ON -DBUILD_STATIC=ON -DBUILD_FULL=ON -DENABLE_COVERAGE=OFF -DENABLE_OPTIMIZATIONS=ON -DBUILD_STATIC=ON -DPLUGINS="ALL" -DTOOLS="ALL" -DINSTALL_SYSTEM_FILES=ON -DGSETTINGS_MODULE_PRIORITY=200 ..
ninja
(sudo) ninja install
(sudo) kdb gmount dbus
```

# Debugging

`G_MESSAGES_DEBUG=ElektraSettings` will enable debug output of the backend. If you have
set the priority of the module below `100` and dconf installed you also have to
set `GSETTINGS_BACKEND=elektra` to be able to test elektra as default backend.

##Examples

### Get/Set and Reset a Gsettings Value

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

### Force Elektra as Default Backend When Starting an Application:

```sh
G_MESSAGES_DEBUG=ElektraSettings GSETTINGS_BACKEND=elektra gedit
```

an application can still request a different backend then elektra with `g_settings_new_with_backend ()`
