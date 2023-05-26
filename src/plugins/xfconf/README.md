- infos = Information about the xfconf plugin is in keys below
- infos/author = Richard Stöckl <e11908080@student.tuwien.ac.at>
- infos/licence = BSD
- infos/needs =
- infos/provides = storage/xml
- infos/recommends =
- infos/placements = getstorage setstorage
- infos/status = maintained libc configurable experimental unfinished concept limited memleak
- infos/metadata =
- infos/description = storage plugin for xfconf

## Introduction

The xfconf plugin is a storage plugin to mount the xfconf configuration settings.
This allows to operate on the XFCE configuration with libelektra.
As usual, this allows the plugin to read and write to the XFCE configuration.

You can refer to the [official XFCE documentation of xfconf](https://docs.xfce.org/xfce/xfconf/start) to learn more about it.

## Xfconf Terminology

### Property

A property in xfconf is the same as a key in libelektra i.e. it has a name and can hold a value.
In contrast to libelektra, the value can be more complex i.e. it can be an array.
In this case, the value of the property is mapped as multiple keys in libelektra using the array structure.

### Channel

A channel is a type of namespace used in the xfconf library.
Usually, it is used to separate the properties of different applications which is helpful if different applications rely on a property with the same name but require them to hold different values.
For example Thunar uses a channel named `thunar`, Xfwm uses a channel named `xfwm4` and so on.
Keep in mind that channels are only used to separate the properties such as namespaces.
They are not a security feature i.e. every application has read/write access to every channel.

The list of all channels is stored in the `system:/elektra/modules/xfconf/channels` which is an array of all channel names.
Channels cannot be created manually with libelektra.
Instead, when mounting a channel (see [Plugin Configuration](#plugin-configuration) how to specify a channel) and setting a key value below the mount, it will be automatically created.

### Locks

Xfconf uses so-called locks to prevent users or applications to set properties to a new value.
They can be referred as a type of constant.
As of now, locks cannot be set or unset in xfconf which makes locked properties read-only forever.

## Dependencies

The xfconf library from the XFCE project is the main dependency of this plugin.
Usually, this library is called something such as `xfconf` (Arch, Fedora, `xfconf-devel` for compiling), `libxfconf-0` (Debian, `libxfconf-0-dev` for compmiling) or `xfce4-conf` (FreeBSD) in the package manager.
As xfconf itself depends on dbus and glib, these are dependecies too but should be installed with the package manager automatically.
This plugin is tested with the xfconf versions `4.16` and above.

Before you can start working with the plugin, you have to make sure dbus is running.
If you use a common desktop environment this should already be fulfilled.
Otherwise, `export $(dbus-launch)` can be used to start it.
You may also need to use `systemd-machine-id-setup` to create `/etc/machine-id`.

Note that this plugin does not support macOS since Xfce is not commonly used there, Mac Ports only provides a very old version (`4.12`) and Brew does not provide Xfce at all.

## Usage

The usage is identical to most storage plugins except that the channel option during mount must be defined in order to tell xfconf which channel to mount.

## Plugin Configuration

The required `channel` configuration option is used to tell xfconf which channel to mount.

## Examples

```zsh
# Backup-and-Restore: user:/tests/xfconf

# mount the xfwm channel
kdb mount -R noresolver none /test/xfwm xfconf "channel=xfwm4"

# store old button layout
set "OLD_LAYOUT=$(kdb get /test/xfwm/general/button_layout)"

# set only a close button
kdb set system:/test/xfwm/general/button_layout "C|"

# read the new layout
kdb get /test/xfwm/general/button_layout
#> C|

# restore old layout
kdb set /test/xfwm/general/button_layout "$OLD_LAYOUT"

# umount the channel
kdb umount /test/xfwm
```

## Limitations

- xfconf locks can only be read but not set as this is not possible in xfconf
- comments and sorting are not implemented due the lack of both in xfconf
- due to memory leaks in xfconf upstream, valgrind reports errors when running the tests
