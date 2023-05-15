- infos =
- infos/author = Richard Stöckl <e11908080@student.tuwien.ac.at>
- infos/licence = BSD
- infos/status = experimental maintained
- infos/provides =
- infos/description = Elektra Bindings for Xfconf

## Introduction

These bindings provide an implementation of the Xfconf header files such that libelektra can be used as a drop-in replacement for Xfconf.
In other words, it makes applications which rely on Xfconf for their configuration use libelektra without the necessity for modifying their source code or even recompiling them.

## Xfconf Terminology

### Property

A property in Xfconf is the same as a key in libelektra i.e. it has a name and can hold a value.
In contrast to libelektra, the value can be more complex i.e. it can be an array.
In this case, the value of the property is mapped as multiple keys in libelektra using the array structure.

### Channel

A channel is a type of namespace used in the Xfconf library.
Usually, it is used to separate the properties of different applications which is helpful if different applications rely on a property with the same name but require them to hold different values.
For example Thunar uses a channel named `thunar`, Xfwm uses a channel named `xfwm4` and so on.
Keep in mind that channels are only used to separate the properties such as namespaces.
They are not a security feature i.e. every application has read/write access to every channel.

## Files

- The header files of this implementation are provided by Xfconf and are not included here.
- The resulting library is called `libxfconf-elektra.so`

## Classes

- [elektra-xfconf](elektra-xfconf.c) contains all global variables for the bindings and everything required for the initialization and shutdown of the Xfconf infrastructure.
- [elektra-xfconf-binding](elektra-xfconf-binding.c) contains a notification systems which makes it possible for functions to bind to properties.
- [elektra-xfconf-channel](elektra-xfconf-channel.c) contains everything for Xfconf channel functionality such as setting or getting properties.
- [elektra-xfconf-errors](elektra-xfconf-errors.c) contains Xfconfs internal error handling and is therefore not implemented.
- [elektra-xfconf-types](elektra-xfconf-types.c) contains Xfconf internal functions for type conversions. Similar to the error handling this is therefore not implemented. However, in order to work properly, every function defined in the Xfconf upstream headers require an implementation, otherwise symbols could not be found at runtime.

The following links provide the API documentation for Xfconf:

- https://developer.xfce.org/xfconf/xfconf-Xfconf-Library-Core.html
- https://developer.xfce.org/xfconf/xfconf-Xfconf-Channel.html
- https://developer.xfce.org/xfconf/xfconf-Xfconf-GObject-Binding.html

## Dependencies

### Compile-Time

The Xfconf library from the XFCE project is the main dependency of this plugin.
Usually, this library is called something such as `xfconf` (Arch, Fedora, `xfconf-devel` for compiling), `libxfconf-0` (Debian/Ubuntu, `libxfconf-0-dev` for compmiling) or `xfce4-conf` (FreeBSD) in the package manager.
As Xfconf itself depends on dbus and glib, these are dependencies too but should be installed with the package manager automatically.
This binding requires the Xfce, Xfconf versions `4.16` and above.

### Runtime

Beside libelektra and glib, nothing else is required at runtime.
However, as these bindings act as a drop-in replacement, the `xfconf` library might be a conflicting library if you want to install it as such.

## Installation

In order that these bindings can be used as a drop-in replacement for Xfconf, the original library must be replaced.

**Caution:** Although it is in general possible to use this binding as a replacement it is not recommended at all.
Although, Xfconf related properties should work as desired, configuration settings outside (such as Gtks) may result into an inconsistent state.
A few of the undesired effects are:

- Missing symbols in menus despite being configured to appear
- Unable to change the desktop background
- Inconsistent Gtk theme
- Unable to detect changes made to properties which live outside Xfconf

Use with caution and on non-production systems such as virtual machines only.

The installation can be achieved using `kdb xfconf-system-lib-replace` or `kdb xfconf-user-lib-replace`.
The first one will replace the Xfconf library of the system and requires therefore root privileges and will only work on systems where elektra is properly installed.
However, the benefit of the first option is that it can be simply undone with `kdb xfconf-system-lib-restore`.

To restore the changes of the `kdb xfconf-user-lib-replace` command, manual interaction is required in form of removing `LD_*` exports from `$HOME/.xprofile`

## Debugging

For debugging purposes, it might be useful to output and log all debug messages from glib.
This can be done by appending the `G_MESSAGES_DEBUG=all` environment variable to all the above-mentioned places.
All components responsible for starting the Xfce session will log their output wherever the display-manager stores the log files.
All applications started from the command line will put their debug log to the stderr.

## Quality

Note that this binding only handles configuration directly handled within Xfconf.
It is not able to configure settings which live outside Xfconf such as Gtk themes.
However, since settings such as the Gtk theme are part of the Xfce desktop itself, this binding is not able to configure all aspects of Xfce.

## Further Links

- Official Xfconf docs: https://docs.xfce.org/xfce/xfconf/start
- Official Xfconf query docs: https://docs.xfce.org/xfce/xfconf/xfconf-query
