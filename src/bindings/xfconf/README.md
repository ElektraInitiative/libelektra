- infos =
- infos/author = Richard Stöckl <e11908080@student.tuwien.ac.at>
- infos/licence = BSD
- infos/status = experimental maintained
- infos/provides =
- infos/description = Elektra Bindings for xfconf

## Introduction

These bindings provide an implementation of the xfconf header files such that libelektra can be used as a drop-in replacement for xfconf.
In other words, it makes applications which rely on xfconf for their configuration use libelektra without the necessity for modifying their source code or even recompiling them.

For further understanding how xfconf is structured, please refer to the [xfconf plugin](../../../src/plugins/xfconf/README.md#xfconf-terminology).

todo.

## Files

- The header files of this implementation are provided by xfconf and are not included here.
- The resulting library is called `libxfconfbinding.so`

## Classes

Explain which [classes](/doc/help/elektra-glossary.md) the library contains.
Add a link to the API documentation.
todo.

## Dependencies

### Compile-Time

The xfconf library from the XFCE project is the main dependency of this plugin.
Usually, this library is called something such as `xfconf` (Arch, Fedora, `xfconf-devel` for compiling), `libxfconf-0` (Debian, `libxfconf-0-dev` for compmiling) or `xfce4-conf` (FreeBSD) in the package manager.
As xfconf itself depends on dbus and glib, these are dependencies too but should be installed with the package manager automatically.
This plugin is tested with the xfconf versions `4.16` and above.

### Runtime

Beside libelektra and glib, nothing else is required at runtime.
However, as these bindings act as a drop-in replacement, the `xfconf` library might be a conflicting library if you want to install it as such.

## Installation

In order that these bindings can be used as a drop-in replacement for xfconf, the original library must be replaced.
Assuming that the library directory is `/usr/local/lib`, this can be achieved as follows:

1. copy the `libxfconfbinding.so` to `/usr/local/lib/libxfconfbinding.so.0.0.1`
2. symlink `/usr/local/lib/libxfconfbinding.so.0` to `/usr/local/lib/libxfconfbinding.so.0.0.1`
3. symlink `/usr/local/lib/libxfconfbinding.so` to `/usr/local/lib/libxfconfbinding.so.0.0.1`
4. symlink `/usr/local/lib/libxfconf-0.so.3.0.0` to `/usr/local/lib/libxfconfbinding.so.0.0.1`
5. take the symlinks `/usr/local/lib/libxfconf-0.so.3` and `/usr/local/lib/libxfconf-.so` which point to `/usr/local/lib/libxfconf-0.so` from the upstream xfconf library and place them into you system

## Using the binding as a replacement in Xfce

It is currently possible to use this binding instead of Xfconf in order to start and use Xfce.

**Caution:** Although it is in general possible to use this binding as a replacement it is not recommended at all.
Currently, it shows an unpredictable behavior for an unknown reason.
A few of the undesired effects are:

- Inconsistent layout of the taskbar every session restart
- Missing symbols in menus despite being configured to appear
- Weird animations during the session startup
  One guess is, this is the result of race-conditions since Xfce consists of multiple components which concurrently read and write configuration.

Use with caution and on non-production systems such as virtual machines only.

The following instructions only have an effect on the current user which allows a quick revert through a different user.

Build elektra as instructed in the [COMPILE](../../../doc/COMPILE.md#developer-options) and do not forget to include the `xfconfbinding` and set the `KDB_DB_SYSTEM` and `CMAKE_INSTALL_PREFIX` paths properly.
In this example, it is assumed, that the cmake build directory is `$HOME/build` and stored in the environment variable `CMAKE_BUILD_XFCONFBINDING` and `KDB_DB_SYSTEM=$CMAKE_BUILD_XFCONFBINDING/system` and `CMAKE_INSTALL_PREFIX=$CMAKE_BUILD_XFCONFBINDING/install`.

After building, a file at `~/.xprofile` with following content must be created:

```shell
export CMAKE_BUILD_XFCONFBINDING=$HOME/build
export LD_LIBRARY_PATH=$CMAKE_BUILD_XFCONFBINDING/lib
export LD_PRELOAD=$CMAKE_BUILD_XFCONFBINDING/lib/libxfconfbinding.so
```

Also make sure to use these variables in your shell.
A possible solution is to copy this file to `.bash_profile`/`~/.zshenv` when using bash/zsh.

Then, re-login into a text session, a graphical session with Xfce will fail at this point.
Xfce needs to be populated with some configuration settings.
This can be archived using the [provided script](scripts/populate-xfconf.sh).

When done, the graphical session can bes started and will now use elektra for the Xfce configuration.

To restore the usage of the systems Xfconf, it is enough to restore the content of the variable files created previously.

## Quality

More information than `infos/status` including Performance Characteristics.
Mention open issues here.
todo.

## Further Links

Link to tutorials etc.
todo.

## Notes

None.
todo.
