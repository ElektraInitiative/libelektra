# Elektra Qt GUI #

## Introduction ##

The tool `qt-gui` offers a graphical interface for users of Elektra.
It allows users to create, manage, edit, and delete keys stored in the global key database (KDB).

## Compiling and Installation ##

### Dependencies ###

In order to compile and use the new `qt-gui` there are a few dependencies which must be installed. 

- qt5.3 or greater
- libdrm-dev
- libdiscount (libmarkdown2-dev)
- the following qt5 modules: `declarative` `quickcontrols`

Additionally, you may need `qtdeclarative5-dev` which is available in `wheezy-backports`.

Optional dependencies are (are automatically deactivated if dependencies are not found):
- `Qt5Svg` for SVG icon themes
- `Qt5DBus` so that `qt-gui` will be notified on changes.


I was able to install the correct dependencies on my system, running Kubuntu 14.10 using the command:	
`sudo apt-get install qt5-default qml-module-qtquick-controls qml-module-qtquick-dialogs qml-module-qtquick-layouts qml-module-qtgraphicaleffects libdrm-dev libmarkdown2-dev libqt5svg5-dev`


### Change Notification ###

It is recommended to use the viewer mode if DBus notifications are expected.
Use "Settings -> Viewermode" to activate the viewermode.

To publish changes to KDB via DBus use:

`kdb global-mount dbus`

`qt-gui` also subscribes to DBus notifications and automatically synchronizes
the configuration when notified.

Known Issue: On reload the current element in the tree view gets unselected,
you need to click on the element of interest again.


### Compiling ###
Compile Elektra as normal as per the [COMPILE document](/doc/COMPILE.md) making sure to include the `qt-gui` tool using the `-DTOOLS` flag.

For instance:
`-DTOOLS=ALL` or `-DTOOLS=qt-gui`

Note: If you install qt5 manually, you must either:
- make sure the qt5 libraries are included in `LD_LIBRARY_PATH`
- OR make sure to specify `-DCMAKE_PREFIX_PATH=/opt/Qt5.3.0/5.3/gcc_64/lib/cmake` 

### Installing ###

You can now install Elektra as you normally would or as described in the [install documentation](/doc/INSTALL.md).

## To Run ##

The following command will launch the Qt-GUI:

    kdb qt-gui
