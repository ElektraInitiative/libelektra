# Elektra Qt GUI

## Introduction

The tool `qt-gui` offers a graphical interface for users of Elektra.
It allows users to create, manage, edit, and delete keys stored in the global key database (KDB).

## Compiling and Installation

### Dependencies

In order to compile and use the new `qt-gui` there are a few dependencies which must be installed.

- qt5.3 or greater
- libdrm-dev
- libdiscount (libmarkdown2-dev)
- the following qt5 modules: `quickcontrols` `declarative` (`qtdeclarative5-dev`)
- Alternatively, you may need `sudo apt-get install qml-module-qt-labs-folderlistmodel`
  and `sudo apt-get install qml-module-qt-labs-settings`

Optional dependencies are (are automatically deactivated if dependencies are not found):

- `Qt5Svg` for SVG icon themes (`sudo apt-get install libqt5svg5-dev`)
- `Qt5DBus` so that `qt-gui` will be notified on changes.

On Ubuntu (tested on 14.10 and 20.04) you can install the dependencies with

```
sudo apt-get install \
    qt5-default \
    qml-module-qtquick-controls \
    qml-module-qtquick-dialogs \
    qml-module-qtquick-layouts \
    qml-module-qtgraphicaleffects \
    libdrm-dev \
    libmarkdown2-dev \
    libqt5svg5-dev
```

### Change Notification

<!-- FIXME [new_backend]: outdated -->

It is recommended to use the viewer mode if DBus notifications are expected.
Use "Settings -> Viewermode" to activate the viewermode.

To publish changes to KDB via DBus use:

`kdb global-mount dbus`

`qt-gui` also subscribes to DBus notifications and automatically synchronizes
the configuration when notified.

Known Issue: On reload the current element in the tree view gets unselected,
you need to click on the element of interest again.

### Compiling

Compile Elektra as normal as per the [COMPILE document](/doc/COMPILE.md) making sure to include the `qt-gui` tool using the `-DTOOLS` flag.

For instance:
`-DTOOLS=ALL` or `-DTOOLS=qt-gui`

Note: If you install qt5 manually, you must either:

- make sure the qt5 libraries are included in `LD_LIBRARY_PATH`
- OR make sure to specify `-DCMAKE_PREFIX_PATH=/opt/Qt5.3.0/5.3/gcc_64/lib/cmake`

### Installing

You can now install Elektra as you normally would or as described in the [install documentation](/doc/INSTALL.md).
The package is called `elektra-qt-gui`.

## To Run

The following command will launch the Qt-GUI:

```sh
kdb qt-gui
```

After that a window will open that looks something like that:

![empty GUI](/src/tools/qt-gui/images/Qt-GUI-1.png)

You can mount a backend by going to _Database_ --> _Mount Backend..._

![mount backend](/src/tools/qt-gui/images/Qt-GUI-2.png)

This will open the mounting wizard where you can add a path:

![mounting wizard](/src/tools/qt-gui/images/Qt-GUI-3.png)

Next you can browse through the available backends and read their documentation.

![mounting documentation](/src/tools/qt-gui/images/Qt-GUI-4.png)

After you are done reading the documentation you can close the window again.

If you want to add a new key to the database you can choose a namespace in the left panel then click on Edit --> New --> Key. This will open a new window that looks like this:

![key wizard](/src/tools/qt-gui/images/Qt-GUI-5.png)

After entering the key information, you can view it in the list view. Just click on the namespace you chose and select the key.

![key view](/src/tools/qt-gui/images/Qt-GUI-6.png)
