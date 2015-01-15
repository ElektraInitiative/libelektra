# Elektra GUI Documentation #

## Introduction ##

This document aims to provide information about Elektra's new `qt-gui` feature. `qt-gui` offers a graphical
interface for users of Elektra. It allows users to create, manage, edit, and delete keys stored in KDB.

## Compiling and Installation ##

### Dependencies ###

In order to compile and use the new `qt-gui` there are a few dependencies which must be installed. 

- qt5.3 or greater
- libdrm-dev

I was able to install the correct dependencies on KDE using the command:	
`sudo apt-get install qt5-default qml-module-qtquick-controls qml-module-qtquick-dialogs qml-module-qtquick-layouts qml-module-qtgraphicaleffects libdrm-dev`

Note: If you install qt5 manually, make sure the qt5 libarires are inlcude in `LD_LIBRARY`.

### Compiling ###
Compile Elektra as normal as per the [COMPILE document](COMPILE.md) making sure to include the `qt-gui` tool using the `DPLUGINS` flag.

For instance:	
`-DPLUGINS=ALL` or `-DPLUGINS=qt-gui`

### Installing ###

To install `qt-gui` just run `sudo make install` after running CMAKE.

## To Run ##

The following command will lanuch the Qt-GUI:	
`kdb qt-gui`