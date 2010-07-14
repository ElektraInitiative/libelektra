/***************************************************************************
                     dbus.c  -  Skeleton of a plugin
                             -------------------
    begin                : Fri May 21 2010
    copyright            : (C) 2010 by Markus Raab
    email                : elektra@markus-raab.org
 ***************************************************************************/

/***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the BSD License (revised).                      *
 *                                                                         *
 ***************************************************************************/

/***************************************************************************
 *                                                                         *
 *   This is the skeleton of the methods you'll have to implement in order *
 *   to provide a valid plugin.                                            *
 *   Simple fill the empty functions with your code and you are            *
 *   ready to go.                                                          *
 *                                                                         *
 ***************************************************************************/


#ifndef ELEKTRA_PLUGIN_DBUS_H
#define ELEKTRA_PLUGIN_DBUS_H

#include <kdbplugin.h>

#include <dbus/dbus.h>

int elektraDbusSendMessage (DBusBusType type);

int elektraDbusOpen(Plugin *handle, Key *errorKey);
int elektraDbusClose(Plugin *handle, Key *errorKey);
int elektraDbusGet(Plugin *handle, KeySet *ks, Key *parentKey);
int elektraDbusSet(Plugin *handle, KeySet *ks, Key *parentKey);
int elektraDbusError(Plugin *handle, KeySet *ks, Key *parentKey);

Plugin *ELEKTRA_PLUGIN_EXPORT(dbus);

#endif
