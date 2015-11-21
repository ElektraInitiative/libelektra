/**
* \file
*
* @brief Source for dbus plugin
*
* \copyright BSD License (see doc/COPYING or http://www.libelektra.org)
*
*/

#ifndef ELEKTRA_PLUGIN_DBUS_H
#define ELEKTRA_PLUGIN_DBUS_H

#include <kdbplugin.h>

#include <dbus/dbus.h>
#include <stdio.h>
#include <string.h>


int elektraDbusSendMessage (DBusBusType type, const char *keyName, const char *signalName);
int elektraDbusReceiveMessage (DBusBusType type, DBusHandleMessageFunction filter_func);

int elektraDbusClose(Plugin *handle, Key *errorKey);
int elektraDbusGet(Plugin *handle, KeySet *ks, Key *parentKey);
int elektraDbusSet(Plugin *handle, KeySet *ks, Key *parentKey);

Plugin *ELEKTRA_PLUGIN_EXPORT(dbus);

#endif
