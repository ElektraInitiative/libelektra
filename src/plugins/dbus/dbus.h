/**
 * @file
 *
 * @brief Source for dbus plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#ifndef ELEKTRA_PLUGIN_DBUS_H
#define ELEKTRA_PLUGIN_DBUS_H

#include <kdbioplugin.h>
#include <kdbplugin.h>

#include <dbus/dbus.h>
#include <stdio.h>
#include <string.h>

#include "../../bindings/io/adapter/dbus/dbus.h"

typedef struct
{
	// remember all keys
	KeySet * keys;

	ElektraIoInterface * ioBinding;

	DBusConnection * systemBus;
	ElektraIoDbusAdapterHandle * systemBusAdapter;
	DBusConnection * sessionBus;
	ElektraIoDbusAdapterHandle * sessionBusAdapter;

} ElektraDbusPluginData;

int elektraDbusSendMessage (ElektraDbusPluginData * data, DBusBusType type, const char * keyName, const char * signalName);
int elektraDbusReceiveMessage (DBusBusType type, DBusHandleMessageFunction filter_func);
int elektraDbusSetupReceiveMessage (DBusConnection * connection, DBusHandleMessageFunction filter_func, void * data);
int elektraDbusTeardownReceiveMessage (DBusConnection * connection, DBusHandleMessageFunction filter_func, void * data);

void elektraDbusSetIoBinding (Plugin * handle, ElektraIoInterface * binding);

int elektraDbusOpen (Plugin * handle, Key * errorKey);
int elektraDbusClose (Plugin * handle, Key * errorKey);
int elektraDbusGet (Plugin * handle, KeySet * ks, Key * parentKey);
int elektraDbusSet (Plugin * handle, KeySet * ks, Key * parentKey);

Plugin * ELEKTRA_PLUGIN_EXPORT (dbus);

#endif
