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
#include <kdbnotificationplugin.h>
#include <kdbplugin.h>

#include <dbus/dbus.h>
#include <stdio.h>
#include <string.h>

#include "../../bindings/io/adapter/dbus/dbus.h"

typedef struct
{
	ElektraIoInterface * ioBinding;

	ElektraNotificationCallback notificationCallback;
	void * notificationPayload;

	int dbusInitialized;
	DBusConnection * systemBus;
	ElektraIoDbusAdapterHandle * systemBusAdapter;
	DBusConnection * sessionBus;
	ElektraIoDbusAdapterHandle * sessionBusAdapter;


} ElektraDbusRecvPluginData;

int elektraDbusRecvSetupReceive (ElektraDbusRecvPluginData * pluginData, DBusBusType type, DBusHandleMessageFunction filter_func);
int elektraDbusRecvTeardownReceive (ElektraDbusRecvPluginData * pluginData, DBusBusType type, DBusHandleMessageFunction filter_func);
DBusHandlerResult elektraDbusRecvMessageHandler (DBusConnection * connection, DBusMessage * message, void * data);

void elektraDbusRecvSetIoBinding (Plugin * handle, ElektraIoInterface * binding);
void elektraDbusRecvOpenNotification (Plugin * handle, ElektraNotificationCallback callback, void * data);
void elektraDbusRecvCloseNotification (Plugin * handle);

int elektraDbusRecvOpen (Plugin * handle, Key * errorKey);
int elektraDbusRecvClose (Plugin * handle, Key * errorKey);
int elektraDbusRecvGet (Plugin * handle, KeySet * ks, Key * parentKey);

Plugin * ELEKTRA_PLUGIN_EXPORT (dbus);

#endif
