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

#include <kdbassert.h>
#include <kdbioplugin.h>
#include <kdbnotificationinternal.h>
#include <kdbplugin.h>

#include <dbus/dbus.h>
#include <string.h>

#include "../../bindings/io/adapter/dbus/dbus.h"

/**
 * @internal
 * Private plugin data
 */
typedef struct
{
	// I/O binding (may be NULL)
	ElektraIoInterface * ioBinding;

	// Notification callback (may be NULL)
	ElektraNotificationCallback notificationCallback;
	void * notificationContext;

	// Indicates whether D-Bus connections are initialized
	int dbusInitialized;

	// D-Bus connections (may be NULL)
	DBusConnection * systemBus;
	DBusConnection * sessionBus;

	// D-Bus I/O adapter handles
	ElektraIoDbusAdapterHandle * systemBusAdapter;
	ElektraIoDbusAdapterHandle * sessionBusAdapter;

} ElektraDbusRecvPluginData;

int elektraDbusRecvSetupReceive (ElektraDbusRecvPluginData * pluginData, DBusBusType type, DBusHandleMessageFunction filter_func);
int elektraDbusRecvTeardownReceive (ElektraDbusRecvPluginData * pluginData, DBusBusType type, DBusHandleMessageFunction filter_func);
DBusHandlerResult elektraDbusRecvMessageHandler (DBusConnection * connection, DBusMessage * message, void * data);

void elektraDbusRecvSetIoBinding (Plugin * handle, ElektraIoInterface * binding);
void elektraDbusRecvOpenNotification (Plugin * handle, ElektraNotificationCallback callback, ElektraNotificationCallbackContext * context);
void elektraDbusRecvCloseNotification (Plugin * handle);

int elektraDbusRecvOpen (Plugin * handle, Key * errorKey);
int elektraDbusRecvClose (Plugin * handle, Key * errorKey);
int elektraDbusRecvGet (Plugin * handle, KeySet * ks, Key * parentKey);

Plugin * ELEKTRA_PLUGIN_EXPORT (dbus);

#endif
