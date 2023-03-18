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

#include <internal/utility/assert.h>
#include <elektra/io/plugin.h>
#include <elektra/plugin/plugin.h>

#include <dbus/dbus.h>
#include <string.h>

// elektraIoDbus*()
#include <elektra/io/adapters/dbus.h>

/**
 * @internal
 * Private plugin data
 */
typedef struct
{
	// remember all keys
	KeySet * keys;

	// D-Bus connections (may be NULL)
	DBusConnection * systemBus;
	DBusConnection * sessionBus;
} ElektraDbusPluginData;

int elektraDbusSendMessage (ElektraDbusPluginData * data, DBusBusType type, const char * keyName, const char * signalName);
int elektraDbusReceiveMessage (DBusBusType type, DBusHandleMessageFunction filter_func);
int elektraDbusSetupReceiveMessage (DBusConnection * connection, DBusHandleMessageFunction filter_func, void * data);
int elektraDbusTeardownReceiveMessage (DBusConnection * connection, DBusHandleMessageFunction filter_func, void * data);

int elektraDbusOpen (Plugin * handle, Key * errorKey);
int elektraDbusClose (Plugin * handle, Key * errorKey);
int elektraDbusGet (Plugin * handle, KeySet * ks, Key * parentKey);
int elektraDbusCommit (Plugin * handle, KeySet * ks, Key * parentKey);

Plugin * ELEKTRA_PLUGIN_EXPORT;

#endif
