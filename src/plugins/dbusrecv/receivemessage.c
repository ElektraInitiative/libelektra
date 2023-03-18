/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include "dbusrecv.h"

#include <internal/utility/logger.h>

#define RECEIVE_MATCH_RULE "type='signal',interface='org.libelektra',path='/org/libelektra/configuration'"

/**
 * @internal
 * Get and setup D-Bus connection.
 *
 * handlePointer is updated to point to D-Bus I/O adapter handle.
 *
 * @param  type          D-Bus bus type
 * @param  ioBinding     I/O binding (optional)
 * @param  handlePointer Pointer to D-Bus I/O adapter handle
 * @return D-Bus connection or NULL on error
 */
static DBusConnection * dbusGetConnection (DBusBusType type, ElektraIoInterface * ioBinding, ElektraIoAdapterDbusHandle ** handlePointer)
{
	DBusError error;
	dbus_error_init (&error);

	DBusConnection * connection = dbus_bus_get_private (type, &error);
	if (connection == NULL)
	{
		ELEKTRA_LOG_WARNING ("Failed to open connection to %s message bus: %s", (type == DBUS_BUS_SYSTEM) ? "system" : "session",
				     error.message);
		dbus_error_free (&error);
		return NULL;
	}
	dbus_error_free (&error);

	dbus_connection_set_exit_on_disconnect (connection, FALSE);

	ElektraIoAdapterDbusHandle * handle = elektraIoAdapterDbusAttach (connection, ioBinding);
	if (!handle)
	{
		ELEKTRA_LOG_WARNING ("Failed to attach to the I/O binding");
		return NULL;
	}
	*handlePointer = handle;

	return connection;
}

/**
 * @internal
 * Revert changes made to D-Bus connection by elektraDbusRecvSetupReceive().
 *
 * @param  handle      Plugin handle
 * @param  connection  D-bus bus type
 * @param  filter_func message handler
 * @retval 1 on success
 * @retval 0 on error
 */
int elektraDbusRecvTeardownReceive (Plugin * handle, DBusBusType type, DBusHandleMessageFunction filter_func)
{
	ElektraDbusRecvPluginData * pluginData = elektraPluginGetData (handle);

	DBusConnection * connection;
	DBusError error;

	switch (type)
	{
	case DBUS_BUS_SYSTEM:
		if (!pluginData->systemBus)
		{
			pluginData->systemBus = dbusGetConnection (type, pluginData->ioBinding, &pluginData->systemBusAdapter);
		}
		connection = pluginData->systemBus;
		break;
	case DBUS_BUS_SESSION:
		if (!pluginData->sessionBus)
		{
			pluginData->sessionBus = dbusGetConnection (type, pluginData->ioBinding, &pluginData->sessionBusAdapter);
		}
		connection = pluginData->sessionBus;
		break;
	default:
		connection = NULL;
	}
	if (connection == NULL)
	{
		return 0;
	}

	dbus_error_init (&error);

	dbus_bus_remove_match (connection, RECEIVE_MATCH_RULE, &error);
	if (dbus_error_is_set (&error)) goto error;

	dbus_connection_remove_filter (connection, filter_func, handle);

	return 1;
error:
	ELEKTRA_LOG_WARNING ("Error occurred\n");
	dbus_error_free (&error);
	return 0;
}

/**
 * @internal
 * Setup D-Bus connection for receiving Elektra's signal messages.
 *
 * @param  handle      Plugin handle
 * @param  connection  D-Bus connection
 * @param  filter_func message handler
 * @retval 1 on success
 * @retval 0 on error
 */
int elektraDbusRecvSetupReceive (Plugin * handle, DBusBusType type, DBusHandleMessageFunction filter_func)
{
	ElektraDbusRecvPluginData * pluginData = elektraPluginGetData (handle);

	DBusConnection * connection;
	DBusError error;

	switch (type)
	{
	case DBUS_BUS_SYSTEM:
		if (!pluginData->systemBus)
		{
			pluginData->systemBus = dbusGetConnection (type, pluginData->ioBinding, &pluginData->systemBusAdapter);
		}
		connection = pluginData->systemBus;
		break;
	case DBUS_BUS_SESSION:
		if (!pluginData->sessionBus)
		{
			pluginData->sessionBus = dbusGetConnection (type, pluginData->ioBinding, &pluginData->sessionBusAdapter);
		}
		connection = pluginData->sessionBus;
		break;
	default:
		connection = NULL;
	}
	if (connection == NULL)
	{
		return 0;
	}

	dbus_error_init (&error);

	dbus_bus_add_match (connection, RECEIVE_MATCH_RULE, &error);
	if (dbus_error_is_set (&error)) goto error;

	if (!dbus_connection_add_filter (connection, filter_func, handle, NULL))
	{
		goto error;
	}

	return 1;
error:
	ELEKTRA_LOG_WARNING ("Error occurred\n");
	dbus_error_free (&error);
	return 0;
}
