/**
 * @file
 *
 * @brief I/O Adapter for D-Bus.
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */
#include <elektra/io/adapters/dbus.h>
#include <internal/macros/attributes.h>
#include <internal/utility/alloc.h>
#include <internal/utility/logger.h>
#include <stdlib.h>
#include <string.h>


typedef struct _ElektraIoAdapterDbusHandle
{
	DBusConnection * connection;
	ElektraIoInterface * ioBinding;
	ElektraIoIdleOperation * dispatchIdle;
} _ElektraIoAdapterDbusHandle;

typedef struct DbusAdapterWatchInfo
{
	_ElektraIoAdapterDbusHandle * private;
	DBusWatch * watch;
} DbusAdapterWatchInfo;

static int dbusToFlags (int dbus)
{
	int flags = 0;
	if (dbus & DBUS_WATCH_READABLE)
	{
		flags |= ELEKTRA_IO_READABLE;
	}
	if (dbus & DBUS_WATCH_WRITABLE)
	{
		flags |= ELEKTRA_IO_WRITABLE;
	}
	return flags;
}

static void dbusWrapperDispatch (ElektraIoIdleOperation * idle)
{
	_ElektraIoAdapterDbusHandle * priv = elektraIoIdleGetData (idle);

	if (dbus_connection_get_dispatch_status (priv->connection) == DBUS_DISPATCH_DATA_REMAINS)
	{
		dbus_connection_dispatch (priv->connection);
	}
	// Disable idle if dispatching is done
	if (dbus_connection_get_dispatch_status (priv->connection) != DBUS_DISPATCH_DATA_REMAINS)
	{
		elektraIoIdleSetEnabled (priv->dispatchIdle, 0);
		elektraIoBindingUpdateIdle (priv->dispatchIdle);
	}
}

static void dbusWrapperHandleDispatch (DBusConnection * connection ELEKTRA_UNUSED, DBusDispatchStatus status, void * data)
{
	_ElektraIoAdapterDbusHandle * priv = (_ElektraIoAdapterDbusHandle *) data;
	if (status == DBUS_DISPATCH_DATA_REMAINS)
	{
		elektraIoIdleSetEnabled (priv->dispatchIdle, 1);
		elektraIoBindingUpdateIdle (priv->dispatchIdle);
	}
}

static void dbusWrapperPoll (ElektraIoFdOperation * fdOp, int flags)
{
	DbusAdapterWatchInfo * watchData = elektraIoFdGetData (fdOp);
	DBusWatch * watch = watchData->watch;
	_ElektraIoAdapterDbusHandle * priv = watchData->private;

	int dbus_condition = 0;
	if (flags & ELEKTRA_IO_READABLE)
	{
		dbus_condition |= DBUS_WATCH_READABLE;
	}
	if (flags & ELEKTRA_IO_WRITABLE)
	{
		dbus_condition |= DBUS_WATCH_WRITABLE;
	}

	dbus_watch_handle (watch, dbus_condition);

	dbusWrapperHandleDispatch (priv->connection, DBUS_DISPATCH_DATA_REMAINS, priv);
}

static void dbusWrapperTimeout (ElektraIoTimerOperation * timerOp)
{
	DBusTimeout * timeout = elektraIoTimerGetData (timerOp);
	dbus_timeout_handle (timeout);
}

static dbus_bool_t dbusWrapperAddWatch (DBusWatch * watch, void * data)
{
	// printf ("dbusWrapperAddWatch\n");
	_ElektraIoAdapterDbusHandle * private = (_ElektraIoAdapterDbusHandle *) data;
	ElektraIoInterface * ioBinding = private->ioBinding;

	// Get file descriptor from watch
	int fd = dbus_watch_get_unix_fd (watch);
	if (fd == -1)
	{
		fd = dbus_watch_get_socket (watch);
	}
	if (fd == -1)
	{
		ELEKTRA_LOG_WARNING ("Could not get file descriptor for watch");
		return FALSE;
	}

	DbusAdapterWatchInfo * watchData = elektraMalloc (sizeof (*watchData));
	if (!watchData)
	{
		return FALSE;
	}
	watchData->watch = watch;
	watchData->private = private;

	// Create new file descriptor info
	int flags = dbusToFlags (dbus_watch_get_flags (watch));
	int enabled = dbus_watch_get_enabled (watch);
	ElektraIoFdOperation * fdOp = elektraIoNewFdOperation (fd, flags, enabled, dbusWrapperPoll, watchData);
	if (!fdOp)
	{
		return FALSE;
	}

	// Store file descriptor info in watcher
	dbus_watch_set_data (watch, (void *) fdOp, elektraFree);

	int success = elektraIoBindingAddFd (ioBinding, fdOp);
	if (!success)
	{
		return FALSE;
	}

	return TRUE;
}


static void dbusWrapperRemoveWatch (DBusWatch * watch, void * data ELEKTRA_UNUSED)
{
	ElektraIoFdOperation * fdOp = dbus_watch_get_data (watch);
	DbusAdapterWatchInfo * watchInfo = elektraIoFdGetData (fdOp);

	elektraIoBindingRemoveFd (fdOp);

	elektraFree (watchInfo);
}

static void dbusWrapperWatchToggled (DBusWatch * watch, void * data ELEKTRA_UNUSED)
{
	ElektraIoFdOperation * fdOp = dbus_watch_get_data (watch);

	elektraIoFdSetEnabled (fdOp, dbus_watch_get_enabled (watch));
	elektraIoFdSetFlags (fdOp, dbusToFlags (dbus_watch_get_flags (watch)));

	elektraIoBindingUpdateFd (fdOp);
}

static dbus_bool_t dbusWrapperAddTimeout (DBusTimeout * timeout, void * data)
{
	_ElektraIoAdapterDbusHandle * private = (_ElektraIoAdapterDbusHandle *) data;
	ElektraIoInterface * ioBinding = private->ioBinding;

	// Create new file descriptor info
	int interval = dbus_timeout_get_interval (timeout);
	int enabled = dbus_timeout_get_enabled (timeout);
	ElektraIoTimerOperation * timerOp = elektraIoNewTimerOperation (interval, enabled, dbusWrapperTimeout, timeout);
	if (!timerOp)
	{
		return FALSE;
	}

	// Store file descriptor info in timeouter
	dbus_timeout_set_data (timeout, (void *) timerOp, elektraFree);

	elektraIoBindingAddTimer (ioBinding, timerOp);
	return TRUE;
}


static void dbusWrapperRemoveTimeout (DBusTimeout * timeout, void * data ELEKTRA_UNUSED)
{
	ElektraIoTimerOperation * timerOp = dbus_timeout_get_data (timeout);

	elektraIoBindingRemoveTimer (timerOp);
}

static void dbusWrapperTimeoutToggled (DBusTimeout * timeout, void * data ELEKTRA_UNUSED)
{
	ElektraIoTimerOperation * timerOp = dbus_timeout_get_data (timeout);

	elektraIoTimerSetEnabled (timerOp, dbus_timeout_get_enabled (timeout));
	elektraIoTimerSetInterval (timerOp, dbus_timeout_get_interval (timeout));

	elektraIoBindingUpdateTimer (timerOp);
}

ElektraIoAdapterDbusHandle * elektraIoAdapterDbusAttach (DBusConnection * connection, ElektraIoInterface * ioBinding)
{
	_ElektraIoAdapterDbusHandle * priv = elektraMalloc (sizeof (*priv));
	if (!priv)
	{
		return 0;
	}
	priv->connection = connection;
	priv->ioBinding = ioBinding;

	dbus_connection_set_watch_functions (connection, dbusWrapperAddWatch, dbusWrapperRemoveWatch, dbusWrapperWatchToggled, priv, NULL);

	dbus_connection_set_timeout_functions (connection, dbusWrapperAddTimeout, dbusWrapperRemoveTimeout, dbusWrapperTimeoutToggled, priv,
					       NULL);

	// Add timeout for reading messages
	ElektraIoIdleOperation * dispatchIdle = elektraIoNewIdleOperation (0, dbusWrapperDispatch, priv);
	if (!dispatchIdle)
	{
		return 0;
	}
	elektraIoBindingAddIdle (ioBinding, dispatchIdle);

	priv->dispatchIdle = dispatchIdle;

	dbus_connection_set_dispatch_status_function (connection, dbusWrapperHandleDispatch, priv, NULL);

	return priv;
}

// TODO rename to elektraIoAdapterDbusDetach when complete reversal is possible
int elektraIoAdapterDbusCleanup (ElektraIoAdapterDbusHandle * priv)
{
	DBusConnection * connection = priv->connection;

	// TODO currently not possible because dbus uses multiple watches for the same fd
	// which raises an assertion in uv when trying to remove one of them
	// calling dbus_connection_close() & dbus_connection_unref() works fine
	// dbus_connection_set_watch_functions (connection, NULL, NULL, NULL, NULL, NULL);
	// dbus_connection_set_timeout_functions (connection, NULL, NULL, NULL, NULL, NULL);

	dbus_connection_set_dispatch_status_function (connection, NULL, NULL, NULL);

	elektraIoBindingRemoveIdle (priv->dispatchIdle);
	elektraFree (priv->dispatchIdle);

	elektraFree (priv);

	return 1;
}
