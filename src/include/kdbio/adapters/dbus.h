/**
 * @file
 *
 * @brief I/O Adapter for D-Bus.
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */
#ifndef ELEKTRA_IO_ADAPTER_DBUS_H
#define ELEKTRA_IO_ADAPTER_DBUS_H

#include <dbus/dbus.h>
#include <stdlib.h>
#include <string.h>

#include <elektra/io/api.h>

/**
 * D-Bus Adapter Handle.
 *
 * Returned by elektraIoAdapterDbusAttach().
 */
typedef struct _ElektraIoAdapterDbusHandle ElektraIoAdapterDbusHandle;

/**
 * Attach D-Bus connection to asynchronous I/O binding.
 *
 * Messages are sent and received using the I/O binding
 *
 * @param  connection D-Bus connection
 * @param  ioBinding  I/O binding
 * @return            handle to be used with elektraIoAdapterDbusCleanup() or NULL on error
 */
ElektraIoAdapterDbusHandle * elektraIoAdapterDbusAttach (DBusConnection * connection, ElektraIoInterface * ioBinding);

/**
 * Remove D-Bus connection from I/O binding.
 *
 * This function frees the passed handle.
 *
 * Currently it is NOT possible to revert all changes made to a connection by
 * elektraIoAdapterDbusAttach(). It is advisable to call dbus_connection_unref()
 * or dbus_connection_close() in case of a private bus connection afterwards.
 *
 * @param  handle adapter handle
 * @retval 1 on success
 * @retval 0 on error
 */
int elektraIoAdapterDbusCleanup (ElektraIoAdapterDbusHandle * handle);

#endif
