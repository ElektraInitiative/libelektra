#ifndef __ELEKTAR_IO_DBUS_ADAPTER_H__
#define __ELEKTAR_IO_DBUS_ADAPTER_H__

#include <dbus/dbus.h>
#include <stdlib.h>
#include <string.h>

#include <kdbio.h>

/**
 * D-Bus Adapter Handle.
 *
 * Returned by elektraIoDbusAdapterAttach().
 */
typedef struct _ElektraIoDbusAdapterHandle ElektraIoDbusAdapterHandle;

/**
 * Attach D-Bus connection to asynchronous I/O binding.
 *
 * Messages are sent and received using the I/O binding
 *
 * @param  connection D-Bus connection
 * @param  ioBinding  I/O binding
 * @return            handle to be used with elektraIoDbusAdapterCleanup() or NULL on error
 */
ElektraIoDbusAdapterHandle * elektraIoDbusAdapterAttach (DBusConnection * connection, ElektraIoInterface * ioBinding);

/**
 * Remove D-Bus connection from I/O binding.
 *
 * This function frees the passed handle.
 *
 * Currently it is NOT possible to revert all changes made to a connection by
 * elektraIoDbusAdapterAttach(). It is advisable to call dbus_connection_unref()
 * or dbus_connection_close() in case of a private bus connection afterwards.
 *
 * @param  handle adapter handle
 * @retval 1 on success
 * @retval 0 on error
 */
int elektraIoDbusAdapterCleanup (ElektraIoDbusAdapterHandle * handle);

#endif
