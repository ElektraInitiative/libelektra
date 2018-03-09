/**
 * @file
 *
 * @brief Headers for dbusrecv plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#ifndef ELEKTRA_PLUGIN_ZEROMQSEND_H
#define ELEKTRA_PLUGIN_ZEROMQSEND_H

#include <kdbassert.h>
#include <kdbioplugin.h>
#include <kdbnotificationinternal.h>
#include <kdbplugin.h>

#include <zmq.h>

#include <kdbio_adapter_zeromq.h> // elektraIoAdapterZeroMq*()


typedef struct ElektraZeroMqRecvQueuedNotification
{
	char * changeType;
	char * keyName;

	struct ElektraZeroMqRecvQueuedNotification * next;
} ElektraZeroMqRecvQueuedNotification;

/**
 * @internal
 * Private plugin state
 */
typedef struct
{
	// I/O binding (may be NULL)
	ElektraIoInterface * ioBinding;

	// Notification callback (may be NULL)
	ElektraNotificationCallback notificationCallback;
	void * notificationContext;

	// ZeroMQ context and socket (NULL until initialized at first elektraZeroMqRecvPublish())
	void * zmqContext;
	void * zmqSubscriber;

	// ZeroMQ I/O adapter handle (NULL without I/O binding)
	ElektraIoAdapterZeroMqHandle * zmqAdapter;

} ElektraZeroMqRecvPluginData;

void elektraZeroMqRecvSetup (ElektraZeroMqRecvPluginData * data);
void elektraZeroMqRecvTeardown (ElektraZeroMqRecvPluginData * data);

int elektraZeroMqRecvOpen (Plugin * handle, Key * errorKey);
int elektraZeroMqRecvClose (Plugin * handle, Key * errorKey);
int elektraZeroMqRecvGet (Plugin * handle, KeySet * ks, Key * parentKey);

Plugin * ELEKTRA_PLUGIN_EXPORT (zeromqrecv);

#endif
