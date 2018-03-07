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

// elektraIoDbus*()
//#include <kdbio_adapter_zeromq.h>

/**
 * @internal
 * Private plugin data
 */
typedef struct
{
	// remember keys for change detection
	KeySet * keys;

	// I/O binding (may be NULL)
	ElektraIoInterface * ioBinding;

	// ZeroMQ context and socket (may be NULL)
	void * zmqContext;
	void * zmqPublisher;

	// ZeroMQ I/O adapter handle
	// ElektraIoAdapterZeroMqHandle * zmqAdapter;

} ElektraZeroMqSendPluginData;

void elektraZeroMqSendPublish (const char * changeType, const char * keyName, ElektraZeroMqSendPluginData * data);
int elektraZeroMqSendNotification (void * socket, const char * changeType, const char * keyName);

int elektraZeroMqSendOpen (Plugin * handle, Key * errorKey);
int elektraZeroMqSendClose (Plugin * handle, Key * errorKey);
int elektraZeroMqSendGet (Plugin * handle, KeySet * ks, Key * parentKey);
int elektraZeroMqSendSet (Plugin * handle, KeySet * ks, Key * parentKey);

Plugin * ELEKTRA_PLUGIN_EXPORT (zeromqsend);

#endif
