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
#include <kdbplugin.h>

#include <time.h> // struct timespec

#include <zmq.h>

#define ELEKTRA_ZEROMQ_DEFAULT_PUB_ENDPOINT "tcp://localhost:6000"

/**
 * @internal
 * Private plugin state
 */
typedef struct
{
	// ZeroMQ context and socket (NULL until initialized at first elektraZeroMqSendPublish())
	void * zmqContext;
	void * zmqPublisher;
	void * zmqPublisherMonitor;

	// endpoint for publish socket
	const char * endpoint;

	int hasSubscriber;
} ElektraZeroMqSendPluginData;

int elektraZeroMqSendConnect (ElektraZeroMqSendPluginData * data);
int elektraZeroMqSendPublish (const char * changeType, const char * keyName, ElektraZeroMqSendPluginData * data);
int elektraZeroMqSendNotification (void * socket, const char * changeType, const char * keyName);

int elektraZeroMqSendOpen (Plugin * handle, Key * errorKey);
int elektraZeroMqSendClose (Plugin * handle, Key * errorKey);
int elektraZeroMqSendGet (Plugin * handle, KeySet * ks, Key * parentKey);
int elektraZeroMqSendSet (Plugin * handle, KeySet * ks, Key * parentKey);

Plugin * ELEKTRA_PLUGIN_EXPORT (zeromqsend);

#endif
