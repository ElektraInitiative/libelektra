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

#include <internal/utility/assert.h>
#include <elektra/plugin/plugin.h>

#include <time.h> // struct timespec

#include <zmq.h>

/** default endpoint for plugin */
#define ELEKTRA_ZEROMQ_DEFAULT_PUB_ENDPOINT "tcp://localhost:6000"

/** default connection timeout for plugin */
#define ELEKTRA_ZEROMQ_DEFAULT_CONNECT_TIMEOUT 1000

/** default subscription timeout for plugin */
#define ELEKTRA_ZEROMQ_DEFAULT_SUBSCRIBE_TIMEOUT 200

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

	// timeouts
	long connectTimeout;
	long subscribeTimeout;

	int hasSubscriber;
} ElektraZeroMqSendPluginData;

int elektraZeroMqSendConnect (ElektraZeroMqSendPluginData * data);
int elektraZeroMqSendPublish (const char * changeType, const char * keyName, ElektraZeroMqSendPluginData * data);
int elektraZeroMqSendNotification (void * socket, const char * changeType, const char * keyName);

int elektraZeroMqSendOpen (Plugin * handle, Key * errorKey);
int elektraZeroMqSendClose (Plugin * handle, Key * errorKey);
int elektraZeroMqSendGet (Plugin * handle, KeySet * ks, Key * parentKey);
int elektraZeroMqSendCommit (Plugin * handle, KeySet * ks, Key * parentKey);

Plugin * ELEKTRA_PLUGIN_EXPORT;

#endif
