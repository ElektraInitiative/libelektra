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

/**
 * @internal
 * Private plugin state
 */
typedef struct
{
	// remember keys for change detection
	KeySet * keys;

	// ZeroMQ context and socket (NULL until initialized at first elektraZeroMqSendPublish())
	void * zmqContext;
	void * zmqPublisher;

	// Timestamp when the connection was created
	struct timespec timeConnect;

} ElektraZeroMqSendPluginData;

int elektraZeroMqSendConnect (ElektraZeroMqSendPluginData * data);
void elektraZeroMqSendPublish (const char * changeType, const char * keyName, ElektraZeroMqSendPluginData * data);
int elektraZeroMqSendNotification (void * socket, const char * changeType, const char * keyName);

int elektraZeroMqSendOpen (Plugin * handle, Key * errorKey);
int elektraZeroMqSendClose (Plugin * handle, Key * errorKey);
int elektraZeroMqSendGet (Plugin * handle, KeySet * ks, Key * parentKey);
int elektraZeroMqSendSet (Plugin * handle, KeySet * ks, Key * parentKey);

Plugin * ELEKTRA_PLUGIN_EXPORT (zeromqsend);

#endif
