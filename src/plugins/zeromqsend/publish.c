/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include "zeromqsend.h"

#include <kdbhelper.h>
#include <kdblogger.h>
#include <unistd.h>

/** time (300ms) to wait until zmq connections are established and sending & receiving works */
#define TIME_SETTLE (300 * 1000)

void elektraZeroMqSendPublish (const char * changeType, const char * keyName, ElektraZeroMqSendPluginData * data)
{
	// do  synchronous sending for now
	// TODO store context and socket in data
	// create zmq context
	if (!data->zmqContext)
	{
		data->zmqContext = zmq_ctx_new ();
		if (data->zmqContext == NULL)
		{
			ELEKTRA_LOG_WARNING ("zmq_ctx_new failed %s", zmq_strerror (zmq_errno ()));
			return;
		}
	}

	// create publish socket
	if (!data->zmqPublisher)
	{
		data->zmqPublisher = zmq_socket (data->zmqContext, ZMQ_PUB);
		if (data->zmqPublisher == NULL)
		{
			ELEKTRA_LOG_WARNING ("zmq_socket failed %s", zmq_strerror (zmq_errno ()));
			zmq_close (data->zmqPublisher);
			return;
		}
		// connect to endpoint
		char * endpoint = "tcp://localhost:6000"; // TODO make configurable
		int result = zmq_connect (data->zmqPublisher, endpoint);
		if (result != 0)
		{
			ELEKTRA_LOG_WARNING ("zmq_connect error: %s\n", zmq_strerror (zmq_errno ()));
			zmq_close (data->zmqPublisher);
			data->zmqPublisher = NULL;
			return;
		}
		// wait for connection to be available
		usleep (TIME_SETTLE);
	}

	// send notification
	int result = elektraZeroMqSendNotification (data->zmqPublisher, changeType, keyName);
	if (!result)
	{
		ELEKTRA_LOG_WARNING ("could not send notification\n");
	}
}

int elektraZeroMqSendNotification (void * socket, const char * changeType, const char * keyName)
{
	unsigned int size;

	// Send change type
	size = zmq_send (socket, changeType, elektraStrLen (changeType), ZMQ_SNDMORE);
	if (size != elektraStrLen (changeType))
	{
		return 0;
	}

	size = zmq_send (socket, keyName, elektraStrLen (keyName), 0);
	if (size != elektraStrLen (keyName))
	{
		return 0;
	}

	return 1;
}
