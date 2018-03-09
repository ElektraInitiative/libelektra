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

#include <time.h>   // clock_gettime()
#include <unistd.h> // usleep()

/** time (300ms) to wait until zmq connections are established and sending & receiving works */
#define TIME_SETTLE_US (300 * 1000)

/**
 * @internal
 * Connect to ZeroMq SUB or XSUB socket bound at endpoint.
 *
 * @param  data plugin data
 * @retval 1 on success
 * @retval 0 on error
 */
int elektraZeroMqSendConnect (ElektraZeroMqSendPluginData * data)
{
	// create zmq context
	if (!data->zmqContext)
	{
		data->zmqContext = zmq_ctx_new ();
		if (data->zmqContext == NULL)
		{
			ELEKTRA_LOG_WARNING ("zmq_ctx_new failed %s", zmq_strerror (zmq_errno ()));
			return 0;
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
			return 0;
		}
		// connect to endpoint
		char * endpoint = "tcp://localhost:6000"; // TODO make configurable
		int result = zmq_connect (data->zmqPublisher, endpoint);
		if (result != 0)
		{
			ELEKTRA_LOG_WARNING ("zmq_connect error: %s\n", zmq_strerror (zmq_errno ()));
			zmq_close (data->zmqPublisher);
			data->zmqPublisher = NULL;
			return 0;
		}

		// store timestamp when connection was initiated (not established...)
		clock_gettime (CLOCK_MONOTONIC, &data->timeConnect);
	}

	return 1;
}

/**
 * Publish notification on ZeroMq connection.
 *
 * @param changeType type of change
 * @param keyName    name of changed key
 * @param data       plugin data
 */
void elektraZeroMqSendPublish (const char * changeType, const char * keyName, ElektraZeroMqSendPluginData * data)
{
	/* NOTE
	 * - context and socket are created
	 * - socket connects to endpoint
	 *   - if I/O binding is not present
	 *     - wait until connection is available (using usleep) otherwise messages are lost when writing immediately
	 *   - send the notification
	 */
	if (!elektraZeroMqSendConnect (data))
	{
		ELEKTRA_LOG_WARNING ("could not connect to endpoint");
		return;
	}

	// check if settle time has passed
	struct timespec wait;
	struct timespec now;
	if (clock_gettime (CLOCK_MONOTONIC, &now) == 0)
	{
		// calculate remaining settle time
		wait.tv_sec = (now.tv_sec - data->timeConnect.tv_sec);
		if (wait.tv_sec > 0)
		{
			// more than a second has passed, do not wait
			wait.tv_sec = 0;
			wait.tv_nsec = 0;
		}
		else
		{
			// wait remaining settle time
			wait.tv_nsec = (TIME_SETTLE_US * 1000) - (now.tv_nsec - data->timeConnect.tv_nsec);
		}

		ELEKTRA_LOG_DEBUG ("remaining settle time %ld us", wait.tv_nsec / 1000);
	}
	else
	{
		ELEKTRA_LOG_WARNING ("cannot use clock_gettime(); using complete settle time");
		wait.tv_sec = 0;
		wait.tv_nsec = (TIME_SETTLE_US * 1000);
	}
	while (!nanosleep (&wait, &wait) && errno == EINTR)
		;

	// send notification
	if (!elektraZeroMqSendNotification (data->zmqPublisher, changeType, keyName))
	{
		ELEKTRA_LOG_WARNING ("could not send notification");
	}
}

/**
 * @internal
 * Send notification over ZeroMq socket.
 *
 * zmq_send() asynchronous.
 * Processing already handled in a thread created by ZeroMq.
 *
 * @param  socket     ZeroMq socket
 * @param  changeType type of change
 * @param  keyName    name of changed key
 * @retval 1 on success
 * @retval 0 on error
 */
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
