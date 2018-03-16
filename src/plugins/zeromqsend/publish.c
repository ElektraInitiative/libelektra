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

/** wait inside loop while waiting for subscribers (10ms)  */
#define ELEKTRA_ZEROMQSEND_LOOPDELAY_NS (10 * 1000 * 1000)

/** timeout for waiting for a subscription: 1 second */
#define ELEKTRA_ZEROMQSEND_SUBSCRIBE_TIMEOUT (1)

/** first byte of a subscription message */
#define ELEKTRA_ZEROMQSEND_SUBSCRIPTION_MESSAGE '\x01'

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
		data->zmqPublisher = zmq_socket (data->zmqContext, ZMQ_XPUB);
		if (data->zmqPublisher == NULL)
		{
			ELEKTRA_LOG_WARNING ("zmq_socket failed %s", zmq_strerror (zmq_errno ()));
			zmq_close (data->zmqPublisher);
			return 0;
		}
		// connect to endpoint
		int result = zmq_connect (data->zmqPublisher, data->endpoint);
		if (result != 0)
		{
			ELEKTRA_LOG_WARNING ("zmq_connect error: %s\n", zmq_strerror (zmq_errno ()));
			zmq_close (data->zmqPublisher);
			data->zmqPublisher = NULL;
			return 0;
		}
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
	if (!elektraZeroMqSendConnect (data))
	{
		ELEKTRA_LOG_WARNING ("could not connect to endpoint");
		return;
	}

	// wait for subscription message
	if (!data->hasSubscriber)
	{
		// NOTE zmq_connect() returns before a connection is established since
		// ZeroMq asynchronously does that in the background.
		// All notifications sent before the connection is established and and the
		// socket has a subscriber are lost since ZMQ_(X)PUB sockets handle message
		// filtering: Without subscribers all messages are discarded.
		// Therefore we use a ZMQ_XPUB socket which allows us to wait for a
		// subscription message.
		time_t start = time (NULL);
		struct timespec wait;
		zmq_msg_t message;
		zmq_msg_init (&message);
		int lastErrno = 0;
		do
		{
			wait.tv_sec = 0;
			wait.tv_nsec = ELEKTRA_ZEROMQSEND_LOOPDELAY_NS;
			while (!nanosleep (&wait, &wait) && errno == EINTR)
				;

			lastErrno = 0;
			int result = zmq_msg_recv (&message, data->zmqPublisher, ZMQ_DONTWAIT);
			if (result == -1)
			{
				lastErrno = zmq_errno ();
			}

			if (time (NULL) - start > ELEKTRA_ZEROMQSEND_SUBSCRIBE_TIMEOUT)
			{
				ELEKTRA_LOG_WARNING ("subscribing timed out. could not publish notification");
				zmq_msg_close (&message);
				return;
			}

			if (result == -1)
			{
				if (lastErrno != EAGAIN)
				{
					ELEKTRA_LOG_WARNING ("sending failed %s", zmq_strerror (lastErrno));
					zmq_msg_close (&message);
					return;
				}
			}
			else
			{
				// we have received a message subscription or unsubscription message
				char * messageData = zmq_msg_data (&message);
				if (messageData[0] == ELEKTRA_ZEROMQSEND_SUBSCRIPTION_MESSAGE)
				{
					data->hasSubscriber = 1;
				}
			}
		} while (lastErrno == EAGAIN && !data->hasSubscriber);

		zmq_msg_close (&message);
	}

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
