/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include "zeromqsend.h"

#include <internal/utility/old_helper.h>
#include <internal/utility/logger.h>

#include <time.h>   // clock_gettime()
#include <unistd.h> // usleep()

/** wait inside loop while waiting for messages (10ms)  */
#define ELEKTRA_ZEROMQSEND_LOOPDELAY_NS (10 * 1000 * 1000)

/** first byte of a subscription message */
#define ELEKTRA_ZEROMQSEND_SUBSCRIPTION_MESSAGE '\x01'

#define ELEKTRA_ZEROMQSEND_MONITOR_ENDPOINT "inproc://zmqpublish-monitor"

/**
 * Receive and return events from a ZeroMQ monitor socket.
 *
 * @param  monitor monitor socket
 * @return         ZMQ_EVENT_ number
 * @retval  0   if receiving interrupted or no message available
 * @retval -1   on invalid message
 */
static int getMonitorEvent (void * monitor)
{
	// First frame in message contains event number and value
	zmq_msg_t msg;
	zmq_msg_init (&msg);
	if (zmq_msg_recv (&msg, monitor, ZMQ_DONTWAIT) == -1)
	{
		// presumably interrupted or no message available
		return 0;
	}
	if (!zmq_msg_more (&msg))
	{
		ELEKTRA_LOG_WARNING ("Invalid monitor message received!");
		return -1;
	}

	uint8_t * data = (uint8_t *) zmq_msg_data (&msg);
	uint16_t event = *(uint16_t *) (data);

	// Second frame in message contains event address
	// We receive it to clear the buffer, since we are only
	// interested in the event number
	zmq_msg_init (&msg);
	if (zmq_msg_recv (&msg, monitor, ZMQ_DONTWAIT) == -1)
	{
		// presumably interrupted
		return 0;
	}
	if (zmq_msg_more (&msg))
	{
		ELEKTRA_LOG_WARNING ("Invalid monitor message received!");
		return -1;
	}

	return event;
}

static struct timespec ts_diff (struct timespec now, struct timespec start)
{
	struct timespec diff;
	if ((now.tv_nsec - start.tv_nsec) < 0)
	{
		diff.tv_sec = now.tv_sec - start.tv_sec - 1;
		diff.tv_nsec = 1000000000 + now.tv_nsec - start.tv_nsec;
	}
	else
	{
		diff.tv_sec = now.tv_sec - start.tv_sec;
		diff.tv_nsec = now.tv_nsec - start.tv_nsec;
	}
	return diff;
}

/**
 * Wait for connection message from ZeroMQ monitor socket.
 *
 * @param  monitorSocket socket
 * @retval  1 when connected
 * @retval -1 on timeout
 * @retval  0 on other errors
 */
static int waitForConnection (void * monitorSocket, long connectTimeout)
{
	struct timespec wait;
	struct timespec start;
	struct timespec now;
	struct timespec diff;
	time_t startFallback = -1;
	long timeoutSec = (connectTimeout / (1000));
	long timeoutNsec = (connectTimeout % (1000)) * (1000 * 1000);
	if (clock_gettime (CLOCK_MONOTONIC, &start) == -1)
	{
		ELEKTRA_LOG_WARNING ("Using slower fallback for timeout detection");
		startFallback = time (NULL);
		// minimum timeout is 1 second when using the fallback
		if (timeoutSec == 0)
		{
			timeoutSec = 1;
		}
	}

	// wait for connection established event
	int connected = 0;
	while (!connected)
	{
		wait.tv_sec = 0;
		wait.tv_nsec = ELEKTRA_ZEROMQSEND_LOOPDELAY_NS;
		while (!nanosleep (&wait, &wait) && errno == EINTR)
			;

		int event = getMonitorEvent (monitorSocket);

		int timeout = 0;
		if (startFallback == -1)
		{
			clock_gettime (CLOCK_MONOTONIC, &now);
			diff = ts_diff (now, start);
			timeout = diff.tv_sec >= timeoutSec && diff.tv_nsec >= timeoutNsec;
		}
		else
		{
			timeout = time (NULL) - startFallback >= timeoutSec;
		}
		if (timeout)
		{
			ELEKTRA_LOG_WARNING ("connection timed out. could not publish notification");
			zmq_close (monitorSocket);
			return -1;
		}

		switch (event)
		{
		case ZMQ_EVENT_CONNECTED:
			// we do not need the publisher monitor anymore
			zmq_close (monitorSocket);
			connected = 1;
			break;
		case -1:
			// abort, inconsistencies detected
			ELEKTRA_LOG_WARNING ("Cannot monitor connection events");
			return 0;
			break;
		case 0:
			// no message available or interrupted, try again
			break;
		default:
			// other ZMQ event, ignore
			break;
		}
	}
	return 1;
}

/**
 * Wait for first subscription message on ZeroMQ socket.
 *
 * @param  socket socket
 * @retval  1 on success
 * @retval -2 on timeout
 * @retval  0 on other errors
 */
static int waitForSubscription (void * socket, long subscribeTimeout)
{
	struct timespec start;
	struct timespec now;
	struct timespec wait;
	struct timespec diff;
	time_t startFallback = -1;
	long timeoutSec = (subscribeTimeout / (1000));
	long timeoutNsec = (subscribeTimeout % (1000)) * (1000 * 1000);
	if (clock_gettime (CLOCK_MONOTONIC, &start) == -1)
	{
		ELEKTRA_LOG_WARNING ("Using slower fallback for timeout detection");
		startFallback = time (NULL);
		// minimum timeout is 1 second when using the fallback
		if (timeoutSec == 0)
		{
			timeoutSec = 1;
		}
	}

	// wait until we receive the first subscription message
	zmq_msg_t message;
	zmq_msg_init (&message);
	int hasSubscriber = 0;
	int lastErrno = 0;
	do
	{
		wait.tv_sec = 0;
		wait.tv_nsec = ELEKTRA_ZEROMQSEND_LOOPDELAY_NS;
		while (!nanosleep (&wait, &wait) && errno == EINTR)
			;

		lastErrno = 0;
		int result = zmq_msg_recv (&message, socket, ZMQ_DONTWAIT);
		if (result == -1)
		{
			lastErrno = zmq_errno ();
		}

		int timeout = 0;
		if (startFallback == -1)
		{
			clock_gettime (CLOCK_MONOTONIC, &now);
			diff = ts_diff (now, start);
			timeout = diff.tv_sec >= timeoutSec && diff.tv_nsec >= timeoutNsec;
		}
		else
		{
			timeout = time (NULL) - startFallback >= timeoutSec;
		}
		if (timeout)
		{
			ELEKTRA_LOG_WARNING ("subscribing timed out. could not publish notification");
			zmq_msg_close (&message);
			return -2;
		}

		if (result == -1)
		{
			if (lastErrno != EAGAIN)
			{
				ELEKTRA_LOG_WARNING ("receiving failed %s", zmq_strerror (lastErrno));
				zmq_msg_close (&message);
				return 0;
			}
		}
		else
		{
			// we have received a message subscription or unsubscription message
			char * messageData = zmq_msg_data (&message);
			if (messageData[0] == ELEKTRA_ZEROMQSEND_SUBSCRIPTION_MESSAGE)
			{
				hasSubscriber = 1;
			}
		}
	} while (lastErrno == EAGAIN && !hasSubscriber);

	zmq_msg_close (&message);

	return 1;
}

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

	if (!data->zmqPublisher)
	{
		// create publish socket
		data->zmqPublisher = zmq_socket (data->zmqContext, ZMQ_XPUB);
		if (data->zmqPublisher == NULL)
		{
			ELEKTRA_LOG_WARNING ("zmq_socket failed %s", zmq_strerror (zmq_errno ()));
			zmq_close (data->zmqPublisher);
			return 0;
		}

		// setup socket monitor
		if (zmq_socket_monitor (data->zmqPublisher, ELEKTRA_ZEROMQSEND_MONITOR_ENDPOINT, ZMQ_EVENT_CONNECTED) == -1)
		{
			ELEKTRA_LOG_WARNING ("creating socket monitor failed: %s", zmq_strerror (zmq_errno ()));
			return 0;
		}
		data->zmqPublisherMonitor = zmq_socket (data->zmqContext, ZMQ_PAIR);
		if (zmq_connect (data->zmqPublisherMonitor, ELEKTRA_ZEROMQSEND_MONITOR_ENDPOINT) != 0)
		{
			ELEKTRA_LOG_WARNING ("connecting to socket monitor failed: %s", zmq_strerror (zmq_errno ()));
			return 0;
		}

		// connect to endpoint
		int result = zmq_connect (data->zmqPublisher, data->endpoint);
		if (result != 0)
		{
			ELEKTRA_LOG_WARNING ("zmq_connect error: %s", zmq_strerror (zmq_errno ()));
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
 * @retval 1 on success
 * @retval -1 on connection timeout
 * @retval -2 on subscription timeout
 * @retval 0 on other errors
 */
int elektraZeroMqSendPublish (const char * changeType, const char * keyName, ElektraZeroMqSendPluginData * data)
{
	if (!elektraZeroMqSendConnect (data))
	{
		ELEKTRA_LOG_WARNING ("could not connect to endpoint");
		return 0;
	}

	// wait for subscription message
	if (!data->hasSubscriber)
	{
		// NOTE zmq_connect() returns before a connection is established since
		// ZeroMq asynchronously does that in the background.
		// All notifications sent before the connection is established and and the
		// socket has a subscriber are lost since ZMQ_(X)PUB sockets handle message
		// filtering: Without subscribers all messages are discarded.
		// Therefore we monitor the socket for until the connection is established
		// and then wait for the first subscription message.
		// A ZMQ_XPUB socket instead of a ZMQ_PUB socket allows us to receive
		// subscription messages
		int result = waitForConnection (data->zmqPublisherMonitor, data->connectTimeout);
		if (result != 1)
		{
			return result;
		}
		result = waitForSubscription (data->zmqPublisher, data->subscribeTimeout);
		if (result == 1)
		{
			data->hasSubscriber = 1;
		}
		else
		{
			return result;
		}
	}

	// send notification
	if (!elektraZeroMqSendNotification (data->zmqPublisher, changeType, keyName))
	{
		ELEKTRA_LOG_WARNING ("could not send notification");
		return 0;
	}

	return 1;
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
