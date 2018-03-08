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
#define TIME_SETTLE_MS (300)

/** settle time in microseconds */
#define TIME_SETTLE_US (TIME_SETTLE_MS * 1000)


static void zeroMqSendSocketWritable (void * socket, void * context)
{
	ElektraZeroMqSendPluginData * data = context;

	// process next notification
	ElektraZeroMqSendQueuedNotification * notification = data->head;
	if (notification != NULL)
	{
		// remove notification from list
		data->head = notification->next;

		if (!elektraZeroMqSendNotification (socket, notification->changeType, notification->keyName))
		{
			ELEKTRA_LOG_WARNING ("could not send notification");
		}

		elektraFree (notification->changeType);
		elektraFree (notification->keyName);
		elektraFree (notification);
	}

	if (data->head == NULL)
	{
		// disable adapter until next notifications are sent
		elektraIoZeroMqAdapterSetEnabled (data->zmqAdapter, 0);
	}
}

static void zeroMqSettleTimeout (ElektraIoTimerOperation * timer)
{
	ElektraIoAdapterZeroMqHandle * zmqAdapter = elektraIoTimerGetData (timer);
	ELEKTRA_NOT_NULL (zmqAdapter);

	// enable adapter
	elektraIoZeroMqAdapterSetEnabled (zmqAdapter, 1);

	elektraIoBindingRemoveTimer (timer);
	elektraFree (timer);
}

/**
 * Append notification to the end of the list.
 * @internal
 *
 * @param pluginData   internal plugin data
 * @param notification notification ot append
 */
static void zeroMqQueueNotification (ElektraZeroMqSendPluginData * pluginData, ElektraZeroMqSendQueuedNotification * notification)
{
	notification->next = NULL;

	if (pluginData->head == NULL)
	{
		// Initialize list
		pluginData->head = pluginData->last = notification;
	}
	else
	{
		// Make new notification end of list
		pluginData->last->next = notification;
		pluginData->last = notification;
	}
}

void elektraZeroMqSendPublish (const char * changeType, const char * keyName, ElektraZeroMqSendPluginData * data)
{
	/* NOTE
	 * - context and socket are created
	 * - socket connects to endpoint
	 *   - if I/O binding is not present
	 *     - wait until connection is available (using usleep) otherwise messages are lost when writing immediately
	 *
	 * if I/O binding is present
	 *   - if necessary create zeromq adapter which calls zeroMqSendSocketWritable when the socket is writable
	 *     - wait until connection is available (using ElektraIoTimerOperation) otherwise socket would become writable immediately and
	 *       messages are lost
	 *     - the timer enables the adapter
	 *     - which sends the notification and disables the adapter
	 *   - else (adapter is already created)
	 *     - the connection is already established
	 *     - we enable the adapter
	 *     - which sends the notification and disables the adapter
	 * else (I/O binding is not present)
	 *   - send the notification
	 */
	int haveIoBinding = data->ioBinding != NULL;

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

		if (!haveIoBinding)
		{
			// wait for connection to be available
			usleep (TIME_SETTLE_US);
		}
	}

	if (haveIoBinding)
	{
		ElektraZeroMqSendQueuedNotification * notification = elektraMalloc (sizeof (*notification));
		if (!notification)
		{
			ELEKTRA_LOG_WARNING ("malloc failed");
			return;
		}
		notification->changeType = elektraStrDup (changeType);
		notification->keyName = elektraStrDup (keyName);
		zeroMqQueueNotification (data, notification);

		if (!data->zmqAdapter)
		{
			// attach ZeroMq adater and wait for socket to be writable
			data->zmqAdapter = elektraIoAdapterZeroMqAttach (data->zmqPublisher, data->ioBinding,
									 ELEKTRA_IO_ADAPTER_ZEROMQCB_WRITE, zeroMqSendSocketWritable, data);
			if (!data->zmqAdapter)
			{
				ELEKTRA_LOG_WARNING ("could not attach zmq adapter");
				return;
			}
			// disable adapter until connection is available
			elektraIoZeroMqAdapterSetEnabled (data->zmqAdapter, 0);

			// asynchronously wait for connection to be available
			data->settleTimer = elektraIoNewTimerOperation (TIME_SETTLE_MS, 1, zeroMqSettleTimeout, data->zmqAdapter);
			elektraIoBindingAddTimer (data->ioBinding, data->settleTimer);
		}
		else
		{
			elektraIoZeroMqAdapterSetEnabled (data->zmqAdapter, 1);
		}
	}
	else
	{
		// send notification
		if (!elektraZeroMqSendNotification (data->zmqPublisher, changeType, keyName))
		{
			ELEKTRA_LOG_WARNING ("could not send notification");
		}
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
