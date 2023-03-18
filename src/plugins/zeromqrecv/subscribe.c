/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include "zeromqrecv.h"

#include <internal/utility/logger.h>
#include <internal/utility/old_helper.h>

static int setupNotificationCallback (Plugin * handle)
{
	ELEKTRA_NOT_NULL (handle);
	ElektraZeroMqRecvPluginData * pluginData = elektraPluginGetData (handle);
	ELEKTRA_NOT_NULL (pluginData);

	KeySet * global = elektraPluginGetGlobalKeySet (handle);

	ElektraNotificationCallback callback;
	Key * callbackKey = ksLookupByName (global, "system:/elektra/notification/callback", 0);
	const void * callbackPtr = keyValue (callbackKey);

	if (callbackPtr == NULL)
	{
		return -1;
	}

	callback = *(ElektraNotificationCallback *) keyValue (callbackKey);

	ElektraNotificationCallbackContext * context;
	Key * contextKey = ksLookupByName (global, "system:/elektra/notification/context", 0);
	const void * contextPtr = keyValue (contextKey);
	context = contextPtr == NULL ? NULL : *(ElektraNotificationCallbackContext **) contextPtr;


	pluginData->notificationCallback = callback;
	pluginData->notificationContext = context;

	return 0;
}

/**
 * @internal
 * Called whenever the socket becomes readable.
 * ZeroMq since sends multipart messages atomically (all or nothing)
 * both message parts are instantly available.
 *
 * @param socket  ZeroMq socket
 * @param context context passed to elektraIoAdapterZeroMqAttach()
 */
static void zeroMqRecvSocketReadable (void * socket, void * context)
{
	Plugin * handle = (Plugin *) context;
	ELEKTRA_NOT_NULL (handle);
	ElektraZeroMqRecvPluginData * data = elektraPluginGetData (handle);
	ELEKTRA_NOT_NULL (data);

	if (data->notificationCallback == NULL)
	{
		if (setupNotificationCallback (handle) != 0)
		{
			ELEKTRA_LOG_WARNING ("notificationCallback not set up; aborting");
			return;
		}
	}

	char * changeType;
	char * changedKeyName;

	zmq_msg_t message;
	zmq_msg_init (&message);

	int result = zmq_msg_recv (&message, socket, ZMQ_DONTWAIT);
	if (result == -1)
	{
		ELEKTRA_LOG_WARNING ("receiving change type failed: %s; aborting", zmq_strerror (zmq_errno ()));
		zmq_msg_close (&message);
		return;
	}
	if (!zmq_msg_more (&message))
	{
		ELEKTRA_LOG_WARNING ("message has only one part; aborting");
		zmq_msg_close (&message);
		return;
	}
	int length = zmq_msg_size (&message);
	changeType = elektraMemDup (zmq_msg_data (&message), length + 1);
	changeType[length] = '\0';
	ELEKTRA_LOG_DEBUG ("received change type %s", changeType);

	result = zmq_msg_recv (&message, socket, ZMQ_DONTWAIT);
	if (result == -1)
	{
		ELEKTRA_LOG_WARNING ("receiving key name failed: %s; aborting", zmq_strerror (zmq_errno ()));
		elektraFree (changeType);
		zmq_msg_close (&message);
		return;
	}
	length = zmq_msg_size (&message);
	changedKeyName = elektraMemDup (zmq_msg_data (&message), length + 1);
	changedKeyName[length] = '\0';
	ELEKTRA_LOG_DEBUG ("received key name %s", changedKeyName);

	// notify about changes
	Key * changedKey = keyNew (changedKeyName, KEY_END);
	data->notificationCallback (changedKey, data->notificationContext);

	zmq_msg_close (&message);
	elektraFree (changeType);
	elektraFree (changedKeyName);
}

/**
 * @internal
 * Setup ZeroMq for receiving notifications.
 *
 * @param handle plugin handle
 */
void elektraZeroMqRecvSetup (Plugin * handle)
{
	ElektraZeroMqRecvPluginData * data = elektraPluginGetData (handle);
	ELEKTRA_NOT_NULL (data);

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
	if (!data->zmqSubscriber)
	{
		data->zmqSubscriber = zmq_socket (data->zmqContext, ZMQ_SUB);
		if (data->zmqSubscriber == NULL)
		{
			ELEKTRA_LOG_WARNING ("zmq_socket failed %s", zmq_strerror (zmq_errno ()));
			zmq_close (data->zmqSubscriber);
			return;
		}

		// subscribe to notifications
		char * keyCommitType = "Commit";
		if (zmq_setsockopt (data->zmqSubscriber, ZMQ_SUBSCRIBE, keyCommitType, elektraStrLen (keyCommitType)) != 0)
		{
			ELEKTRA_LOG_WARNING ("failed to subscribe to %s messages", keyCommitType);
		}

		// connect to endpoint
		int result = zmq_connect (data->zmqSubscriber, data->endpoint);
		if (result != 0)
		{
			ELEKTRA_LOG_WARNING ("zmq_connect error: %s\n", zmq_strerror (zmq_errno ()));
			zmq_close (data->zmqSubscriber);
			data->zmqSubscriber = NULL;
			return;
		}
	}

	if (!data->zmqAdapter)
	{
		// attach ZeroMq adater and wait for socket to be writable
		data->zmqAdapter = elektraIoAdapterZeroMqAttach (data->zmqSubscriber, data->ioBinding, ELEKTRA_IO_ADAPTER_ZEROMQCB_READ,
								 zeroMqRecvSocketReadable, handle);
		if (!data->zmqAdapter)
		{
			ELEKTRA_LOG_WARNING ("could not attach zmq adapter");
			zmq_close (data->zmqSubscriber);
			data->zmqSubscriber = NULL;
			return;
		}
	}
}

/**
 * @internal
 * Cleanup ZeroMq.
 *
 * @param handle plugin handle
 */
void elektraZeroMqRecvTeardown (Plugin * handle)
{
	ElektraZeroMqRecvPluginData * data = elektraPluginGetData (handle);
	ELEKTRA_NOT_NULL (data);

	if (data->zmqAdapter)
	{
		elektraIoAdapterZeroMqDetach (data->zmqAdapter);
		data->zmqAdapter = NULL;
	}

	if (data->zmqSubscriber)
	{
		zmq_close (data->zmqSubscriber);
		data->zmqSubscriber = NULL;
	}

	if (data->zmqContext)
	{
		zmq_ctx_destroy (data->zmqContext);
		data->zmqContext = NULL;
	}
}
