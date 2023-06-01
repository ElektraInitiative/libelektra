/**
 * @file
 *
 * @brief Source for zeromqsend plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#ifndef HAVE_KDBCONFIG
#include <internal/config.h>
#endif

#include <elektra/core/errors.h>

#include "./zeromqsend.h"

#include <errno.h> // errno
#include <internal/utility/alloc.h>
#include <internal/utility/logger.h>
#include <stdlib.h> // strtol()

static long convertUnsignedLong (const char * string, long defaultValue)
{
	char * end;
	errno = 0;
	long value = strtol (string, &end, 10);
	if (*end == 0 && errno == 0)
	{
		return value;
	}
	else
	{
		return defaultValue;
	}
}

int elektraZeroMqSendOpen (Plugin * handle, Key * errorKey ELEKTRA_UNUSED)
{
	// read endpoint from configuration
	Key * endpointKey = ksLookupByName (elektraPluginGetConfig (handle), "/endpoint", 0);
	const char * endpoint = ELEKTRA_ZEROMQ_DEFAULT_PUB_ENDPOINT;
	if (endpointKey)
	{
		endpoint = keyString (endpointKey);
	}

	// read timeout for connections from plugin configuration
	Key * connectTimeoutKey = ksLookupByName (elektraPluginGetConfig (handle), "/connectTimeout", 0);
	long connectTimeout = ELEKTRA_ZEROMQ_DEFAULT_CONNECT_TIMEOUT;
	if (connectTimeoutKey)
	{
		connectTimeout = convertUnsignedLong (keyString (connectTimeoutKey), ELEKTRA_ZEROMQ_DEFAULT_CONNECT_TIMEOUT);
	}

	// read timeout for subscriptions from plugin configuration
	Key * subscribeTimeoutKey = ksLookupByName (elektraPluginGetConfig (handle), "/subscribeTimeout", 0);
	long subscribeTimeout = ELEKTRA_ZEROMQ_DEFAULT_SUBSCRIBE_TIMEOUT;
	if (subscribeTimeoutKey)
	{
		subscribeTimeout = convertUnsignedLong (keyString (subscribeTimeoutKey), ELEKTRA_ZEROMQ_DEFAULT_SUBSCRIBE_TIMEOUT);
	}

	ElektraZeroMqSendPluginData * data = elektraPluginGetData (handle);
	if (!data)
	{
		data = elektraMalloc (sizeof (*data));
		data->zmqContext = NULL;
		data->zmqPublisher = NULL;
		data->endpoint = endpoint;
		data->connectTimeout = connectTimeout;
		data->subscribeTimeout = subscribeTimeout;
		data->hasSubscriber = 0;
	}
	elektraPluginSetData (handle, data);

	return 1; /* success */
}

int elektraZeroMqSendGet (Plugin * handle ELEKTRA_UNUSED, KeySet * returned, Key * parentKey)
{
	if (!strcmp (keyName (parentKey), "system:/elektra/modules/zeromqsend"))
	{
		KeySet * contract = ksNew (
			32, keyNew ("system:/elektra/modules/zeromqsend", KEY_VALUE, "zeromqsend plugin waits for your orders", KEY_END),
			keyNew ("system:/elektra/modules/zeromqsend/exports", KEY_END),
			keyNew ("system:/elektra/modules/zeromqsend/exports/open", KEY_FUNC, elektraZeroMqSendOpen, KEY_END),
			keyNew ("system:/elektra/modules/zeromqsend/exports/get", KEY_FUNC, elektraZeroMqSendGet, KEY_END),
			keyNew ("system:/elektra/modules/zeromqsend/exports/commit", KEY_FUNC, elektraZeroMqSendCommit, KEY_END),
			keyNew ("system:/elektra/modules/zeromqsend/exports/hook/notification/send/get", KEY_FUNC, elektraZeroMqSendGet,
				KEY_END),
			keyNew ("system:/elektra/modules/zeromqsend/exports/hook/notification/send/set", KEY_FUNC, elektraZeroMqSendCommit,
				KEY_END),
			keyNew ("system:/elektra/modules/zeromqsend/exports/close", KEY_FUNC, elektraZeroMqSendClose, KEY_END),
#include ELEKTRA_README
			keyNew ("system:/elektra/modules/zeromqsend/infos/version", KEY_VALUE, PLUGINVERSION, KEY_END), KS_END);
		ksAppend (returned, contract);
		ksDel (contract);

		return 1; /* success */
	}

	return 1; /* success */
}

int elektraZeroMqSendCommit (Plugin * handle, KeySet * returned ELEKTRA_UNUSED, Key * parentKey ELEKTRA_UNUSED)
{
	ElektraZeroMqSendPluginData * pluginData = elektraPluginGetData (handle);
	ELEKTRA_NOT_NULL (pluginData);

	int result = elektraZeroMqSendPublish ("Commit", keyName (parentKey), pluginData);
	switch (result)
	{
	case 1:
		// success!
		break;
	case -1:
		// connection timeout - hub not running
		ELEKTRA_ADD_INSTALLATION_WARNING (parentKey, "Could not connect to hub. Please start hub using `kdb run-hub-zeromq`");
		break;
	case -2:
		// subscription timeout - no applications are listening for notifications, can be ignored
		break;
	default:
		ELEKTRA_ADD_PLUGIN_MISBEHAVIOR_WARNING (parentKey, "Could not send notifications");
		break;
	}

	return 1; /* success */
}

int elektraZeroMqSendClose (Plugin * handle, Key * parentKey ELEKTRA_UNUSED)
{
	ElektraZeroMqSendPluginData * pluginData = elektraPluginGetData (handle);
	if (pluginData == NULL)
	{
		return 1;
	}

	if (pluginData->zmqPublisher)
	{
		zmq_close (pluginData->zmqPublisher);
		pluginData->zmqPublisher = NULL;
	}

	if (pluginData->zmqContext)
	{
		zmq_ctx_destroy (pluginData->zmqContext);
		pluginData->zmqContext = NULL;
	}

	elektraFree (pluginData);
	elektraPluginSetData (handle, NULL);

	return 1; /* success */
}

Plugin * ELEKTRA_PLUGIN_EXPORT
{
	// clang-format off
	return elektraPluginExport("zeromqsend",
		ELEKTRA_PLUGIN_OPEN,	&elektraZeroMqSendOpen,
		ELEKTRA_PLUGIN_GET,	&elektraZeroMqSendGet,
		ELEKTRA_PLUGIN_COMMIT,	&elektraZeroMqSendCommit,
		ELEKTRA_PLUGIN_CLOSE,	&elektraZeroMqSendClose,
		ELEKTRA_PLUGIN_END);
}
