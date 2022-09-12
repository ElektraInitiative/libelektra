/**
 * @file
 *
 * @brief Source for zeromqsend plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#ifndef HAVE_KDBCONFIG
#include "kdbconfig.h"
#endif

#include <kdberrors.h>

#include "zeromqsend.h"

#include <kdbhelper.h>
#include <kdblogger.h>

#include <errno.h>  // errno
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

int elektraZeroMqSendOpen (Plugin * handle, ElektraKey * errorKey ELEKTRA_UNUSED)
{
	// read endpoint from configuration
	ElektraKey * endpointKey = elektraKeysetLookupByName (elektraPluginGetConfig (handle), "/endpoint", 0);
	const char * endpoint = ELEKTRA_ZEROMQ_DEFAULT_PUB_ENDPOINT;
	if (endpointKey)
	{
		endpoint = elektraKeyString (endpointKey);
	}

	// read timeout for connections from plugin configuration
	ElektraKey * connectTimeoutKey = elektraKeysetLookupByName (elektraPluginGetConfig (handle), "/connectTimeout", 0);
	long connectTimeout = ELEKTRA_ZEROMQ_DEFAULT_CONNECT_TIMEOUT;
	if (connectTimeoutKey)
	{
		connectTimeout = convertUnsignedLong (elektraKeyString (connectTimeoutKey), ELEKTRA_ZEROMQ_DEFAULT_CONNECT_TIMEOUT);
	}

	// read timeout for subscriptions from plugin configuration
	ElektraKey * subscribeTimeoutKey = elektraKeysetLookupByName (elektraPluginGetConfig (handle), "/subscribeTimeout", 0);
	long subscribeTimeout = ELEKTRA_ZEROMQ_DEFAULT_SUBSCRIBE_TIMEOUT;
	if (subscribeTimeoutKey)
	{
		subscribeTimeout = convertUnsignedLong (elektraKeyString (subscribeTimeoutKey), ELEKTRA_ZEROMQ_DEFAULT_SUBSCRIBE_TIMEOUT);
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

int elektraZeroMqSendGet (Plugin * handle ELEKTRA_UNUSED, ElektraKeyset * returned, ElektraKey * parentKey)
{
	if (!strcmp (elektraKeyName (parentKey), "system:/elektra/modules/zeromqsend"))
	{
		ElektraKeyset * contract = elektraKeysetNew (
			30, elektraKeyNew ("system:/elektra/modules/zeromqsend", ELEKTRA_KEY_VALUE, "zeromqsend plugin waits for your orders", ELEKTRA_KEY_END),
			elektraKeyNew ("system:/elektra/modules/zeromqsend/exports", ELEKTRA_KEY_END),
			elektraKeyNew ("system:/elektra/modules/zeromqsend/exports/open", ELEKTRA_KEY_FUNC, elektraZeroMqSendOpen, ELEKTRA_KEY_END),
			elektraKeyNew ("system:/elektra/modules/zeromqsend/exports/get", ELEKTRA_KEY_FUNC, elektraZeroMqSendGet, ELEKTRA_KEY_END),
			elektraKeyNew ("system:/elektra/modules/zeromqsend/exports/set", ELEKTRA_KEY_FUNC, elektraZeroMqSendSet, ELEKTRA_KEY_END),
			elektraKeyNew ("system:/elektra/modules/zeromqsend/exports/close", ELEKTRA_KEY_FUNC, elektraZeroMqSendClose, ELEKTRA_KEY_END),
#include ELEKTRA_README
			elektraKeyNew ("system:/elektra/modules/zeromqsend/infos/version", ELEKTRA_KEY_VALUE, PLUGINVERSION, ELEKTRA_KEY_END), ELEKTRA_KS_END);
		elektraKeysetAppend (returned, contract);
		elektraKeysetDel (contract);

		return 1; /* success */
	}

	return 1; /* success */
}

int elektraZeroMqSendSet (Plugin * handle, ElektraKeyset * returned ELEKTRA_UNUSED, ElektraKey * parentKey ELEKTRA_UNUSED)
{
	ElektraZeroMqSendPluginData * pluginData = elektraPluginGetData (handle);
	ELEKTRA_NOT_NULL (pluginData);

	int result = elektraZeroMqSendPublish ("Commit", elektraKeyName (parentKey), pluginData);
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

int elektraZeroMqSendClose (Plugin * handle, ElektraKey * parentKey ELEKTRA_UNUSED)
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
		ELEKTRA_PLUGIN_SET,	&elektraZeroMqSendSet,
		ELEKTRA_PLUGIN_CLOSE,	&elektraZeroMqSendClose,
		ELEKTRA_PLUGIN_END);
}
