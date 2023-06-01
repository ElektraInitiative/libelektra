/**
 * @file
 *
 * @brief Source for zeromqrecv plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#ifndef HAVE_KDBCONFIG
#include <internal/config.h>
#endif

#include "./zeromqrecv.h"

#include <internal/utility/alloc.h>
#include <internal/utility/logger.h>
#include <stdio.h>

int elektraZeroMqRecvOpen (Plugin * handle, Key * errorKey ELEKTRA_UNUSED)
{
	Key * endpointKey = ksLookupByName (elektraPluginGetConfig (handle), "/endpoint", 0);
	const char * endpoint;
	if (endpointKey)
	{
		endpoint = keyString (endpointKey);
	}
	else
	{
		endpoint = ELEKTRA_ZEROMQ_DEFAULT_SUB_ENDPOINT;
	}

	ElektraZeroMqRecvPluginData * data = elektraPluginGetData (handle);
	if (data == NULL)
	{
		data = elektraMalloc (sizeof (*data));
		data->ioBinding = NULL;
		data->zmqContext = NULL;
		data->zmqSubscriber = NULL;
		data->zmqAdapter = NULL;
		data->endpoint = endpoint;
		data->notificationCallback = NULL;
		data->notificationContext = NULL;
		elektraPluginSetData (handle, data);
	}

	if (data->ioBinding == NULL)
	{
		KeySet * global = elektraPluginGetGlobalKeySet (handle);

		Key * ioBindingKey = ksLookupByName (global, "system:/elektra/io/binding", 0);
		const void * bindingPtr = keyValue (ioBindingKey);
		ElektraIoInterface * binding = bindingPtr == NULL ? NULL : *(ElektraIoInterface **) keyValue (ioBindingKey);

		data->ioBinding = binding;
	}

	// init zeromq connections
	if (data->ioBinding != NULL)
	{
		elektraZeroMqRecvSetup (handle);
	}
	else
	{
		ELEKTRA_LOG_DEBUG ("no I/O binding present. plugin in noop mode");
	}

	return 1; /* success */
}

int elektraZeroMqRecvGet (Plugin * handle ELEKTRA_UNUSED, KeySet * returned, Key * parentKey)
{
	if (!strcmp (keyName (parentKey), "system:/elektra/modules/zeromqrecv"))
	{
		KeySet * contract = ksNew (
			30, keyNew ("system:/elektra/modules/zeromqrecv", KEY_VALUE, "zeromqrecv plugin waits for your orders", KEY_END),
			keyNew ("system:/elektra/modules/zeromqrecv/exports", KEY_END),
			keyNew ("system:/elektra/modules/zeromqrecv/exports/open", KEY_FUNC, elektraZeroMqRecvOpen, KEY_END),
			keyNew ("system:/elektra/modules/zeromqrecv/exports/get", KEY_FUNC, elektraZeroMqRecvGet, KEY_END),
			keyNew ("system:/elektra/modules/zeromqrecv/exports/close", KEY_FUNC, elektraZeroMqRecvClose, KEY_END),
#include ELEKTRA_README
			keyNew ("system:/elektra/modules/zeromqrecv/infos/version", KEY_VALUE, PLUGINVERSION, KEY_END), KS_END);
		ksAppend (returned, contract);
		ksDel (contract);

		return 1; /* success */
	}

	return 1; /* success */
}

int elektraZeroMqRecvClose (Plugin * handle, Key * parentKey ELEKTRA_UNUSED)
{
	elektraZeroMqRecvTeardown (handle);

	ElektraZeroMqRecvPluginData * pluginData = elektraPluginGetData (handle);
	if (pluginData == NULL)
	{
		return 1;
	}

	elektraFree (pluginData);
	elektraPluginSetData (handle, NULL);

	return 1; /* success */
}

Plugin * ELEKTRA_PLUGIN_EXPORT
{
	// clang-format off
	return elektraPluginExport("zeromqrecv",
		ELEKTRA_PLUGIN_OPEN,	&elektraZeroMqRecvOpen,
		ELEKTRA_PLUGIN_GET,	&elektraZeroMqRecvGet,
		ELEKTRA_PLUGIN_CLOSE,	&elektraZeroMqRecvClose,
		ELEKTRA_PLUGIN_END);
}
