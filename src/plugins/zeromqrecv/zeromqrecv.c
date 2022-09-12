/**
 * @file
 *
 * @brief Source for zeromqrecv plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#ifndef HAVE_KDBCONFIG
#include "kdbconfig.h"
#endif

#include "zeromqrecv.h"

#include <kdbhelper.h>
#include <kdblogger.h>

#include <stdio.h>

int elektraZeroMqRecvOpen (Plugin * handle, ElektraKey * errorKey ELEKTRA_UNUSED)
{
	ElektraKey * endpointKey = elektraKeysetLookupByName (elektraPluginGetConfig (handle), "/endpoint", 0);
	const char * endpoint;
	if (endpointKey)
	{
		endpoint = elektraKeyString (endpointKey);
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
		ElektraKeyset * global = elektraPluginGetGlobalKeySet (handle);

		ElektraKey * ioBindingKey = elektraKeysetLookupByName (global, "system:/elektra/io/binding", 0);
		const void * bindingPtr = elektraKeyValue (ioBindingKey);
		ElektraIoInterface * binding = bindingPtr == NULL ? NULL : *(ElektraIoInterface **) elektraKeyValue (ioBindingKey);

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

int elektraZeroMqRecvGet (Plugin * handle ELEKTRA_UNUSED, ElektraKeyset * returned, ElektraKey * parentKey)
{
	if (!strcmp (elektraKeyName (parentKey), "system:/elektra/modules/zeromqrecv"))
	{
		ElektraKeyset * contract = elektraKeysetNew (
			30, elektraKeyNew ("system:/elektra/modules/zeromqrecv", ELEKTRA_KEY_VALUE, "zeromqrecv plugin waits for your orders", ELEKTRA_KEY_END),
			elektraKeyNew ("system:/elektra/modules/zeromqrecv/exports", ELEKTRA_KEY_END),
			elektraKeyNew ("system:/elektra/modules/zeromqrecv/exports/open", ELEKTRA_KEY_FUNC, elektraZeroMqRecvOpen, ELEKTRA_KEY_END),
			elektraKeyNew ("system:/elektra/modules/zeromqrecv/exports/get", ELEKTRA_KEY_FUNC, elektraZeroMqRecvGet, ELEKTRA_KEY_END),
			elektraKeyNew ("system:/elektra/modules/zeromqrecv/exports/close", ELEKTRA_KEY_FUNC, elektraZeroMqRecvClose, ELEKTRA_KEY_END),
#include ELEKTRA_README
			elektraKeyNew ("system:/elektra/modules/zeromqrecv/infos/version", ELEKTRA_KEY_VALUE, PLUGINVERSION, ELEKTRA_KEY_END), ELEKTRA_KS_END);
		elektraKeysetAppend (returned, contract);
		elektraKeysetDel (contract);

		return 1; /* success */
	}

	return 1; /* success */
}

int elektraZeroMqRecvClose (Plugin * handle, ElektraKey * parentKey ELEKTRA_UNUSED)
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
