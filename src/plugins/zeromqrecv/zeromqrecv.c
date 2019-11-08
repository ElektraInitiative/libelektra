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


/**
 * @see ElektraIoPluginSetBinding (kdbioplugin.h)
 */
void elektraZeroMqRecvSetIoBinding (Plugin * handle, KeySet * parameters)
{
	ELEKTRA_NOT_NULL (handle);
	ELEKTRA_NOT_NULL (parameters);
	ElektraZeroMqRecvPluginData * data = elektraPluginGetData (handle);
	ELEKTRA_NOT_NULL (data);

	Key * ioBindingKey = ksLookupByName (parameters, "/ioBinding", 0);
	ELEKTRA_NOT_NULL (ioBindingKey);
	ElektraIoInterface * binding = *(ElektraIoInterface **) keyValue (ioBindingKey);

	data->ioBinding = binding;
}

/**
 * @see ElektraNotificationOpenNotification (kdbnotificationinternal.h)
 */
void elektraZeroMqRecvOpenNotification (Plugin * handle, KeySet * parameters)
{
	ELEKTRA_NOT_NULL (handle);
	ElektraZeroMqRecvPluginData * pluginData = elektraPluginGetData (handle);
	ELEKTRA_NOT_NULL (pluginData);

	ElektraNotificationCallback callback;
	Key * callbackKey = ksLookupByName (parameters, "/callback", 0);
	ELEKTRA_NOT_NULL (callbackKey);
	callback = *(ElektraNotificationCallback *) keyValue (callbackKey);

	ElektraNotificationCallbackContext * context;
	Key * contextKey = ksLookupByName (parameters, "/context", 0);
	if (contextKey != NULL)
	{
		context = *(ElektraNotificationCallbackContext **) keyValue (contextKey);
	}
	else
	{
		context = NULL;
	}

	pluginData->notificationCallback = callback;
	pluginData->notificationContext = context;

	// init dbus connections
	if (pluginData->ioBinding)
	{
		elektraZeroMqRecvSetup (pluginData);
	}
	else
	{
		ELEKTRA_LOG_DEBUG ("no I/O binding present. plugin in noop mode");
	}
}

/**
 * @see ElektraNotificationCloseNotification (kdbnotificationinternal.h)
 */
void elektraZeroMqRecvCloseNotification (Plugin * handle, KeySet * parameters ELEKTRA_UNUSED)
{
	ElektraZeroMqRecvPluginData * pluginData = elektraPluginGetData (handle);
	pluginData->notificationCallback = NULL;
	pluginData->notificationContext = NULL;

	elektraZeroMqRecvTeardown (pluginData);
}

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
	if (!data)
	{
		data = elektraMalloc (sizeof (*data));
		data->ioBinding = NULL;
		data->zmqContext = NULL;
		data->zmqSubscriber = NULL;
		data->zmqAdapter = NULL;
		data->endpoint = endpoint;
	}
	elektraPluginSetData (handle, data);

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
			keyNew ("system:/elektra/modules/zeromqrecv/exports/setIoBinding", KEY_FUNC, elektraZeroMqRecvSetIoBinding,
				KEY_END),
			keyNew ("system:/elektra/modules/zeromqrecv/exports/openNotification", KEY_FUNC, elektraZeroMqRecvOpenNotification,
				KEY_END),
			keyNew ("system:/elektra/modules/zeromqrecv/exports/closeNotification", KEY_FUNC,
				elektraZeroMqRecvCloseNotification, KEY_END),
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
