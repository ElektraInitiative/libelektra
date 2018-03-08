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

#include "zeromqsend.h"

#include <kdbhelper.h>
#include <kdblogger.h>

#include <stdio.h>


/**
 * @see ElektraIoPluginSetBinding (kdbioplugin.h)
 */
void elektraZeroMqSendSetIoBinding (Plugin * handle, ElektraIoInterface * binding)
{
	ELEKTRA_NOT_NULL (handle);
	ElektraZeroMqSendPluginData * data = elektraPluginGetData (handle);
	ELEKTRA_NOT_NULL (data);

	data->ioBinding = binding;
}

/**
 * @internal
 * Announce multiple keys with same signal name.
 *
 * @param ks         key set containing modified keys
 * @param changeType signal name to use
 * @param data       plugin data containing connections, etc.
 */
static void announceKeys (KeySet * ks, const char * changeType, ElektraZeroMqSendPluginData * data)
{
	ELEKTRA_NOT_NULL (ks);
	ELEKTRA_NOT_NULL (changeType);

	ksRewind (ks);
	Key * k = 0;
	while ((k = ksNext (ks)) != 0)
	{
		elektraZeroMqSendPublish (changeType, keyName (k), data);
	}
}


int elektraZeroMqSendOpen (Plugin * handle, Key * errorKey ELEKTRA_UNUSED)
{
	ElektraZeroMqSendPluginData * data = elektraPluginGetData (handle);
	if (!data)
	{
		data = elektraMalloc (sizeof (*data));
		data->keys = NULL;
		data->ioBinding = NULL;
		data->zmqContext = NULL;
		data->zmqPublisher = NULL;
		data->zmqAdapter = NULL;
		data->settleTimer = NULL;
		data->head = NULL;
		data->last = NULL;
	}
	elektraPluginSetData (handle, data);

	return 1; /* success */
}

int elektraZeroMqSendGet (Plugin * handle ELEKTRA_UNUSED, KeySet * returned, Key * parentKey)
{
	if (!strcmp (keyName (parentKey), "system/elektra/modules/zeromqsend"))
	{
		KeySet * contract = ksNew (
			30, keyNew ("system/elektra/modules/zeromqsend", KEY_VALUE, "zeromqsend plugin waits for your orders", KEY_END),
			keyNew ("system/elektra/modules/zeromqsend/exports", KEY_END),
			keyNew ("system/elektra/modules/zeromqsend/exports/open", KEY_FUNC, elektraZeroMqSendOpen, KEY_END),
			keyNew ("system/elektra/modules/zeromqsend/exports/get", KEY_FUNC, elektraZeroMqSendGet, KEY_END),
			keyNew ("system/elektra/modules/zeromqsend/exports/set", KEY_FUNC, elektraZeroMqSendSet, KEY_END),
			keyNew ("system/elektra/modules/zeromqsend/exports/close", KEY_FUNC, elektraZeroMqSendClose, KEY_END),
			keyNew ("system/elektra/modules/zeromqsend/exports/setIoBinding", KEY_FUNC, elektraZeroMqSendSetIoBinding, KEY_END),
#include ELEKTRA_README (zeromqsend)
			keyNew ("system/elektra/modules/zeromqsend/infos/version", KEY_VALUE, PLUGINVERSION, KEY_END), KS_END);
		ksAppend (returned, contract);
		ksDel (contract);

		return 1; /* success */
	}

	// remember all keys
	ElektraZeroMqSendPluginData * pluginData = elektraPluginGetData (handle);
	ELEKTRA_NOT_NULL (pluginData);

	KeySet * ks = pluginData->keys;
	if (ks) ksDel (ks);
	pluginData->keys = ksDup (returned);

	return 1; /* success */
}

int elektraZeroMqSendSet (Plugin * handle, KeySet * returned, Key * parentKey ELEKTRA_UNUSED)
{
	ElektraZeroMqSendPluginData * pluginData = elektraPluginGetData (handle);
	ELEKTRA_NOT_NULL (pluginData);

	KeySet * oldKeys = pluginData->keys;
	// because elektraPluginGet will always be executed before elektraPluginSet
	// we know that oldKeys must exist here!
	ksRewind (oldKeys);
	ksRewind (returned);

	KeySet * addedKeys = ksDup (returned);
	KeySet * changedKeys = ksNew (0, KS_END);
	KeySet * removedKeys = ksNew (0, KS_END);

	Key * k = 0;
	while ((k = ksNext (oldKeys)) != 0)
	{
		Key * p = ksLookup (addedKeys, k, KDB_O_POP);
		// Note: keyDel not needed, because at least two references exist
		if (p)
		{
			if (keyNeedSync (p))
			{
				ksAppendKey (changedKeys, p);
			}
		}
		else
		{
			ksAppendKey (removedKeys, k);
		}
	}

	announceKeys (addedKeys, "KeyAdded", pluginData);
	announceKeys (changedKeys, "KeyChanged", pluginData);
	announceKeys (removedKeys, "KeyDeleted", pluginData);

	ksDel (oldKeys);
	ksDel (addedKeys);
	ksDel (changedKeys);
	ksDel (removedKeys);

	// for next invocation of elektraLogchangeSet, remember our current keyset
	pluginData->keys = ksDup (returned);

	return 1; /* success */
}

int elektraZeroMqSendClose (Plugin * handle, Key * parentKey ELEKTRA_UNUSED)
{
	ElektraZeroMqSendPluginData * pluginData = elektraPluginGetData (handle);
	if (pluginData == NULL)
	{
		return 1;
	}

	KeySet * ks = pluginData->keys;
	if (ks) ksDel (ks);
	if (pluginData->zmqAdapter)
	{
		if (!elektraIoZeroMqAdapterDetach (pluginData->zmqAdapter))
		{
			ELEKTRA_LOG_WARNING ("detach adapter failed");
		}
		pluginData->zmqAdapter = NULL;
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

Plugin * ELEKTRA_PLUGIN_EXPORT (zeromqsend)
{
	// clang-format off
	return elektraPluginExport("zeromqsend",
		ELEKTRA_PLUGIN_OPEN,	&elektraZeroMqSendOpen,
		ELEKTRA_PLUGIN_GET,	&elektraZeroMqSendGet,
		ELEKTRA_PLUGIN_SET,	&elektraZeroMqSendSet,
		ELEKTRA_PLUGIN_CLOSE,	&elektraZeroMqSendClose,
		ELEKTRA_PLUGIN_END);
}
