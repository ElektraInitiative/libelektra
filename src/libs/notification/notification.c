/**
 * @file
 *
 * @brief Implementation of notification functions as defined in
 * kdbnotification.h
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#include <kdbassert.h>
#include <kdbease.h>
#include <kdbhelper.h>
#include <kdbinvoke.h>
#include <kdbioprivate.h>
#include <kdblogger.h>
#include <kdbnotification.h>
#include <kdbnotificationinternal.h>
#include <kdbplugin.h>
#include <kdbprivate.h> // for elektraGetPluginFunction, elektraPluginFindGlobal, kdb->globalPlugins and plugin->config

#include <stdio.h>

static void pluginsOpenNotification (KDB * kdb, ElektraNotificationCallback callback, ElektraNotificationCallbackContext * context)
{
	ELEKTRA_NOT_NULL (kdb);
	ELEKTRA_NOT_NULL (callback);

	KeySet * parameters = ksNew (2, keyNew ("/callback", KEY_FUNC, callback, KEY_END),
				     keyNew ("/context", KEY_BINARY, KEY_SIZE, sizeof (context), KEY_VALUE, &context, KEY_END), KS_END);

	// iterate over global plugins
	for (int positionIndex = 0; positionIndex < NR_GLOBAL_POSITIONS; positionIndex++)
	{
		for (int subPositionIndex = 0; subPositionIndex < NR_GLOBAL_SUBPOSITIONS; subPositionIndex++)
		{
			Plugin * plugin = kdb->globalPlugins[positionIndex][subPositionIndex];
			if (!plugin)
			{
				continue;
			}


			elektraDeferredCall (plugin, "openNotification", parameters);
		}
	}

	ksDel (parameters);
}

static void pluginsCloseNotification (KDB * kdb)
{
	ELEKTRA_NOT_NULL (kdb);

	// iterate over global plugins
	for (int positionIndex = 0; positionIndex < NR_GLOBAL_POSITIONS; positionIndex++)
	{
		for (int subPositionIndex = 0; subPositionIndex < NR_GLOBAL_SUBPOSITIONS; subPositionIndex++)
		{
			Plugin * plugin = kdb->globalPlugins[positionIndex][subPositionIndex];
			if (!plugin)
			{
				continue;
			}

			elektraDeferredCall (plugin, "closeNotification", NULL);
		}
	}
}

/**
 * @see kdbnotificationinternal.h ::ElektraNotificationKdbUpdate
 */
static void elektraNotificationKdbUpdate (KDB * kdb, Key * changedKey)
{
	KeySet * ks = ksNew (0, KS_END);
	kdbGet (kdb, ks, changedKey);
	ksDel (ks);
}

int elektraNotificationOpen (KDB * kdb)
{
	// Make sure kdb is not null
	if (!kdb)
	{
		ELEKTRA_LOG_WARNING ("kdb was not set");
		return 0;
	}

	Plugin * notificationPlugin = elektraPluginFindGlobal (kdb, "internalnotification");
	// Allow open only once
	if (notificationPlugin)
	{
		ELEKTRA_LOG_WARNING ("elektraNotificationOpen already called for kdb");
		return 0;
	}

	// Create context for notification callback
	ElektraNotificationCallbackContext * context = elektraMalloc (sizeof (*context));
	if (context == NULL)
	{
		return 0;
	}
	context->kdb = kdb;
	context->kdbUpdate = &elektraNotificationKdbUpdate;

	// FIXME: kdbEnsure
	return 0;

	/*Key * parent = keyNew ("/", KEY_END);
	KeySet * contract = ksNew (2, keyNew ("system:/elektra/ensure/plugins/global/internalnotification", KEY_VALUE, "mounted", KEY_END),
				   keyNew ("system:/elektra/ensure/plugins/global/internalnotification/config/context", KEY_BINARY,
					   KEY_SIZE, sizeof (context), KEY_VALUE, &context, KEY_END),
				   KS_END);
	if (kdbEnsure (kdb, contract, parent) != 0)
	{
		keyDel (parent);
		ELEKTRA_LOG_WARNING ("kdbEnsure failed");
		return 0;
	}

	notificationPlugin = elektraPluginFindGlobal (kdb, "internalnotification");
	if (notificationPlugin == NULL)
	{
		ELEKTRA_LOG_WARNING ("kdbEnsure failed");
		return 0;
	}

	context->notificationPlugin = notificationPlugin;
	*/

	// Get notification callback from notification plugin
	size_t func = elektraPluginGetFunction (notificationPlugin, "notificationCallback");
	// FIXME: kdbEnsure
	/*if (!func)
	{
		// remove notification plugin again
		contract = ksNew (1, keyNew ("system:/elektra/ensure/plugins/global/internalnotification", KEY_VALUE, "unmounted", KEY_END),
				  KS_END);
		if (kdbEnsure (kdb, contract, parent) != 0)
		{
			ELEKTRA_LOG_WARNING ("kdbEnsure failed");
		}
		keyDel (parent);
		return 0;
	}*/
	ElektraNotificationCallback notificationCallback = (ElektraNotificationCallback) func;

	// FIXME: kdbEnsure
	// keyDel (parent);

	// Open notification for plugins
	pluginsOpenNotification (kdb, notificationCallback, context);

	return 1;
}

int elektraNotificationClose (KDB * kdb)
{
	// Make sure kdb is not null
	if (!kdb)
	{
		ELEKTRA_LOG_WARNING ("kdb was not set");
		return 0;
	}

	Plugin * notificationPlugin = elektraPluginFindGlobal (kdb, "internalnotification");
	// Make sure open was called
	if (notificationPlugin == NULL)
	{
		ELEKTRA_LOG_WARNING ("elektraNotificationOpen not called before elektraPluginClose");
		return 0;
	}

	Key * contextKey = ksLookupByName (notificationPlugin->config, "user:/context", 0);
	ElektraNotificationCallbackContext * context = *(ElektraNotificationCallbackContext **) keyValue (contextKey);
	elektraFree (context);

	// FIXME: kdbEnsure
	/*// Unmount the plugin
	Key * parent = keyNew ("/", KEY_END);
	KeySet * contract =
		ksNew (1, keyNew ("system:/elektra/ensure/plugins/global/internalnotification", KEY_VALUE, "unmounted", KEY_END), KS_END);
	if (kdbEnsure (kdb, contract, parent) != 0)
	{
		ELEKTRA_LOG_WARNING ("kdbEnsure failed");
	}
	keyDel (parent);*/

	// Close notification for plugins
	pluginsCloseNotification (kdb);

	return 1;
}

/**
 * @internal
 * Get notification plugin from kdb.
 *
 * @param  kdb KDB handle
 * @return     Notification plugin handle or NULL if not present
 */
static Plugin * getNotificationPlugin (KDB * kdb)
{
	ELEKTRA_NOT_NULL (kdb);

	Plugin * notificationPlugin = elektraPluginFindGlobal (kdb, "internalnotification");
	if (notificationPlugin)
	{
		return notificationPlugin;
	}
	else
	{
		ELEKTRA_LOG_WARNING (
			"notificationPlugin not set. use "
			"elektraNotificationOpen before calling other "
			"elektraNotification-functions");
		return NULL;
	}
}

ELEKTRA_NOTIFICATION_TYPE_DEFINITION (int, Int)
ELEKTRA_NOTIFICATION_TYPE_DEFINITION (unsigned int, UnsignedInt)
ELEKTRA_NOTIFICATION_TYPE_DEFINITION (long, Long)
ELEKTRA_NOTIFICATION_TYPE_DEFINITION (unsigned long, UnsignedLong)
ELEKTRA_NOTIFICATION_TYPE_DEFINITION (float, Float)
ELEKTRA_NOTIFICATION_TYPE_DEFINITION (double, Double)

ELEKTRA_NOTIFICATION_TYPE_DEFINITION (kdb_boolean_t, KdbBoolean)
ELEKTRA_NOTIFICATION_TYPE_DEFINITION (kdb_char_t, KdbChar)
ELEKTRA_NOTIFICATION_TYPE_DEFINITION (kdb_octet_t, KdbOctet)
ELEKTRA_NOTIFICATION_TYPE_DEFINITION (kdb_short_t, KdbShort)
ELEKTRA_NOTIFICATION_TYPE_DEFINITION (kdb_unsigned_short_t, KdbUnsignedShort)
ELEKTRA_NOTIFICATION_TYPE_DEFINITION (kdb_long_t, KdbLong)
ELEKTRA_NOTIFICATION_TYPE_DEFINITION (kdb_unsigned_long_t, KdbUnsignedLong)
ELEKTRA_NOTIFICATION_TYPE_DEFINITION (kdb_long_long_t, KdbLongLong)
ELEKTRA_NOTIFICATION_TYPE_DEFINITION (kdb_unsigned_long_long_t, KdbUnsignedLongLong)
ELEKTRA_NOTIFICATION_TYPE_DEFINITION (kdb_float_t, KdbFloat)
ELEKTRA_NOTIFICATION_TYPE_DEFINITION (kdb_double_t, KdbDouble)
ELEKTRA_NOTIFICATION_TYPE_DEFINITION (kdb_long_double_t, KdbLongDouble)

int elektraNotificationRegisterCallback (KDB * kdb, Key * key, ElektraNotificationChangeCallback callback, void * context)
{
	if (!kdb || !key || !callback)
	{
		ELEKTRA_LOG_WARNING ("null pointer passed");
		return 0;
	}

	// Find notification plugin
	Plugin * notificationPlugin = getNotificationPlugin (kdb);
	if (!notificationPlugin)
	{
		return 0;
	}

	// Get register function from plugin
	size_t func = elektraPluginGetFunction (notificationPlugin, "registerCallback");
	if (!func)
	{
		return 0;
	}

	// Call register function
	ElektraNotificationPluginRegisterCallback registerFunc = (ElektraNotificationPluginRegisterCallback) func;
	return registerFunc (notificationPlugin, key, callback, context);
}

int elektraNotificationRegisterCallbackSameOrBelow (KDB * kdb, Key * key, ElektraNotificationChangeCallback callback, void * context)
{
	if (!kdb || !key || !callback)
	{
		ELEKTRA_LOG_WARNING ("null pointer passed");
		return 0;
	}

	// Find notification plugin
	Plugin * notificationPlugin = getNotificationPlugin (kdb);
	if (!notificationPlugin)
	{
		return 0;
	}

	// Get register function from plugin
	size_t func = elektraPluginGetFunction (notificationPlugin, "registerCallbackSameOrBelow");
	if (!func)
	{
		return 0;
	}

	// Call register function
	ElektraNotificationPluginRegisterCallbackSameOrBelow registerFunc = (ElektraNotificationPluginRegisterCallbackSameOrBelow) func;
	return registerFunc (notificationPlugin, key, callback, context);
}

int elektraNotificationSetConversionErrorCallback (KDB * kdb, ElektraNotificationConversionErrorCallback callback, void * context)
{
	if (!kdb || !callback)
	{
		ELEKTRA_LOG_WARNING ("null pointer passed");
		return 0;
	}

	// Find notification plugin
	Plugin * notificationPlugin = getNotificationPlugin (kdb);
	if (!notificationPlugin)
	{
		return 0;
	}

	// Get register function from plugin
	size_t func = elektraPluginGetFunction (notificationPlugin, "setConversionErrorCallback");
	if (!func)
	{
		return 0;
	}

	// Call register function
	ElektraNotificationSetConversionErrorCallback setCallbackFunc = (ElektraNotificationSetConversionErrorCallback) func;
	setCallbackFunc (notificationPlugin, callback, context);
	return 1;
}
