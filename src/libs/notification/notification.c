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

/**
 * @see kdbnotificationinternal.h ::ElektraNotificationKdbUpdate
 */
static void elektraNotificationKdbUpdate (ElektraKdb * kdb, ElektraKey * changedKey)
{
	ElektraKeyset * ks = ksNew (0, ELEKTRA_KS_END);
	kdbGet (kdb, ks, changedKey);
	ksDel (ks);
}

int elektraNotificationContract (ElektraKeyset * contract)
{
	if (contract == NULL) return -1;

	ksAppendKey (contract, keyNew ("system:/elektra/contract/mountglobal/internalnotification", ELEKTRA_KEY_END));

	ElektraNotificationCallbackContext * context = elektraMalloc (sizeof (*context));
	context->kdbUpdate = &elektraNotificationKdbUpdate;
	ksAppendKey (contract, keyNew ("system:/elektra/contract/mountglobal/internalnotification/context", ELEKTRA_KEY_BINARY, ELEKTRA_KEY_SIZE,
				       sizeof (context), ELEKTRA_KEY_VALUE, &context, ELEKTRA_KEY_END));

	return 0;
}

/**
 * @internal
 * Get notification plugin from kdb.
 *
 * @param  kdb KDB handle
 * @return     Notification plugin handle or NULL if not present
 */
static Plugin * getNotificationPlugin (ElektraKdb * kdb)
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
			"elektraNotifiationContract before calling other "
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
#ifdef ELEKTRA_HAVE_KDB_LONG_DOUBLE
ELEKTRA_NOTIFICATION_TYPE_DEFINITION (kdb_long_double_t, KdbLongDouble)
#endif // ELEKTRA_HAVE_KDB_LONG_DOUBLE

int elektraNotificationRegisterCallback (ElektraKdb * kdb, ElektraKey * key, ElektraNotificationChangeCallback callback, void * context)
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

int elektraNotificationRegisterCallbackSameOrBelow (ElektraKdb * kdb, ElektraKey * key, ElektraNotificationChangeCallback callback, void * context)
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

int elektraNotificationSetConversionErrorCallback (ElektraKdb * kdb, ElektraNotificationConversionErrorCallback callback, void * context)
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
