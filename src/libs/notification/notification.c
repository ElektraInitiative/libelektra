/**
 * @file
 *
 * @brief Implementation of notification functions as defined in
 * kdbnotification.h
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#include <elektra/kdb/hooks.h>
#include <elektra/notifications.h>
#include <elektra/plugin/invoke.h>
#include <elektra/plugin/plugin.h>

#include <internal/io/private.h>
#include <internal/kdbprivate.h>
#include <internal/notifications.h>
#include <internal/plugin/load.h>
#include <internal/utility/alloc.h>
#include <internal/utility/assert.h>
#include <internal/utility/logger.h>

#include <stdio.h>

/**
 * @see kdbnotificationinternal.h ::ElektraNotificationKdbUpdate
 */
static void elektraNotificationKdbUpdate (KDB * kdb, Key * changedKey)
{
	KeySet * ks = ksNew (0, KS_END);
	kdbGet (kdb, ks, changedKey);
	ksDel (ks);
}

int elektraNotificationContract (KeySet * contract)
{
	if (contract == NULL) return -1;

	ksAppendKey (contract, keyNew ("system:/elektra/contract/mountglobal/internalnotification", KEY_END));

	ElektraNotificationCallbackContext * context = elektraMalloc (sizeof (*context));
	context->kdbUpdate = &elektraNotificationKdbUpdate;
	ksAppendKey (contract, keyNew ("system:/elektra/contract/mountglobal/internalnotification/context", KEY_BINARY, KEY_SIZE,
				       sizeof (context), KEY_VALUE, &context, KEY_END));

	return 0;
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

	Plugin * notificationPlugin = elektraFindInternalNotificationPlugin (kdb);
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
