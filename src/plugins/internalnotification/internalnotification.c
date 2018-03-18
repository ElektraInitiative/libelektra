/**
 * @file
 *
 * @brief Source for internalnotification plugin
 *
 * @copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 *
 */

#include "internalnotification.h"

#include <kdb.h>
#include <kdbassert.h>
#include <kdbhelper.h>
#include <kdblogger.h>
#include <kdbnotificationinternal.h>

#include <errno.h>  // errno
#include <stdlib.h> // strto* functions
/*
	checklist for types
	[ ] TYPE_BOOLEAN = 1 << 0,
	[ ] TYPE_CHAR = 1 << 1,
	[ ] TYPE_OCTET = 1 << 2,
	[ ] TYPE_SHORT = 1 << 3,
	[ ] TYPE_UNSIGNED_SHORT = 1 << 4,
	[x] TYPE_INT = 1 << 5,
	[x] TYPE_LONG = 1 << 6,
	[x] TYPE_UNSIGNED_LONG = 1 << 7,
	[ ] TYPE_LONG_LONG = 1 << 8,
	[ ] TYPE_UNSIGNED_LONG_LONG = 1 << 9,
	[x] TYPE_FLOAT = 1 << 10,
	[x] TYPE_DOUBLE = 1 << 11,
	[ ] TYPE_LONG_DOUBLE = 1 << 12,
	[x] TYPE_CALLBACK = 1 << 13,
*/

/**
 * Structure for registered key variable pairs
 * @internal
 */
struct _KeyRegistration
{
	char * name;
	char * lastValue;
	ElektraNotificationChangeCallback callback;
	void * context;
	struct _KeyRegistration * next;
};
typedef struct _KeyRegistration KeyRegistration;

/**
 * Structure for internal plugin state
 * @internal
 */
struct _PluginState
{
	KeyRegistration * head;
	KeyRegistration * last;
};
typedef struct _PluginState PluginState;

/**
 * @internal
 * Convert key name into cascading key name for comparison.
 *
 * Key name is not modified, nothing to free
 *
 * @param  keyName key name
 * @return         pointer to cascading name
 */
static const char * toCascadingName (const char * keyName)
{
	while (keyName[0] != '/')
	{
		keyName++;
	}
	return keyName;
}

/**
 * @internal
 * Call kdbGet if there are registrations below the changed key.
 *
 * On kdbGet this plugin implicitly updates registered keys.
 *
 * @see ElektraNotificationChangeCallback (kdbnotificationinternal.h)
 * @param key     changed key
 * @param context callback context
 */
static void elektraInternalnotificationDoUpdate (Key * changedKey, ElektraNotificationCallbackContext * context)
{
	ELEKTRA_NOT_NULL (changedKey);
	ELEKTRA_NOT_NULL (context);

	KDB * kdb = context->kdb;
	Plugin * plugin = context->notificationPlugin;

	PluginState * pluginState = elektraPluginGetData (plugin);
	ELEKTRA_NOT_NULL (pluginState);

	int kdbChanged = 0;
	KeyRegistration * keyRegistration = pluginState->head;
	while (keyRegistration != NULL)
	{
		Key * registereKey = keyNew (keyRegistration->name, KEY_END);

		if (keyIsBelow (changedKey, registereKey))
		{
			kdbChanged |= 1;
		}
		else if (keyGetNamespace (registereKey) == KEY_NS_CASCADING || keyGetNamespace (changedKey) == KEY_NS_CASCADING)
		{
			const char * cascadingRegisterdKey = toCascadingName (keyRegistration->name);
			const char * cascadingChangedKey = toCascadingName (keyName (changedKey));
			kdbChanged |= elektraStrCmp (cascadingChangedKey, cascadingRegisterdKey) == 0;
		}

		keyRegistration = keyRegistration->next;
		keyDel (registereKey);
	}

	if (kdbChanged)
	{
		KeySet * ks = ksNew (0, KS_END);
		kdbGet (kdb, ks, changedKey);
		ksDel (ks);
	}
	keyDel (changedKey);
}

/**
 * Creates a new KeyRegistration structure and appends it at the end of the registration list
 * @internal
 *
 * @param pluginState		internal plugin data structure
 *
 * @return pointer to created KeyRegistration structure or NULL if memory allocation failed
 */
static KeyRegistration * elektraInternalnotificationAddNewRegistration (PluginState * pluginState, Key * key,
									ElektraNotificationChangeCallback callback, void * context)
{
	KeyRegistration * item = elektraMalloc (sizeof *item);
	if (item == NULL)
	{
		return NULL;
	}
	item->next = NULL;
	item->lastValue = NULL;
	item->name = elektraStrDup (keyName (key));
	item->callback = callback;
	item->context = context;

	if (pluginState->head == NULL)
	{
		// Initialize list
		pluginState->head = pluginState->last = item;
	}
	else
	{
		// Make new item end of list
		pluginState->last->next = item;
		pluginState->last = item;
	}

	return item;
}

/**
 * Updates all KeyRegistrations according to data from the given KeySet
 * @internal
 *
 * @param plugin    internal plugin handle
 * @param keySet    key set retrieved from hooks
 *                  e.g. elektraInternalnotificationGet or elektraInternalnotificationSet)
 *
 */
void elektraInternalnotificationUpdateRegisteredKeys (Plugin * plugin, KeySet * keySet)
{
	PluginState * pluginState = elektraPluginGetData (plugin);
	ELEKTRA_ASSERT (pluginState != NULL, "plugin state was not initialized properly");

	KeyRegistration * registeredKey = pluginState->head;
	while (registeredKey != NULL)
	{
		Key * key = ksLookupByName (keySet, registeredKey->name, 0);
		if (key == NULL)
		{
			registeredKey = registeredKey->next;
			continue;
		}

		// Detect changes for string keys
		int changed = 0;
		if (!keyIsString (key))
		{
			// always notify for binary keys
			changed = 1;
		}
		else
		{
			const char * currentValue = keyString (key);
			changed = registeredKey->lastValue == NULL || strcmp (currentValue, registeredKey->lastValue) != 0;

			if (changed)
			{
				// Save last value
				char * buffer = elektraStrDup (currentValue);
				if (buffer)
				{
					if (registeredKey->lastValue != NULL)
					{
						// Free previous value
						elektraFree (registeredKey->lastValue);
					}
					registeredKey->lastValue = buffer;
				}
			}
		}

		if (changed)
		{
			ELEKTRA_LOG_DEBUG ("found changed registeredKey=%s with string value \"%s\". using context or variable=%p",
					   registeredKey->name, keyString (key), registeredKey->context);

			// Invoke callback
			ElektraNotificationChangeCallback callback = *(ElektraNotificationChangeCallback) registeredKey->callback;
			callback (key, registeredKey->context);
		}

		registeredKey = registeredKey->next;
	}
}

// Generate register and conversion functions
INTERNALNOTIFICATION_TYPE (int, long int, Int, (strtol (string, &end, 10)),
			   INTERNALNOTIFICATION_CHECK_CONVERSION_RANGE (value <= INT_MAX && value >= INT_MIN))
INTERNALNOTIFICATION_TYPE (unsigned int, unsigned long int, UnsignedInt, (strtoul (string, &end, 10)),
			   INTERNALNOTIFICATION_CHECK_CONVERSION_RANGE (value <= UINT_MAX))

INTERNALNOTIFICATION_TYPE (long, long, Long, (strtol (string, &end, 10)), INTERNALNOTIFICATION_CHECK_CONVERSION)
INTERNALNOTIFICATION_TYPE (unsigned long, unsigned long, UnsignedLong, (strtoul (string, &end, 10)), INTERNALNOTIFICATION_CHECK_CONVERSION)

INTERNALNOTIFICATION_TYPE (float, float, Float, (strtof (string, &end)), INTERNALNOTIFICATION_CHECK_CONVERSION)
INTERNALNOTIFICATION_TYPE (double, double, Double, (strtod (string, &end)), INTERNALNOTIFICATION_CHECK_CONVERSION)

INTERNALNOTIFICATION_TYPE (kdb_boolean_t, int, KdbBoolean, (!strcmp (string, "1")), 1)
INTERNALNOTIFICATION_TYPE (kdb_char_t, kdb_char_t, KdbChar, (string[0]), 1)
INTERNALNOTIFICATION_TYPE (kdb_octet_t, unsigned int, KdbOctet, (strtoul (string, &end, 10)),
			   INTERNALNOTIFICATION_CHECK_CONVERSION_RANGE (value <= 255))
INTERNALNOTIFICATION_TYPE (kdb_short_t, int, KdbShort, (strtol (string, &end, 10)),
			   INTERNALNOTIFICATION_CHECK_CONVERSION_RANGE (value <= SHRT_MAX && value >= SHRT_MIN))
INTERNALNOTIFICATION_TYPE (kdb_unsigned_short_t, unsigned int, KdbUnsignedShort, (strtoul (string, &end, 10)),
			   INTERNALNOTIFICATION_CHECK_CONVERSION_RANGE (value <= USHRT_MAX))
INTERNALNOTIFICATION_TYPE (kdb_long_t, kdb_long_t, KdbLong, (strtol (string, &end, 10)), INTERNALNOTIFICATION_CHECK_CONVERSION)
INTERNALNOTIFICATION_TYPE (kdb_unsigned_long_t, kdb_unsigned_long_t, KdbUnsignedLong, (strtoul (string, &end, 10)),
			   INTERNALNOTIFICATION_CHECK_CONVERSION)
INTERNALNOTIFICATION_TYPE (kdb_long_long_t, kdb_long_long_t, KdbLongLong, (ELEKTRA_LONG_LONG_S (string, &end, 10)),
			   INTERNALNOTIFICATION_CHECK_CONVERSION)
INTERNALNOTIFICATION_TYPE (kdb_unsigned_long_long_t, kdb_unsigned_long_long_t, KdbUnsignedLongLong,
			   (ELEKTRA_UNSIGNED_LONG_LONG_S (string, &end, 10)), INTERNALNOTIFICATION_CHECK_CONVERSION)
INTERNALNOTIFICATION_TYPE (kdb_float_t, kdb_float_t, KdbFloat, (strtof (string, &end)), INTERNALNOTIFICATION_CHECK_CONVERSION)
INTERNALNOTIFICATION_TYPE (kdb_double_t, kdb_double_t, KdbDouble, (strtod (string, &end)), INTERNALNOTIFICATION_CHECK_CONVERSION)
INTERNALNOTIFICATION_TYPE (kdb_long_double_t, kdb_long_double_t, KdbLongDouble, (strtold (string, &end)),
			   INTERNALNOTIFICATION_CHECK_CONVERSION)

/**
 * @see kdbnotificationinternal.h ::ElektraNotificationPluginRegisterCallback
 */
int elektraInternalnotificationRegisterCallback (Plugin * handle, Key * key, ElektraNotificationChangeCallback callback, void * context)
{
	PluginState * pluginState = elektraPluginGetData (handle);
	ELEKTRA_ASSERT (pluginState != NULL, "plugin state was not initialized properly");

	KeyRegistration * registeredKey = elektraInternalnotificationAddNewRegistration (pluginState, key, callback, context);
	if (registeredKey == NULL)
	{
		return 0;
	}

	return 1;
}

/**
 * Updates registrations with current data from storage.
 * Part of elektra plugin contract.
 *
 * @param  handle    plugin handle
 * @param  returned  key set containing current data from storage
 * @param  parentKey key for errors
 *
 * @retval 1 on success
 * @retval -1 on failure
 */
int elektraInternalnotificationGet (Plugin * handle, KeySet * returned, Key * parentKey)
{
	if (!elektraStrCmp (keyName (parentKey), "system/elektra/modules/internalnotification"))
	{
		KeySet * contract = ksNew (
			30,
			keyNew ("system/elektra/modules/internalnotification", KEY_VALUE,
				"internalnotification plugin waits for your orders", KEY_END),
			keyNew ("system/elektra/modules/internalnotification/exports", KEY_END),
			keyNew ("system/elektra/modules/internalnotification/exports/get", KEY_FUNC, elektraInternalnotificationGet,
				KEY_END),
			keyNew ("system/elektra/modules/internalnotification/exports/set", KEY_FUNC, elektraInternalnotificationSet,
				KEY_END),
			keyNew ("system/elektra/modules/internalnotification/exports/open", KEY_FUNC, elektraInternalnotificationOpen,
				KEY_END),
			keyNew ("system/elektra/modules/internalnotification/exports/close", KEY_FUNC, elektraInternalnotificationClose,
				KEY_END),

			keyNew ("system/elektra/modules/internalnotification/exports/notificationCallback", KEY_FUNC,
				elektraInternalnotificationDoUpdate, KEY_END),

			// Export register* functions
			INTERNALNOTIFICATION_EXPORT_FUNCTION (Int), INTERNALNOTIFICATION_EXPORT_FUNCTION (UnsignedInt),
			INTERNALNOTIFICATION_EXPORT_FUNCTION (Long), INTERNALNOTIFICATION_EXPORT_FUNCTION (UnsignedLong),
			INTERNALNOTIFICATION_EXPORT_FUNCTION (Float), INTERNALNOTIFICATION_EXPORT_FUNCTION (Double),

			// Export register* functions for kdb_*_t types
			INTERNALNOTIFICATION_EXPORT_FUNCTION (KdbBoolean), INTERNALNOTIFICATION_EXPORT_FUNCTION (KdbChar),
			INTERNALNOTIFICATION_EXPORT_FUNCTION (KdbOctet), INTERNALNOTIFICATION_EXPORT_FUNCTION (KdbShort),
			INTERNALNOTIFICATION_EXPORT_FUNCTION (KdbUnsignedShort), INTERNALNOTIFICATION_EXPORT_FUNCTION (KdbLong),
			INTERNALNOTIFICATION_EXPORT_FUNCTION (KdbUnsignedLong), INTERNALNOTIFICATION_EXPORT_FUNCTION (KdbLongLong),
			INTERNALNOTIFICATION_EXPORT_FUNCTION (KdbUnsignedLongLong), INTERNALNOTIFICATION_EXPORT_FUNCTION (KdbFloat),
			INTERNALNOTIFICATION_EXPORT_FUNCTION (KdbDouble), INTERNALNOTIFICATION_EXPORT_FUNCTION (KdbLongDouble),

			keyNew ("system/elektra/modules/internalnotification/exports/registerCallback", KEY_FUNC,
				elektraInternalnotificationRegisterCallback, KEY_END),

#include ELEKTRA_README (internalnotification)

			keyNew ("system/elektra/modules/internalnotification/infos/version", KEY_VALUE, PLUGINVERSION, KEY_END), KS_END);
		ksAppend (returned, contract);
		ksDel (contract);

		return 1;
	}

	elektraInternalnotificationUpdateRegisteredKeys (handle, returned);

	return 1;
}

/**
 * Updates registrations with data written by the application.
 * Part of elektra plugin contract.
 *
 * @param  handle    plugin handle
 * @param  returned  key set containing current data from the application
 * @param  parentKey key for errors

 * @retval 1 on success
 * @retval -1 on failure
 */
int elektraInternalnotificationSet (Plugin * handle, KeySet * returned, Key * parentKey ELEKTRA_UNUSED)
{
	elektraInternalnotificationUpdateRegisteredKeys (handle, returned);

	return 1;
}

/**
 * Initialize data plugin data structures.
 * Part of elektra plugin contract.
 *
 * @param  handle         plugin handle
 * @param  parentKey      key for errors
 *
 * @retval 1 on success
 * @retval -1 on failure
 */
int elektraInternalnotificationOpen (Plugin * handle, Key * parentKey ELEKTRA_UNUSED)
{
	PluginState * pluginState = elektraPluginGetData (handle);
	if (pluginState == NULL)
	{
		pluginState = elektraMalloc (sizeof *pluginState);
		if (pluginState == NULL)
		{
			return -1;
		}
		elektraPluginSetData (handle, pluginState);

		// Initialize list pointers for registered keys
		pluginState->head = NULL;
		pluginState->last = NULL;
	}

	return 1;
}

/**
 * Free used memory.
 * Part of elektra plugin contract.
 *
 * @param  handle         plugin handle
 * @param  parentKey      key for errors
 *
 * @retval 1 on success
 * @retval -1 on failure
 */
int elektraInternalnotificationClose (Plugin * handle, Key * parentKey ELEKTRA_UNUSED)
{
	PluginState * pluginState = elektraPluginGetData (handle);
	if (pluginState != NULL)
	{
		// Free registrations
		KeyRegistration * current = pluginState->head;
		KeyRegistration * next;
		while (current != NULL)
		{
			next = current->next;
			elektraFree (current->name);
			if (current->lastValue != NULL)
			{
				elektraFree (current->lastValue);
			}
			elektraFree (current);

			current = next;
		}

		// Free list pointer
		elektraFree (pluginState);
		elektraPluginSetData (handle, NULL);
	}

	return 1;
}

Plugin * ELEKTRA_PLUGIN_EXPORT (internalnotification)
{
	// clang-format off
	return elektraPluginExport ("internalnotification",
		ELEKTRA_PLUGIN_GET,	&elektraInternalnotificationGet,
		ELEKTRA_PLUGIN_SET,	&elektraInternalnotificationSet,
		ELEKTRA_PLUGIN_OPEN, &elektraInternalnotificationOpen,
		ELEKTRA_PLUGIN_CLOSE, &elektraInternalnotificationClose,
		ELEKTRA_PLUGIN_END);
}
