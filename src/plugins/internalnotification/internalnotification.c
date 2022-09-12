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

#include <ctype.h>  // isspace()
#include <errno.h>  // errno
#include <stdlib.h> // strto* functions

/**
 * Structure for registered key variable pairs
 * @internal
 */
struct _KeyRegistration
{
	char * name;
	char * lastValue;
	int sameOrBelow;
	int freeContext;
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
	ElektraNotificationConversionErrorCallback conversionErrorCallback;
	void * conversionErrorCallbackContext;
};
typedef struct _PluginState PluginState;

/**
 * @see kdbnotificationinternal.h ::ElektraNotificationSetConversionErrorCallback
 */
static void elektraInternalnotificationSetConversionErrorCallback (Plugin * handle, ElektraNotificationConversionErrorCallback callback,
								   void * context)
{
	ELEKTRA_NOT_NULL (handle);
	ELEKTRA_NOT_NULL (callback);
	PluginState * data = elektraPluginGetData (handle);
	ELEKTRA_NOT_NULL (data);

	data->conversionErrorCallback = callback;
	data->conversionErrorCallbackContext = context;
}

/**
 * @internal
 * Check if two keys have the same name.
 * If one of the keys is cascading only the cascading names are compared.
 *
 * @param  key   key
 * @param  check check
 * @retval 1 if keys have the same name
 * @retval 0 otherwise
 */
static int checkKeyIsSame (ElektraKey * key, ElektraKey * check)
{
	int result = 0;
	if (elektraKeyGetNamespace (check) == ELEKTRA_NS_CASCADING || elektraKeyGetNamespace (key) == ELEKTRA_NS_CASCADING)
	{
		const char * cascadingCheck = strrchr (elektraKeyName (check), '/');
		const char * cascadingKey = strrchr (elektraKeyName (key), '/');
		if (cascadingCheck != NULL && cascadingKey != NULL)
		{
			result = elektraStrCmp (cascadingKey, cascadingCheck) == 0;
		}
		else
		{
			if (!cascadingCheck || !cascadingKey)
			{
				ELEKTRA_LOG_WARNING ("invalid key given: 'NULL' is not a valid key");
			}
		}
	}
	else
	{
		result = elektraStrCmp (elektraKeyName (check), elektraKeyName (key)) == 0;
	}
	return result;
}

/**
 * @internal
 * Check if a key has the same name or is below a given key.
 *
 * @param  key   key
 * @param  check check
 * @retval 1 if key has the same name or is below
 * @retval 0 otherwise
 */
static int checkKeyIsBelowOrSame (ElektraKey * key, ElektraKey * check)
{
	int result = 0;
	if (elektraKeyIsBelow (key, check))
	{
		result = 1;
	}
	else
	{
		result = checkKeyIsSame (key, check);
	}

	return result;
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
void elektraInternalnotificationDoUpdate (ElektraKey * changedKey, ElektraNotificationCallbackContext * context)
{
	ELEKTRA_NOT_NULL (changedKey);
	ELEKTRA_NOT_NULL (context);

	Plugin * plugin = context->notificationPlugin;

	PluginState * pluginState = elektraPluginGetData (plugin);
	ELEKTRA_NOT_NULL (pluginState);

	int kdbChanged = 0;
	KeyRegistration * keyRegistration = pluginState->head;
	while (keyRegistration != NULL)
	{
		ElektraKey * registeredKey = elektraKeyNew (keyRegistration->name, ELEKTRA_KEY_END);

		// check if registered key is same or below changed/commit key
		kdbChanged |= checkKeyIsBelowOrSame (changedKey, registeredKey);

		if (keyRegistration->sameOrBelow)
		{
			// check if registered key is also above changed/commit key
			kdbChanged |= checkKeyIsBelowOrSame (registeredKey, changedKey);
		}

		keyRegistration = keyRegistration->next;
		elektraKeyDel (registeredKey);
	}

	if (kdbChanged)
	{
		ElektraKeyset * global = elektraPluginGetGlobalKeySet (plugin);
		ElektraKey * kdbKey = elektraKeysetLookupByName (global, "system:/elektra/kdb", 0);
		const void * kdbPtr = elektraKeyValue (kdbKey);
		ElektraKdb * kdb = kdbPtr == NULL ? NULL : *(ElektraKdb **) elektraKeyValue (kdbKey);
		context->kdbUpdate (kdb, changedKey);
	}
	elektraKeyDel (changedKey);
}

/**
 * Creates a new KeyRegistration structure and appends it at the end of the registration list
 * @internal
 *
 * @param pluginState   internal plugin data structure
 * @param key           key
 * @param callback      callback for changes
 * @param context       context for callback
 * @param freeContext   context needs to be freed on close
 *
 * @return pointer to created KeyRegistration structure or NULL if memory allocation failed
 */
static KeyRegistration * elektraInternalnotificationAddNewRegistration (PluginState * pluginState, ElektraKey * key,
									ElektraNotificationChangeCallback callback, void * context,
									int freeContext)
{
	KeyRegistration * item = elektraMalloc (sizeof *item);
	if (item == NULL)
	{
		return NULL;
	}
	item->next = NULL;
	item->lastValue = NULL;
	item->name = elektraStrDup (elektraKeyName (key));
	item->callback = callback;
	item->context = context;
	item->sameOrBelow = 0;
	item->freeContext = freeContext;

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
 * @internal
 * Check if a key set contains a key that is same or below a given key.
 *
 * @param  key  key
 * @param  ks   key set
 * @retval 1 if the key set contains the key
 * @retval 0 otherwise
 */
static int keySetContainsSameOrBelow (ElektraKey * check, ElektraKeyset * ks)
{
	ElektraKey * current;
	elektraKeysetRewind (ks);
	while ((current = elektraKeysetNext (ks)) != NULL)
	{
		if (checkKeyIsBelowOrSame (check, current))
		{
			return 1;
		}
	}
	return 0;
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
void elektraInternalnotificationUpdateRegisteredKeys (Plugin * plugin, ElektraKeyset * keySet)
{
	PluginState * pluginState = elektraPluginGetData (plugin);
	ELEKTRA_ASSERT (pluginState != NULL, "plugin state was not initialized properly");

	KeyRegistration * registeredKey = pluginState->head;
	while (registeredKey != NULL)
	{
		int changed = 0;
		ElektraKey * key;
		if (registeredKey->sameOrBelow)
		{
			ElektraKey * checkKey = elektraKeyNew (registeredKey->name, ELEKTRA_KEY_END);
			if (keySetContainsSameOrBelow (checkKey, keySet))
			{
				changed = 1;
				key = checkKey;
			}
			else
			{
				elektraKeyDel (checkKey);
			}
		}
		else
		{
			key = elektraKeysetLookupByName (keySet, registeredKey->name, 0);
			if (key != NULL)
			{
				// Detect changes for string keys
				if (!elektraKeyIsString (key))
				{
					// always notify for binary keys
					changed = 1;
				}
				else
				{
					const char * currentValue = elektraKeyString (key);
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
			}
		}

		if (changed)
		{
			ELEKTRA_LOG_DEBUG ("found changed registeredKey=%s with string value \"%s\". using context or variable=%p",
					   registeredKey->name, elektraKeyString (key), registeredKey->context);

			// Invoke callback
			ElektraNotificationChangeCallback callback = *(ElektraNotificationChangeCallback) registeredKey->callback;
			callback (key, registeredKey->context);
			if (registeredKey->sameOrBelow)
			{
				elektraKeyDel (key);
			}
		}

		// proceed with next registered key
		registeredKey = registeredKey->next;
	}
}

// Generate register and conversion functions
// for built-in C types
#define TYPE int
#define VALUE_TYPE long int
#define TYPE_NAME Int
#define TO_VALUE (strtol (string, &end, 10))
#define CHECK_CONVERSION ELEKTRA_TYPE_CHECK_CONVERSION_RANGE (value <= INT_MAX && value >= INT_MIN)
#include "macros/add_type.h"

#define TYPE unsigned int
#define VALUE_TYPE unsigned long int
#define TYPE_NAME UnsignedInt
#define TO_VALUE (strtoul (string, &end, 10))
#define PRE_CHECK_BLOCK ELEKTRA_TYPE_NEGATIVE_PRE_CHECK_BLOCK
#define PRE_CHECK_CONVERSION ELEKTRA_TYPE_NEGATIVE_PRE_CHECK
#define CHECK_CONVERSION ELEKTRA_TYPE_CHECK_CONVERSION_RANGE (value <= UINT_MAX)
#include "macros/add_type.h"

#define TYPE long
#define TYPE_NAME Long
#define TO_VALUE (strtol (string, &end, 10))
#define CHECK_CONVERSION ELEKTRA_TYPE_CHECK_CONVERSION
#include "macros/add_type.h"

#define TYPE unsigned long
#define TYPE_NAME UnsignedLong
#define TO_VALUE (strtoul (string, &end, 10))
#define PRE_CHECK_BLOCK ELEKTRA_TYPE_NEGATIVE_PRE_CHECK_BLOCK
#define PRE_CHECK_CONVERSION ELEKTRA_TYPE_NEGATIVE_PRE_CHECK
#define CHECK_CONVERSION ELEKTRA_TYPE_CHECK_CONVERSION
#include "macros/add_type.h"

#define TYPE long long
#define TYPE_NAME LongLong
#define TO_VALUE (strtoll (string, &end, 10))
#define CHECK_CONVERSION ELEKTRA_TYPE_CHECK_CONVERSION
#include "macros/add_type.h"

#define TYPE unsigned long long
#define TYPE_NAME UnsignedLongLong
#define TO_VALUE (strtoull (string, &end, 10))
#define PRE_CHECK_BLOCK ELEKTRA_TYPE_NEGATIVE_PRE_CHECK_BLOCK
#define PRE_CHECK_CONVERSION ELEKTRA_TYPE_NEGATIVE_PRE_CHECK
#define CHECK_CONVERSION ELEKTRA_TYPE_CHECK_CONVERSION
#include "macros/add_type.h"

#define TYPE float
#define TYPE_NAME Float
#define TO_VALUE (strtof (string, &end))
#define CHECK_CONVERSION ELEKTRA_TYPE_CHECK_CONVERSION
#include "macros/add_type.h"

#define TYPE double
#define TYPE_NAME Double
#define TO_VALUE (strtod (string, &end))
#define CHECK_CONVERSION ELEKTRA_TYPE_CHECK_CONVERSION
#include "macros/add_type.h"

// for kdb_*_t Types
#define TYPE kdb_boolean_t
#define TYPE_NAME KdbBoolean
#define TO_VALUE (!strcmp (string, "1"))
#include "macros/add_type.h"

#define TYPE kdb_char_t
#define TYPE_NAME KdbChar
#define TO_VALUE (string[0])
#include "macros/add_type.h"

#define TYPE kdb_octet_t
#define VALUE_TYPE unsigned int
#define TYPE_NAME KdbOctet
#define TO_VALUE (strtoul (string, &end, 10))
#define CHECK_CONVERSION ELEKTRA_TYPE_CHECK_CONVERSION_RANGE (value <= 255)
#include "macros/add_type.h"

#define TYPE kdb_short_t
#define VALUE_TYPE int
#define TYPE_NAME KdbShort
#define TO_VALUE (strtol (string, &end, 10))
#define CHECK_CONVERSION ELEKTRA_TYPE_CHECK_CONVERSION_RANGE (value <= SHRT_MAX && value >= SHRT_MIN)
#include "macros/add_type.h"

#define TYPE kdb_unsigned_short_t
#define VALUE_TYPE unsigned int
#define TYPE_NAME KdbUnsignedShort
#define TO_VALUE (strtoul (string, &end, 10))
#define CHECK_CONVERSION ELEKTRA_TYPE_CHECK_CONVERSION_RANGE (value <= USHRT_MAX)
#include "macros/add_type.h"

#define TYPE kdb_long_t
#define TYPE_NAME KdbLong
#define TO_VALUE (strtol (string, &end, 10))
#define CHECK_CONVERSION ELEKTRA_TYPE_CHECK_CONVERSION
#include "macros/add_type.h"

#define TYPE kdb_unsigned_long_t
#define TYPE_NAME KdbUnsignedLong
#define TO_VALUE (strtoul (string, &end, 10))
#define PRE_CHECK_BLOCK ELEKTRA_TYPE_NEGATIVE_PRE_CHECK_BLOCK
#define PRE_CHECK_CONVERSION ELEKTRA_TYPE_NEGATIVE_PRE_CHECK
#define CHECK_CONVERSION ELEKTRA_TYPE_CHECK_CONVERSION
#include "macros/add_type.h"

#define TYPE kdb_long_long_t
#define TYPE_NAME KdbLongLong
#define TO_VALUE (ELEKTRA_LONG_LONG_S (string, &end, 10))
#define CHECK_CONVERSION ELEKTRA_TYPE_CHECK_CONVERSION
#include "macros/add_type.h"

#define TYPE kdb_unsigned_long_long_t
#define TYPE_NAME KdbUnsignedLongLong
#define TO_VALUE (ELEKTRA_UNSIGNED_LONG_LONG_S (string, &end, 10))
#define PRE_CHECK_BLOCK ELEKTRA_TYPE_NEGATIVE_PRE_CHECK_BLOCK
#define PRE_CHECK_CONVERSION ELEKTRA_TYPE_NEGATIVE_PRE_CHECK
#define CHECK_CONVERSION ELEKTRA_TYPE_CHECK_CONVERSION
#include "macros/add_type.h"

#define TYPE kdb_float_t
#define TYPE_NAME KdbFloat
#define TO_VALUE (strtof (string, &end))
#define CHECK_CONVERSION ELEKTRA_TYPE_CHECK_CONVERSION
#include "macros/add_type.h"

#define TYPE kdb_double_t
#define TYPE_NAME KdbDouble
#define TO_VALUE (strtod (string, &end))
#define CHECK_CONVERSION ELEKTRA_TYPE_CHECK_CONVERSION
#include "macros/add_type.h"

#ifdef ELEKTRA_HAVE_KDB_LONG_DOUBLE
#define TYPE kdb_long_double_t
#define TYPE_NAME KdbLongDouble
#define TO_VALUE (strtold (string, &end))
#define CHECK_CONVERSION ELEKTRA_TYPE_CHECK_CONVERSION
#include "macros/add_type.h"
#endif // ELEKTRA_HAVE_KDB_LONG_DOUBLE

/**
 * @see kdbnotificationinternal.h ::ElektraNotificationPluginRegisterCallback
 */
int elektraInternalnotificationRegisterCallback (Plugin * handle, ElektraKey * key, ElektraNotificationChangeCallback callback, void * context)
{
	PluginState * pluginState = elektraPluginGetData (handle);
	ELEKTRA_ASSERT (pluginState != NULL, "plugin state was not initialized properly");

	KeyRegistration * registeredKey = elektraInternalnotificationAddNewRegistration (pluginState, key, callback, context, 0);
	if (registeredKey == NULL)
	{
		return 0;
	}

	return 1;
}

/**
 * @see kdbnotificationinternal.h ::ElektraNotificationPluginRegisterCallbackSameOrBelow
 */
int elektraInternalnotificationRegisterCallbackSameOrBelow (Plugin * handle, ElektraKey * key, ElektraNotificationChangeCallback callback,
							    void * context)
{
	PluginState * pluginState = elektraPluginGetData (handle);
	ELEKTRA_ASSERT (pluginState != NULL, "plugin state was not initialized properly");

	KeyRegistration * registeredKey = elektraInternalnotificationAddNewRegistration (pluginState, key, callback, context, 0);
	if (registeredKey == NULL)
	{
		return 0;
	}
	registeredKey->sameOrBelow = 1;

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
int elektraInternalnotificationGet (Plugin * handle, ElektraKeyset * returned, ElektraKey * parentKey)
{
	if (!elektraStrCmp (elektraKeyName (parentKey), "system:/elektra/modules/internalnotification"))
	{
		ElektraKeyset * contract = elektraKeysetNew (
			30,
			elektraKeyNew ("system:/elektra/modules/internalnotification", ELEKTRA_KEY_VALUE,
				"internalnotification plugin waits for your orders", ELEKTRA_KEY_END),
			elektraKeyNew ("system:/elektra/modules/internalnotification/exports", ELEKTRA_KEY_END),
			elektraKeyNew ("system:/elektra/modules/internalnotification/exports/get", ELEKTRA_KEY_FUNC, elektraInternalnotificationGet,
				ELEKTRA_KEY_END),
			elektraKeyNew ("system:/elektra/modules/internalnotification/exports/set", ELEKTRA_KEY_FUNC, elektraInternalnotificationSet,
				ELEKTRA_KEY_END),
			elektraKeyNew ("system:/elektra/modules/internalnotification/exports/open", ELEKTRA_KEY_FUNC, elektraInternalnotificationOpen,
				ELEKTRA_KEY_END),
			elektraKeyNew ("system:/elektra/modules/internalnotification/exports/close", ELEKTRA_KEY_FUNC, elektraInternalnotificationClose,
				ELEKTRA_KEY_END),

			// Export register* functions
			INTERNALNOTIFICATION_EXPORT_FUNCTION (Int), INTERNALNOTIFICATION_EXPORT_FUNCTION (UnsignedInt),
			INTERNALNOTIFICATION_EXPORT_FUNCTION (Long), INTERNALNOTIFICATION_EXPORT_FUNCTION (UnsignedLong),
			INTERNALNOTIFICATION_EXPORT_FUNCTION (LongLong), INTERNALNOTIFICATION_EXPORT_FUNCTION (UnsignedLongLong),
			INTERNALNOTIFICATION_EXPORT_FUNCTION (Float), INTERNALNOTIFICATION_EXPORT_FUNCTION (Double),

			// Export register* functions for kdb_*_t types
			INTERNALNOTIFICATION_EXPORT_FUNCTION (KdbBoolean), INTERNALNOTIFICATION_EXPORT_FUNCTION (KdbChar),
			INTERNALNOTIFICATION_EXPORT_FUNCTION (KdbOctet), INTERNALNOTIFICATION_EXPORT_FUNCTION (KdbShort),
			INTERNALNOTIFICATION_EXPORT_FUNCTION (KdbUnsignedShort), INTERNALNOTIFICATION_EXPORT_FUNCTION (KdbLong),
			INTERNALNOTIFICATION_EXPORT_FUNCTION (KdbUnsignedLong), INTERNALNOTIFICATION_EXPORT_FUNCTION (KdbLongLong),
			INTERNALNOTIFICATION_EXPORT_FUNCTION (KdbUnsignedLongLong), INTERNALNOTIFICATION_EXPORT_FUNCTION (KdbFloat),
			INTERNALNOTIFICATION_EXPORT_FUNCTION (KdbDouble),
#ifdef ELEKTRA_HAVE_KDB_LONG_DOUBLE
			INTERNALNOTIFICATION_EXPORT_FUNCTION (KdbLongDouble),
#endif // ELEKTRA_HAVE_KDB_LONG_DOUBLE

			elektraKeyNew ("system:/elektra/modules/internalnotification/exports/registerCallback", ELEKTRA_KEY_FUNC,
				elektraInternalnotificationRegisterCallback, ELEKTRA_KEY_END),
			elektraKeyNew ("system:/elektra/modules/internalnotification/exports/registerCallbackSameOrBelow", ELEKTRA_KEY_FUNC,
				elektraInternalnotificationRegisterCallbackSameOrBelow, ELEKTRA_KEY_END),
			elektraKeyNew ("system:/elektra/modules/internalnotification/exports/setConversionErrorCallback", ELEKTRA_KEY_FUNC,
				elektraInternalnotificationSetConversionErrorCallback, ELEKTRA_KEY_END),

#include ELEKTRA_README

			elektraKeyNew ("system:/elektra/modules/internalnotification/infos/version", ELEKTRA_KEY_VALUE, PLUGINVERSION, ELEKTRA_KEY_END), ELEKTRA_KS_END);
		elektraKeysetAppend (returned, contract);
		elektraKeysetDel (contract);

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
int elektraInternalnotificationSet (Plugin * handle, ElektraKeyset * returned, ElektraKey * parentKey ELEKTRA_UNUSED)
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
int elektraInternalnotificationOpen (Plugin * handle, ElektraKey * parentKey ELEKTRA_UNUSED)
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
		pluginState->conversionErrorCallback = NULL;
		pluginState->conversionErrorCallbackContext = NULL;
	}

	ElektraKeyset * config = elektraPluginGetConfig (handle);
	ElektraKeyset * global = elektraPluginGetGlobalKeySet (handle);

	if (global != NULL)
	{
		elektraKeysetAppendKey (global,
			     elektraKeyNew ("system:/elektra/notification/callback", ELEKTRA_KEY_FUNC, elektraInternalnotificationDoUpdate, ELEKTRA_KEY_END));

		ElektraKey * contextKey = elektraKeysetLookupByName (config, "/context", 0);
		if (contextKey != NULL)
		{
			ElektraNotificationCallbackContext * context = *(ElektraNotificationCallbackContext **) elektraKeyValue (contextKey);
			elektraKeysetAppendKey (global, elektraKeyNew ("system:/elektra/notification/context", ELEKTRA_KEY_BINARY, ELEKTRA_KEY_SIZE, sizeof (context),
						     ELEKTRA_KEY_VALUE, &context, ELEKTRA_KEY_END));
		}
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
int elektraInternalnotificationClose (Plugin * handle, ElektraKey * parentKey ELEKTRA_UNUSED)
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
			if (current->freeContext)
			{
				elektraFree (current->context);
			}
			elektraFree (current);

			current = next;
		}

		// Free list pointer
		elektraFree (pluginState);
		elektraPluginSetData (handle, NULL);
	}

	ElektraKeyset * config = elektraPluginGetConfig (handle);
	ElektraKey * contextKey = elektraKeysetLookupByName (config, "/context", ELEKTRA_KDB_O_POP);
	if (contextKey != NULL)
	{
		ElektraNotificationCallbackContext * context = *(ElektraNotificationCallbackContext **) elektraKeyValue (contextKey);
		elektraFree (context);
	}
	elektraKeyDel (contextKey);

	return 1;
}

Plugin * ELEKTRA_PLUGIN_EXPORT
{
	// clang-format off
	return elektraPluginExport ("internalnotification",
		ELEKTRA_PLUGIN_GET,	&elektraInternalnotificationGet,
		ELEKTRA_PLUGIN_SET,	&elektraInternalnotificationSet,
		ELEKTRA_PLUGIN_OPEN, &elektraInternalnotificationOpen,
		ELEKTRA_PLUGIN_CLOSE, &elektraInternalnotificationClose,
		ELEKTRA_PLUGIN_END);
}
