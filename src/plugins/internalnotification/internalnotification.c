/**
 * @file
 *
 * @brief Source for internalnotification plugin
 *
 * @copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 *
 */

#include "internalnotification.h"

#include <errno.h>
#include <stdlib.h>

#include <kdbassert.h>
#include <kdbhelper.h>
#include <kdblogger.h>

typedef enum {
	TYPE_INT = 1 << 0,
	TYPE_CALLBACK = 1 << 1,
} KeyRegistrationType;

/**
 * Structure for registered key variable pairs
 * @internal
 */
struct _KeyRegistration
{
	char * name;
	KeyRegistrationType type;
	char * lastValue;
	union
	{
		int * variable;
		ElektraNotificationChangeCallback callback;
	} ref;
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
 * Creates a new KeyRegistration structure and appends it at the end of the registration list
 * @internal
 *
 * @param pluginState		internal plugin data structure
 *
 * @return pointer to created KeyRegistration structure or NULL if memory allocation failed
 */
static KeyRegistration * elektraInternalnotificationAddNewRegistration (PluginState * pluginState)
{
	KeyRegistration * item = elektraCalloc (sizeof *item);
	if (item == NULL)
	{
		return NULL;
	}
	item->next = NULL;

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
			// Perform actions depending on type
			switch (registeredKey->type)
			{
			case TYPE_INT:
				ELEKTRA_LOG_DEBUG ("found registeredKey=%s; updating variable=%p with string value \"%s\"",
						   registeredKey->name, (void *)registeredKey->ref.variable, keyString (key));

				// Convert string value to long
				char * end;
				errno = 0;
				long int value = strtol (keyString (key), &end, 10);
				// Update variable if conversion was successful and did not exceed integer range
				if (*end == 0 && errno == 0 && value <= INT_MAX && value >= INT_MIN)
				{
					*(registeredKey->ref.variable) = value;
				}
				else
				{
					ELEKTRA_LOG_WARNING ("conversion failed! keyString=\"%s\" *end=%c, errno=%d, value=%ld",
							     keyString (key), *end, errno, value);
				}
				break;
			case TYPE_CALLBACK:
				ELEKTRA_LOG_DEBUG ("found registeredKey=%s; invoking callback", registeredKey->name);
				ElektraNotificationChangeCallback callback =
					*(ElektraNotificationChangeCallback)registeredKey->ref.callback;
				callback (key);
				break;
			}
		}

		registeredKey = registeredKey->next;
	}
}


/**
 * Subscribe for automatic updates to a given integer variable when the given
 * key value has changed.
 *
 * Implementation of ElektraNotificationPluginRegisterInt()
 * @see kdbnotificationplugin.h
 *
 * @param  handle   plugin handle
 * @param  variable integer variable
 * @param  key      key to watch for changes
 *
 * @retval 1 on success
 * @retval 0 on failure
 */
int elektraInternalnotificationRegisterInt (Plugin * handle, Key * key, int * variable)
{
	PluginState * pluginState = elektraPluginGetData (handle);
	ELEKTRA_ASSERT (pluginState != NULL, "plugin state was not initialized properly");

	KeyRegistration * registeredKey = elektraInternalnotificationAddNewRegistration (pluginState);
	if (registeredKey == NULL)
	{
		return 0;
	}

	// Copy key name
	size_t nameBufferSize = keyGetNameSize (key);
	char * nameBuffer = elektraMalloc (nameBufferSize);
	if (nameBuffer == NULL)
	{
		return 0;
	}
	ssize_t result = keyGetName (key, nameBuffer, nameBufferSize);
	if (result == 1 || result == -1)
	{
		return 0;
	}

	// Save key registration
	registeredKey->name = nameBuffer;
	registeredKey->type = TYPE_INT;
	registeredKey->ref.variable = variable;

	return 1;
}

/**
 * Subscribe for updates via callback when a given key value is changed.
 * key value has changed.
 *
 * Implementation of ElektraNotificationPluginRegisterCallback()
 * @see kdbnotificationplugin.h
 *
 * @param  handle   plugin handle
 * @param  key      key to watch for changes
 * @param  callback callback function
 *
 * @retval 1 on success
 * @retval 0 on failure
 */
int elektraInternalnotificationRegisterCallback (Plugin * handle, Key * key, ElektraNotificationChangeCallback callback)
{
	PluginState * pluginState = elektraPluginGetData (handle);
	ELEKTRA_ASSERT (pluginState != NULL, "plugin state was not initialized properly");

	KeyRegistration * registeredKey = elektraInternalnotificationAddNewRegistration (pluginState);
	if (registeredKey == NULL)
	{
		return 0;
	}

	// Copy key name
	size_t nameBufferSize = keyGetNameSize (key);
	char * nameBuffer = elektraMalloc (nameBufferSize);
	if (nameBuffer == NULL)
	{
		return 0;
	}
	ssize_t result = keyGetName (key, nameBuffer, nameBufferSize);
	if (result == 1 || result == -1)
	{
		return 0;
	}

	// Save key registration
	registeredKey->name = nameBuffer;
	registeredKey->type = TYPE_CALLBACK;
	registeredKey->ref.callback = callback;

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

			// Export registerInt function and required plugin handle
			keyNew ("system/elektra/modules/internalnotification/exports/registerInt", KEY_FUNC,
				elektraInternalnotificationRegisterInt, KEY_END),
			keyNew ("system/elektra/modules/internalnotification/exports/registerCallback", KEY_FUNC,
				elektraInternalnotificationRegisterCallback, KEY_END),
			keyNew ("system/elektra/modules/internalnotification/exports/handle", KEY_BINARY, KEY_SIZE, sizeof handle,
				KEY_VALUE, &handle, KEY_END),

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
