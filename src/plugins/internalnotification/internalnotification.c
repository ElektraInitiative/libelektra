/**
 * @file
 *
 * @brief Source for internalnotification plugin
 *
 * @copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 *
 */

#include "internalnotification.h"

#if PLUGIN_INTERNALNOTIFICATION_VERBOSE
#include <stdio.h>
#endif

#include <errno.h>
#include <stdlib.h>

#include <kdbhelper.h>

/**
 * Structure for registered key variable pairs
 */
struct _KeyRegistration
{
	char * name;
	int * variable;
	struct _KeyRegistration * next;
};
typedef struct _KeyRegistration KeyRegistration;

/**
 * Structure for internal plugin state
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
	KeyRegistration * item = elektraMalloc (sizeof *item);
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
 * @param pluginState		internal plugin data structure
 * @param keySet 				key set retrieved from hooks
 *                   		(e.g. elektraInternalnotificationGet or elektraInternalnotificationSet)
 *
 */
static void elektraInternalnotificationUpdateRegisteredKeys (PluginState * pluginState, KeySet * keySet)
{
	KeyRegistration * registeredKey = pluginState->head;
	while (registeredKey != NULL)
	{
#if PLUGIN_INTERNALNOTIFICATION_VERBOSE
		fprintf (stderr, "elektraInternalnotificationUpdateRegisteredKeys looking up registeredKey=%s\n", registeredKey->name);
#endif

		Key * key = ksLookupByName (keySet, registeredKey->name, 0);
		if (key != 0)
		{
#if PLUGIN_INTERNALNOTIFICATION_VERBOSE
			fprintf (stderr,
				 "elektraInternalnotificationUpdateRegisteredKeys found registeredKey=%s; updating variable=%p with string "
				 "value %s\n",
				 registeredKey->name, (void *)registeredKey->variable, keyString (key));
#endif
			// Convert string value to long
			char * end;
			errno = 0;
			long int value = strtol (keyString (key), &end, 10);
			// Update variable if conversion was successful and did not exceed integer range
			if (*end == 0 && errno == 0 && value <= INT_MAX && value >= INT_MIN)
			{
				*(registeredKey->variable) = value;
			}
#if PLUGIN_INTERNALNOTIFICATION_VERBOSE
			else
			{
				fprintf (stderr,
					 "elektraInternalnotificationUpdateRegisteredKeys conversion failed! keyString=%s, *end=%c, "
					 "errno=%d, value=%ld\n",
					 keyString (key), *end, errno, value);
			}
#endif
		}

		registeredKey = registeredKey->next;
	}
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
	const char * parentKeyName = keyName (parentKey);

	if (!elektraStrCmp (parentKeyName, "system/elektra/modules/internalnotification"))
	{
		KeySet * contract = ksNew (
			30, keyNew ("system/elektra/modules/internalnotification", KEY_VALUE,
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
			keyNew ("system/elektra/modules/internalnotification/exports/elektraInternalnotificationRegisterInt", KEY_FUNC,
				elektraInternalnotificationRegisterInt, KEY_END),
			keyNew ("system/elektra/modules/internalnotification/exports/handle", KEY_BINARY, KEY_SIZE, sizeof handle,
				KEY_VALUE, &handle, KEY_END),

#include ELEKTRA_README (internalnotification)

			keyNew ("system/elektra/modules/internalnotification/infos/version", KEY_VALUE, PLUGINVERSION, KEY_END), KS_END);
		ksAppend (returned, contract);
		ksDel (contract);

		return 1;
	}
#if PLUGIN_INTERNALNOTIFICATION_VERBOSE
	fprintf (stderr, "elektraInternalnotificationGet keyName=%s\n", parentKeyName);
#endif

	PluginState * pluginState = elektraPluginGetData (handle);
	elektraInternalnotificationUpdateRegisteredKeys (pluginState, returned);

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
int elektraInternalnotificationSet (Plugin * handle, KeySet * returned, Key * parentKey)
{

#if PLUGIN_INTERNALNOTIFICATION_VERBOSE
	fprintf (stderr, "elektraInternalnotificationSet keyName=%s\n", keyName (parentKey));
#endif

	PluginState * pluginState = elektraPluginGetData (handle);
	elektraInternalnotificationUpdateRegisteredKeys (pluginState, returned);

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
#if PLUGIN_INTERNALNOTIFICATION_VERBOSE
	fprintf (stderr, "elektraInternalnotificationOpen\n");
#endif

	PluginState * pluginState = elektraPluginGetData (handle);
	if (pluginState == NULL)
	{
		pluginState = elektraMalloc (sizeof *pluginState);
		if (pluginState == NULL)
		{
#if PLUGIN_INTERNALNOTIFICATION_VERBOSE
			fprintf (stderr, "elektraInternalnotificationOpen elektraMalloc failed!\n");
#endif
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
#if PLUGIN_INTERNALNOTIFICATION_VERBOSE
	fprintf (stderr, "elektraInternalnotificationClose\n");
#endif

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
			elektraFree (current);

			current = next;
		}

		// Free list pointer
		elektraFree (pluginState);
		elektraPluginSetData (handle, NULL);
	}

	return 1;
}

/**
 * Subscribe for automatic updates to a given integer variable when the given
 * key value has changed.
 *
 * @param  handle   plugin handle
 * @param  variable integer variable
 * @param  key      key to watch for changes
 *
 * @retval 1 on success
 * @retval -1 on failure
 */
int elektraInternalnotificationRegisterInt (Plugin * handle, int * variable, Key * key)
{
	PluginState * pluginState = elektraPluginGetData (handle);

#if PLUGIN_INTERNALNOTIFICATION_VERBOSE
	fprintf (stderr, "elektraInternalnotificationRegisterInt variable=%p, keyName=%s\n", (void *)variable, keyName (key));
#endif

	KeyRegistration * registeredKey = elektraInternalnotificationAddNewRegistration (pluginState);
	if (registeredKey == NULL)
	{
		return -1;
	}

	// Copy key name
	size_t nameBufferSize = keyGetNameSize (key);
	char * nameBuffer = elektraMalloc (nameBufferSize);
	if (nameBuffer == NULL)
	{
		return -1;
	}
	ssize_t result = keyGetName (key, nameBuffer, nameBufferSize);
	if (result == 1 || result == -1)
	{
		return -1;
	}

	// Save key registration
	registeredKey->name = nameBuffer;
	registeredKey->variable = variable;

#if PLUGIN_INTERNALNOTIFICATION_VERBOSE
	fprintf (stderr, "elektraInternalnotificationRegisterInt registered key (name=%s, variable=%p)\n", registeredKey->name,
		 (void *)registeredKey->variable);
#endif

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
