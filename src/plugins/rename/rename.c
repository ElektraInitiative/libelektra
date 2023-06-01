/**
 * @file
 *
 * @brief Implementation of plugin rename
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#include "./rename.h"

#ifndef HAVE_KDBCONFIG
#include <internal/config.h>
#endif


#include <ctype.h>
#include <errno.h>
#include <internal/utility/old_helper.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>

#include <internal/kdbprivate.h>

#define ELEKTRA_ORIGINAL_NAME_META "origname"
#define TOLOWER (-1)
#define TOUPPER 1
#define UNCHNGD 0
#define KEYNAME 2

static void doConversion (char * newName, int levels, const int toCase)
{
	int (*conversion) (int);

	if (toCase == TOUPPER)
	{
		conversion = toupper;
	}
	else
	{
		conversion = tolower;
	}
	char * returnName = elektraCalloc (strlen (newName) + 1);
	if (levels == 0)
	{
		unsigned int i = 0;
		for (; i < strlen (newName); ++i)
		{
			returnName[i] = conversion (newName[i]);
		}
	}
	else
	{
		short levelCount = 0;
		int i = 0;
		for (i = strlen (newName); i >= 0 && levelCount < levels; --i)
		{
			if (newName[i] == '/')
			{
				++levelCount;
				returnName[i] = newName[i];
				continue;
			}
			returnName[i] = conversion (newName[i]);
		}
		for (; i >= 0; --i)
		{
			returnName[i] = newName[i];
		}
	}
	strncpy (newName, returnName, strlen (newName));
	free (returnName);
}

Key * elektraKeyCreateNewName (const Key * key, const Key * parentKey, const char * cutPath, const char * replaceWith,
			       const char * toUpperPath, const char * toLowerPath, const int initialConversion)
{
	size_t addToLen = 0;
	if (replaceWith != NULL) addToLen = strlen (replaceWith);

	size_t maxNewLength = strlen (keyName (key)) + addToLen;

	char * newName = elektraCalloc (maxNewLength + 1);
	short replace = 0;

	char * parentKeyName = elektraMalloc (keyGetNameSize (parentKey));
	keyGetName (parentKey, parentKeyName, keyGetNameSize (parentKey));
	char * curKeyName = elektraMalloc (keyGetNameSize (key));
	keyGetName (key, curKeyName, keyGetNameSize (key));

	char * afterParentString = curKeyName + (strlen (parentKeyName));
	char * ptr;

	if (initialConversion != UNCHNGD)
	{
		doConversion (afterParentString, 0, initialConversion);
		replace = 1;
	}

	if (cutPath && (cutPath[0] != '/') && ((ptr = strstr (afterParentString, cutPath)) != NULL))
	{
		strncpy (newName, afterParentString, (ptr - afterParentString));
		if (replaceWith)
		{
			strncpy (newName + strlen (newName), replaceWith, elektraStrLen (replaceWith));
		}
		strncat (newName, ptr + (strlen (cutPath)), strlen (afterParentString) - strlen (cutPath));
		replace = 1;
	}
	else
	{
		strncpy (newName, afterParentString, elektraStrLen (afterParentString));
	}
	int toLower = toLowerPath ? atoi (toLowerPath) : 0;
	int toUpper = toUpperPath ? atoi (toUpperPath) : 0;

	if (strlen (newName) > 0)
	{
		if (toUpperPath && toLowerPath)
		{
			if (toUpper < toLower)
			{
				doConversion (newName, toLower, TOLOWER);
				doConversion (newName, toUpper, TOUPPER);
			}
			else
			{
				doConversion (newName, toUpper, TOUPPER);
				doConversion (newName, toLower, TOLOWER);
			}
			replace = 1;
		}
		else if (toUpperPath)
		{
			doConversion (newName, toUpper, TOUPPER);
			replace = 1;
		}
		else if (toLowerPath)
		{
			doConversion (newName, toLower, TOLOWER);
			replace = 1;
		}
	}
	elektraFree (parentKeyName);
	elektraFree (curKeyName);
	if (replace)
	{
		Key * result = keyDup (key, KEY_CP_ALL);
		keySetName (result, keyName (parentKey));
		keyAddName (result, newName);
		elektraFree (newName);
		return result;
	}
	elektraFree (newName);
	return 0;
}

static Key * renameGet (Key * key, Key * parentKey, Key * cutConfig, Key * replaceWithConfig, Key * toUpperConfig, Key * toLowerConfig,
			Key * getCase)
{
	char * cutPath = 0;
	char * replaceWith = 0;
	char * toUpperPath = 0;
	char * toLowerPath = 0;
	const Key * cutMeta = keyGetMeta (key, "rename/cut");
	const Key * toMeta = keyGetMeta (key, "rename/to");
	const Key * toUpperMeta = keyGetMeta (key, "rename/toupper");
	const Key * toLowerMeta = keyGetMeta (key, "rename/tolower");

	int initialConversion = 0;
	if (getCase)
	{
		const char * str = keyString (getCase);
		if (!strcmp (str, "toupper"))
		{
			initialConversion = TOUPPER;
		}
		else if (!strcmp (str, "tolower"))
		{
			initialConversion = TOLOWER;
		}
		else
		{
			initialConversion = UNCHNGD;
		}
	}
	/* if the meta config exists, it takes precedence over the global config */
	if (cutMeta)
		cutPath = (char *) keyString (cutMeta);
	else if (cutConfig)
		cutPath = (char *) keyString (cutConfig);
	if (toMeta)
		replaceWith = (char *) keyString (toMeta);
	else if (replaceWithConfig)
		replaceWith = (char *) keyString (replaceWithConfig);
	if (toUpperMeta)
		toUpperPath = (char *) keyString (toUpperMeta);
	else if (toUpperConfig)
		toUpperPath = (char *) keyString (toUpperConfig);
	if (toLowerMeta)
		toLowerPath = (char *) keyString (toLowerMeta);
	else if (toLowerConfig)
		toLowerPath = (char *) keyString (toLowerConfig);

	return elektraKeyCreateNewName (key, parentKey, cutPath, replaceWith, toUpperPath, toLowerPath, initialConversion);
}

static Key * restoreKeyName (Key * key, const Key * parentKey, const Key * configKey)
{
	const Key * origNameKey = keyGetMeta (key, ELEKTRA_ORIGINAL_NAME_META);
	if (origNameKey)
	{
		if (strcmp (keyString (origNameKey), keyName (key)))
		{
			Key * result = keyDup (key, KEY_CP_ALL);
			keySetName (result, keyString (origNameKey));
			keySetMeta (result, ELEKTRA_ORIGINAL_NAME_META, 0);

			return result;
		}
	}
	else
	{
		if (configKey)
		{
			Key * result = keyDup (key, KEY_CP_ALL);
			keySetName (result, keyName (parentKey));
			keyAddName (result, keyString (configKey));

			if (keyGetNameSize (key) > keyGetNameSize (parentKey))
			{
				/* this calculation does not work for the parent key but is also not needed */
				const char * relativePath = keyName (key) + keyGetNameSize (parentKey);
				keyAddName (result, relativePath);
			}

			return result;
		}
	}

	return 0;
}

int elektraRenameGet (Plugin * handle, KeySet * returned, Key * parentKey)
{
	/* configuration only */
	if (!strcmp (keyName (parentKey), "system:/elektra/modules/rename"))
	{
		KeySet * info =
#include "./contract.h"

			ksAppend (returned, info);
		ksDel (info);
		return 1;
	}


	KeySet * config = elektraPluginGetConfig (handle);
	KeySet * iterateKs = ksDup (returned);

	Key * cutConfig = ksLookupByName (config, "/cut", KDB_O_NONE);
	Key * toUpper = ksLookupByName (config, "/toupper", KDB_O_NONE);
	Key * toLower = ksLookupByName (config, "/tolower", KDB_O_NONE);
	Key * replaceWith = ksLookupByName (config, "/replacewith", KDB_O_NONE);
	Key * getCase = ksLookupByName (config, "/get/case", KDB_O_NONE);


	for (elektraCursor it = 0; it < ksGetSize (iterateKs); ++it)
	{
		Key * key = ksAtCursor (iterateKs, it);
		Key * renamedKey = renameGet (key, parentKey, cutConfig, replaceWith, toUpper, toLower, getCase);

		if (renamedKey)
		{
			keySetMeta (renamedKey, ELEKTRA_ORIGINAL_NAME_META, keyName (key));
			ksLookup (returned, key, KDB_O_POP);
			keyDel (key);

			/*
			 * if the parentKey is replaced by a rename operation
			 * make sure that we do not loose its reference (ksAppendKey
			 * would otherwise delete it)
			 */
			if (keyCmp (renamedKey, parentKey) == 0)
			{
				/* make sure the parent key is not deleted */
				keyIncRef (parentKey);
				ksAppendKey (returned, renamedKey);
				keyDecRef (parentKey);
			}
			else
			{
				ksAppendKey (returned, renamedKey);
			}
		}
		else
		{
			keySetMeta (key, ELEKTRA_ORIGINAL_NAME_META, keyName (key));
		}
	}

	/* make sure the parent key is not deleted */
	keyIncRef (parentKey);
	ksDel (iterateKs);
	keyDecRef (parentKey);

	return 1; /* success */
}

int elektraRenameSet (Plugin * handle, KeySet * returned, Key * parentKey)
{

	KeySet * iterateKs = ksDup (returned);

	KeySet * config = elektraPluginGetConfig (handle);
	Key * cutConfig = ksLookupByName (config, "/cut", KDB_O_NONE);

	Key * setCase = ksLookupByName (config, "/set/case", KDB_O_NONE);

	int writeConversion = 0;
	if (setCase)
	{
		const char * str = keyString (setCase);
		if (!strcmp (str, "toupper"))
		{
			writeConversion = TOUPPER;
		}
		else if (!strcmp (str, "tolower"))
		{
			writeConversion = TOLOWER;
		}
		else if (!strcmp (str, "keyname"))
		{
			writeConversion = KEYNAME;
		}
		else
		{
			writeConversion = UNCHNGD;
		}
	}


	char * parentKeyName = elektraMalloc (keyGetNameSize (parentKey));
	keyGetName (parentKey, parentKeyName, keyGetNameSize (parentKey));

	for (elektraCursor it = 0; it < ksGetSize (iterateKs); ++it)
	{
		Key * key = ksAtCursor (iterateKs, it);
		Key * renamedKey = NULL;
		if (writeConversion != KEYNAME)
		{
			renamedKey = restoreKeyName (key, parentKey, cutConfig);

			if (!renamedKey) renamedKey = keyDup (key, KEY_CP_ALL);
			if (writeConversion == TOUPPER || writeConversion == TOLOWER)
			{
				char * curKeyName = elektraMalloc (keyGetNameSize (renamedKey));
				keyGetName (renamedKey, curKeyName, keyGetNameSize (renamedKey));

				char * afterParentString = curKeyName + (strlen (parentKeyName));

				doConversion (afterParentString, 0, writeConversion);

				keySetName (renamedKey, curKeyName);
				elektraFree (curKeyName);
			}
			/*
			 * if something is restored from the parentKey, do
			 * not delete the parentKey (might cause troubles)
			 */
			if (keyCmp (key, parentKey) != 0)
			{
				keyDel (ksLookup (returned, key, KDB_O_POP));
			}
			ksAppendKey (returned, renamedKey);
		}
		else
		{
			if (keyCmp (key, parentKey) != 0)
			{
				keyDel (ksLookupByName (returned, keyString (keyGetMeta (key, ELEKTRA_ORIGINAL_NAME_META)), KDB_O_POP));
			}
			ksAppendKey (returned, key);
		}
	}

	keyIncRef (parentKey);
	ksDel (iterateKs);
	keyDecRef (parentKey);

	elektraFree (parentKeyName);
	return 1; /* success */
}

Plugin * ELEKTRA_PLUGIN_EXPORT
{
	// clang-format off
	return elektraPluginExport("rename",
		ELEKTRA_PLUGIN_GET,	&elektraRenameGet,
		ELEKTRA_PLUGIN_SET,	&elektraRenameSet,
		ELEKTRA_PLUGIN_END);
}
