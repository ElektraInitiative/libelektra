/**
 * @file
 *
 * @brief Implementation of plugin rename
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#include "rename.h"

#ifndef HAVE_KDBCONFIG
#include "kdbconfig.h"
#endif


#include <ctype.h>
#include <errno.h>
#include <kdbhelper.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>

#include <kdbprivate.h> // for access to sync bit (keyClearSync)

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

ElektraKey * elektraKeyCreateNewName (const ElektraKey * key, const ElektraKey * parentKey, const char * cutPath, const char * replaceWith,
			       const char * toUpperPath, const char * toLowerPath, const int initialConversion)
{
	size_t addToLen = 0;
	if (replaceWith != NULL) addToLen = strlen (replaceWith);

	size_t maxNewLength = strlen (elektraKeyName (key)) + addToLen;

	char * newName = elektraCalloc (maxNewLength + 1);
	short replace = 0;

	char * parentKeyName = elektraMalloc (elektraKeyGetNameSize (parentKey));
	elektraKeyGetName (parentKey, parentKeyName, elektraKeyGetNameSize (parentKey));
	char * curKeyName = elektraMalloc (elektraKeyGetNameSize (key));
	elektraKeyGetName (key, curKeyName, elektraKeyGetNameSize (key));

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
		ElektraKey * result = elektraKeyDup (key, ELEKTRA_KEY_CP_ALL);
		elektraKeySetName (result, elektraKeyName (parentKey));
		elektraKeyAddName (result, newName);
		elektraFree (newName);
		return result;
	}
	elektraFree (newName);
	return 0;
}

static ElektraKey * renameGet (ElektraKey * key, ElektraKey * parentKey, ElektraKey * cutConfig, ElektraKey * replaceWithConfig, ElektraKey * toUpperConfig, ElektraKey * toLowerConfig,
			ElektraKey * getCase)
{
	char * cutPath = 0;
	char * replaceWith = 0;
	char * toUpperPath = 0;
	char * toLowerPath = 0;
	const ElektraKey * cutMeta = elektraKeyGetMeta (key, "rename/cut");
	const ElektraKey * toMeta = elektraKeyGetMeta (key, "rename/to");
	const ElektraKey * toUpperMeta = elektraKeyGetMeta (key, "rename/toupper");
	const ElektraKey * toLowerMeta = elektraKeyGetMeta (key, "rename/tolower");

	int initialConversion = 0;
	if (getCase)
	{
		const char * str = elektraKeyString (getCase);
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
		cutPath = (char *) elektraKeyString (cutMeta);
	else if (cutConfig)
		cutPath = (char *) elektraKeyString (cutConfig);
	if (toMeta)
		replaceWith = (char *) elektraKeyString (toMeta);
	else if (replaceWithConfig)
		replaceWith = (char *) elektraKeyString (replaceWithConfig);
	if (toUpperMeta)
		toUpperPath = (char *) elektraKeyString (toUpperMeta);
	else if (toUpperConfig)
		toUpperPath = (char *) elektraKeyString (toUpperConfig);
	if (toLowerMeta)
		toLowerPath = (char *) elektraKeyString (toLowerMeta);
	else if (toLowerConfig)
		toLowerPath = (char *) elektraKeyString (toLowerConfig);

	return elektraKeyCreateNewName (key, parentKey, cutPath, replaceWith, toUpperPath, toLowerPath, initialConversion);
}

static ElektraKey * restoreKeyName (ElektraKey * key, const ElektraKey * parentKey, const ElektraKey * configKey)
{
	const ElektraKey * origNameKey = elektraKeyGetMeta (key, ELEKTRA_ORIGINAL_NAME_META);
	if (origNameKey)
	{
		if (strcmp (elektraKeyString (origNameKey), elektraKeyName (key)))
		{
			int hasSync = elektraKeyNeedSync (key); // test_bit(key->flags, KEY_FLAG_SYNC);
			ElektraKey * result = elektraKeyDup (key, ELEKTRA_KEY_CP_ALL);
			elektraKeySetName (result, elektraKeyString (origNameKey));
			elektraKeySetMeta (result, ELEKTRA_ORIGINAL_NAME_META, 0);

			if (!hasSync)
			{
				elektraKeyClearSync (result);
			}
			return result;
		}
	}
	else
	{
		if (configKey)
		{
			int hasSync = elektraKeyNeedSync (key); // test_bit(key->flags, KEY_FLAG_SYNC);
			ElektraKey * result = elektraKeyDup (key, ELEKTRA_KEY_CP_ALL);
			elektraKeySetName (result, elektraKeyName (parentKey));
			elektraKeyAddName (result, elektraKeyString (configKey));

			if (elektraKeyGetNameSize (key) > elektraKeyGetNameSize (parentKey))
			{
				/* this calculation does not work for the parent key but is also not needed */
				const char * relativePath = elektraKeyName (key) + elektraKeyGetNameSize (parentKey);
				elektraKeyAddName (result, relativePath);
			}

			if (!hasSync)
			{
				elektraKeyClearSync (result);
			}
			return result;
		}
	}

	return 0;
}

int elektraRenameGet (Plugin * handle, ElektraKeyset * returned, ElektraKey * parentKey)
{
	/* configuration only */
	if (!strcmp (elektraKeyName (parentKey), "system:/elektra/modules/rename"))
	{
		ElektraKeyset * info =
#include "contract.h"

			elektraKeysetAppend (returned, info);
		elektraKeysetDel (info);
		return 1;
	}


	ElektraKeyset * config = elektraPluginGetConfig (handle);
	ElektraKeyset * iterateKs = elektraKeysetDup (returned);

	elektraKeysetRewind (iterateKs);

	ElektraKey * cutConfig = elektraKeysetLookupByName (config, "/cut", ELEKTRA_KDB_O_NONE);
	ElektraKey * toUpper = elektraKeysetLookupByName (config, "/toupper", ELEKTRA_KDB_O_NONE);
	ElektraKey * toLower = elektraKeysetLookupByName (config, "/tolower", ELEKTRA_KDB_O_NONE);
	ElektraKey * replaceWith = elektraKeysetLookupByName (config, "/replacewith", ELEKTRA_KDB_O_NONE);
	ElektraKey * getCase = elektraKeysetLookupByName (config, "/get/case", ELEKTRA_KDB_O_NONE);


	ElektraKey * key;
	while ((key = elektraKeysetNext (iterateKs)) != 0)
	{

		ElektraKey * renamedKey = renameGet (key, parentKey, cutConfig, replaceWith, toUpper, toLower, getCase);

		if (renamedKey)
		{
			elektraKeySetMeta (renamedKey, ELEKTRA_ORIGINAL_NAME_META, elektraKeyName (key));
			elektraKeysetLookup (returned, key, ELEKTRA_KDB_O_POP);
			elektraKeyDel (key);

			/*
			 * if the parentKey is replaced by a rename operation
			 * make sure that we do not loose its reference (ksAppendKey
			 * would otherwise delete it)
			 */
			if (elektraKeyCmp (renamedKey, parentKey) == 0)
			{
				/* make sure the parent key is not deleted */
				elektraKeyIncRef (parentKey);
				elektraKeysetAppendKey (returned, renamedKey);
				elektraKeyDecRef (parentKey);
			}
			else
			{
				elektraKeysetAppendKey (returned, renamedKey);
			}
		}
		else
		{
			elektraKeySetMeta (key, ELEKTRA_ORIGINAL_NAME_META, elektraKeyName (key));
		}
	}

	/* make sure the parent key is not deleted */
	elektraKeyIncRef (parentKey);
	elektraKeysetDel (iterateKs);
	elektraKeyDecRef (parentKey);

	return 1; /* success */
}

int elektraRenameSet (Plugin * handle, ElektraKeyset * returned, ElektraKey * parentKey)
{

	ElektraKeyset * iterateKs = elektraKeysetDup (returned);

	ElektraKeyset * config = elektraPluginGetConfig (handle);
	ElektraKey * cutConfig = elektraKeysetLookupByName (config, "/cut", ELEKTRA_KDB_O_NONE);

	ElektraKey * setCase = elektraKeysetLookupByName (config, "/set/case", ELEKTRA_KDB_O_NONE);

	int writeConversion = 0;
	if (setCase)
	{
		const char * str = elektraKeyString (setCase);
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
	elektraKeysetRewind (iterateKs);
	ElektraKey * key;
	char * parentKeyName = elektraMalloc (elektraKeyGetNameSize (parentKey));
	elektraKeyGetName (parentKey, parentKeyName, elektraKeyGetNameSize (parentKey));
	while ((key = elektraKeysetNext (iterateKs)) != 0)
	{
		ElektraKey * renamedKey = NULL;
		if (writeConversion != KEYNAME)
		{
			renamedKey = restoreKeyName (key, parentKey, cutConfig);

			if (!renamedKey) renamedKey = elektraKeyDup (key, ELEKTRA_KEY_CP_ALL);
			if (writeConversion == TOUPPER || writeConversion == TOLOWER)
			{
				char * curKeyName = elektraMalloc (elektraKeyGetNameSize (renamedKey));
				elektraKeyGetName (renamedKey, curKeyName, elektraKeyGetNameSize (renamedKey));

				char * afterParentString = curKeyName + (strlen (parentKeyName));

				doConversion (afterParentString, 0, writeConversion);

				elektraKeySetName (renamedKey, curKeyName);
				elektraFree (curKeyName);
			}
			/*
			 * if something is restored from the parentKey, do
			 * not delete the parentKey (might cause troubles)
			 */
			if (elektraKeyCmp (key, parentKey) != 0)
			{
				elektraKeyDel (elektraKeysetLookup (returned, key, ELEKTRA_KDB_O_POP));
			}
			elektraKeysetAppendKey (returned, renamedKey);
		}
		else
		{
			if (elektraKeyCmp (key, parentKey) != 0)
			{
				elektraKeyDel (elektraKeysetLookupByName (returned, elektraKeyString (elektraKeyGetMeta (key, ELEKTRA_ORIGINAL_NAME_META)), ELEKTRA_KDB_O_POP));
			}
			elektraKeysetAppendKey (returned, key);
		}
	}

	elektraKeyIncRef (parentKey);
	elektraKeysetDel (iterateKs);
	elektraKeyDecRef (parentKey);

	elektraKeysetRewind (returned);
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
