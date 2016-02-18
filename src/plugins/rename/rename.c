/**
 * @file
 *
 * @brief A plugin that converts keys to metakeys and vice versa
 *
 * @copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 *
 */

#include "rename.h"

#ifndef HAVE_KDBCONFIG
# include "kdbconfig.h"
#endif


#include <errno.h>
#include <stdlib.h>
#include <stdbool.h>
#include <stdio.h>
#include <ctype.h>
#include <kdbhelper.h>

#include <kdbprivate.h> // for access to sync bit (keyClearSync)

#define ELEKTRA_ORIGINAL_NAME_META "origname"
#define TOLOWER (-1)
#define TOUPPER 1
// TODO defined privately in keyhelpers.c, API break possible..
char *keyNameGetOneLevel (const char * name, size_t * size);

static void doConversion (char * newName, int levels, int toCase)
{
	int (*conversion)(int);
	
	if(toCase == TOUPPER)
	{
		conversion = toupper;
	}
	else
	{
		conversion = tolower;
	}
	char *returnName = elektraCalloc(strlen(newName)+1);
	if(levels == 0)
	{
		unsigned int i = 0;
		for(;i < strlen(newName); ++i)
		{
			returnName[i] = conversion(newName[i]);
		}
	}
	else
	{
		short levelCount = 0;
		int i = 0;
		for(i = strlen(newName);i >= 0 && levelCount < levels; --i)
		{
			if(newName[i] == '/')
			{
				++levelCount;
				returnName[i] = newName[i];
				continue;
			}
			returnName[i] = conversion(newName[i]);
		}
		for(; i >= 0; --i)
		{
			returnName[i] = newName[i];
		}
	}
	strncpy(newName, returnName, strlen(newName));
	free(returnName);
}

Key * elektraKeyCreateNewName (const Key * key, const Key * parentKey, const char * cutPath, const char * replaceWith, const char * toUpperPath, const char * toLowerPath)
{
	size_t addToLen = 0;
	if(replaceWith != NULL)
		addToLen = strlen(replaceWith);

	size_t maxNewLength = strlen(keyName(key)) + addToLen;

	char *newName = elektraCalloc(maxNewLength+1);
	short replace = 0;
	
	char *parentKeyName = elektraMalloc(keyGetFullNameSize(parentKey));
	keyGetFullName(parentKey, parentKeyName, keyGetFullNameSize(parentKey));
	char *curKeyName = elektraMalloc(keyGetFullNameSize(key));
	keyGetFullName(key, curKeyName, keyGetFullNameSize(key));

	const char *afterParentString = curKeyName+(strlen(parentKeyName));
	char *ptr;

	if(cutPath && (cutPath[0] != '/') && ((ptr = strstr(afterParentString, cutPath)) != NULL))
	{
		strncpy(newName, afterParentString, (ptr - afterParentString));
		if(replaceWith)
		{
			strncpy(newName+strlen(newName), replaceWith, strlen(replaceWith));
		}
		strncat(newName, ptr+(strlen(cutPath)), strlen(afterParentString)-strlen(cutPath));
		replace = 1;
	}
	else
	{
		strncpy(newName, afterParentString, strlen(afterParentString));
	}
	int toLower = toLowerPath ? atoi(toLowerPath) : 0;
	int toUpper = toUpperPath ? atoi(toUpperPath) : 0;

	if(strlen(newName) > 0)
	{
		if(toUpperPath && toLowerPath)
		{
			if(toUpper < toLower)
			{
				doConversion(newName, toLower, TOLOWER);
				doConversion(newName, toUpper, TOUPPER);
			}
			else
			{
				doConversion(newName, toUpper, TOUPPER);
				doConversion(newName, toLower, TOLOWER);
			}
			replace = 1;
		}
		else if(toUpperPath)
		{
			doConversion(newName, toUpper, TOUPPER);
			replace = 1;
		}
		else if(toLowerPath)
		{
			doConversion(newName, toLower, TOLOWER);
			replace = 1;
		}
	}	
	elektraFree(parentKeyName);
	elektraFree(curKeyName);
	if(replace)
	{	
		Key *result = keyDup(key);
		keySetName(result, keyName(parentKey));
		keyAddName(result, newName);
		elektraFree(newName);
		return result;
	}
	elektraFree(newName);
	return 0;
}

static void keyAddUnescapedBasePath (Key * key, const char * path)
{
	size_t size=0;
	char *p=keyNameGetOneLevel(path+size,&size);
	while (*p)
	{
		char *buffer = elektraMalloc(size+1);
		strncpy(buffer, p, size);
		buffer[size] = 0;
		keyAddBaseName(key, buffer);
		elektraFree(buffer);
		p=keyNameGetOneLevel(p+size,&size);
	}
}

static Key * renameGet(Key * key, Key * parentKey, Key * cutConfig, Key * replaceWithConfig, Key * toUpperConfig, Key * toLowerConfig)
{
	char *cutPath = 0;
	char *replaceWith = 0;
	char *toUpperPath = 0;
	char *toLowerPath = 0;
	const Key *cutMeta = keyGetMeta(key, "rename/cut");
	const Key *toMeta = keyGetMeta(key, "rename/to");
	const Key *toUpperMeta = keyGetMeta(key, "rename/toupper");
	const Key *toLowerMeta = keyGetMeta(key, "rename/tolower");
	/* if the meta config exists, it takes precedence over the global config */
	if(cutMeta)
		cutPath = (char *)keyString(cutMeta);
	else if(cutConfig)
		cutPath = (char *)keyString(cutConfig);
	if(toMeta)
		replaceWith = (char *)keyString(toMeta);
	else if(replaceWithConfig)
		replaceWith = (char *)keyString(replaceWithConfig);
	if(toUpperMeta)
		toUpperPath = (char *)keyString(toUpperMeta);
	else if(toUpperConfig)
		toUpperPath = (char *)keyString(toUpperConfig);
	if(toLowerMeta)
		toLowerPath = (char *)keyString(toLowerMeta);
	else if(toLowerConfig)
		toLowerPath = (char *)keyString(toLowerConfig);

	return elektraKeyCreateNewName (key, parentKey, cutPath, replaceWith, toUpperPath, toLowerPath);
}

static Key * restoreKeyName (Key * key, const Key * parentKey, const Key * configKey)
{
	const Key *origNameKey = keyGetMeta (key, ELEKTRA_ORIGINAL_NAME_META);
	if (origNameKey)
	{
		if (strcmp (keyString(origNameKey), keyName(key)))
		{
			int hasSync = keyNeedSync(key); // test_bit(key->flags, KEY_FLAG_SYNC);
			Key *result = keyDup(key);
			keySetName(result, keyString(origNameKey));
			keySetMeta(result, ELEKTRA_ORIGINAL_NAME_META, 0);

			if (!hasSync)
			{
				keyClearSync(result);
			}
			return result;
		}
	}
	else
	{
		if (configKey)
		{
			int hasSync = keyNeedSync(key); // test_bit(key->flags, KEY_FLAG_SYNC);
			Key *result = keyDup(key);
			keySetName(result, keyName(parentKey));
			keyAddUnescapedBasePath(result, keyString(configKey));

			if (keyGetNameSize(key) > keyGetNameSize(parentKey))
			{
				/* this calculation does not work for the parent key but is also not needed */
				const char *relativePath = keyName(key) + keyGetNameSize(parentKey);
				keyAddUnescapedBasePath(result, relativePath);
			}

			if (!hasSync)
			{
				keyClearSync(result);
			}
			return result;
		}
	}

	return 0;
}

int elektraRenameGet (Plugin * handle, KeySet * returned, Key * parentKey)
{
	/* configuration only */
	if (!strcmp (keyName(parentKey), "system/elektra/modules/rename"))
	{
		KeySet *info =
			#include "contract.h"

		ksAppend (returned, info);
		ksDel (info);
		return 1;
	}


	KeySet *config = elektraPluginGetConfig (handle);
	KeySet *iterateKs = ksDup(returned);

	ksRewind(iterateKs);

	Key *cutConfig = ksLookupByName(config, "/cut", KDB_O_NONE);
	Key *toUpper = ksLookupByName(config, "/toupper", KDB_O_NONE);
	Key *toLower = ksLookupByName(config, "/tolower", KDB_O_NONE);
	Key *replaceWith = ksLookupByName(config, "/replacewith", KDB_O_NONE);
	Key *key;
	while ((key = ksNext (iterateKs)) != 0)
	{
		Key *renamedKey = renameGet(key, parentKey, cutConfig, replaceWith, toUpper, toLower);

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
			keySetMeta (key, ELEKTRA_ORIGINAL_NAME_META, keyName(key));
		}

	}

	/* make sure the parent key is not deleted */
	keyIncRef (parentKey);
	ksDel(iterateKs);
	keyDecRef (parentKey);

	return 1; /* success */
}

int elektraRenameSet (Plugin * handle, KeySet * returned, Key * parentKey)
{

	KeySet *iterateKs = ksDup(returned);

	KeySet *config = elektraPluginGetConfig (handle);
	Key *cutConfig = ksLookupByName(config, "/cut", KDB_O_NONE);

	ksRewind(iterateKs);
	Key *key;
	while ((key = ksNext(iterateKs)) != 0)
	{
		Key *renamedKey = restoreKeyName(key, parentKey, cutConfig);

		if (renamedKey)
		{
			/*
			 * if something is restored from the parentKey, do
			 * not delete the parentKey (might cause troubles)
			 */
			if (keyCmp(key, parentKey) != 0)
			{
				ksLookup(returned, key, KDB_O_POP);
				keyDel (key);
			}
			ksAppendKey(returned, renamedKey);
		}
	}

	ksDel(iterateKs);

	return 1; /* success */
}

Plugin * ELEKTRA_PLUGIN_EXPORT (rename)
{
	// clang-format off
	return elektraPluginExport("rename",
		ELEKTRA_PLUGIN_GET,	&elektraRenameGet,
		ELEKTRA_PLUGIN_SET,	&elektraRenameSet,
		ELEKTRA_PLUGIN_END);
}

