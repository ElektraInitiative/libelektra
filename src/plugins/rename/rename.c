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

#define ELEKTRA_ORIGINAL_NAME_META "origname"

// TODO defined privately in keyhelpers.c, API break possible..
char *keyNameGetOneLevel(const char *name, size_t *size);

Key *elektraKeyCutNamePart(const Key *key, const Key *parentKey, const char *cutPath)
{
	size_t cutPathLen = strlen(cutPath);
	size_t keyNameLen = strlen(keyName(key));

	/* marks the position in the key name where the parent key path ends */
	size_t afterParentKey = strlen(keyName(parentKey)) + 1;

	/* marks the position in the key name where the retained parts start */
	size_t afterCut = afterParentKey + cutPathLen;

	if (keyNameLen < afterCut) return 0;

	if (!strncmp(keyName(key) + afterParentKey, cutPath, cutPathLen))
	{
		Key *result = keyDup (key);
		keySetName(result, keyName(parentKey));
		keyAddName(result, keyName(key) + afterCut);
		return result;
	}

	return 0;
}

static void keyAddUnescapedBasePath(Key *key, const char *path)
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

static Key *cutGet(Key *key, Key *parentKey, Key *configKey)
{
	const char *cutPath;
	const Key *cutMeta = keyGetMeta(key, "rename/cut");

	/* if the meta config exists, it takes precedence over the global config */
	cutPath = cutMeta ? keyString(cutMeta) : keyString(configKey);

	return elektraKeyCutNamePart(key, parentKey, cutPath);
}

static Key *restoreKeyName(Key *key, const Key *parentKey, const Key *configKey)
{
	const Key *origNameKey = keyGetMeta (key, ELEKTRA_ORIGINAL_NAME_META);
	if (origNameKey)
	{
		if (strcmp (keyString(origNameKey), keyName(key)))
		{
			Key *result = keyDup(key);
			keySetName(result, keyString(origNameKey));
			keySetMeta(result, ELEKTRA_ORIGINAL_NAME_META, 0);
			return result;
		}
	}
	else
	{
		if (configKey)
		{
			Key *result = keyDup(key);
			keySetName(result, keyName(parentKey));
			keyAddUnescapedBasePath(result, keyString(configKey));

			if (keyGetNameSize(key) > keyGetNameSize(parentKey))
			{
				/* this calculation does not work for the parent key but is also not needed */
				const char *relativePath = keyName(key) + keyGetNameSize(parentKey);
				keyAddUnescapedBasePath(result, relativePath);
			}

			return result;
		}
	}

	return 0;
}

int elektraRenameGet(Plugin *handle, KeySet *returned, Key *parentKey)
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
	Key *key;
	while ((key = ksNext (iterateKs)) != 0)
	{
		Key *renamedKey = cutGet(key, parentKey, cutConfig);

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

int elektraRenameSet(Plugin *handle, KeySet *returned, Key *parentKey)
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

Plugin *ELEKTRA_PLUGIN_EXPORT(rename)
{
	return elektraPluginExport("rename",
		ELEKTRA_PLUGIN_GET,	&elektraRenameGet,
		ELEKTRA_PLUGIN_SET,	&elektraRenameSet,
		ELEKTRA_PLUGIN_END);
}

