/**
 * \file
 *
 * \brief A plugin that converts keys to metakeys and vice versa
 *
 * \copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 *
 */

#include "rename.h"

#ifndef HAVE_KDBCONFIG
# include "kdbconfig.h"
#endif


#include <errno.h>
#include <stdlib.h>
#include <stdbool.h>

#define ORIGINAL_NAME_META "origname"

Key *elektraKeyCutNamePart(const Key *key, const Key *parentKey, const char *cutPath)
{
	size_t cutPathLen = strlen(cutPath);
	size_t keyNameLen = strlen(keyName(key));

	/* marks the position in the key name where the parent key path ends */
	size_t afterParentKey = strlen(keyName(parentKey)) + 1;

	/* marks the position in the key name where the retained parts start */
	size_t afterCut = afterParentKey + cutPathLen;

	if (keyNameLen <= afterCut) return 0;

	if (!strncmp(keyName(key) + afterParentKey, cutPath, cutPathLen))
	{
		Key *result = keyDup (key);
		keySetName(result, keyName(parentKey));
		keyAddName(result, keyName(key) + afterCut);
		return result;
	}

	return 0;
}


static Key *cutGet(Key *key, Key *parentKey, Key *configKey)
{
	const char *cutPath;
	const Key *cutMeta = keyGetMeta(key, "rename/cut");

	/* if the meta config exists, it takes precedence over the global config */
	cutPath = cutMeta ? keyString(cutMeta) : keyString(configKey);

	return elektraKeyCutNamePart(key, parentKey, cutPath);
}

static Key *restoreKeyName(Key *key)
{
	const Key *origNameKey = keyGetMeta (key, ORIGINAL_NAME_META);
	if (origNameKey)
	{
		Key *result = keyDup(key);
		keySetName(result, keyString(origNameKey));
		keySetMeta(result, ORIGINAL_NAME_META, 0);
		return result;
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
			keySetMeta (renamedKey, ORIGINAL_NAME_META, keyName (key));
			ksLookup(returned, key, KDB_O_POP);
			keyDel(key);
			ksAppendKey(returned, renamedKey);
		}
	}

	ksDel(iterateKs);

	return 1; /* success */
}

int elektraRenameSet(Plugin *handle ELEKTRA_UNUSED, KeySet *returned, Key *parentKey ELEKTRA_UNUSED)
{

	KeySet *iterateKs = ksDup(returned);

	ksRewind(iterateKs);
	Key *key;
	while((key = ksNext(iterateKs)) != 0)
	{
		Key *renamedKey = restoreKeyName(key);

		if (renamedKey)
		{
			ksLookup(returned, key, KDB_O_POP);
			keyDel (key);
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

