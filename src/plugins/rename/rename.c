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


static Key *cutKeyNamePart(Key *key, Key *parentKey, const char *cutPath)
{
	size_t cutPathLen = strlen(cutPath);
	size_t keyNameLen = strlen(keyName(key));
	size_t parentKeyPathLen = strlen(keyName(parentKey));

	if (keyNameLen <= parentKeyPathLen + cutPathLen + 1) return 0;

	if (!strncmp(keyName(key) + parentKeyPathLen + 1, cutPath, cutPathLen))
	{
		Key *result = keyDup (key);
		keyDel(key);
		keySetName(result, keyName(parentKey));
		keyAddName(result, keyName(key) + parentKeyPathLen + 1 + cutPathLen);
		return result;
	}

	return 0;
}

static Key *restoreKeyName(Key *key)
{
	const Key *origNameKey = keyGetMeta(key, "rename/origname");
	if (origNameKey)
	{
		Key *result = keyDup(key);
		keyDel(key);
		keySetName(result, keyString(origNameKey));
		keySetMeta(result, "rename/origname", 0);
		return result;
	}

	return 0;
}

int elektraRenameGet(Plugin *handle, KeySet *returned, Key *parentKey ELEKTRA_UNUSED)
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
	Key* cutPath = ksLookupByName (config, "/cut", 0);
	KeySet *iterateKs = ksDup(returned);

	ksRewind(iterateKs);

	Key *key;
	while ((key = ksNext (iterateKs)) != 0)
	{
		Key *renamedKey = cutKeyNamePart(key, parentKey, keyString(cutPath));

		if (renamedKey)
		{
			keySetMeta(renamedKey, "rename/origname", keyName(key));
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

